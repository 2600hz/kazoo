%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_callback_worker).
-behaviour(gen_server).

-export([start_link/2
        ,start_link/3
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ananke.hrl").

-define(SERVER, ?MODULE).

-record(state, {request      :: kz_term:proplist() | 'undefined'
               ,timer        :: kz_term:api_reference()
               ,schedule     :: pos_integers() | 'undefined'
               ,check = 'true' :: check_fun()
               }).

-type state() :: #state{}.

-spec start_link(kz_term:proplist(), pos_integers()) -> {'ok', pid()} | {'error', any()}.
start_link(Req, [_ | _] = Schedule) ->
    start_link(#state{request = Req, schedule = Schedule}).

-spec start_link(kz_term:proplist(), pos_integers(), check_fun()) -> {'ok', pid()} | {'error', any()}.
start_link(Req, [_ | _] = Schedule, CheckFun) ->
    start_link(#state{request = Req
                     ,schedule = Schedule
                     ,check = CheckFun
                     }).

-spec start_link(state()) -> {'ok', pid()} | {'error', any()}.
start_link(#state{} = State) ->
    gen_server:start_link(?SERVER, State, []).

-spec init(state()) -> {'ok', state()}.
init(#state{schedule = [Interval | Schedule]} = State) ->
    Timer = start_originate_timer(Interval * ?MILLISECONDS_IN_SECOND),
    {'ok', State#state{timer = Timer, schedule = Schedule}}.

-spec handle_call(any(), any(), state()) -> {'noreply', state()}.
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info('originate', state()) -> {'noreply', state()} | {'stop', any(), state()}.
handle_info('originate', #state{request = Req} = State) ->
    ReqTimeout = props:get_value(<<"Timeout">>, Req) * ?MILLISECONDS_IN_SECOND,
    Routines = [{fun maybe_set_timer/2, ReqTimeout}
               ,{fun check_condition/2, {}}
               ,{fun send_request/2, Req}
               ],
    return(State, Routines).

-spec terminate(any(), state()) -> 'ok'.
terminate(_, #state{timer = Timer}) ->
    _ = stop_originate_timer(Timer),
    %% supervisor doesn't delete stopped child specification
    ananke_tasks_sup:delete_child(self(), 1 * ?MILLISECONDS_IN_SECOND),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec is_resp(kz_json:object() | kz_json:objects()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    kapi_resource:originate_resp_v(JObj)
        orelse kz_api:error_resp_v(JObj).

-type routine_ret() :: state() | {'stop', any(), state()} | 'stop' | 'continue'.
-type routine_fun() :: fun((state(), any()) -> routine_ret()).
-type routine() :: {routine_fun(), any()}.
-type routines() :: [routine()].

-spec return(state(), routines()) -> {'stop', any(), state()} | {'noreply', state()}.
return(#state{} = State, [{Fun, Args} | Routines]) ->
    case Fun(State, Args) of
        #state{} = NewState ->
            return(NewState, Routines);
        'stop' -> {'stop', 'normal', State};
        {'stop', _, #state{}} = Return ->
            Return;
        'continue' ->
            return(State, Routines)
    end;
return(#state{} = NewState, []) ->
    {'noreply', NewState}.

-spec check_condition(state(), any()) -> routine_ret().
check_condition(#state{check = 'true'}, _) ->
    'continue';
check_condition(#state{check = {Module, Fun, Args}}, _) ->
    case erlang:apply(Module, Fun, Args) of
        'true' -> 'continue';
        'false' ->
            lager:info("condition failed, stopping"),
            'stop'
    end;
check_condition(#state{check = Fun}, _) when is_function(Fun, 0) ->
    case Fun() of
        'true' -> 'continue';
        'false' ->
            lager:info("condition failed, stopping"),
            'stop'
    end.

-spec send_request(state(), kz_term:proplist()) -> routine_ret().
send_request(State, Req) ->
    lager:debug("sending originate request"),
    ReqTimeout = props:get_value(<<"Timeout">>, Req) * ?MILLISECONDS_IN_SECOND,
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_resource:publish_originate_req/1
                                    ,fun is_resp/1
                                    ,ReqTimeout + 10 * ?MILLISECONDS_IN_SECOND)
    of
        {'ok', [Resp | _]} ->
            handle_originate_response(Resp, State);
        _Other ->
            lager:notice("not called: ~p", [_Other]),
            maybe_stop(State)
    end.

-spec maybe_set_timer(state(), pos_integer()) -> state().
maybe_set_timer(#state{schedule = [TimeoutS | Schedule]} = State, ReqTimeout) ->
    Timeout = TimeoutS * ?MILLISECONDS_IN_SECOND,
    TimerTimeout = case (ReqTimeout > Timeout) of
                       'true' -> ReqTimeout + Timeout;
                       'false' -> Timeout
                   end,
    Timer = start_originate_timer(TimerTimeout),
    State#state{timer = Timer, schedule = Schedule};
maybe_set_timer(#state{schedule = []} = State, _ReqTimeout) ->
    State#state{timer = 'undefined', schedule = 'undefined'}.

-spec start_originate_timer(integer()) -> reference().
start_originate_timer(Timeout) ->
    lager:debug("starting timer ~pms", [Timeout]),
    erlang:send_after(Timeout, self(), 'originate').

-spec stop_originate_timer(reference()) -> 'ok' | {'error', any()}.
stop_originate_timer('undefined') -> 'ok';
stop_originate_timer(Timer) ->
    erlang:cancel_timer(Timer).

-spec handle_originate_response(kz_json:object(), state()) -> {'stop', 'normal', state()} |
                                                              state().
handle_originate_response(Resp, State) ->
    case kz_json:get_first_defined([<<"Application-Response">>, <<"Error-Message">>], Resp) of
        <<"SUCCESS">> ->
            lager:info("answered, stopping"),
            {'stop', 'normal', State};
        <<"NORMAL_CLEARING">> ->
            lager:info("busy, will try later"),
            maybe_stop(State);
        <<"NO_ANSWER">> ->
            lager:info("not answered, will try later"),
            maybe_stop(State);
        _Other ->
            lager:notice("unexpected response ~p, will try later", [_Other]),
            maybe_stop(State)
    end.

-spec maybe_stop(state()) -> {'stop', 'normal', state()} | state().
maybe_stop(#state{timer = 'undefined'} = State) ->
    lager:info("timer has not been set, stopping"),
    {'stop', 'normal', State};
maybe_stop(State) ->
    State.
