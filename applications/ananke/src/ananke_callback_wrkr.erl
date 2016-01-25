%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_callback_wrkr).

-behaviour(gen_server).

-export([start_link/2
         ,originate/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ananke.hrl").

-record(state, {request       :: wh_proplist()
                ,timer        :: api_pid()
                ,schedule     :: pos_integers()
               }).

-type state() :: #state{}.

-spec start_link(wh_proplist(), pos_integers()) ->
    {'ok', pid()} | {'error', term()}.
start_link(Req, [_ | _] = Schedule) ->
    gen_server:start_link(?MODULE
                          ,#state{request = Req
                                  ,schedule = Schedule
                                 }
                          ,[]).

-spec originate(pid()) -> 'ok'.
originate(Pid) ->
    gen_server:cast(Pid, 'originate').

-spec init(state()) -> {'ok', state()}.
init(#state{schedule = [Interval | Schedule]} = State) ->
    Timer = start_originate_timer(Interval * ?MILLISECONDS_IN_SECOND),
    {'ok', State#state{timer = Timer, schedule = Schedule}}.

-spec handle_call(any(), any(), state()) -> {'noreply', state()}.
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

-spec handle_cast('originate', state()) -> {'noreply', state()} | {'stop', any(), state()}.
handle_cast('originate', #state{request = Req} = State) ->
    ReqTimeout = props:get_value(<<"Timeout">>, Req) * ?MILLISECONDS_IN_SECOND,
    NewState = maybe_set_timer(ReqTimeout, State),

    lager:debug("sending originate request"),
    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_resource:publish_originate_req/1
                                     ,fun is_resp/1
                                     ,ReqTimeout + 10 * ?MILLISECONDS_IN_SECOND)
    of
        {'ok', [Resp | _]} ->
            handle_originate_response(Resp, NewState);
        _Other ->
            lager:notice("not called: ~p", [_Other]),
            maybe_stop(NewState)
    end.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_, #state{timer = Timer}) ->
    _ = stop_originate_timer(Timer),
    %% supervisor doesn't delete stopped child specification
    ananke_callback_sup:delete_child(self(), 1 * ?MILLISECONDS_IN_SECOND),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec is_resp(wh_json:object() | wh_json:objects()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    wapi_resource:originate_resp_v(JObj)
    orelse wh_api:error_resp_v(JObj).

-spec maybe_set_timer(pos_integer(), state()) -> state().
maybe_set_timer(ReqTimeout, #state{schedule = [TimeoutS | Schedule]} = State) ->
    Timeout = TimeoutS * ?MILLISECONDS_IN_SECOND,
    TimerTimeout = case (ReqTimeout > Timeout) of
                       'true' -> ReqTimeout + Timeout;
                       'false' -> Timeout
                   end,
    lager:debug("starting timer ~p", [Timeout]),
    Timer = start_originate_timer(TimerTimeout),
    State#state{timer = Timer, schedule = Schedule};
maybe_set_timer(_ReqTimeout, #state{schedule = []} = State) ->
    State#state{timer = 'undefined', schedule = 'undefined'}.

-spec start_originate_timer(integer()) -> pid().
start_originate_timer(Timeout) ->
    {'ok', Pid} = amqp_cron:schedule_task({'oneshot', Timeout}, {?MODULE, originate, [self()]}),
    Pid.

-spec stop_originate_timer(pid()) -> 'ok' | {'error', term()}.
stop_originate_timer('undefined') ->
    'ok';
stop_originate_timer(Timer) ->
    amqp_cron:cancel_task(Timer).

-spec handle_originate_response(wh_json:object(), state()) -> {'stop', 'normal', state()}
                                                             | {'noreply', state()}.
handle_originate_response(Resp, State) ->
    case wh_json:get_first_defined([<<"Application-Response">>, <<"Error-Message">>], Resp) of
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

-spec maybe_stop(state()) -> {'stop', 'normal', state()} | {'noreply', state()}.
maybe_stop(#state{timer = 'undefined'} = State) ->
    lager:info("timer has not been set, stopping"),
    {'stop', 'normal', State};
maybe_stop(State) ->
    {'noreply', State}.
