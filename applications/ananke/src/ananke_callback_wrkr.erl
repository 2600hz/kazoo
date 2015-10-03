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

-export([start_link/3
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

-record(state, {request, attempts, tried = 0, interval, timer}).

-spec start_link(wh_proplist(), integer(), integer()) ->
    {'ok', pid()} | {'error', term()}.
start_link(Req, Attempts, Interval)
  when is_integer(Attempts) andalso Attempts > 0
       andalso is_integer(Interval) andalso Interval > 0 ->
    gen_server:start_link(?MODULE
                          ,#state{request = Req
                                  ,attempts = Attempts
                                  ,interval = Interval * ?MILLISECONDS_IN_SECOND}
                          ,[]).

-spec originate(pid()) -> 'ok'.
originate(Pid) ->
    gen_server:cast(Pid, 'originate').

-spec init(#state{}) -> {'ok', #state{}}.
init(#state{interval = Interval} = State) ->
    Timer = start_originate_timer(Interval),
    {'ok', State#state{timer = Timer}}.

-spec handle_call(any(), any(), #state{}) -> {'noreply', #state{}}.
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

-spec handle_cast('originate', #state{}) -> {'noreply', #state{}} | {'stop', any(), #state{}}.
handle_cast('originate', #state{tried = Tried, attempts = Tried} = State) ->
    lager:info("not answered in ~p attempts, stopping", [Tried]),
    {'stop', 'normal', State};
handle_cast('originate', #state{request = Req, tried = Tried, interval = Timeout} = State) ->
    ReqTimeout = props:get_value(<<"Timeout">>, Req) * ?MILLISECONDS_IN_SECOND,
    TimerTimeout = case (ReqTimeout > Timeout) of
                       'true' -> ReqTimeout + Timeout;
                       'false' -> Timeout
                   end,
    Timer = start_originate_timer(TimerTimeout),
    NewState = State#state{timer = Timer, tried = Tried + 1},

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
            {'noreply', NewState}
    end.

-spec handle_info(any(), #state{}) -> {'noreply', #state{}}.
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec terminate(any(), #state{}) -> 'ok'.
terminate(_, #state{timer = Timer}) ->
    _ = stop_originate_timer(Timer),
    %% supervisor doesn't delete stopped child specification
    ananke_callback_sup:delete_child(self(), 1 * ?MILLISECONDS_IN_SECOND),
    'ok'.

-spec code_change(any(), #state{}, any()) -> {'ok', #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec is_resp(wh_json:object() | wh_json:objects()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    wapi_resource:originate_resp_v(JObj)
    orelse wh_api:error_resp_v(JObj).

-spec start_originate_timer(integer()) -> reference().
start_originate_timer(Timeout) ->
    {'ok', Pid} = leader_cron:schedule_task({'oneshot', Timeout}, {?MODULE, originate, [self()]}),
    Pid.

-spec stop_originate_timer(pid()) -> 'ok' | {'error', term()}.
stop_originate_timer(Timer) ->
    leader_cron:cancel_task(Timer).

-spec handle_originate_response(wh_json:object(), #state{}) -> {'stop', 'normal', #state{}}
                                                             | {'noreply', #state{}}.
handle_originate_response(Resp, State) ->
    case wh_json:get_first_defined([<<"Application-Response">>, <<"Error-Message">>], Resp) of
        <<"SUCCESS">> ->
            lager:info("answered, stopping"),
            {'stop', 'normal', State};
        <<"NORMAL_CLEARING">> ->
            lager:info("busy, will retry later"),
            {'noreply', State};
        _Other ->
            {'noreply', State}
    end.
