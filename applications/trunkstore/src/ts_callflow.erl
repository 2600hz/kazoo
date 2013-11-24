%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Common functionality for onnet and offnet call handling
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_callflow).

-export([init/1
         ,start_amqp/1
         ,send_park/1
         ,wait_for_win/1
         ,wait_for_bridge/1
         ,wait_for_cdr/1
         ,send_hangup/1, send_hangup/2
         ,finish_leg/2
        ]).

%% data access functions
-export([get_request_data/1
         ,get_my_queue/1
         ,get_control_queue/1
         ,set_endpoint_data/2
         ,set_account_id/2
         ,get_aleg_id/1
         ,get_bleg_id/1
         ,get_call_cost/1
         ,set_failover/2
         ,get_failover/1
         ,get_endpoint_data/1
         ,get_account_id/1
        ]).

-include("ts.hrl").

-define(WAIT_FOR_WIN_TIMEOUT, 5000). %% 5 seconds
-define(WAIT_FOR_BRIDGE_TIMEOUT, 30000). %% 30 secs
-define(WAIT_FOR_HANGUP_TIMEOUT, 1000 * 60 * 60 * 1). %% 1 hour
-define(WAIT_FOR_CDR_TIMEOUT, 5000).

-type ts_state() :: #ts_callflow_state{}.

-spec init(wh_json:object()) -> ts_state() |
                                      {'error', 'not_ts_account'}.
init(RouteReqJObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    put(callid, CallID),

    case is_trunkstore_acct(RouteReqJObj) of
        false ->
            lager:info("request is not for a trunkstore account"),
            {error, not_ts_account};
        true ->
            AcctID = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], RouteReqJObj),

            #ts_callflow_state{
                        aleg_callid=CallID
                        ,route_req_jobj=RouteReqJObj
                        ,acctid=AcctID
                        ,acctdb=wh_util:format_account_id(AcctID, encoded)
                       }
    end.

-spec start_amqp(ts_state()) -> ts_state().
start_amqp(#ts_callflow_state{}=State) ->
    Q = amqp_util:new_queue(),

    %% Bind the queue to an exchange
    _ = amqp_util:bind_q_to_targeted(Q),
    _ = amqp_util:basic_consume(Q, [{exclusive, false}]),

    lager:info("started AMQP with queue ~s", [Q]),
    State#ts_callflow_state{my_q=Q}.

-spec send_park(ts_state()) -> ts_state().
send_park(#ts_callflow_state{aleg_callid=CallID, my_q=Q, route_req_jobj=JObj
                             ,acctid=AccountId}=State) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Pre-Park">>, pre_park_action()}
            ,{<<"Method">>, <<"park">>}
            ,{<<"From-Realm">>, wh_util:get_account_realm(AccountId)}
            ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    lager:info("trunkstore knows how to route this call, sending park route response"),
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp),
    _ = amqp_util:bind_q_to_callevt(Q, CallID),
    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    _ = amqp_util:basic_consume(Q, [{exclusive, false}]), %% need to verify if this step is needed
    State.

-spec wait_for_win(ts_state()) -> {'won' | 'lost', ts_state()}.
wait_for_win(#ts_callflow_state{aleg_callid=CallID}=State) ->
    receive
        #'basic.consume_ok'{} -> wait_for_win(State);

        %% call events come from callevt exchange, ignore for now
        {#'basic.deliver'{exchange = <<"targeted">>}, #amqp_msg{payload=Payload}} ->
            WinJObj = wh_json:decode(Payload),
            true = wapi_route:win_v(WinJObj),
            CallID = wh_json:get_value(<<"Call-ID">>, WinJObj),

            CallctlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),

            lager:info("callflow has received a route win, taking control of the call"),

            {won, State#ts_callflow_state{callctl_q=CallctlQ}}
    after ?WAIT_FOR_WIN_TIMEOUT ->
            lager:info("timed out(~b) waiting for route_win, going down", [?WAIT_FOR_WIN_TIMEOUT]),
            {lost, State}
    end.

-spec wait_for_bridge(ts_state()) ->
                             {'bridged' | 'error' | 'hangup' | 'timeout', ts_state()}.
-spec wait_for_bridge(ts_state(), integer()) ->
                             {'bridged' | 'error' | 'hangup' | 'timeout', ts_state()}.
wait_for_bridge(State) ->
    wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT).
wait_for_bridge(State, Timeout) ->
    receive
        #'basic.consume_ok'{} -> wait_for_bridge(State, Timeout);
        {_, #amqp_msg{payload=Payload}} ->
            JObj = wh_json:decode(Payload),
            case process_event_for_bridge(State, JObj) of
                'ignore' ->
                    wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT);
                {'bridged', _}=Success -> Success;
                {'error', _}=Error -> Error;
                {'hangup', _}=Hangup -> Hangup
            end;
        _E ->
            lager:info("unexpected msg: ~p", [_E]),
            wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT)
    end.

-spec process_event_for_bridge(ts_state(), wh_json:object()) ->
                                      'ignore' |
                                      {'bridged' | 'error' | 'hangup', ts_state()}.
process_event_for_bridge(#ts_callflow_state{aleg_callid=ALeg
                                            ,my_q=Q
                                            ,callctl_q=CtlQ
                                           }=State, JObj) ->
    case { wh_json:get_value(<<"Application-Name">>, JObj)
           ,wh_json:get_value(<<"Event-Name">>, JObj)
           ,wh_json:get_value(<<"Event-Category">>, JObj) } of

        {_, <<"offnet_resp">>, <<"resource">>} ->
            case wh_json:get_value(<<"Response-Message">>, JObj) of
                <<"SUCCESS">> ->
                    lager:info("offnet bridge has completed"),
                    lager:info("~p", [JObj]),
                    {'hangup', State};
                _Err ->
                    Failure = wh_json:get_value(<<"Error-Message">>, JObj, wh_json:get_value(<<"Response-Code">>, JObj)),
                    lager:info("offnet failed: ~s(~s)", [Failure, _Err]),
                    {'error', State}
            end;

        { _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
            BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
            lager:info("bridged from ~s to ~s successful", [ALeg, BLeg]),

            _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
            _ = amqp_util:basic_consume(Q),
            {'bridged', State#ts_callflow_state{bleg_callid=BLeg}};

        { _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
            lager:info("channel hungup before bridge"),
            {'hangup', State};

        { _, _, <<"error">> } ->
            lager:info("execution failed"),
            {'error', State};

        {_, <<"call_detail">>, <<"cdr">> } ->
            true = wapi_call:cdr_v(JObj),
            Leg = wh_json:get_value(<<"Call-ID">>, JObj),
            Duration = ts_util:get_call_duration(JObj),

            lager:info("CDR received for leg ~s", [Leg]),
            lager:info("leg to be billed for ~b seconds", [Duration]),
            {error, State};

        {_, <<"cdr">>, <<"call_detail">> } ->
            true = wapi_call:cdr_v(JObj),
            Leg = wh_json:get_value(<<"Call-ID">>, JObj),
            Duration = ts_util:get_call_duration(JObj),

            lager:info("CDR received for leg ~s", [Leg]),
            lager:info("leg to be billed for ~b seconds", [Duration]),
            {error, State};

        { _, <<"resource_error">>, <<"resource">> } ->
            Code = wh_json:get_value(<<"Failure-Code">>, JObj, <<"486">>),
            Message = wh_json:get_value(<<"Failure-Message">>, JObj),

            lager:info("failed to bridge to offnet"),
            lager:info("failure message: ~s", [Message]),
            lager:info("failure code: ~s", [Code]),

            %% send failure code to Call
            _ = wh_call_response:send(ALeg, CtlQ, Code, Message),

            {hangup, State};

        {<<"answer">>,<<"CHANNEL_EXECUTE_COMPLETE">>,<<"call_event">>} ->
            %% support one legged bridges such as on-net conference
            lager:info("successful one legged bridge", []),
            {bridged, State};

        {<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>} ->
            Resp = wh_json:get_value(<<"Application-Response">>, JObj),

            lager:info("bridge completed unexpectedly: ~s(~s)", [Resp, wh_json:get_value(<<"Hangup-Cause">>, JObj)]),

            case lists:member(Resp, ?SUCCESSFUL_HANGUPS) of
                true -> {hangup, State};
                false -> {error, State}
            end;
        _Unhandled ->
            lager:info("unhandled combo: ~p", [_Unhandled]),
            ignore
    end.

-spec wait_for_cdr(ts_state()) ->
                                {'timeout', ts_state()} |
                                {'cdr', 'aleg' | 'bleg', wh_json:object(), ts_state()}.
-spec wait_for_cdr(ts_state(), pos_integer() | 'infinity') ->
                                {'timeout', ts_state()} |
                                {'cdr', 'aleg' | 'bleg', wh_json:object(), ts_state()}.
wait_for_cdr(State) ->
    wait_for_cdr(State, 10000).
wait_for_cdr(State, Timeout) ->
    receive
        #'basic.consume_ok'{} -> wait_for_cdr(State, Timeout);
        {_, #amqp_msg{payload=Payload}} ->
            JObj = wh_json:decode(Payload),
            case process_event_for_cdr(State, JObj) of
                {cdr, _, _, _}=CDR -> CDR;
                {hangup, State1} ->
                    send_hangup(State1),
                    wait_for_cdr(State1, ?WAIT_FOR_CDR_TIMEOUT);
                ignore -> wait_for_cdr(State, Timeout)
            end
    end.

-spec process_event_for_cdr(ts_state(), wh_json:object()) ->
                                         {'hangup', ts_state()} | 'ignore' |
                                         {'cdr', 'aleg' | 'bleg', wh_json:object(), ts_state()}.
process_event_for_cdr(#ts_callflow_state{aleg_callid=ALeg}=State, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"resource">>, <<"offnet_resp">>} ->
            case wh_json:get_value(<<"Response-Message">>, JObj) of
                <<"SUCCESS">> ->
                    lager:info("bridge was successful, still waiting on the CDR"),
                    ignore;
                _Else ->
                    Failure = wh_json:get_value(<<"Error-Message">>, JObj, wh_json:get_value(<<"Response-Code">>, JObj)),
                    lager:info("offnet failed: ~s but waiting for the CDR still", [Failure]),
                    ignore
            end;

        { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
            lager:info("Hangup received, waiting on CDR"),
            {hangup, State};

        { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
            lager:info("Unbridge received, waiting on CDR"),
            {hangup, State};

        { <<"call_event">>, <<"CHANNEL_DESTROY">> } ->
            lager:info("channel has been destroyed"),
            {hangup, State};

        { <<"error">>, _ } ->
            lager:info("Error received, waiting on CDR"),
            {hangup, State};

        { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case {wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Application-Response">>, JObj)} of
                {<<"bridge">>, <<"SUCCESS">>} ->
                    lager:info("bridge event finished successfully, sending hangup"),
                    send_hangup(State),
                    ignore;
                {<<"bridge">>, Cause} ->
                    lager:info("failed to bridge: ~s", [Cause]),
                    {error, State};
                {_,_} ->
                    ignore
            end;
        { <<"call_detail">>, <<"cdr">> } ->
            true = wapi_call:cdr_v(JObj),
            Leg = wh_json:get_value(<<"Call-ID">>, JObj),
            Duration = ts_util:get_call_duration(JObj),

            lager:info("CDR received for leg ~s", [Leg]),
            lager:info("leg to be billed for ~b seconds", [Duration]),

            case Leg =:= ALeg of
                true -> {cdr, aleg, JObj, State};
                false -> {cdr, bleg, JObj, State}
            end;
        _E ->
            lager:info("ignorable event: ~p", [_E]),
            ignore
    end.

-spec finish_leg(ts_state(), api_binary()) -> 'ok'.
finish_leg(_State, undefined) -> ok;
finish_leg(#ts_callflow_state{}=State, _Leg) ->
    send_hangup(State).

-spec send_hangup(ts_state()) -> 'ok'.
send_hangup(#ts_callflow_state{callctl_q = <<>>}) -> ok;
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}) -> ok;
send_hangup(#ts_callflow_state{callctl_q=CtlQ, my_q=Q, aleg_callid=CallID}) ->
    Command = [
               {<<"Application-Name">>, <<"hangup">>}
               ,{<<"Call-ID">>, CallID}
               ,{<<"Insert-At">>, <<"now">>}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    lager:info("Sending hangup to ~s: ~p", [CtlQ, Command]),
    wapi_dialplan:publish_command(CtlQ, Command).

send_hangup(#ts_callflow_state{callctl_q = <<>>}, _) -> ok;
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}, _) -> ok;
send_hangup(#ts_callflow_state{callctl_q=CtlQ, aleg_callid=CallId}, Code) ->
    lager:debug("responding to aleg with ~p", [Code]),
    wh_call_response:send(CallId, CtlQ, Code).

%%%-----------------------------------------------------------------------------
%%% Data access functions
%%%-----------------------------------------------------------------------------
-spec get_request_data(ts_state()) -> wh_json:object().
get_request_data(#ts_callflow_state{route_req_jobj=JObj}) ->
    JObj.

-spec set_endpoint_data(ts_state(), wh_json:object()) -> ts_state().
set_endpoint_data(State, Data) ->
    State#ts_callflow_state{ep_data=Data}.

-spec get_endpoint_data(ts_state()) -> wh_json:object().
get_endpoint_data(#ts_callflow_state{ep_data=EP}) ->
    EP.

-spec set_account_id(ts_state(), ne_binary()) -> ts_state().
set_account_id(State, ID) ->
    State#ts_callflow_state{acctid=ID}.

-spec get_account_id(ts_state()) -> ne_binary().
get_account_id(#ts_callflow_state{acctid=ID}) ->
    ID.

-spec get_my_queue(ts_state()) -> ne_binary().
-spec get_control_queue(ts_state()) -> ne_binary().
get_my_queue(#ts_callflow_state{my_q=Q}) ->
    Q.
get_control_queue(#ts_callflow_state{callctl_q=CtlQ}) ->
    CtlQ.

-spec get_aleg_id(ts_state()) -> ne_binary().
-spec get_bleg_id(ts_state()) -> ne_binary().
get_aleg_id(#ts_callflow_state{aleg_callid=ALeg}) ->
    ALeg.
get_bleg_id(#ts_callflow_state{bleg_callid=ALeg}) ->
    ALeg.

-spec get_call_cost(ts_state()) -> float().
get_call_cost(#ts_callflow_state{call_cost=Cost}) ->
    Cost.

-spec set_failover(ts_state(), wh_json:object()) -> ts_state().
set_failover(State, Failover) ->
    State#ts_callflow_state{failover=Failover}.

-spec get_failover(ts_state()) -> wh_json:object() | 'undefined'.
get_failover(#ts_callflow_state{failover=Fail}) ->
    Fail.

-spec is_trunkstore_acct(wh_json:object()) -> boolean().
is_trunkstore_acct(JObj) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-Type">>], JObj) of
        <<"sys_info">> -> true;
        undefined -> true;
        _ -> false
    end.

-spec pre_park_action() -> ne_binary().
pre_park_action() ->
    case whapps_config:get_is_true(<<"trunkstore">>, <<"ring_ready_offnet">>, 'true') of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.
