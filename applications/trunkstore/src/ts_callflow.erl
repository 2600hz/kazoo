%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Common functionality for onnet and offnet call handling
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_callflow).

-export([init/2
         ,start_amqp/1
         ,send_park/1
         ,wait_for_win/1
         ,wait_for_bridge/1
         ,send_hangup/1
         ,send_hangup/2
        ]).

%% data access functions
-export([get_request_data/1
         ,get_my_queue/1
         ,get_control_queue/1
         ,get_custom_channel_vars/1
         ,get_custom_sip_headers/1
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

-define(WAIT_FOR_WIN_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND). %% 5 seconds

-type state() :: #ts_callflow_state{}.

-export_type([state/0]).

-spec init(wh_json:object(), api_binary()) ->
                  state() | {'error', 'not_ts_account'}.
init(RouteReqJObj, Type) ->
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    wh_util:put_callid(CallID),
    case is_trunkstore_acct(RouteReqJObj, Type) of
        'false' ->
            lager:info("request is not for a trunkstore account"),
            {'error', 'not_ts_account'};
        'true' ->
            AcctID = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], RouteReqJObj),
            #ts_callflow_state{
               aleg_callid=CallID
               ,route_req_jobj=RouteReqJObj
               ,acctid=AcctID
               ,acctdb=wh_util:format_account_id(AcctID, 'encoded')
              }
    end.

-spec start_amqp(state()) -> state().
start_amqp(#ts_callflow_state{}=State) ->
    %% Trunkstore is pre-gen_listener so do it
    %% manually till it can be refactored
    Q = amqp_util:new_queue(),
    _ = wapi_self:bind_q(Q, []),
    _ = amqp_util:basic_consume(Q, [{'exclusive', 'false'}]),
    lager:info("started AMQP with queue ~s", [Q]),
    State#ts_callflow_state{my_q=Q}.

-spec send_park(state()) -> state().
send_park(#ts_callflow_state{my_q=Q
                             ,route_req_jobj=JObj
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
    State.

-spec wait_for_win(state()) -> {'won' | 'lost', state()}.
wait_for_win(#ts_callflow_state{aleg_callid=CallID
                                ,my_q=Q}=State) ->
    receive
        #'basic.consume_ok'{} -> wait_for_win(State);
        %% call events come from callevt exchange, ignore for now
        {#'basic.deliver'{exchange = <<"targeted">>}, #amqp_msg{payload=Payload}} ->
            WinJObj = wh_json:decode(Payload),
            'true' = wapi_route:win_v(WinJObj),
            CallID = wh_json:get_value(<<"Call-ID">>, WinJObj),
            CallctlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),
            lager:info("callflow has received a route win, taking control of the call"),
            wapi_call:bind_q(Q, [{'callid', CallID}]),
            {'won', State#ts_callflow_state{callctl_q=CallctlQ}}
    after ?WAIT_FOR_WIN_TIMEOUT ->
            lager:info("timed out(~b) waiting for route_win, going down", [?WAIT_FOR_WIN_TIMEOUT]),
            {'lost', State}
    end.

-spec wait_for_bridge(state()) -> {'hangup' | 'error', state()}.
wait_for_bridge(State) ->
    receive
        #'basic.consume_ok'{} -> wait_for_bridge(State);
        {_, #amqp_msg{payload=Payload}} ->
            JObj = wh_json:decode(Payload),
            case process_event_for_bridge(State, JObj) of
                'ignore' -> wait_for_bridge(State);
                {'error', _}=Error -> Error;
                {'hangup', _}=Hangup -> Hangup
            end;
        _E ->
            lager:info("unexpected msg: ~p", [_E]),
            wait_for_bridge(State)
    end.

-spec process_event_for_bridge(state(), wh_json:object()) ->
                                      'ignore' | {'hangup' | 'error', state()}.
process_event_for_bridge(#ts_callflow_state{aleg_callid=ALeg
                                            ,callctl_q=CtlQ
                                           }=State, JObj) ->
    case get_event_type(JObj) of
        {<<"resource">>, <<"offnet_resp">>, _} ->
            case wh_json:get_value(<<"Response-Message">>, JObj) of
                <<"SUCCESS">> ->
                    lager:info("offnet bridge has completed successfully"),
                    {'hangup', State};
                _Err ->
                    Failure = wh_json:get_first_defined([<<"Error-Message">>
                                                         ,<<"Response-Code">>
                                                        ], JObj),
                    lager:info("offnet failed: ~s ~s", [Failure, _Err]),
                    {'error', State}
            end;
        {<<"resource">>, <<"resource_error">>, _} ->
            Code = wh_json:get_value(<<"Failure-Code">>, JObj, <<"486">>),
            Message = wh_json:get_value(<<"Failure-Message">>, JObj),
            lager:info("offnet failed: ~s ~s", [Code, Message]),
            %% send failure code to Call
            _ = wh_call_response:send(ALeg, CtlQ, Code, Message),
            {'hangup', State};
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            lager:info("channel hungup before bridge"),
            {'hangup', State};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
            case wh_json:get_value(<<"Disposition">>, JObj) =:= <<"SUCCESS">>
                orelse
                wh_json:get_first_defined([<<"Application-Response">>
                                           ,<<"Hangup-Cause">>
                                          ], JObj, <<"UNSPECIFIED">>) =:= <<"SUCCESS">>
            of
                'true' ->
                    lager:info("bridge completed sucessfully", []),
                    {'hangup', State};
                'false' ->
                    lager:info("bridge failed: ~s"
                               ,[wh_json:encode(JObj)]),
                    {'error', State}
             end;
        {<<"error">>, _, <<"bridge">>} ->
            lager:debug("channel execution error while waiting for bridge: ~s"
                        ,[wh_json:encode(JObj)]),
            {'error', State};
        {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>,<<"answer">>} ->
            %% support one legged bridges such as on-net conference
            lager:info("channel was answered", []),
            'ignore';
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
            BLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            lager:debug("channel ~s bridged to ~s", [ALeg, BLeg]),
            'ignore';
        _Unhandled ->
            lager:info("unhandled combo: ~p", [_Unhandled]),
            'ignore'
    end.

-spec get_event_type(wh_json:object()) -> {api_binary(), api_binary(), api_binary()}.
get_event_type(JObj) ->
    {C, N} = wh_util:get_event_type(JObj),
    {C, N, get_app(JObj)}.

get_app(JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        'undefined' -> wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj);
        App -> App
    end.

-spec send_hangup(state()) -> 'ok'.
send_hangup(#ts_callflow_state{callctl_q = <<>>}) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q=CtlQ
                               ,my_q=Q
                               ,aleg_callid=CallID}) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Call-ID">>, CallID}
               ,{<<"Insert-At">>, <<"now">>}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    lager:info("Sending hangup to ~s: ~p", [CtlQ, Command]),
    wapi_dialplan:publish_command(CtlQ, Command).

send_hangup(#ts_callflow_state{callctl_q = <<>>}, _) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}, _) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q=CtlQ
                               ,aleg_callid=CallId}, Code) ->
    lager:debug("responding to aleg with ~p", [Code]),
    wh_call_response:send(CallId, CtlQ, Code).

%%%-----------------------------------------------------------------------------
%%% Data access functions
%%%-----------------------------------------------------------------------------
-spec get_request_data(state()) -> wh_json:object().
get_request_data(#ts_callflow_state{route_req_jobj=JObj}) -> JObj.

-spec get_custom_channel_vars(state()) -> wh_json:object().
get_custom_channel_vars(#ts_callflow_state{route_req_jobj=JObj}) ->
    wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()).

-spec get_custom_sip_headers(state()) -> api_object().
get_custom_sip_headers(#ts_callflow_state{route_req_jobj=JObj}) ->
    wh_json:get_value(<<"Custom-SIP-Headers">>, JObj).

-spec set_endpoint_data(state(), wh_json:object()) -> state().
set_endpoint_data(State, Data) -> State#ts_callflow_state{ep_data=Data}.

-spec get_endpoint_data(state()) -> wh_json:object().
get_endpoint_data(#ts_callflow_state{ep_data=EP}) -> EP.

-spec set_account_id(state(), ne_binary()) -> state().
set_account_id(State, ID) -> State#ts_callflow_state{acctid=ID}.

-spec get_account_id(state()) -> ne_binary().
get_account_id(#ts_callflow_state{acctid=ID}) -> ID.

-spec get_my_queue(state()) -> ne_binary().
-spec get_control_queue(state()) -> ne_binary().
get_my_queue(#ts_callflow_state{my_q=Q}) -> Q.
get_control_queue(#ts_callflow_state{callctl_q=CtlQ}) -> CtlQ.

-spec get_aleg_id(state()) -> api_binary().
-spec get_bleg_id(state()) -> api_binary().
get_aleg_id(#ts_callflow_state{aleg_callid=ALeg}) -> ALeg.
get_bleg_id(#ts_callflow_state{bleg_callid=ALeg}) -> ALeg.

-spec get_call_cost(state()) -> float().
get_call_cost(#ts_callflow_state{call_cost=Cost}) -> Cost.

-spec set_failover(state(), wh_json:object()) -> state().
set_failover(State, Failover) -> State#ts_callflow_state{failover=Failover}.

-spec get_failover(state()) -> wh_json:object() | 'undefined'.
get_failover(#ts_callflow_state{failover=Fail}) -> Fail.

-spec is_trunkstore_acct(wh_json:object(), api_binary()) -> boolean().
is_trunkstore_acct(JObj, Type) ->
    Type =:= wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-Type">>], JObj).

-spec pre_park_action() -> ne_binary().
pre_park_action() ->
    case whapps_config:get_is_true(<<"trunkstore">>, <<"ring_ready_offnet">>, 'true') of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.
