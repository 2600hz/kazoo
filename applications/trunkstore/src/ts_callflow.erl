%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Common functionality for onnet and offnet call handling
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_callflow).

-export([init/2
        ,start_amqp/1
        ,cleanup_amqp/1
        ,send_park/1
        ,wait_for_bridge/2
        ,send_hangup/1, send_hangup/2
        ,send_command/3
        ]).

%% data access functions
-export([get_request_data/1
        ,get_control_queue/1
        ,get_worker_queue/1
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
        ,get_kapps_call/1
        ]).

-include("ts.hrl").

-define(WAIT_FOR_WIN_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-type state() :: #ts_callflow_state{}.
-type event_type() :: {kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()}.

-export_type([state/0]).

-spec init(kapi_route:req(), kz_term:api_binary() | kz_term:api_binaries()) ->
                  state() |
                  {'error', 'not_ts_account'}.
init(RouteReqJObj, Type) ->
    CallID = kapi_route:call_id(RouteReqJObj),
    kz_util:put_callid(CallID),
    case is_trunkstore_acct(RouteReqJObj, Type) of
        'false' ->
            lager:info("request is not for a trunkstore account"),
            {'error', 'not_ts_account'};
        'true' ->
            AccountId = kapi_route:account_id(RouteReqJObj),
            #ts_callflow_state{aleg_callid=CallID
                              ,route_req_jobj=RouteReqJObj
                              ,acctid=AccountId
                              ,acctdb=kz_util:format_account_id(AccountId, 'encoded')
                              ,kapps_call=kapps_call:from_route_req(RouteReqJObj)
                              }
    end.

-spec start_amqp(state()) -> state().
start_amqp(#ts_callflow_state{}=State) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    lager:debug("using worker ~p", [Worker]),
    State#ts_callflow_state{amqp_worker=Worker}.

-spec cleanup_amqp(state()) -> 'ok'.
cleanup_amqp(#ts_callflow_state{amqp_worker=Worker
                               ,aleg_callid=CallId
                               }) ->
    gen_listener:rm_binding(Worker, 'call', [{'callid', CallId}]),
    kz_amqp_worker:stop_relay(Worker, self()).

-spec send_park(state()) -> {'won' | 'lost', state()}.
send_park(#ts_callflow_state{route_req_jobj=JObj
                            ,acctid=AccountId
                            ,amqp_worker=Worker
                            }=State) ->
    Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
           ,{<<"Routes">>, []}
           ,{<<"Pre-Park">>, pre_park_action()}
           ,{<<"Method">>, <<"park">>}
           ,{<<"From-Realm">>, kzd_accounts:fetch_realm(AccountId)}
           ,{<<"Custom-Channel-Vars">>, kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())}
           ,{<<"Custom-Application-Vars">>, kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj)}
            | kz_api:default_headers(get_worker_queue(State)
                                    ,?APP_NAME, ?APP_VERSION
                                    )
           ],
    lager:info("trunkstore knows how to route this call, sending park route response"),
    kz_amqp_worker:relay_to(Worker, self()),
    _ = kz_amqp_worker:cast(Resp
                           ,fun(API) -> kapi_route:publish_resp(kz_api:server_id(JObj), API) end
                           ,Worker
                           ),
    wait_for_win(State, ?WAIT_FOR_WIN_TIMEOUT).

-spec wait_for_win(state(), pos_integer()) -> {'won' | 'lost', state()}.
wait_for_win(State, Timeout) ->
    wait_for_win(State, Timeout, kapps_call_command:receive_event(Timeout)).

-spec wait_for_win(state(), pos_integer(), kapps_call_command:request_return()) ->
                          {'won' | 'lost', state()}.
wait_for_win(State, Timeout, {'ok', JObj}) ->
    case kapi_route:win_v(JObj) of
        'true' -> route_won(State, JObj);
        'false' -> wait_for_win(State, Timeout)
    end;
wait_for_win(State, _Timeout, {'error', 'timeout'}) ->
    lager:info("failed to receive route_win"),
    {'lost', State}.

-spec route_won(state(), kz_json:object()) -> {'won', state()}.
route_won(#ts_callflow_state{amqp_worker=Worker, kapps_call=Call}=State, RouteWin) ->
    gen_listener:add_binding(Worker
                            ,'call'
                            ,[{'callid', kapi_route:call_id(RouteWin)}]
                            ),

    lager:info("callflow has received a route win, taking control of the call"),

    {'won', State#ts_callflow_state{callctl_q=kapi_route:control_queue(RouteWin)
                                   ,kapps_call=kapps_call:from_route_win(RouteWin, Call)
                                   }
    }.

-spec wait_for_bridge(state(), kz_term:api_integer()) ->
                             {'hangup' | 'error' | 'bridged', state()}.
wait_for_bridge(State, 'undefined') ->
    wait_for_bridge(State, 20);
wait_for_bridge(State, Timeout) ->
    wait_for_bridge(State, Timeout, kapps_call_command:receive_event(Timeout * 1000)).

-spec wait_for_bridge(state(), kz_term:api_integer(), kapps_call_command:request_return()) ->
                             {'hangup' | 'error' | 'bridged', state()}.
wait_for_bridge(State, Timeout, {'ok', EventJObj}) ->
    case process_event_for_bridge(State, EventJObj) of
        'ignore' -> wait_for_bridge(State, Timeout);
        {'error', _}=Error -> Error;
        {'hangup', _}=Hangup -> Hangup;
        {'bridged', _}=Done -> Done
    end;
wait_for_bridge(State, Timeout, {'error', 'timeout'}) ->
    lager:info("timed out waiting for bridge, waiting again"),
    wait_for_bridge(State, Timeout).

-spec process_event_for_bridge(state(), kz_json:object()) ->
                                      'ignore' | {'hangup' | 'error' | 'bridged', state()}.
process_event_for_bridge(State, JObj) ->
    process_event_for_bridge(State, JObj, get_event_type(JObj)).

-spec process_event_for_bridge(state(), kz_json:object(), event_type()) ->
                                      'ignore' | {'hangup' | 'error' | 'bridged', state()}.
process_event_for_bridge(#ts_callflow_state{aleg_callid=ALeg} = State
                        ,JObj
                        ,{<<"resource">>, <<"offnet_resp">>, _}
                        ) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj, ALeg),
    case is_success(<<"Response-Message">>, JObj)
        orelse was_bridge_blocked(JObj)
    of
        'true' when CallId =:= ALeg ->
            lager:info("offnet bridge has finished"),
            {'hangup', State};
        'false' when CallId =:= ALeg ->
            Failure = kz_json:get_first_defined([<<"Error-Message">>
                                                ,<<"Response-Code">>
                                                ]
                                               ,JObj
                                               ),
            lager:info("offnet failed: ~s ~s"
                      ,[Failure, kz_json:get_value(<<"Response-Message">>, JObj)]
                      ),
            {'error', State};
        _Else ->
            lager:debug("ignoring offnet response for call id ~s", [CallId]),
            'ignore'
    end;
process_event_for_bridge(#ts_callflow_state{aleg_callid=ALeg
                                           ,callctl_q=CtlQ
                                           }=State
                        ,JObj
                        ,{<<"resource">>, <<"resource_error">>, _}
                        ) ->
    Code = kz_json:get_value(<<"Failure-Code">>, JObj, <<"486">>),
    Message = kz_json:get_value(<<"Failure-Message">>, JObj),
    lager:info("offnet failed: ~s ~s", [Code, Message]),
    %% send failure code to Call
    _ = kz_call_response:send(ALeg, CtlQ, Code, Message),
    {'hangup', State};
process_event_for_bridge(State, _JObj, {<<"call_event">>, <<"CHANNEL_DESTROY">>, _}) ->
    lager:info("channel hungup before bridge"),
    {'hangup', State};
process_event_for_bridge(State, JObj, {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>}) ->
    case was_bridge_successful(JObj)
        orelse was_bridge_blocked(JObj)
    of
        'true' ->
            lager:info("bridge finished, time to hangup"),
            {'hangup', State};
        'false' ->
            lager:info("bridge failed: ~s",[kz_json:encode(JObj)]),
            {'error', State}
    end;
process_event_for_bridge(State, JObj, {<<"error">>, _, <<"bridge">>}) ->
    lager:debug("channel execution error while waiting for bridge: ~s"
               ,[kz_json:encode(JObj)]
               ),
    case was_bridge_blocked(JObj) of
        'true' ->
            lager:info("bridge was blocked"),
            {'hangup', State};
        'false' ->
            {'error', State}
    end;
process_event_for_bridge(_State, _JObj, {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"answer">>}) ->
    %% support one legged bridges such as on-net conference
    lager:info("channel was answered"),
    'ignore';
process_event_for_bridge(#ts_callflow_state{aleg_callid=ALeg}=State
                        ,JObj
                        ,{<<"call_event">>, <<"CHANNEL_ANSWER">>, _}
                        ) ->
    BLeg = kz_call_event:other_leg_call_id(JObj),
    lager:debug("channel ~s bridged to ~s", [ALeg, BLeg]),
    {'bridged', State};
process_event_for_bridge(_State, _JObj, _Unhandled) ->
    lager:debug("ignoring ~p", [_Unhandled]),
    'ignore'.

-spec was_bridge_successful(kz_json:object()) -> boolean().
was_bridge_successful(JObj) ->
    is_success(<<"Disposition">>, JObj)
        orelse is_success([<<"Application-Response">>
                          ,<<"Hangup-Cause">>
                          ]
                         ,JObj
                         ,<<"UNSPECIFIED">>
                         ).

-spec was_bridge_blocked(kz_json:object()) -> boolean().
was_bridge_blocked(JObj) ->
    Blocked = not kz_call_event:is_authorized(JObj),
    lager:debug("was bridge blocked: ~p", [Blocked]),
    Blocked.

-spec get_event_type(kz_json:object()) -> event_type().
get_event_type(JObj) ->
    {C, N} = kz_util:get_event_type(JObj),
    try get_app(JObj) of
        App -> {C, N, App}
    catch
        'error':'badarg' -> {C, N, 'undefined'}
    end.

-spec get_app(kz_json:object()) -> kz_term:api_binary().
get_app(JObj) ->
    case kz_json:get_ne_binary_value(<<"Application-Name">>, JObj) of
        'undefined' -> kz_json:get_ne_binary_value([<<"Request">>, <<"Application-Name">>], JObj);
        App -> App
    end.

-spec send_hangup(state()) -> 'ok'.
send_hangup(#ts_callflow_state{callctl_q = <<>>}) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q=CtlQ
                              ,aleg_callid=CallID
                              ,amqp_worker=Worker
                              }=State) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Call-ID">>, CallID}
              ,{<<"Insert-At">>, <<"now">>}
               | kz_api:default_headers(get_worker_queue(State)
                                       ,<<"call">>, <<"command">>
                                       ,?APP_NAME, ?APP_VERSION
                                       )
              ],
    lager:info("sending hangup to ~s: ~p", [CtlQ, Command]),
    'ok' = kz_amqp_worker:cast(Command
                              ,fun(P)-> kapi_dialplan:publish_command(CtlQ, P) end
                              ,Worker
                              ).

-spec send_hangup(state(), kz_term:api_binary()) -> 'ok'.
send_hangup(#ts_callflow_state{callctl_q = <<>>}, _) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q = 'undefined'}, _) -> 'ok';
send_hangup(#ts_callflow_state{callctl_q=CtlQ
                              ,aleg_callid=CallId
                              }
           ,Code
           ) ->
    lager:debug("responding to aleg with ~p", [Code]),
    {'ok', _} = kz_call_response:send(CallId, CtlQ, Code),
    'ok'.

-spec send_command(state(), kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
send_command(#ts_callflow_state{amqp_worker=Worker}, Command, PubFun) ->
    'ok' = kz_amqp_worker:cast(Command, PubFun, Worker).

%%%-----------------------------------------------------------------------------
%%% Data access functions
%%%-----------------------------------------------------------------------------
-spec get_request_data(state()) -> kapi_route:req().
get_request_data(#ts_callflow_state{route_req_jobj=JObj}) -> JObj.

-spec get_custom_channel_vars(state()) -> kz_json:object().
get_custom_channel_vars(#ts_callflow_state{route_req_jobj=JObj}) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

-spec get_custom_sip_headers(state()) -> kz_term:api_object().
get_custom_sip_headers(#ts_callflow_state{route_req_jobj=JObj}) ->
    kz_json:get_json_value(<<"Custom-SIP-Headers">>, JObj).

-spec set_endpoint_data(state(), kz_json:object()) -> state().
set_endpoint_data(State, Data) -> State#ts_callflow_state{ep_data=Data}.

-spec get_endpoint_data(state()) -> kz_json:object().
get_endpoint_data(#ts_callflow_state{ep_data=EP}) -> EP.

-spec set_account_id(state(), kz_term:ne_binary()) -> state().
set_account_id(State, ID) -> State#ts_callflow_state{acctid=ID}.

-spec get_account_id(state()) -> kz_term:ne_binary().
get_account_id(#ts_callflow_state{acctid=ID}) -> ID.

-spec get_control_queue(state()) -> kz_term:api_binary().
get_control_queue(#ts_callflow_state{callctl_q=CtlQ}) -> CtlQ.

-spec get_worker_queue(state()) -> kz_term:ne_binary().
get_worker_queue(#ts_callflow_state{amqp_worker=Worker}) ->
    gen_listener:queue_name(Worker).

-spec get_aleg_id(state()) -> kz_term:api_binary().
get_aleg_id(#ts_callflow_state{aleg_callid=ALeg}) -> ALeg.

-spec get_bleg_id(state()) -> kz_term:api_binary().
get_bleg_id(#ts_callflow_state{bleg_callid=ALeg}) -> ALeg.

-spec get_call_cost(state()) -> float().
get_call_cost(#ts_callflow_state{call_cost=Cost}) -> Cost.

-spec set_failover(state(), kz_json:object()) -> state().
set_failover(State, Failover) -> State#ts_callflow_state{failover=Failover}.

-spec get_failover(state()) -> kz_term:api_object().
get_failover(#ts_callflow_state{failover=Fail}) -> Fail.

-spec is_trunkstore_acct(kz_json:object(), kz_term:api_binary() | kz_term:api_binaries()) -> boolean().
is_trunkstore_acct(RouteReqJObj, Type) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, RouteReqJObj, kz_json:new()),
    lager:info("checking type(s) ~p against CCVs ~s", [Type, kz_json:encode(CCVs)]),
    check_ccvs_for_type(CCVs, Type).

-spec check_ccvs_for_type(kz_json:object(), kz_term:api_binary() | kz_term:api_binaries()) -> boolean().
check_ccvs_for_type(CCVs, [Type|Types]) ->
    check_ccvs_for_type(CCVs, Type)
        orelse check_ccvs_for_type(CCVs, Types);
check_ccvs_for_type(_CCVs, []) ->
    lager:info("no types left to check, not a trunkstore account"),
    'false';
check_ccvs_for_type(CCVs, <<"sys_info">> = Type) ->
    is_authorized(CCVs, Type)
        orelse has_trunkstore_id(CCVs)
        andalso is_not_redirected(CCVs);
check_ccvs_for_type(CCVs, Type) ->
    is_authorized(CCVs, Type).

-spec is_authorized(kz_json:object(), kz_term:api_ne_binary()) -> boolean().
is_authorized(CCVs, Type) ->
    Type =:= kz_json:get_ne_binary_value([<<"Authorizing-Type">>], CCVs).

-spec has_trunkstore_id(kz_json:object()) -> boolean().
has_trunkstore_id(CCVs) ->
    'undefined' =/= kz_json:get_ne_binary_value([<<"Trunkstore-ID">>], CCVs).

-spec is_not_redirected(kz_json:object()) -> boolean().
is_not_redirected(CCVs) ->
    'undefined' =/= kz_json:get_ne_binary_value([<<"Referred-By">>], CCVs)
        orelse 'undefined' =/= kz_json:get_ne_binary_value([<<"Redirected-By">>], CCVs).

-spec pre_park_action() -> kz_term:ne_binary().
pre_park_action() ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"ring_ready_offnet">>, 'true') of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.

-spec is_success(kz_term:ne_binary(), kz_json:object()) -> boolean().
is_success(Key, JObj) ->
    kz_json:get_value(Key, JObj) =:= <<"SUCCESS">>.

-spec is_success(kz_term:ne_binaries(), kz_json:object(), kz_term:ne_binary()) -> boolean().
is_success(Key, JObj, Default) ->
    kz_json:get_first_defined(Key, JObj, Default) =:= <<"SUCCESS">>.

-spec get_kapps_call(state()) -> kapps_call:call().
get_kapps_call(#ts_callflow_state{kapps_call=Call}) -> Call.
