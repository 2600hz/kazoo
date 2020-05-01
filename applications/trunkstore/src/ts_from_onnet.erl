%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Calls coming from known clients, getting settings for caller-id and
%%% what not, and sending the calls offnet.
%%%
%%% If the call is destined for a known client, the offnet whapp (probably
%%% stepswitch) will redirect the request back in, to be picked up by
%%% ts_from_offnet.
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_from_onnet).

-export([start_link/2, init/3]).

-include("ts.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

-define(SERVER, ?MODULE).

-spec start_link(kapi_route:req(), pid()) -> kz_types:startlink_ret().
start_link(RouteReqJObj, AMQPWorker) ->
    proc_lib:start_link(?SERVER, 'init', [self(), RouteReqJObj, AMQPWorker]).

-spec init(pid(), kapi_route:req(), pid()) -> 'ok'.
init(Parent, RouteReqJObj, AMQPWorker) ->
    proc_lib:init_ack(Parent, {'ok', self()}),

    Funs = [fun maybe_referred_call/1
           ,fun maybe_redirected_call/1
           ],
    JObj = kz_json:exec(Funs, RouteReqJObj),
    start_amqp(ts_callflow:init(JObj, <<"sys_info">>), AMQPWorker).

start_amqp({'error', 'not_ts_account'}, AMQPWorker) ->
    kz_amqp_worker:checkin_worker(AMQPWorker, trunkstore_sup:pool_name());
start_amqp(State, AMQPWorker) ->
    maybe_onnet_data(ts_callflow:start_amqp(State, AMQPWorker)).

maybe_onnet_data(State) ->
    RouteReq = ts_callflow:get_request_data(State),
    CallID = ts_callflow:get_aleg_id(State),
    AccountId = ts_callflow:get_account_id(State),
    {ToUser, _} = kapps_util:get_destination(RouteReq, ?APP_NAME, <<"outbound_user_field">>),
    ToDID = knm_converters:normalize(ToUser, AccountId),
    FromUser = kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, RouteReq),

    lager:info("onnet request from ~s(~s) to ~s", [FromUser, AccountId, ToDID]),
    Options =
        case ts_util:lookup_did(FromUser, AccountId) of
            {'ok', Opts} -> Opts;
            _ ->
                Username = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Username">>], RouteReq, <<>>),
                Realm = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Realm">>], RouteReq, <<>>),

                case ts_util:lookup_user_flags(Username, Realm, AccountId) of
                    {'ok', Opts} -> Opts;
                    _ -> kz_json:new()
                end
        end,
    ServerOptions = kz_json:get_json_value([<<"server">>, <<"options">>], Options, kz_json:new()),
    case knm_converters:is_reconcilable(ToDID)
        orelse knm_converters:classify(ToDID) =:= <<"emergency">>
        orelse kz_json:is_true(<<"hunt_non_reconcilable">>, ServerOptions, 'false')
        orelse kapps_config:get_is_true(?CONFIG_CAT, <<"default_hunt_non_reconcilable">>, 'false')
    of
        'false' ->
            lager:debug("number ~p is non_reconcilable and the server does not allow it", [ToDID]);
        'true' ->
            onnet_data(CallID, AccountId, FromUser, ToDID, Options, State)
    end.

onnet_data(CallID, AccountId, FromUser, ToDID, Options, State) ->
    DIDOptions = kz_json:get_value(<<"DID_Opts">>, Options, kz_json:new()),
    AccountOptions = kz_json:get_value(<<"account">>, Options, kz_json:new()),
    ServerOptions = kz_json:get_value([<<"server">>, <<"options">>], Options, kz_json:new()),
    RouteReq = ts_callflow:get_request_data(State),
    CustomSIPHeaders = ts_callflow:get_custom_sip_headers(State),
    MediaHandling = ts_util:get_media_handling([kz_json:get_value(<<"media_handling">>, DIDOptions)
                                               ,kz_json:get_value(<<"media_handling">>, ServerOptions)
                                               ,kz_json:get_value(<<"media_handling">>, AccountOptions)
                                               ]),
    SIPHeaders = ts_util:sip_headers([kz_json:get_value(<<"sip_headers">>, DIDOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, ServerOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, AccountOptions)
                                     ,CustomSIPHeaders
                                     ]),

    EmergencyCallerID =
        case ts_util:caller_id([kz_json:get_value(<<"emergency_caller_id">>, DIDOptions)
                               ,kz_json:get_value(<<"emergency_caller_id">>, ServerOptions)
                               ,kz_json:get_value(<<"emergency_caller_id">>, AccountOptions)
                               ])
        of
            {'undefined', 'undefined'} -> [];
            {ECIDName, ECIDNum} ->
                [{?KEY_E_CALLER_ID_NAME, ECIDName}
                ,{?KEY_E_CALLER_ID_NUMBER
                 ,ts_util:maybe_ensure_cid_valid('emergency', ECIDNum, FromUser, AccountId, CustomSIPHeaders)}

                ]
        end,
    OriginalCIdNumber = kz_json:get_value(<<"Caller-ID-Number">>, RouteReq),
    OriginalCIdName = kz_json:get_value(<<"Caller-ID-Name">>, RouteReq),
    CallerID =
        case ts_util:caller_id([kz_json:get_value(<<"caller_id">>, DIDOptions)
                               ,kz_json:get_value(<<"caller_id">>, ServerOptions)
                               ,kz_json:get_value(<<"caller_id">>, AccountOptions)
                               ])
        of
            {'undefined', 'undefined'} ->
                case kapps_config:get_is_true(?CONFIG_CAT, <<"ensure_valid_caller_id">>, 'false') of
                    'true' ->
                        ValidCID = ts_util:maybe_ensure_cid_valid('external', OriginalCIdNumber, FromUser, AccountId, CustomSIPHeaders),
                        [{?KEY_OUTBOUND_CALLER_ID_NUMBER, ValidCID}
                        ,{?KEY_OUTBOUND_CALLER_ID_NAME, OriginalCIdName}
                         | EmergencyCallerID
                        ];
                    'false' ->
                        [{?KEY_OUTBOUND_CALLER_ID_NUMBER, OriginalCIdNumber}
                        ,{?KEY_OUTBOUND_CALLER_ID_NAME, OriginalCIdName}
                         | EmergencyCallerID
                        ]
                end;
            {CIDName, CIDNum} ->
                [{?KEY_OUTBOUND_CALLER_ID_NAME, CIDName}
                ,{?KEY_OUTBOUND_CALLER_ID_NUMBER
                 ,ts_util:maybe_ensure_cid_valid('external', CIDNum, FromUser, AccountId, CustomSIPHeaders)}
                 | EmergencyCallerID
                ]
        end,

    Command = [KV
               || {_,V}=KV <- CallerID
                      ++ EmergencyCallerID
                      ++ [{?KEY_CALL_ID, CallID}
                         ,{?KEY_RESOURCE_TYPE, <<"audio">>}
                         ,{?KEY_TO_DID, ToDID}
                         ,{?KEY_ACCOUNT_ID, AccountId}
                         ,{?KEY_APPLICATION_NAME, <<"bridge">>}
                         ,{?KEY_FLAGS, get_flags(DIDOptions, ServerOptions, AccountOptions, State)}
                         ,{?KEY_MEDIA, MediaHandling}
                         ,{?KEY_TIMEOUT, kz_json:get_value(<<"timeout">>, DIDOptions)}
                         ,{?KEY_IGNORE_EARLY_MEDIA, kz_json:get_value(<<"ignore_early_media">>, DIDOptions)}
                         ,{?KEY_RINGBACK, kz_json:get_value(<<"ringback">>, DIDOptions)}
                         ,{?KEY_CSHS, SIPHeaders}
                         ,{?KEY_HUNT_ACCOUNT_ID, kz_json:get_value(<<"hunt_account_id">>, ServerOptions)}
                         ,{?KEY_CCVS, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
                         ,{?KEY_REQUESTOR_CCVS, ts_callflow:get_custom_channel_vars(State)}
                         ,{?KEY_REQUESTOR_CSHS, CustomSIPHeaders}
                          | kz_api:default_headers(ts_callflow:get_worker_queue(State)
                                                  ,?APP_NAME, ?APP_VERSION
                                                  )
                         ],
                  V =/= 'undefined',
                  V =/= <<>>
              ],
    try
        lager:debug("we know how to route this call, sending park route response"),
        send_park(State, Command)
    catch
        ?STACKTRACE(_A, _B, ST)
        lager:info("exception ~p:~p", [_A, _B]),
        kz_log:log_stacktrace(ST),
        ts_callflow:send_hangup(State)
    after
        ts_callflow:cleanup_amqp(State)
    end.

-spec get_flags(kz_json:object(), kz_json:object(), kz_json:object(), ts_callflow:state()) -> kz_term:ne_binaries().
get_flags(DIDOptions, ServerOptions, AccountOptions, State) ->
    Call = ts_callflow:get_kapps_call(State),
    Flags = kz_attributes:get_flags(?APP_NAME, Call),
    Routines = [fun get_offnet_flags/5
               ,fun get_offnet_dynamic_flags/5
               ],
    lists:foldl(fun(F, A) -> F(DIDOptions, ServerOptions, AccountOptions, Call, A) end, Flags, Routines).

-spec get_offnet_flags(kz_json:object(), kz_json:object(), kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_offnet_flags(DIDOptions, ServerOptions, AccountOptions, _, Flags) ->
    case ts_util:offnet_flags([kz_json:get_value(<<"DID_Opts">>, DIDOptions)
                              ,kz_json:get_value(<<"flags">>, ServerOptions)
                              ,kz_json:get_value(<<"flags">>, AccountOptions)
                              ])
    of
        'undefined' -> Flags;
        DIDFlags -> Flags ++ DIDFlags
    end.

-spec get_offnet_dynamic_flags(kz_json:object(), kz_json:object(), kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_offnet_dynamic_flags(_, ServerOptions, AccountOptions, Call, Flags) ->
    case ts_util:offnet_flags([kz_json:get_value(<<"dynamic_flags">>, ServerOptions)
                              ,kz_json:get_value(<<"dynamic_flags">>, AccountOptions)
                              ])
    of
        'undefined' -> Flags;
        DynamicFlags -> kz_attributes:process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

send_park(State, Command) ->
    case ts_callflow:send_park(State) of
        {'lost', _} -> 'normal';
        {'won', State1} ->
            case ts_util:maybe_restrict_call(State1, Command) of
                'true' ->
                    lager:debug("trunkstore call to ~p restricted", [props:get_value(<<"To-DID">>, Command)]),
                    ts_callflow:send_hangup(State1, <<"403">>);
                _ ->
                    send_offnet(State1, Command)
            end
    end.

send_offnet(State, Command) ->
    CtlQ = ts_callflow:get_control_queue(State),
    ts_callflow:send_command(State
                            ,[{<<"Control-Queue">>, CtlQ}
                              |Command
                             ]
                            ,fun kapi_offnet_resource:publish_req/1
                            ),
    Timeout = props:get_integer_value(<<"Timeout">>, Command),
    wait_for_bridge(State, CtlQ, Timeout).

wait_for_bridge(State, CtlQ, Timeout) ->
    case ts_callflow:wait_for_bridge(State, Timeout) of
        {'bridged', _} -> lager:info("channel bridged, we're done here");
        {'hangup', _} -> ts_callflow:send_hangup(State);
        {'error', #ts_callflow_state{aleg_callid='undefined'}} -> 'ok';
        {'error', #ts_callflow_state{aleg_callid=CallId}=State1} ->
            lager:info("responding to aleg ~s with 686", [CallId]),
            _ = kz_call_response:send(CallId, CtlQ, <<"686">>),
            ts_callflow:send_hangup(State1, <<"686">>)
    end.

-spec maybe_referred_call(kapi_route:req()) -> kz_json:object().
maybe_referred_call(RouteReq) ->
    maybe_fix_request(get_referred_by(RouteReq), RouteReq).

-spec maybe_redirected_call(kapi_route:req()) -> kz_json:object().
maybe_redirected_call(RouteReq) ->
    maybe_fix_request(get_redirected_by(RouteReq), RouteReq).

-spec maybe_fix_request( 'undefined' | {kz_term:ne_binary(), kz_term:ne_binary()}, kapi_route:req()) -> kapi_route:req().
maybe_fix_request('undefined', RouteReq) -> RouteReq;
maybe_fix_request({Username, Realm}, RouteReq) ->
    AccountId = kapi_route:account_id(RouteReq),
    case ts_util:lookup_user_flags(Username, Realm, AccountId) of
        {'ok', _Opts} -> kz_json:set_values(fix_request_values(Username, Realm), RouteReq);
        _ -> RouteReq
    end.

-spec fix_request_values(binary(), binary()) -> [{kz_json:path(), kz_term:ne_binary()}].
fix_request_values(Username, Realm) ->
    [{[<<"Custom-Channel-Vars">>, <<"Username">>], Username}
    ,{[<<"Custom-Channel-Vars">>, <<"Realm">>], Realm}
    ,{[<<"Custom-Channel-Vars">>, <<"Authorizing-Type">>], <<"sys_info">>}
    ].

-spec get_referred_by(kz_json:object()) ->
          'undefined' | {kz_term:ne_binary(), kz_term:ne_binary()}.
get_referred_by(JObj) ->
    ReferredBy = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Referred-By">>], JObj),
    extract_sip_username(ReferredBy).

-spec get_redirected_by(kz_json:object()) ->
          'undefined' | {kz_term:ne_binary(), kz_term:ne_binary()}.
get_redirected_by(JObj) ->
    RedirectedBy = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Redirected-By">>], JObj),
    extract_sip_username(RedirectedBy).

-spec extract_sip_username(kz_term:api_ne_binary()) ->
          'undefined' | {kz_term:ne_binary(), kz_term:ne_binary()}.
extract_sip_username('undefined') -> 'undefined';
extract_sip_username(Contact) ->
    ReOptions = [{'capture', 'all_but_first', 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@(.*)">>, ReOptions)) of
        {'match', [User, Realm]} -> {User, Realm};
        _ -> 'undefined'
    end.
