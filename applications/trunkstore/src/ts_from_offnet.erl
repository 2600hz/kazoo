%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Calls coming from offnet (in this case, likely stepswitch) potentially
%%% destined for a trunkstore client, or, if the account exists and
%%% failover is configured, to an external DID or SIP URI
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_from_offnet).

-export([start_link/2, init/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).

-spec start_link(kapi_route:req(), pid()) -> kz_types:startlink_ret().
start_link(RouteReqJObj, AMQPWorker) ->
    proc_lib:start_link(?SERVER, 'init', [self(), RouteReqJObj, AMQPWorker]).

-spec init(pid(), kapi_route:req(), pid()) -> 'ok'.
init(Parent, RouteReqJObj, AMQPWorker) ->
    proc_lib:init_ack(Parent, {'ok', self()}),
    start_amqp(ts_callflow:init(RouteReqJObj, ['undefined', <<"resource">>]), AMQPWorker).

-spec start_amqp(ts_callflow:state() | {'error', 'not_ts_account'}, pid()) -> 'ok'.
start_amqp({'error', 'not_ts_account'}, AMQPWorker) ->
    kz_amqp_worker:checkin_worker(AMQPWorker, trunkstore_sup:pool_name());
start_amqp(State, AMQPWorker) ->
    endpoint_data(ts_callflow:start_amqp(State, AMQPWorker)).

-spec endpoint_data(ts_callflow:state()) -> 'ok'.
endpoint_data(State) ->
    try get_endpoint_data(State) of
        {'endpoint', Endpoint} ->
            RouteReq = ts_callflow:get_request_data(State),
            proceed_with_endpoint(State, Endpoint, RouteReq)
    catch
        'throw':'no_did_found' ->
            lager:info("call was not for a trunkstore number");
        'throw':'unknown_account' -> 'ok';
        'throw':_E ->
            lager:info("thrown exception caught, not continuing: ~p", [_E])
    after
        ts_callflow:cleanup_amqp(State)
    end.

-spec proceed_with_endpoint(ts_callflow:state(), kz_json:object(), kapi_route:req()) -> 'ok'.
proceed_with_endpoint(State, Endpoint, RouteReq) ->
    CallID = ts_callflow:get_aleg_id(State),
    'true' = kapi_dialplan:bridge_endpoint_v(Endpoint),

    InceptionAccountId = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Inception-Account-ID">>], RouteReq),
    MediaHandling = media_handling(InceptionAccountId, Endpoint),

    Id = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], Endpoint),

    Command = [{<<"Application-Name">>, <<"bridge">>}
              ,{<<"Endpoints">>, [Endpoint]}
              ,{<<"Media">>, MediaHandling}
              ,{<<"Dial-Endpoint-Method">>, <<"single">>}
              ,{<<"Call-ID">>, CallID}
              ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Trunkstore-ID">>, Id}])}
               | default_command_headers(State)
              ],
    State1 = ts_callflow:set_failover(State, kz_json:get_json_value(<<"Failover">>, Endpoint, kz_json:new())),
    State2 = ts_callflow:set_endpoint_data(State1, Endpoint),
    send_park(State2, Command).

-spec default_command_headers(ts_callflow:state()) -> kz_term:proplist().
default_command_headers(State) ->
    kz_api:default_headers(ts_callflow:get_worker_queue(State)
                          ,<<"call">>, <<"command">>
                          ,?APP_NAME, ?APP_VERSION
                          ).

-spec media_handling(kz_term:api_ne_binary(), kz_json:object()) -> kz_term:ne_binary().
media_handling('undefined', Endpoint) ->
    case kz_json:is_true(<<"Bypass-Media">>, Endpoint) of
        'false' -> <<"process">>;
        'true' -> <<"bypass">>
    end;
media_handling(_InceptionAccountId, _Endpoint) ->
    <<"process">>.

-spec send_park(ts_callflow:state(), kz_term:proplist()) -> 'ok'.
send_park(State, Command) ->
    case ts_callflow:send_park(State) of
        {'lost', _} -> 'ok';
        {'won', State1} -> route_call(State1, Command)
    end.

-spec route_call(ts_callflow:state(), kz_term:proplist()) -> 'ok'.
route_call(State, Command) ->
    lager:info("route won, sending command"),
    send_onnet(State, Command),

    Timeout = endpoint_timeout(Command),

    wait_for_bridge(State, Timeout).

-spec endpoint_timeout(kz_term:proplist()) -> kz_term:api_integer().
endpoint_timeout(Command) when is_list(Command) ->
    kz_json:get_integer_value([<<"Endpoints">>, 1, <<"Endpoint-Timeout">>]
                             ,kz_json:from_list(Command)
                             ).

-spec send_onnet(ts_callflow:state(), kz_term:proplist()) -> 'ok'.
send_onnet(State, Command) ->
    lager:info("sending onnet command: ~p", [Command]),
    'ok' = maybe_send_privacy(State),

    send_bridge(State, Command).

-spec send_bridge(ts_callflow:state(), kz_term:proplist()) -> 'ok'.
send_bridge(State, Command) ->
    CtlQ = ts_callflow:get_control_queue(State),
    ts_callflow:send_command(State
                            ,Command
                            ,fun(API) -> kapi_dialplan:publish_command(CtlQ, API) end
                            ).

-spec maybe_send_privacy(ts_callflow:state()) -> 'ok'.
maybe_send_privacy(State) ->
    case kz_privacy:has_flags(ts_callflow:get_custom_channel_vars(State)) of
        'false' -> 'ok';
        'true' -> send_privacy(State)
    end.

-spec send_privacy(ts_callflow:state()) -> 'ok'.
send_privacy(State) ->
    CtlQ = ts_callflow:get_control_queue(State),
    CallID = ts_callflow:get_aleg_id(State),
    Command = [{<<"Application-Name">>, <<"privacy">>}
              ,{<<"Privacy-Mode">>, <<"full">>}
              ,{<<"Call-ID">>, CallID}
               | default_command_headers(State)
              ],
    ts_callflow:send_command(State
                            ,Command
                            ,fun(API) ->  kapi_dialplan:publish_command(CtlQ, API) end
                            ).

-spec wait_for_bridge(ts_callflow:state(), kz_term:api_integer()) -> 'ok'.
wait_for_bridge(State, Timeout) ->
    case ts_callflow:wait_for_bridge(State, Timeout) of
        {'bridged', _} -> lager:info("channel bridged, we're done");
        {'hangup', _} -> 'ok';
        {'error', State1} ->
            lager:info("error waiting for bridge, try failover"),
            try_failover(State1)
    end.

-spec try_failover(ts_callflow:state()) -> 'ok'.
try_failover(State) ->
    case {ts_callflow:get_control_queue(State)
         ,ts_callflow:get_failover(State)
         }
    of
        {<<>>, _} ->
            lager:info("no callctl for failover");
        {_, 'undefined'} ->
            lager:info("no failover defined");
        {_, Failover} ->
            case kz_json:is_empty(Failover) of
                'true' ->
                    lager:info("no failover configured");
                'false' ->
                    lager:info("trying failover"),
                    failover(State, Failover)
            end
    end.

-spec failover(ts_callflow:state(), kz_json:object()) -> 'ok'.
failover(State, Failover) ->
    case kz_json:get_ne_binary_value(<<"e164">>, Failover) of
        'undefined' ->
            try_failover_sip(State, kz_json:get_ne_binary_value(<<"sip">>, Failover));
        DID ->
            try_failover_e164(State, DID)
    end.

-spec try_failover_sip(ts_callflow:state(), kz_term:api_ne_binary()) -> 'ok'.
try_failover_sip(_, 'undefined') ->
    lager:info("SIP failover undefined");
try_failover_sip(State, SIPUri) ->
    CallID = ts_callflow:get_aleg_id(State),
    CtlQ = ts_callflow:get_control_queue(State),

    lager:info("routing to failover sip uri: ~s", [SIPUri]),
    EndPoint = kz_json:from_list([{<<"Invite-Format">>, <<"route">>}
                                 ,{<<"Route">>, SIPUri}
                                 ]),
    %% since we only route to one endpoint, we specify most options on the endpoint's leg
    Command = [{<<"Call-ID">>, CallID}
              ,{<<"Application-Name">>, <<"bridge">>}
              ,{<<"Endpoints">>, [EndPoint]}
               | default_command_headers(State)
              ],
    ts_callflow:send_command(State
                            ,Command
                            ,fun(API) -> kapi_dialplan:publish_command(CtlQ, API) end
                            ),
    wait_for_bridge(ts_callflow:set_failover(State, kz_json:new()), 20).

-spec try_failover_e164(ts_callflow:state(), kz_term:ne_binary()) -> 'ok'.
try_failover_e164(State, ToDID) ->
    RouteReq = ts_callflow:get_request_data(State),
    OriginalCIdNumber = kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, RouteReq),
    OriginalCIdName = kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, RouteReq),
    CallID = ts_callflow:get_aleg_id(State),
    AccountId = ts_callflow:get_account_id(State),

    Endpoint = ts_callflow:get_endpoint_data(State),

    Timeout = kz_json:get_integer_value(<<"timeout">>, Endpoint),

    CtlQ = ts_callflow:get_control_queue(State),

    CCVs = ts_callflow:get_custom_channel_vars(State),

    Req = [{<<"Call-ID">>, CallID}
          ,{<<"Resource-Type">>, <<"audio">>}
          ,{<<"To-DID">>, ToDID}
          ,{<<"Account-ID">>, AccountId}
          ,{<<"Control-Queue">>, CtlQ}
          ,{<<"Application-Name">>, <<"bridge">>}
          ,{<<"Flags">>, kz_json:get_value(<<"flags">>, Endpoint)}
          ,{<<"Timeout">>, Timeout}
          ,{<<"Ignore-Early-Media">>, kz_json:get_value(<<"ignore_early_media">>, Endpoint)}
          ,{<<"Outbound-Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"Outbound-Caller-ID-Name">>, Endpoint, OriginalCIdName)}
          ,{<<"Outbound-Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"Outbound-Caller-ID-Number">>, Endpoint, OriginalCIdNumber)}
          ,{<<"Ringback">>, kz_json:get_ne_binary_value(<<"ringback">>, Endpoint)}
          ,{<<"Hunt-Account-ID">>, kz_json:get_ne_binary_value(<<"Hunt-Account-ID">>, Endpoint)}
          ,{<<"Custom-SIP-Headers">>, ts_callflow:get_custom_sip_headers(State)}
          ,{<<"Inception">>,  kz_json:get_ne_binary_value(<<"Inception">>, CCVs)}
          ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
           | kz_api:default_headers(ts_callflow:get_worker_queue(State)
                                   ,?APP_NAME, ?APP_VERSION
                                   )
          ],
    lager:info("sending offnet request for DID ~s", [ToDID]),
    ts_callflow:send_command(State
                            ,props:filter_undefined(Req)
                            ,fun kapi_offnet_resource:publish_req/1
                            ),
    wait_for_bridge(ts_callflow:set_failover(State, kz_json:new()), Timeout).

%%------------------------------------------------------------------------------
%% Out-of-band functions
%%------------------------------------------------------------------------------
-spec get_endpoint_data(ts_callflow:state()) -> {'endpoint', kz_json:object()}.
get_endpoint_data(State) ->
    RouteReq = ts_callflow:get_request_data(State),
    {ToUser, _} = kapps_util:get_destination(RouteReq, ?APP_NAME, <<"inbound_user_field">>),
    ToDID = knm_converters:normalize(ToUser),
    case knm_numbers:lookup_account(ToDID) of
        {'ok', AccountId, NumberProps} ->
            get_endpoint_data(State, RouteReq, ToDID, AccountId, NumberProps);
        _Else ->
            lager:debug("unable to lookup account for number ~s: ~p", [ToDID, _Else]),
            throw('unknown_account')
    end.

-spec get_endpoint_data(ts_callflow:state(), kapi_route:req(), kz_term:ne_binary(), kz_term:ne_binary(), knm_options:extra_options()) ->
          {'endpoint', kz_json:object()}.
get_endpoint_data(State, RouteReq, ToDID, AccountId, NumberProps) ->
    ForceOut = knm_options:should_force_outbound(NumberProps),
    lager:info("building endpoint for account id ~s with force out ~s", [AccountId, ForceOut]),
    RoutingData1 = routing_data(ToDID, AccountId),

    CidOptions  = props:get_value(<<"Caller-ID-Options">>, RoutingData1),
    CidFormat   = kz_json:get_json_value(<<"format">>, CidOptions),
    OldCNum = kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, RouteReq),
    OldCNam = kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, RouteReq, OldCNum),
    NewCallerId = maybe_anonymize_caller_id(State, {OldCNam, OldCNum}, CidFormat),
    RoutingData = RoutingData1 ++ NewCallerId,

    AuthUser = props:get_value(<<"To-User">>, RoutingData),
    AuthRealm = props:get_value(<<"To-Realm">>, RoutingData),
    AuthzId = props:get_value(<<"Authorizing-ID">>, RoutingData),
    InFormat = props:get_value(<<"Invite-Format">>, RoutingData, <<"username">>),
    Invite = ts_util:invite_format(kz_term:to_lower_binary(InFormat), ToDID) ++ RoutingData,
    Routines = [fun(E) -> get_endpoint_ccvs(E, AuthUser, AuthRealm, AccountId, AuthzId) end
               ,fun(E) -> get_endpoint_sip_headers(E, AuthUser, AuthRealm) end
               ],
    Endpoint = lists:foldl(fun(F, E) -> F(E) end, Invite, Routines),
    {'endpoint', kz_json:from_list(Endpoint)}.

-spec get_endpoint_ccvs(kz_term:proplist(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
get_endpoint_ccvs(Endpoint, AuthUser, AuthRealm, AccountId, AuthzId) ->
    Props = props:filter_undefined([{<<"Auth-User">>, AuthUser}
                                   ,{<<"Auth-Realm">>, AuthRealm}
                                   ,{<<"Direction">>, <<"inbound">>}
                                   ,{<<"Account-ID">>, AccountId}
                                   ,{<<"Authorizing-ID">>, AuthzId}
                                   ,{<<"Authorizing-Type">>, <<"sys_info">>}
                                   ]),
    [{<<"Custom-Channel-Vars">>, kz_json:from_list(Props)} | Endpoint].

-spec get_endpoint_sip_headers(kz_term:proplist(), kz_term:api_binary(), kz_term:api_binary()) -> kz_term:proplist().
get_endpoint_sip_headers(Endpoint, 'undefined', _) -> Endpoint;
get_endpoint_sip_headers(Endpoint, _, 'undefined') -> Endpoint;
get_endpoint_sip_headers(Endpoint, AuthUser, AuthRealm) ->
    Props = [{<<"X-KAZOO-AOR">>, <<"sip:", AuthUser/binary, "@", AuthRealm/binary>>}],
    [{<<"Custom-SIP-Headers">>, kz_json:from_list(Props)} | Endpoint].

-spec routing_data(kz_term:ne_binary(), kz_term:ne_binary()) -> [{<<_:48,_:_*8>>, any()}].
routing_data(ToDID, AccountId) ->
    case ts_util:lookup_did(ToDID, AccountId) of
        {'ok', Settings} ->
            lager:info("got settings for DID ~s", [ToDID]),
            routing_data(ToDID, AccountId, Settings);
        {'error', 'no_did_found'} ->
            lager:info("DID ~s not found in ~s", [ToDID, AccountId]),
            throw('no_did_found');
        {'error', 'timeout'} ->
            lager:error("timed out looking for DID ~s", [ToDID]),
            throw('timeout')
    end.

-spec routing_data(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> [{<<_:48,_:_*8>>, any()}].
routing_data(ToDID, AccountId, Settings) ->
    AuthOpts = kz_json:get_value(<<"auth">>, Settings, kz_json:new()),
    Acct = kz_json:get_value(<<"account">>, Settings, kz_json:new()),
    DIDOptions = kz_json:get_value(<<"DID_Opts">>, Settings, kz_json:new()),
    HuntAccountId = kz_json:get_value([<<"server">>, <<"hunt_account_id">>], Settings),
    RouteOpts = kz_json:get_value(<<"options">>, DIDOptions, []),
    NumConfig = case knm_numbers:get(ToDID, [{'auth_by', AccountId}]) of
                    {'ok', [JObj]} -> JObj;
                    _ -> kz_json:new()
                end,
    AuthU = kz_json:get_value(<<"auth_user">>, AuthOpts),
    AuthR = kz_json:find(<<"auth_realm">>, [AuthOpts, Acct]),

    {Srv, AcctStuff} =
        try ts_util:lookup_user_flags(AuthU, AuthR, AccountId, ToDID) of
            {'ok', AccountSettings} ->
                lager:info("got account settings"),
                {kz_json:get_json_value(<<"server">>, AccountSettings, kz_json:new())
                ,kz_json:get_json_value(<<"account">>, AccountSettings, kz_json:new())
                }
        catch
            _E:_R ->
                lager:info("failed to get account settings: ~p: ~p", [_E, _R]),
                {kz_json:new(), kz_json:new()}
        end,

    SrvOptions = kz_json:get_value(<<"options">>, Srv, kz_json:new()),

    ToIP = kz_json:find(<<"ip">>, [AuthOpts, SrvOptions]),
    ToPort = kz_json:find(<<"port">>, [AuthOpts, SrvOptions]),

    case kz_json:is_true(<<"enabled">>, SrvOptions, 'true') of
        'false' -> throw({'server_disabled', kz_doc:id(Srv)});
        'true' -> 'ok'
    end,

    AcctCidOptions = kz_json:get_json_value(<<"caller_id_options">>, AcctStuff, kz_json:new()),
    CidOptions = kz_json:get_json_value(<<"caller_id_options">>, SrvOptions, AcctCidOptions),

    InboundFormat = kz_json:get_value(<<"inbound_format">>, SrvOptions, <<"npan">>),
    {CalleeName, CalleeNumber} = callee_id([kz_json:get_value(<<"caller_id">>, DIDOptions)
                                           ,kz_json:get_value(<<"callerid_account">>, Settings)
                                           ,kz_json:get_value(<<"callerid_server">>, Settings)
                                           ]),
    ProgressTimeout = ts_util:progress_timeout([kz_json:get_value(<<"progress_timeout">>, DIDOptions)
                                               ,kz_json:get_value(<<"progress_timeout">>, SrvOptions)
                                               ,kz_json:get_value(<<"progress_timeout">>, AcctStuff)
                                               ]),
    BypassMedia = ts_util:bypass_media([kz_json:get_value(<<"media_handling">>, DIDOptions)
                                       ,kz_json:get_value(<<"media_handling">>, SrvOptions)
                                       ,kz_json:get_value(<<"media_handling">>, AcctStuff)
                                       ]),
    FailoverLocations = [kz_json:get_value(<<"failover">>, NumConfig)
                        ,kz_json:get_value(<<"failover">>, DIDOptions)
                        ,kz_json:get_value(<<"failover">>, SrvOptions)
                        ,kz_json:get_value(<<"failover">>, AcctStuff)
                        ],

    Failover = ts_util:failover(FailoverLocations),
    lager:info("failover found: ~p", [Failover]),

    Delay = ts_util:delay([kz_json:get_value(<<"delay">>, DIDOptions)
                          ,kz_json:get_value(<<"delay">>, SrvOptions)
                          ,kz_json:get_value(<<"delay">>, AcctStuff)
                          ]),
    SIPHeaders = ts_util:sip_headers([kz_json:get_value(<<"sip_headers">>, DIDOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, SrvOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, AcctStuff)
                                     ]),
    IgnoreEarlyMedia = ts_util:ignore_early_media([kz_json:get_value(<<"ignore_early_media">>, DIDOptions)
                                                  ,kz_json:get_value(<<"ignore_early_media">>, SrvOptions)
                                                  ,kz_json:get_value(<<"ignore_early_media">>, AcctStuff)
                                                  ]),
    Timeout = ts_util:ep_timeout([kz_json:get_value(<<"timeout">>, DIDOptions)
                                 ,kz_json:get_value(<<"timeout">>, SrvOptions)
                                 ,kz_json:get_value(<<"timeout">>, AcctStuff)
                                 ]),

    [KV || {_,V}=KV <- [{<<"Invite-Format">>, InboundFormat}
                       ,{<<"Codecs">>, kz_json:find(<<"codecs">>, [SrvOptions, Srv])}
                       ,{<<"Bypass-Media">>, BypassMedia}
                       ,{<<"Endpoint-Progress-Timeout">>, ProgressTimeout}
                       ,{<<"Failover">>, Failover}
                       ,{<<"Endpoint-Delay">>, Delay}
                       ,{<<"Custom-SIP-Headers">>, SIPHeaders}
                       ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                       ,{<<"Endpoint-Timeout">>, Timeout}
                       ,{<<"Callee-ID-Name">>, CalleeName}
                       ,{<<"Callee-ID-Number">>, CalleeNumber}
                       ,{<<"To-User">>, AuthU}
                       ,{<<"To-Realm">>, AuthR}
                       ,{<<"Caller-ID-Options">>, CidOptions}
                       ,{<<"To-DID">>, ToDID}
                       ,{<<"To-IP">>, build_ip(ToIP, ToPort)}
                       ,{<<"Route-Options">>, RouteOpts}
                       ,{<<"Hunt-Account-ID">>, HuntAccountId}
                       ,{<<"Authorizing-ID">>, kz_doc:id(Settings)} % connectivity doc id
                       ],
           V =/= 'undefined',
           V =/= <<>>
    ].

-spec build_ip(kz_term:api_binary(), kz_term:api_binary() | integer()) -> kz_term:api_binary().
build_ip('undefined', _) -> 'undefined';
build_ip(IP, 'undefined') -> IP;
build_ip(IP, <<_/binary>> = PortBin) -> build_ip(IP, kz_term:to_integer(PortBin));
build_ip(IP, 5060) -> IP;
build_ip(IP, Port) -> list_to_binary([IP, ":", kz_term:to_binary(Port)]).

callee_id([]) -> {'undefined', 'undefined'};
callee_id(['undefined' | T]) -> callee_id(T);
callee_id([<<>> | T]) -> callee_id(T);
callee_id([JObj | T]) ->
    case kz_json:is_json_object(JObj)
        andalso (not kz_json:is_empty(JObj))
    of
        'false' -> callee_id(T);
        'true' ->
            case {kz_json:get_value(<<"cid_name">>, JObj)
                 ,kz_json:get_value(<<"cid_number">>, JObj)
                 }
            of
                {'undefined', 'undefined'} -> callee_id(T);
                CalleeID -> CalleeID
            end
    end.

-spec maybe_anonymize_caller_id(ts_callflow:state(), {kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:api_object()) ->
          kz_term:proplist().
maybe_anonymize_caller_id(State, {Name, Number}, CidFormat) ->
    CCVs = ts_callflow:get_custom_channel_vars(State),
    [{<<"Outbound-Caller-ID-Number">>, kapps_call:maybe_format_caller_id_str(Number, CidFormat)}
    ,{<<"Outbound-Caller-ID-Name">>, Name}
     | kz_privacy:flags(CCVs)
    ].
