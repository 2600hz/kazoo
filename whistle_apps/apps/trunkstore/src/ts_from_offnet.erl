%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Calls coming from offnet (in this case, likely stepswitch) potentially
%%% destined for a trunkstore client, or, if the account exists and
%%% failover is configured, to an external DID or SIP URI
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_from_offnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    start_amqp(ts_callflow:init(RouteReqJObj)).

start_amqp({error, not_ts_account}) -> ok;
start_amqp(State) ->
    endpoint_data(ts_callflow:start_amqp(State)).

endpoint_data(State) ->
    JObj = ts_callflow:get_request_data(State),

    try get_endpoint_data(JObj) of
        {endpoint, EP} ->
            proceed_with_endpoint(State, EP, JObj)
    catch
        throw:no_did_found ->
            lager:info("call was not for a trunkstore number");
        throw:_E ->
            lager:info("thrown exception caught, not continuing: ~p", [_E])
    end.

proceed_with_endpoint(State, EP, JObj) ->
    CallID = ts_callflow:get_aleg_id(State),
    Q = ts_callflow:get_my_queue(State),

    true = wapi_dialplan:bridge_endpoint_v(EP),

    MediaHandling = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Offnet-Loopback-Number">>], JObj) of
                        undefined ->
                            case wh_util:is_false(wh_json:get_value(<<"Bypass-Media">>, EP)) of
                                true -> <<"process">>; %% bypass media is false, process media
                                false -> <<"bypass">>
                            end;
                        _ -> <<"process">>
                    end,

    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [EP]}
               ,{<<"Media">>, MediaHandling}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Call-ID">>, CallID}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],

    State1 = ts_callflow:set_failover(State, wh_json:get_value(<<"Failover">>, EP, wh_json:new())),
    State2 = ts_callflow:set_endpoint_data(State1, EP),

    send_park(State2, Command).

send_park(State, Command) ->
    State1 = ts_callflow:send_park(State),
    wait_for_win(State1, Command).

wait_for_win(State, Command) ->
    case ts_callflow:wait_for_win(State) of
        {won, State1} ->
            lager:info("route won, sending command"),
            send_onnet(State1, Command);
        {lost, State2} ->
            lager:info("didn't win route, passive listening"),
            wait_for_bridge(State2)
    end.

send_onnet(State, Command) ->
    lager:info("sending onnet command: ~p", [Command]),

    CtlQ = ts_callflow:get_control_queue(State),

    wapi_dialplan:publish_command(CtlQ, Command),
    wait_for_bridge(State).

wait_for_bridge(State) ->
    case ts_callflow:wait_for_bridge(State) of
        {bridged, State1} ->
            wait_for_cdr(State1);
        {error, State2} ->
            lager:info("error waiting for bridge, try failover"),
            try_failover(State2);
        {hangup, State3} ->
            ALeg = ts_callflow:get_aleg_id(State3),
            ts_callflow:finish_leg(State3, ALeg);
        {timeout, State4} ->
            try_failover(State4)
    end.

wait_for_cdr(State) ->
    case ts_callflow:wait_for_cdr(State) of
        {cdr, aleg, _CDR, State1} ->
            AcctID = ts_callflow:get_account_id(State1),
            Cost = ts_callflow:get_call_cost(State1),

            lager:info("a-leg CDR for ~s costs ~p", [AcctID, Cost]),

            case ts_callflow:get_bleg_id(State1) of
                <<>> ->
                    ALeg = ts_callflow:get_aleg_id(State1),
                    ts_callflow:finish_leg(State1, ALeg);
                _Else -> wait_for_other_leg(State1, bleg)
            end;
        {cdr, bleg, _CDR, State2} ->
            BLeg = ts_callflow:get_bleg_id(State2),
            AcctID = ts_callflow:get_account_id(State2),
            Cost = ts_callflow:get_call_cost(State2),

            lager:info("b-leg ~s CDR for ~s costs ~p", [BLeg, AcctID, Cost]),

            wait_for_other_leg(State2, aleg);
        {timeout, State3} ->
            lager:info("Timed out waiting for CDRs, cleaning up"),
            CallID = ts_callflow:get_aleg_id(State3),
            ts_callflow:finish_leg(State3, CallID)
    end.

wait_for_other_leg(State, aleg) ->
    ts_callflow:send_hangup(State),
    wait_for_other_leg(State, aleg, ts_callflow:wait_for_cdr(State));
wait_for_other_leg(State, bleg) ->
    wait_for_other_leg(State, bleg, ts_callflow:wait_for_cdr(State)).

wait_for_other_leg(_State, aleg, {cdr, aleg, _CDR, State1}) ->
    ts_callflow:finish_leg(State1, ts_callflow:get_aleg_id(State1));
wait_for_other_leg(_State, bleg, {cdr, bleg, _CDR, State1}) ->
    ts_callflow:finish_leg(State1, ts_callflow:get_bleg_id(State1));
wait_for_other_leg(_State, Leg, {cdr, _OtherLeg, _CDR, State1}) ->
    lager:info("waiting for leg ~s, got cdr for ~s again, ignoring", [Leg, _OtherLeg]),
    wait_for_other_leg(State1, Leg);
wait_for_other_leg(_State, Leg, {timeout, State1}) ->
    lager:info("timed out waiting for ~s CDR, cleaning up", [Leg]),

    ts_callflow:finish_leg(State1, ts_callflow:get_bleg_id(State1)).

try_failover(State) ->
    case {ts_callflow:get_control_queue(State), ts_callflow:get_failover(State)} of
        {<<>>, _} ->
            lager:info("no callctl for failover"),
            ts_callflow:send_hangup(State),
            wait_for_cdr(State);
        {_, undefined} ->
            lager:info("no failover defined"),
            ts_callflow:send_hangup(State),
            wait_for_cdr(State);
        {_, Failover} ->
            case wh_json:is_empty(Failover) of
                true ->
                    lager:info("no failover configured"),
                    ts_callflow:send_hangup(State),
                    wait_for_cdr(State);
                false ->
                    lager:info("trying failover"),
                    case wh_json:get_value(<<"e164">>, Failover) of
                        undefined -> try_failover_sip(State, wh_json:get_value(<<"sip">>, Failover));
                        DID -> try_failover_e164(State, DID)
                    end
            end
    end.

try_failover_sip(State, undefined) ->
    lager:info("SIP failover undefined"),
    wait_for_cdr(State);
try_failover_sip(State, SIPUri) ->
    CallID = ts_callflow:get_aleg_id(State),
    CtlQ = ts_callflow:get_control_queue(State),
    Q = ts_callflow:get_my_queue(State),

    lager:info("routing to failover sip uri: ~s", [SIPUri]),

    EndPoint = wh_json:from_list([{<<"Invite-Format">>, <<"route">>}
                                  ,{<<"Route">>, SIPUri}
                                 ]),

    %% since we only route to one endpoint, we specify most options on the endpoint's leg
    Command = [{<<"Call-ID">>, CallID}
               ,{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [EndPoint]}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],

    wapi_dialplan:publish_command(CtlQ, Command),
    wait_for_bridge(ts_callflow:set_failover(State, wh_json:new())).

try_failover_e164(State, ToDID) ->
    CallID = ts_callflow:get_aleg_id(State),

    AcctID = ts_callflow:get_account_id(State),
    EP = ts_callflow:get_endpoint_data(State),
    CtlQ = ts_callflow:get_control_queue(State),
    Q = ts_callflow:get_my_queue(State),

    Req = [{<<"Call-ID">>, CallID}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, ToDID}
           ,{<<"Account-ID">>, AcctID}
           ,{<<"Control-Queue">>, CtlQ}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Inception">>, <<"off-net">>}])}
           ,{<<"Flags">>, wh_json:get_value(<<"flags">>, EP)}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, EP)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, EP)}
           ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, EP)}
           ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, EP)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, EP)}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    lager:info("sending offnet request for DID ~s", [ToDID]),
    wapi_offnet_resource:publish_req(Req),

    wait_for_bridge(ts_callflow:set_failover(State, wh_json:new())).

%%--------------------------------------------------------------------
%% Out-of-band functions
%%--------------------------------------------------------------------
-spec get_endpoint_data(wh_json:json_object()) -> {'endpoint', wh_json:json_object()}.
get_endpoint_data(JObj) ->
    %% wh_timer:tick("inbound_route/1"),
    AcctId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),

    {ToUser, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    ToDID = wnm_util:to_e164(ToUser),

    {ok, AcctId, NumberProps} = wh_number_manager:lookup_account_by_number(ToDID),
    ForceOut = props:get_value(force_outbound, NumberProps),
    lager:info("acct ~s force out ~s", [AcctId, ForceOut]),

    RoutingData = routing_data(ToDID, AcctId),

    AuthUser = props:get_value(<<"To-User">>, RoutingData),
    AuthRealm = props:get_value(<<"To-Realm">>, RoutingData),

    InFormat = props:get_value(<<"Invite-Format">>, RoutingData, <<"username">>),
    Invite = ts_util:invite_format(wh_util:to_lower_binary(InFormat), ToDID) ++ RoutingData,

    {endpoint, wh_json:from_list([{<<"Custom-Channel-Vars">>, wh_json:from_list([
                                                                                 {<<"Auth-User">>, AuthUser}
                                                                                 ,{<<"Auth-Realm">>, AuthRealm}
                                                                                 ,{<<"Direction">>, <<"inbound">>}
                                                                                ])
                                  }
                                  | Invite
                                 ])
    }.

-spec routing_data(ne_binary(), ne_binary()) -> [{<<_:48,_:_*8>>,_},...] | [].
-spec routing_data(ne_binary(), ne_binary(), wh_json:json_object()) -> [{<<_:48,_:_*8>>,_},...] | [].
routing_data(ToDID, AcctID) ->
    case ts_util:lookup_did(ToDID, AcctID) of
        {ok, Settings} ->
            lager:info("got settings for DID ~s", [ToDID]),
            routing_data(ToDID, AcctID, Settings);
        {error, no_did_found} ->
            lager:info("DID ~s not found in ~s", [ToDID, AcctID]),
            throw(no_did_found)
    end.

routing_data(ToDID, AcctID, Settings) ->
    AuthOpts = wh_json:get_value(<<"auth">>, Settings, wh_json:new()),
    Acct = wh_json:get_value(<<"account">>, Settings, wh_json:new()),
    DIDOptions = wh_json:get_value(<<"DID_Opts">>, Settings, wh_json:new()),
    RouteOpts = wh_json:get_value(<<"options">>, DIDOptions, []),

    NumConfig = case wh_number_manager:get_public_fields(ToDID, AcctID) of
                    {ok, Fields} -> Fields;
                    {error, _} -> wh_json:new()
                end,

    AuthU = wh_json:get_value(<<"auth_user">>, AuthOpts),
    AuthR = wh_json:get_value(<<"auth_realm">>, AuthOpts, wh_json:get_value(<<"auth_realm">>, Acct)),

    {Srv, AcctStuff} = try
                           {ok, AccountSettings} = ts_util:lookup_user_flags(AuthU, AuthR, AcctID),
                           lager:info("got account settings"),
                           {
                             wh_json:get_value(<<"server">>, AccountSettings, wh_json:new())
                             ,wh_json:get_value(<<"account">>, AccountSettings, wh_json:new())
                           }
                       catch
                           _A:_B ->
                               lager:info("failed to get account settings: ~p: ~p", [_A, _B]),
                               {wh_json:new(), wh_json:new()}
                       end,

    SrvOptions = wh_json:get_value(<<"options">>, Srv, wh_json:new()),

    case wh_util:is_true(wh_json:get_value(<<"enabled">>, SrvOptions)) of
        false -> throw({server_disabled, wh_json:get_value(<<"id">>, Srv)});
        true -> ok
    end,

    InboundFormat = wh_json:get_value(<<"inbound_format">>, SrvOptions, <<"npan">>),

    {CalleeName, CalleeNumber} = callee_id([
                                            wh_json:get_value(<<"caller_id">>, DIDOptions)
                                            ,wh_json:get_value(<<"callerid_account">>, Settings)
                                            ,wh_json:get_value(<<"callerid_server">>, Settings)
                                           ]),

    ProgressTimeout = ts_util:progress_timeout([
                                                wh_json:get_value(<<"progress_timeout">>, DIDOptions)
                                                ,wh_json:get_value(<<"progress_timeout">>, SrvOptions)
                                                ,wh_json:get_value(<<"progress_timeout">>, AcctStuff)
                                               ]),

    BypassMedia = ts_util:bypass_media([
                                        wh_json:get_value(<<"media_handling">>, DIDOptions)
                                        ,wh_json:get_value(<<"media_handling">>, SrvOptions)
                                        ,wh_json:get_value(<<"media_handling">>, AcctStuff)
                                       ]),

    FailoverLocations = [
                         wh_json:get_value(<<"failover">>, NumConfig)
                         ,wh_json:get_value(<<"failover">>, DIDOptions)
                         ,wh_json:get_value(<<"failover">>, SrvOptions)
                         ,wh_json:get_value(<<"failover">>, AcctStuff)
                        ],
    lager:info("looking for failover in ~p", [FailoverLocations]),

    Failover = ts_util:failover(FailoverLocations),
    lager:info("failover found: ~p", [Failover]),

    Delay = ts_util:delay([
                           wh_json:get_value(<<"delay">>, DIDOptions)
                           ,wh_json:get_value(<<"delay">>, SrvOptions)
                           ,wh_json:get_value(<<"delay">>, AcctStuff)
                          ]),

    SIPHeaders = ts_util:sip_headers([
                                      wh_json:get_value(<<"sip_headers">>, DIDOptions)
                                      ,wh_json:get_value(<<"sip_headers">>, SrvOptions)
                                      ,wh_json:get_value(<<"sip_headers">>, AcctStuff)
                                     ]),

    IgnoreEarlyMedia = ts_util:ignore_early_media([
                                                   wh_json:get_value(<<"ignore_early_media">>, DIDOptions)
                                                   ,wh_json:get_value(<<"ignore_early_media">>, SrvOptions)
                                                   ,wh_json:get_value(<<"ignore_early_media">>, AcctStuff)
                                                  ]),

    Timeout = ts_util:ep_timeout([
                                  wh_json:get_value(<<"timeout">>, DIDOptions)
                                  ,wh_json:get_value(<<"timeout">>, SrvOptions)
                                  ,wh_json:get_value(<<"timeout">>, AcctStuff)
                                 ]),

    %% Bridge Endpoint fields go here
    %% See http://wiki.2600hz.org/display/whistle/Dialplan+Actions#DialplanActions-Endpoint
    [KV || {_,V}=KV <- [ {<<"Invite-Format">>, InboundFormat}
                         ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Srv)}
                         ,{<<"Bypass-Media">>, BypassMedia}
                         ,{<<"Endpoint-Progress-Timeout">>, ProgressTimeout}
                         ,{<<"Failover">>, Failover}
                         ,{<<"Endpoint-Delay">>, Delay}
                         ,{<<"SIP-Headers">>, SIPHeaders}
                         ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                         ,{<<"Endpoint-Timeout">>, Timeout}
                         ,{<<"Callee-ID-Name">>, CalleeName}
                         ,{<<"Callee-ID-Number">>, CalleeNumber}
                         ,{<<"To-User">>, AuthU}
                         ,{<<"To-Realm">>, AuthR}
                         ,{<<"To-DID">>, ToDID}
                         ,{<<"Route-Options">>, RouteOpts}
                       ],
           V =/= undefined,
           V =/= <<>>
    ].

callee_id([]) -> {undefined, undefined};
callee_id([undefined | T]) -> callee_id(T);
callee_id([<<>> | T]) -> callee_id(T);
callee_id([JObj | T]) ->
    case wh_json:is_json_object(JObj) andalso (not wh_json:is_empty(JObj)) of
        true ->
            case {wh_json:get_value(<<"cid_name">>, JObj), wh_json:get_value(<<"cid_number">>, JObj)} of
                {undefined, undefined} ->
                    callee_id(T);
                CalleeID -> CalleeID
            end;
        false ->
            callee_id(T)
    end.
