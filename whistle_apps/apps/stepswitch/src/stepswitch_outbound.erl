%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle offnet requests, including rating them
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(stepswitch_outbound).

-export([init/0, handle_req/2]).

-include("stepswitch.hrl").

-type bridge_resp() :: {'error', wh_json:json_object()} |
                       {'error', 'timeout'} |
                       {'ok', wh_json:json_object()} |
                       {'fail', wh_json:json_object()}.

-type execute_ext_resp() ::  {'ok', wh_json:json_object()} |
                             {'ok', 'execute_extension'} |
                             {'fail', wh_json:json_object()}.

-type originate_resp() :: {'error', wh_json:json_object()} |
                          {'ok', wh_json:json_object()} |
                          {'ready', wh_json:json_object()} |
                          {'fail', wh_json:json_object()}.

init() ->
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process a Whistle offnet resource request (outbound) for a audio
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> any().
-spec handle_req/3 :: (ne_binary(), wh_json:json_object(), proplist()) -> any().
handle_req(JObj, Props) ->
    _ = whapps_util:put_callid(JObj),
    true = wapi_offnet_resource:req_v(JObj),
    lager:debug("received outbound request"),
    handle_req(wh_json:get_value(<<"Resource-Type">>, JObj), JObj, Props).

handle_req(<<"audio">>, JObj, Props) ->
    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
    lager:debug("bridge request to ~s from account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
    CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    Result = attempt_to_fulfill_bridge_req(Number, CtrlQ, JObj, Props),
    wapi_offnet_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), response(Result, JObj));
handle_req(<<"originate">>, JObj, Props) ->
    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
    lager:debug("originate request to ~s from account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
    Result = attempt_to_fulfill_originate_req(Number, JObj, Props),
    wapi_offnet_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), response(Result, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec attempt_to_fulfill_bridge_req/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), proplist()) -> bridge_resp() | execute_ext_resp() | {'error', 'no_resources'}.
attempt_to_fulfill_bridge_req(Number, CtrlQ, JObj, Props) ->
    Result = case stepswitch_util:lookup_number(Number) of
                 {ok, AccountId, false} ->
                     lager:debug("found local extension, keeping onnet"),
                     execute_local_extension(Number, AccountId, CtrlQ, JObj);
                 _ ->
                     Flags = wh_json:get_value(<<"Flags">>, JObj),
                     Resources = props:get_value(resources, Props),
                     {Endpoints, IsEmergency} = find_endpoints(Number, Flags, Resources),
                     bridge_to_endpoints(Endpoints, IsEmergency, CtrlQ, JObj)
             end,
    case {Result, correct_shortdial(Number, JObj)} of
        {{error, no_resources}, fail} -> Result;
        {{error, no_resources}, CorrectedNumber} ->
            lager:debug("found no resources for number as dialed, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fulfill_bridge_req(CorrectedNumber, CtrlQ, JObj, Props);
        _Else -> Result
    end.

-spec attempt_to_fulfill_originate_req/3 :: (ne_binary(), wh_json:json_object(), proplist()) -> originate_resp().
attempt_to_fulfill_originate_req(Number, JObj, Props) ->
    Flags = wh_json:get_value(<<"Flags">>, JObj),
    Resources = props:get_value(resources, Props),
    {Endpoints, _} = find_endpoints(Number, Flags, Resources),
    case {originate_to_endpoints(Endpoints, JObj), correct_shortdial(Number, JObj)} of
        {{error, no_resources}, fail} -> {error, no_resources};
        {{error, no_resources}, CorrectedNumber} ->
            lager:debug("found no resources for number as originated, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fulfill_originate_req(CorrectedNumber, JObj, Props);
        {Result, _} -> Result
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build a whistle dialplan API to bridge to the provided endpoints, and
%% block until the bridge is complete.  If the Endpoints that we are
%% attempting to use have been flagged as emergency routes then prefer
%% the emergency CID.
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints/4 :: (proplist(), boolean(), ne_binary(), wh_json:json_object()) -> {'error', 'no_resources'} | bridge_resp().
bridge_to_endpoints([], _, _, _) ->
    {error, no_resources};
bridge_to_endpoints(Endpoints, IsEmergency, CtrlQ, JObj) ->
    lager:debug("found resources that bridge the number...to the cloud!"),
    Q = create_queue(),

    {CIDNum, CIDName} = case IsEmergency of
                            'true' ->
                                lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
                                {wh_json:get_value(<<"Emergency-Caller-ID-Number">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)),
                                 wh_json:get_value(<<"Emergency-Caller-ID-Name">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj))};
                            'false'  ->
                                {wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj),
                                 wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
                        end,
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),

    FromURI = case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, false) of
                  true ->
                      case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                          {undefined, _} -> undefined;
                          {_, undefined} -> undefined;
                          {FromNumber, FromRealm} -> <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
                      end;
                  false -> undefined
              end,
    lager:debug("setting from-uri to ~s", [FromURI]),

    CCVs = wh_json:set_values([ KV || {_,V}=KV <- [{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj, <<>>)}
                                                   ,{<<"From-URI">>, FromURI}
                                                   ,{<<"Ignore-Display-Updates">>, <<"true">>}
                                                  ],
                                      V =/= undefined
                              ], wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),

    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
               ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_dialplan:publish_command(CtrlQ, Command),
    wait_for_bridge(whapps_config:get_integer(<<"stepswitch">>, <<"bridge_timeout">>, 30000)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build a whistle dialplan API to bridge to the provided endpoints, and
%% block until the bridge is complete.  If the Endpoints that we are
%% attempting to use have been flagged as emergency routes then prefer
%% the emergency CID.
%% @end
%%--------------------------------------------------------------------
-spec originate_to_endpoints/2 :: (proplist(), wh_json:json_object()) -> {'error', 'no_resources'} | originate_resp().
originate_to_endpoints([], _) ->
    {error, no_resources};
originate_to_endpoints(Endpoints, JObj) ->
    lager:debug("found resources that can originate the number...to the cloud!"),
    Q = create_queue(),

    CIDNum = wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj),

    FromURI = case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, false) of
                  true ->
                      case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                          {undefined, _} -> undefined;
                          {_, undefined} -> undefined;
                          {FromNumber, FromRealm} -> <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
                      end;
                  false -> undefined
              end,

    lager:debug("setting from-uri to ~s", [FromURI]),

    CCVs = wh_json:set_values([ KV || {_,V}=KV <- [{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj, <<>>)}
                                                   ,{<<"From-URI">>, FromURI}
                                                  ],
                                      V =/= undefined
                              ], wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),

    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:rand_hex_binary(16)),
    Application = wh_json:get_value(<<"Application-Name">>, JObj, <<"park">>),
    Request = [{<<"Application-Name">>, Application}
               ,{<<"Application-Data">>, wh_json:get_value(<<"Application-Data">>, JObj)}
               ,{<<"Msg-ID">>, MsgId}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
               ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               | wh_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    wh_amqp_mgr:register_return_handler(),
    wapi_resource:publish_originate_req(Request),
    wait_for_originate(MsgId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When the outbound number belongs to another account on the system
%% simply execute that callflow in the context of this call (think
%% macro).  This function will block until that callflow is complete.
%% @end
%%--------------------------------------------------------------------
-spec execute_local_extension/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> execute_ext_resp().
execute_local_extension(Number, AccountId, CtrlQ, JObj) ->
    lager:debug("number belongs to another account, executing callflow from that account"),
    Q = create_queue(),
    CIDNum = wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj),
    CIDName = wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj),
    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Inception">>, <<"off-net">>}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Caller-ID-Number">>, CIDNum}
            ,{<<"Caller-ID-Name">>, CIDName}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"Callee-ID-Name">>, get_account_name(Number, AccountId)}
           ],
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Command = [{<<"Call-ID">>, get(callid)}
               ,{<<"Extension">>, Number}
               ,{<<"Reset">>, true}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list([ KV || {_, V}=KV <- CCVs, V =/= undefined ])}
               ,{<<"Application-Name">>, <<"execute_extension">>}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_dialplan:publish_command(CtrlQ, Command),
    wait_for_execute_extension().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the originate response/error
%% @end
%%--------------------------------------------------------------------
-spec wait_for_originate/1 :: (ne_binary()) -> originate_resp() | {'error', 'no_resources'}.
wait_for_originate(MsgId) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"resource">>, <<"originate_resp">>, _} ->
                    {hangup_result(JObj), JObj};
                {<<"error">>, <<"originate_resp">>, _} ->
                    {error, JObj};
                {<<"dialplan">>, <<"originate_ready">>, _} ->
                    {ready, JObj};
                _  ->
                    wait_for_originate(MsgId)
            end;
        %% if there are no FS nodes connected (or ecallmgr is down) we get the message
        %% returned so we know...
        {#'basic.return'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case wh_json:get_value(<<"Msg-ID">>, JObj) of
                MsgId -> {error, no_resources};
                _Else -> wait_for_originate(MsgId)
            end;
        _ ->
            wait_for_originate(MsgId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% execute extension to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_execute_extension/0 :: () -> execute_ext_resp().
wait_for_execute_extension() ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    {hangup_result(JObj), JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"execute_extension">>} ->
                    {ok, execute_extension};
                _  ->
                    wait_for_execute_extension()
            end;
        _ ->
            wait_for_execute_extension()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% the bridge to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge/1 :: ('infinity' | pos_integer()) -> bridge_resp().
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"error">>, <<"dialplan">>, _} ->
                    {error, JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("outbound request bridged to call ~s", [CallId]),
                    wait_for_bridge(infinity);
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    {hangup_result(JObj), JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
                    {hangup_result(JObj), JObj};
                _ when Timeout =:= infinity ->
                    wait_for_bridge(Timeout);
                _ ->
                    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_bridge(Timeout - (DiffMicro div 1000))
            end;
        _ when Timeout =:= infinity ->
            wait_for_bridge(Timeout);
        _ ->
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_bridge(Timeout - (DiffMicro div 1000))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the hangup case indicates a call failure
%% @end
%%--------------------------------------------------------------------
-spec hangup_result/1 :: (wh_json:json_object()) -> 'ok' | 'fail'.
hangup_result(JObj) ->
    AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                    wh_json:get_value(<<"Hangup-Cause">>, JObj)),
    case lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES) of
        true -> ok;
        false -> fail
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a queue for consuming the channel events on.  If we need this
%% queue then we suspect we have a chance to consume a resource so also
%% try to rate this call...
%% @end
%%--------------------------------------------------------------------
-spec create_queue/0 :: () -> ne_binary().
create_queue() ->
    Q = amqp_util:new_queue(),

    ok = amqp_util:basic_consume(Q),
    ok = wapi_call:bind_q(Q, [{restrict_to, [events]}
                              ,{callid, get(callid)}
                             ]),
    ok = wapi_self:bind_q(Q, []),
    Q.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull apart a Whistle API message and return a tuple that we can easily
%% us in a case clause to determine the appropriate action.
%% @end
%%--------------------------------------------------------------------
-spec get_event_type/1 :: (wh_json:json_object()) -> {binary(), binary(), binary()}.
get_event_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj, <<>>)
      ,wh_json:get_value(<<"Event-Name">>, JObj, <<>>)
      ,wh_json:get_value(<<"Application-Name">>, JObj, <<>>)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filter the given resources by any flags required and rules that match
%% this number.  Then sort them by weight and build the Endpoints
%% component of a Whistle dialplan bridge API.
%% @end
%%--------------------------------------------------------------------
-spec find_endpoints/3 :: (ne_binary(), [] | [ne_binary(),...], endpoints()) -> {proplist(), boolean()}.
find_endpoints(Number, Flags, Resources) ->
    Endpoints = case Flags of
                    'undefined' ->
                        stepswitch_util:evaluate_number(Number, Resources);
                    Flags ->
                        _ = [lager:debug("resource must have ~s flag", [F]) || F <- Flags],
                        FilteredResources = stepswitch_util:evaluate_flags(Flags, Resources),
                        stepswitch_util:evaluate_number(Number, FilteredResources)
                end,
    {build_endpoints(Endpoints), contains_emergency_endpoint(Endpoints)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the provided resources contain a emergency route
%% @end
%%--------------------------------------------------------------------
-spec contains_emergency_endpoint/1 :: (endpoints()) -> boolean().
-spec contains_emergency_endpoint/2 :: (endpoints(), boolean()) -> boolean().
contains_emergency_endpoint(Endpoints) ->
    contains_emergency_endpoint(Endpoints, false).

contains_emergency_endpoint([], UseEmergency) ->
    UseEmergency;
contains_emergency_endpoint([{_, _, _, _, IsEmergency}|T], UseEmergency) ->
    contains_emergency_endpoint(T, IsEmergency or UseEmergency).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the provided resource records and build the individual endpoint
%% json objects required by Whistle dialplan bridge APIs.  Build
%% in a delay between resources
%% @end
%%--------------------------------------------------------------------
-spec build_endpoints/1 :: (endpoints()) -> proplist().
-spec build_endpoints/3 :: (endpoints(), non_neg_integer(), proplist()) -> proplist().
build_endpoints(Endpoints) ->
    build_endpoints(Endpoints, 0, []).

build_endpoints([], _, Acc) ->
    lists:reverse(Acc);
build_endpoints([{_, GracePeriod, Number, [Gateway], _}|T], Delay, Acc0) ->
    build_endpoints(T, Delay + GracePeriod, [build_endpoint(Number, Gateway, Delay)|Acc0]);
build_endpoints([{_, GracePeriod, Number, Gateways, _}|T], Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [build_endpoint(Number, Gateway, 0)|AccIn]};
                                 (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [build_endpoint(Number, Gateway, D0)|AccIn]}
                            end, {Delay, Acc0}, Gateways),
    build_endpoints(T, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_endpoint/3 :: (ne_binary(), #gateway{}, non_neg_integer()) -> wh_json:json_object().
build_endpoint(Number, Gateway, _Delay) ->
    Route = stepswitch_util:get_dialstring(Gateway, Number),
    lager:debug("found resource ~s (~s)", [Gateway#gateway.resource_id, Route]),

    CCVs = [{<<"Resource-ID">>, Gateway#gateway.resource_id}],
    Prop = [{<<"Invite-Format">>, Gateway#gateway.invite_format}
            ,{<<"Route">>, stepswitch_util:get_dialstring(Gateway, Number)}
            ,{<<"Callee-ID-Name">>, wh_util:to_binary(Number)}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"To-DID">>, wh_util:to_binary(Number)}
            ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
            ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
            ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(Gateway#gateway.progress_timeout)}
            ,{<<"Codecs">>, Gateway#gateway.codecs}
            ,{<<"Auth-User">>, Gateway#gateway.username}
            ,{<<"Auth-Password">>, Gateway#gateway.password}
            ,{<<"SIP-Headers">>, Gateway#gateway.sip_headers}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
            ,{<<"Endpoint-Type">>, Gateway#gateway.endpoint_type}
            ,{<<"Endpoint-Options">>, Gateway#gateway.endpoint_options}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= 'undefined' andalso V =/= <<"0">>]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% given the result of either wait_for_bridge or wait_for_execute_extension
%% create and send a Whistle offnet resource response
%% @end
%%--------------------------------------------------------------------
-spec response/2 :: ({'error', 'no_resources'} | bridge_resp() | execute_ext_resp() | originate_resp(), wh_json:json_object()) -> proplist().
response({ok, Resp}, JObj) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({ready, Resp}, JObj) ->
    lager:debug("originate is ready to execute"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Resp)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Response-Message">>, <<"READY">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({fail, Resp}, JObj) ->
    lager:debug("resources for outbound request failed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_value(<<"Application-Response">>, Resp
                                                 ,wh_json:get_value(<<"Hangup-Cause">>, Resp, <<"ERROR">>))}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, Resp)}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, no_resources}, JObj) ->
    lager:debug("no available resources"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
     ,{<<"Response-Code">>, <<"sip:404">>}
     ,{<<"Error-Message">>, <<"no available resources">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, timeout}, JObj) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"bridge request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, Error}, JObj) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(Error))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, Error, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the given number is shorter then a known caller id then try
%% to pad the front of the dialed number with values from the
%% callerid.
%% @end
%%--------------------------------------------------------------------
-spec correct_shortdial/2 :: (ne_binary(), wh_json:json_object()) -> ne_binary() | 'fail'.
correct_shortdial(Number, JObj) ->
    CIDNum = wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj
                               ,wh_json:get_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    MaxCorrection = whapps_config:get_integer(<<"stepswitch">>, <<"max_shortdial_correction">>, 5),
    case is_binary(CIDNum) andalso (size(CIDNum) - size(Number)) of
        Length when Length =< MaxCorrection, Length > 0 ->
            wnm_util:to_e164(<<(binary:part(CIDNum, 0, Length))/binary, Number/binary>>);
        _ ->
            fail
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% this function is used when the number dialed in on-net to try to
%% get the name of the account for the callee-id
%% @end
%%--------------------------------------------------------------------
-spec get_account_name/2 :: (ne_binary(), ne_binary()) -> ne_binary().
get_account_name(Number, AccountId) when is_binary(Number) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {ok, JObj} -> wh_json:get_ne_value(<<"name">>, JObj, Number);
        _ -> Number
    end.
