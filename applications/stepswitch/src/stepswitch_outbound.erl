%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle offnet requests, including rating them
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(stepswitch_outbound).

-export([init/0]).
-export([handle_req/2]).
-export([get_emergency_cid_number/1]).

-include("stepswitch.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-type bridge_resp() :: {'error', wh_json:object()} |
                       {'error', 'timeout'} |
                       {'ok', wh_json:object()} |
                       {'fail', wh_json:object()}.

-type execute_ext_resp() ::  {'ok', wh_json:object()} |
                             {'ok', 'execute_extension'} |
                             {'fail', wh_json:object()}.

-type originate_resp() :: {'error', wh_json:object()} |
                          {'ok', wh_json:object()} |
                          {'ready', wh_json:object()} |
                          {'fail', wh_json:object()}.

init() -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process a Whistle offnet resource request (outbound) for a audio
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> any().
-spec handle_req(ne_binary(), wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    _ = whapps_util:put_callid(JObj),
    _ = wh_amqp_channel:remove_consumer_pid(),
    'true' = wapi_offnet_resource:req_v(JObj),
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
-spec attempt_to_fulfill_bridge_req(ne_binary(), ne_binary(), wh_json:object(), wh_proplist()) ->
                                           bridge_resp() |
                                           execute_ext_resp() |
                                           {'error', 'no_resources'}.
attempt_to_fulfill_bridge_req(Number, CtrlQ, JObj, Props) ->
    Result = case lookup_number(Number, wh_json:is_true(<<"Force-Outbound">>, JObj, 'false')) of
                 {'ok', AccountId} ->
                     lager:debug("found local extension, keeping onnet"),
                     execute_local_extension(Number, AccountId, CtrlQ, JObj);
                 _ ->
                     Flags = wh_json:get_value(<<"Flags">>, JObj, []),
                     Resources = props:get_value('resources', Props),
                     {Endpoints, IsEmergency} = find_endpoints(Number, Flags, Resources, JObj),
                     bridge_to_endpoints(Endpoints, IsEmergency, CtrlQ, JObj)
             end,
    case {Result, correct_shortdial(Number, JObj)} of
        {{'error', 'no_resources'}, 'fail'} -> Result;
        {{'error', 'no_resources'}, CorrectedNumber} ->
            lager:debug("found no resources for number as dialed, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fulfill_bridge_req(CorrectedNumber, CtrlQ, JObj, Props);
        _Else -> Result
    end.

-spec lookup_number(ne_binary(), boolean()) ->
                           {'ok', ne_binary()} |
                           {'error', 'not_found'}.
lookup_number(Number, ForceOutbound) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            case props:get_value('force_outbound', Props) andalso ForceOutbound of
                'true' -> {'error', 'not_found'};
                'false' -> {'ok', AccountId}
            end;
        _ -> {'error', 'not_found'}
    end.

-spec attempt_to_fulfill_originate_req(ne_binary(), wh_json:object(), wh_proplist()) -> originate_resp().
attempt_to_fulfill_originate_req(Number, JObj, Props) ->
    Flags = wh_json:get_value(<<"Flags">>, JObj, []),
    Resources = props:get_value('resources', Props),
    {Endpoints, _} = find_endpoints(Number, Flags, Resources, JObj),
    case {originate_to_endpoints(Endpoints, JObj),
          correct_shortdial(Number, JObj)} of
        {{'error', 'no_resources'}, 'fail'} -> {'error', 'no_resources'};
        {{'error', 'no_resources'}, CorrectedNumber} ->
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
-spec bridge_to_endpoints(wh_proplist(), boolean(), ne_binary(), wh_json:object()) ->
                                 {'error', 'no_resources'} |
                                 bridge_resp().
bridge_to_endpoints([], _, _, _) -> {'error', 'no_resources'};
bridge_to_endpoints(Endpoints, IsEmergency, CtrlQ, JObj) ->
    lager:debug("found resources that bridge the number...to the cloud!"),
    Q = create_queue(),

    {CIDNum, CIDName} =
        case IsEmergency of
            'true' ->
                lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
                {get_emergency_cid_number(JObj)
                 ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Name">>, JObj,
                                       wh_json:get_ne_value(<<"Outbound-Caller-ID-Name">>, JObj))
                };
            'false'  ->
                {wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj
                                      ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj))
                 ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Name">>, JObj
                                       ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Name">>, JObj))
                }
        end,
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),

    {CalleeIdNumber, CalleeIdName} =
        {wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)
         ,wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)
        },
    lager:debug("set outbound callee id to ~s '~s'", [CalleeIdNumber, CalleeIdName]),

    FromURI = case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false') of
                  'true' ->
                      case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                          {'undefined', _} -> 'undefined';
                          {_, 'undefined'} -> 'undefined';
                          {FromNumber, FromRealm} -> <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
                      end;
                  'false' -> 'undefined'
              end,
    lager:debug("setting from-uri to ~s", [FromURI]),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Updates = [{<<"Account-ID">>, AccountId}
               ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
               ,{<<"From-URI">>, FromURI}
               ,{<<"Ignore-Display-Updates">>, <<"true">>}
               ,{<<"Global-Resource">>, <<"true">>}
              ],
    CCVs = wh_json:set_values(props:filter_undefined(Updates)
                              ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),

    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj, <<"false">>)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
               ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
               ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
               ,{<<"Outbound-Caller-ID-Name">>, CIDName}
               ,{<<"Outbound-Callee-ID-Number">>, CalleeIdNumber}
               ,{<<"Outbound-Callee-ID-Name">>, CalleeIdName}
               ,{<<"Caller-ID-Number">>, CIDNum}
               ,{<<"Caller-ID-Name">>, CIDName}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Command)),
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
-spec originate_to_endpoints(wh_proplist(), wh_json:object()) ->
                                    {'error', 'no_resources'} |
                                    originate_resp().
originate_to_endpoints([], _) -> {'error', 'no_resources'};
originate_to_endpoints(Endpoints, JObj) ->
    lager:debug("found resources that can originate the number...to the cloud!"),
    Q = create_queue(),

    CIDNum = wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj
                                  ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    CIDName = wh_json:get_ne_value(<<"Outbound-Caller-ID-Name">>, JObj
                                   ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Name">>, JObj)),

    {CalleeIdNumber, CalleeIdName} =
        {wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)
         ,wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)
        },
    lager:debug("set outbound callee id to ~s '~s'", [CalleeIdNumber, CalleeIdName]),


    FromURI = case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false') of
                  'true' ->
                      case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                          {'undefined', _} -> 'undefined';
                          {_, 'undefined'} -> 'undefined';
                          {FromNumber, FromRealm} -> <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
                      end;
                  'false' -> 'undefined'
              end,

    lager:debug("setting from-uri to ~s", [FromURI]),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Updates = [{<<"Account-ID">>, AccountId}
               ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
               ,{<<"From-URI">>, FromURI}
               ,{<<"Global-Resource">>, <<"true">>}
              ],
    CCVs = wh_json:set_values(props:filter_undefined(Updates)
                              ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),

    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:rand_hex_binary(16)),
    Application = wh_json:get_value(<<"Application-Name">>, JObj, <<"park">>),
    Request = props:filter_undefined(
                [{<<"Application-Name">>, Application}
                 ,{<<"Application-Data">>, wh_json:get_value(<<"Application-Data">>, JObj)}
                 ,{<<"Msg-ID">>, MsgId}
                 ,{<<"Endpoints">>, Endpoints}
                 ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
                 ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
                 ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
                 ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
                 ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
                 ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
                 ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                 ,{<<"Outbound-Callee-ID-Number">>, CalleeIdNumber}
                 ,{<<"Outbound-Callee-ID-Name">>, CalleeIdName}
                 ,{<<"Caller-ID-Number">>, CIDNum}
                 ,{<<"Caller-ID-Name">>, CIDName}
                 ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                 ,{<<"Continue-On-Fail">>, <<"true">>}
                 ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
                 ,{<<"Custom-Channel-Vars">>, CCVs}
                 ,{<<"Outbound-Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, JObj)}
                 | wh_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
                ]),
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
-spec execute_local_extension(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> execute_ext_resp().
execute_local_extension(Number, AccountId, CtrlQ, JObj) ->
    lager:debug("number belongs to another account, executing callflow from that account"),
    Q = create_queue(),
    CIDNum = wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj
                                  ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    CIDName = wh_json:get_ne_value(<<"Outbound-Caller-ID-Name">>, JObj
                                   ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Name">>, JObj)),

    OffCCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),

    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
            ,{<<"Inception">>, <<"off-net">>}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Caller-ID-Number">>, CIDNum}
            ,{<<"Caller-ID-Name">>, CIDName}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"Callee-ID-Name">>, get_account_name(Number, AccountId)}
            ,{<<"Global-Resource">>, <<"false">>}
            | wh_json:to_proplist(OffCCVs)
           ],
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),

    Command = [{<<"Call-ID">>, get('callid')}
               ,{<<"Extension">>, Number}
               ,{<<"Reset">>, 'true'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:filter_undefined(CCVs))}
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
-spec wait_for_originate(ne_binary()) ->
                                originate_resp() |
                                {'error', 'no_resources'}.
wait_for_originate(MsgId) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"resource">>, <<"originate_resp">>, _} -> hangup_result(JObj);
                {<<"error">>, <<"originate_resp">>, _} -> {'error', JObj};
                {<<"dialplan">>, <<"originate_ready">>, _} -> {'ready', JObj};
                _  -> wait_for_originate(MsgId)
            end;
        %% if there are no FS nodes connected (or ecallmgr is down) we get the message
        %% returned so we know...
        {#'basic.return'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case wh_json:get_value(<<"Msg-ID">>, JObj) of
                MsgId -> {'error', 'no_resources'};
                _Else -> wait_for_originate(MsgId)
            end;
        _ -> wait_for_originate(MsgId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% execute extension to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_execute_extension() -> execute_ext_resp().
wait_for_execute_extension() ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} -> hangup_result(JObj);
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"execute_extension">>} -> {'ok', 'execute_extension'};
                _  -> wait_for_execute_extension()
            end;
        _ -> wait_for_execute_extension()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% the bridge to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge(wh_timeout()) -> bridge_resp().
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"error">>, <<"dialplan">>, _} -> {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("outbound request bridged to call ~s", [CallId]),
                    wait_for_bridge('infinity');
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} -> hangup_result(JObj);
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} -> hangup_result(JObj);
                _ -> wait_for_bridge(whapps_util:decr_timeout(Timeout, Start))
            end;
        _ -> wait_for_bridge(whapps_util:decr_timeout(Timeout, Start))
    after Timeout ->
            lager:debug("timed out after ~p ms", [Timeout]),
            {'error', 'timeout'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the hangup case indicates a call failure
%% @end
%%--------------------------------------------------------------------
-spec hangup_result(wh_json:object()) -> {'ok' | 'fail', wh_json:object()}.
hangup_result(JObj) ->
    AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                    wh_json:get_value(<<"Hangup-Cause">>, JObj)),
    SuccessfulCause = lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES),
    case wh_json:get_value(<<"Hangup-Code">>, JObj) of
        <<"sip:", Code/binary>> when SuccessfulCause ->
            try wh_util:to_integer(Code) < 400 of
                'true' -> {'ok', JObj};
                'false' when SuccessfulCause ->
                    {'fail', wh_json:set_value(<<"Application-Response">>, <<"NORMAL_TEMPORARY_FAILURE">>, JObj)};
                'false' -> {'fail', JObj}
            catch
                _:_ when SuccessfulCause -> {'ok', JObj};
                _:_  -> {'fail', JObj}
            end;
        _Else when SuccessfulCause -> {'ok', JObj};
        _Else -> {'fail', JObj}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a queue for consuming the channel events on.  If we need this
%% queue then we suspect we have a chance to consume a resource so also
%% try to rate this call...
%% @end
%%--------------------------------------------------------------------
-spec create_queue() -> ne_binary().
create_queue() ->
    Q = amqp_util:new_queue(),

    'ok' = amqp_util:basic_consume(Q),
    'ok' = wapi_call:bind_q(Q, [{'restrict_to', ['events']}
                                ,{'callid', get('callid')}
                               ]),
    'ok' = wapi_self:bind_q(Q, []),
    Q.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull apart a Whistle API message and return a tuple that we can easily
%% us in a case clause to determine the appropriate action.
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(wh_json:object()) -> {binary(), binary(), binary()}.
get_event_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj, <<>>)
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
-spec find_endpoints(ne_binary(), ne_binaries(), endpoints(), wh_json:object()) ->
                            {wh_proplist(), boolean()}.
find_endpoints(Number, Flags, Resources, JObj) ->
    Endpoints = case Flags of
                    'undefined' ->
                        stepswitch_util:evaluate_number(Number, Resources);
                    Flags ->
                        FilteredResources = stepswitch_util:evaluate_flags(Flags, Resources),
                        stepswitch_util:evaluate_number(Number, FilteredResources)
                end,
    {build_endpoints(Endpoints, JObj), contains_emergency_endpoint(Endpoints)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the provided resources contain a emergency route
%% @end
%%--------------------------------------------------------------------
-spec contains_emergency_endpoint(endpoints()) -> boolean().
-spec contains_emergency_endpoint(endpoints(), boolean()) -> boolean().
contains_emergency_endpoint(Endpoints) ->
    contains_emergency_endpoint(Endpoints, 'false').

contains_emergency_endpoint([], UseEmergency) -> UseEmergency;
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
-spec build_endpoints(endpoints(), wh_json:object()) -> wh_proplist().
-spec build_endpoints(endpoints(), wh_json:object(), non_neg_integer(), wh_proplist()) -> wh_proplist().
build_endpoints(Endpoints, JObj) ->
    build_endpoints(Endpoints, JObj, 0, []).

build_endpoints([], _, _, Acc) ->
    lists:reverse(Acc);
build_endpoints([{_, GracePeriod, Number, [Gateway], _}|T], JObj, Delay, Acc0) ->
    build_endpoints(T, JObj, Delay + GracePeriod, [build_endpoint(Number, Gateway, Delay, JObj)|Acc0]);
build_endpoints([{_, GracePeriod, Number, Gateways, _}|T], JObj, Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [build_endpoint(Number, Gateway, 0, JObj)|AccIn]};
                                (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [build_endpoint(Number, Gateway, D0, JObj)|AccIn]}
                             end, {Delay, Acc0}, Gateways),
    build_endpoints(T, JObj, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_endpoint(ne_binary(), #gateway{}, non_neg_integer(), wh_json:object()) -> wh_json:object().
build_endpoint(Number, Gateway, _Delay, JObj) ->
    Route = stepswitch_util:get_dialstring(Gateway, Number),
    lager:debug("found resource ~s (~s)", [Gateway#gateway.resource_id, Route]),
    FromUri = case Gateway#gateway.format_from_uri of
                  'false' -> 'undefined';
                  'true' ->
                      from_uri(wh_json:get_value(<<"Outbound-Caller-ID-Number">>, JObj), Gateway#gateway.realm)
              end,
    lager:debug("setting from-uri to ~p on gateway ~p", [FromUri, Gateway#gateway.resource_id]),

    FaxSettings = get_t38_settings(Gateway#gateway.t38_setting),
    CCVs = [{<<"Resource-ID">>, Gateway#gateway.resource_id}
            ,{<<"From-URI">>, FromUri}
           ],

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
            ,{<<"SIP-Interface">>, Gateway#gateway.sip_interface}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:filter_undefined(CCVs))}
            ,{<<"Endpoint-Type">>, Gateway#gateway.endpoint_type}
            ,{<<"Endpoint-Options">>, Gateway#gateway.endpoint_options}
           ] ++ FaxSettings,
    wh_json:from_list([KV || {_, V}=KV <- Prop, V =/= 'undefined' andalso V =/= <<"0">>]).

get_t38_settings(<<"none">>) ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_t38_settings(<<"passthrough">>) ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'true'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_t38_settings(<<"device">>) ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_t38_settings(<<"carrier">>) ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% given the result of either wait_for_bridge or wait_for_execute_extension
%% create and send a Whistle offnet resource response
%% @end
%%--------------------------------------------------------------------
-spec response({'error', 'no_resources' | 'timeout'} |
               bridge_resp() |
               execute_ext_resp() |
               originate_resp()
               ,wh_json:object()) -> wh_proplist().
response({'ok', Resp}, JObj) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'ready', Resp}, JObj) ->
    lager:debug("originate is ready to execute"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Resp)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, Resp)}
     ,{<<"Response-Message">>, <<"READY">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'fail', Response}, JObj) ->
    lager:debug("resources for outbound request failed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_value(<<"Application-Response">>, Response,
                                                 wh_json:get_value(<<"Hangup-Cause">>, Response))}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, Response)}
     ,{<<"Resource-Response">>, Response}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', 'no_resources'}, JObj) ->
    lager:debug("no available resources"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
     ,{<<"Response-Code">>, <<"sip:404">>}
     ,{<<"Error-Message">>, <<"no available resources">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', 'timeout'}, JObj) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"bridge request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', Error}, JObj) ->
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
-spec correct_shortdial(ne_binary(), wh_json:object()) -> ne_binary() | 'fail'.
correct_shortdial(Number, JObj) ->
    CIDNum = wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj
                                  ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    MaxCorrection = whapps_config:get_integer(<<"stepswitch">>, <<"max_shortdial_correction">>, 5),
    case is_binary(CIDNum) andalso (size(CIDNum) - size(Number)) of
        Length when Length =< MaxCorrection, Length > 0 ->
            wnm_util:to_e164(<<(binary:part(CIDNum, 0, Length))/binary, Number/binary>>);
        _ -> 'fail'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% this function is used when the number dialed in on-net to try to
%% get the name of the account for the callee-id
%% @end
%%--------------------------------------------------------------------
-spec get_account_name(ne_binary(), ne_binary()) -> ne_binary().
get_account_name(Number, AccountId) when is_binary(Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:get_ne_value(<<"name">>, JObj, Number);
        _ -> Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do everything we can to ensure a emergency call uses a number with
%% e911 enabled
%% @end
%%--------------------------------------------------------------------
-spec get_emergency_cid_number(wh_json:object()) -> ne_binary().
get_emergency_cid_number(JObj) ->
    Account = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Candidates = [wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)
                  ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj)
                 ],
    Requested = wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj
                                     ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj)),
    case couch_mgr:open_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {'ok', PhoneNumbers} ->
            Numbers = wh_json:get_keys(wh_json:public_fields(PhoneNumbers)),
            E911Enabled = [Number
                           || Number <- Numbers
                                  ,lists:member(<<"dash_e911">>, wh_json:get_value([Number, <<"features">>], PhoneNumbers, []))
                          ],
            get_emergency_cid_number(Requested, Candidates, E911Enabled);
        {'error', _R} ->
            lager:error("unable to fetch the ~s from account ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Account, _R]),
            get_emergency_cid_number(Requested, Candidates, [])
    end.

-spec get_emergency_cid_number(ne_binary(), api_binaries(), ne_binaries()) -> ne_binary().
%% if there are no e911 enabled numbers then either use the global system default
%% or the requested (if there isnt one)
get_emergency_cid_number(Requested, _, []) ->
    case whapps_config:get_non_empty(<<"stepswitch">>, <<"default_emergency_cid_number">>) of
        'undefined' -> Requested;
        DefaultE911 -> DefaultE911
    end;
%% If neither their emergency cid or outgoung cid is e911 enabled but their account
%% has other numbers with e911 then use the first...
get_emergency_cid_number(_, [], [E911Enabled|_]) -> E911Enabled;
%% due to the way we built the candidates list it can contain the atom 'undefined'
%% handle that condition (ignore)
get_emergency_cid_number(Requested, ['undefined'|Candidates], E911Enabled) ->
    get_emergency_cid_number(Requested, Candidates, E911Enabled);
%% check if the first non-atom undefined element in the list is in the list of
%% e911 enabled numbers, if so use it otherwise keep checking.
get_emergency_cid_number(Requested, [Candidate|Candidates], E911Enabled) ->
    case lists:member(Candidate, E911Enabled) of
        'true' -> Candidate;
        'false' -> get_emergency_cid_number(Requested, Candidates, E911Enabled)
    end.

-spec from_uri(api_binary(), ne_binary()) -> api_binary().
from_uri(?NE_BINARY = CNum, Realm) -> <<"sip:", CNum/binary, "@", Realm/binary>>;
from_uri(_, _) -> 'undefined'.
