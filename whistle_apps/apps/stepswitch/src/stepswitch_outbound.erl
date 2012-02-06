%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle offnet requests, including rating them
%%% @end
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

init() ->
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process a Whistle offnet resource request (outbound) for a audio
%% route
%% @end
%%--------------------------------------------------------------------
handle_req(JObj, Props) ->
    whapps_util:put_callid(JObj),
    true = wapi_offnet_resource:req_v(JObj),
    ?LOG_START("received outbound request"),
    <<"audio">> = wh_json:get_value(<<"Resource-Type">>, JObj),
    Number = wh_json:get_value(<<"To-DID">>, JObj),
    ?LOG("outbound request to ~s from account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
    CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    Result = attempt_to_fullfill_req(Number, CtrlQ, JObj, Props),
    wapi_offnet_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                      ,response(Result, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec attempt_to_fullfill_req/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), proplist()) -> bridge_resp() | execute_ext_resp().
attempt_to_fullfill_req(Number, CtrlQ, JObj, Props) ->
    Result = case stepswitch_util:lookup_number(Number) of
                 {ok, AccountId, false} ->
                     execute_local_extension(Number, AccountId, CtrlQ, JObj);
                 _ ->
                     Flags = wh_json:get_value(<<"Flags">>, JObj),
                     Resources = props:get_value(resources, Props), 
                     {Endpoints, IsEmergency} = find_endpoints(Number, Flags, Resources),
                     bridge_to_endpoints(Endpoints, IsEmergency, CtrlQ, JObj)
             end,
    CIDNum = wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj
                               ,wh_json:get_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    case {Result, correct_shortdial(Number, CIDNum)} of
        {{error, no_resources}, fail} -> Result;
        {{error, no_resources}, CorrectedNumber} -> 
            ?LOG("found no resources for number as dialed, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fullfill_req(CorrectedNumber, CtrlQ, JObj, Props);
        _Else -> Result
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
    ?LOG("found resources that handle the number...to the cloud!"),
    Q = create_queue(JObj),
    CCVs = wh_json:set_value(<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj, <<>>)
                             ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
    {CIDNum, CIDName} = case IsEmergency of
                            'true' ->
                                ?LOG("outbound call is using an emergency route, attempting to set CID accordingly"),
                                {wh_json:get_value(<<"Emergency-Caller-ID-Number">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)),
                                 wh_json:get_value(<<"Emergency-Caller-ID-Name">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj))};
                            'false'  ->
                                {wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj),
                                 wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
                        end,
    ?LOG("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
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
%% When the outbound number belongs to another account on the system
%% simply execute that callflow in the context of this call (think 
%% macro).  This function will block until that callflow is complete.
%% @end
%%--------------------------------------------------------------------
-spec execute_local_extension/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> execute_ext_resp().
execute_local_extension(Number, AccountId, CtrlQ, JObj) ->
    ?LOG("number belongs to another account, executing callflow from that account"),
    Q = create_queue(JObj),
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
    ?LOG("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
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
                    AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                                    wh_json:get_value(<<"Hangup-Cause">>, JObj)),
                    Result = case lists:member(AppResponse, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>
                                                                 ,<<"SUCCESS">>]) of
                                 true -> ok;
                                 false -> fail
                             end,
                    {Result, JObj};
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
            AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                            wh_json:get_value(<<"Hangup-Cause">>, JObj)),
            Result = case lists:member(AppResponse, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>
                                                         ,<<"SUCCESS">>]) of
                         true -> ok;
                         false -> fail
                     end,
            case get_event_type(JObj) of               
                {<<"error">>, <<"dialplan">>, _} ->
                    {error, JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    ?LOG("outbound request bridged to call ~s", [CallId]),
                    wait_for_bridge(infinity);
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    {Result, JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
                    {Result, JObj};
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
%% Create a queue for consuming the channel events on.  If we need this
%% queue then we suspect we have a chance to consume a resource so also
%% try to rate this call...
%% @end
%%--------------------------------------------------------------------
-spec create_queue/1 :: (wh_json:json_object()) -> ne_binary().
create_queue(JObj) ->
    Q = amqp_util:new_queue(),
    amqp_util:basic_consume(Q),
    wapi_call:bind_q(Q, [{restrict_to, [events, cdr]}
                         ,{callid, get(callid)}
                        ]),
    wapi_self:bind_q(Q, []),
    ?LOG("consuming call events"),
    request_rating(JObj),
    Q.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a whistle API request to rate this call
%% @end
%%--------------------------------------------------------------------
-spec request_rating/1 :: (wh_json:json_object()) -> 'ok'.
request_rating(JObj) ->
    whapps_util:put_callid(JObj),
    ?LOG("sending rate request"),
    Req = [{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
           ,{<<"From-DID">>, wh_json:get_value(<<"From-DID">>, JObj)}
           ,{<<"Call-ID">>, get(callid)}
           ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, JObj)}
           ,{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj)}
           ,{<<"Options">>, wh_json:get_value(<<"Flags">>, JObj, [])}
           ,{<<"Direction">>, <<"outbound">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_call:publish_rate_req(get(callid), [KV || {_,V}=KV <- Req, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull apart a Whistle API message and return a tuple that we can easily
%% us in a case clause to determine the appropriate action.
%% @end
%%--------------------------------------------------------------------
-spec get_event_type/1 :: (wh_json:json_object()) -> {binary(), binary(), undefined | binary()}.
get_event_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj, <<>>)
      ,wh_json:get_value(<<"Event-Name">>, JObj, <<>>)
      ,wh_json:get_value(<<"Application-Name">>, JObj, <<>>) }.

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
                        _ = [?LOG("resource must have ~s flag", [F]) || F <- Flags],
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
    ?LOG("found resource ~s (~s)", [Gateway#gateway.resource_id, Route]),
    CCVs = [{<<"Resource-ID">>, Gateway#gateway.resource_id}],
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"Route">>, stepswitch_util:get_dialstring(Gateway, Number)}
            ,{<<"Callee-ID-Name">>, wh_util:to_binary(Number)}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
            ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
            ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(Gateway#gateway.progress_timeout)}
            ,{<<"Codecs">>, Gateway#gateway.codecs}
            ,{<<"Auth-User">>, Gateway#gateway.username}
            ,{<<"Auth-Password">>, Gateway#gateway.password}
            ,{<<"SIP-Headers">>, Gateway#gateway.sip_headers}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= 'undefined' andalso V =/= <<"0">>]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% given the result of either wait_for_bridge or wait_for_execute_extension
%% create and send a Whistle offnet resource response
%% @end
%%--------------------------------------------------------------------
-spec response/2 :: (bridge_resp() | execute_ext_resp(), wh_json:json_object()) -> proplist().
response({ok, _}, JObj) ->
    ?LOG_END("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({fail, BridgeResp}, JObj) ->
    ?LOG_END("resources for outbound request failed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_value(<<"Application-Response">>, BridgeResp
                                                 ,wh_json:get_value(<<"Hangup-Cause">>, BridgeResp, <<"ERROR">>))}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, no_resources}, JObj) ->
    ?LOG_END("no available resources"),
    ErrorMsg = <<"no available resources">>,
    To = wh_json:get_value(<<"To-DID">>, JObj),
    whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
                                    ,"Alert: could not process ~s~n"
                                    ,"Fault: ~p~n"
                                    ,"Call-ID: ~s~n"]
                      ,[?MODULE, ?LINE, To, ErrorMsg, get(callid)]),    
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NO_RESOURCES">>}
     ,{<<"Response-Code">>, <<"sip:404">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, timeout}, JObj) ->
    ?LOG_END("attempt to connect to resources timed out"),
    ErrorMsg = <<"bridge request timed out">>,
    To = wh_json:get_value(<<"To-DID">>, JObj),
    whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
                                    ,"Alert: could not process ~s~n"
                                    ,"Fault: ~p~n"
                                    ,"Call-ID: ~s~n"]
                      ,[?MODULE, ?LINE, To, ErrorMsg, get(callid)]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"ERROR">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, ErrorMsg}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({error, Error}, JObj) ->
    ?LOG_END("error during outbound request: ~s", [wh_json:encode(Error)]),
    ErrorMsg = wh_json:get_value(<<"Error-Message">>, Error),
    To = wh_json:get_value(<<"To-DID">>, JObj),
    whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
                                    ,"Alert: could not process ~s~n"
                                    ,"Fault: ~p~n"
                                    ,"Call-ID: ~s~n"]
                      ,[?MODULE, ?LINE, To, ErrorMsg, get(callid)]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"ERROR">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, ErrorMsg}
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
-spec correct_shortdial/2 :: (ne_binary(), ne_binary()) -> ne_binary() | 'fail'.
correct_shortdial(Number, CIDNum) ->
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
