%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 May 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([build/2, build/3]).

-define(CONFIRM_FILE, <<"/opt/freeswitch/sounds/en/us/callie/ivr/8000/ivr-accept_reject_voicemail.wav">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database, and creates one
%% or more whistle API endpoints for use in a bridge string.  Takes
%% into account settings on the callflow, the endpoint, call forwarding,
%% and ringtones.  More functionality to come, but as it is added
%% it will be implicit in all functions that 'ring an endpoing' like
%% devices, ring groups, and resources.
%% @end
%%--------------------------------------------------------------------
-type build_errors() :: 'called_self' | 'not_found' | 'db_not_reachable'.

-spec build/2 :: (EndpointId, Call) -> {'ok', json_objects()} | {'error', build_errors()} when
      EndpointId :: binary(),
      Call :: #cf_call{}.
-spec build/3 :: (EndpointId, Call, Properties) -> {'ok', json_objects()} | {'error', build_errors()} when
      EndpointId :: binary(),
      Call :: #cf_call{},
      Properties :: 'undefined' | json_object().

build(EndpointId, Call) ->
    build(EndpointId, Call, ?EMPTY_JSON_OBJECT).

build(EndpointId, Call, undefined) ->
    build(EndpointId, Call, ?EMPTY_JSON_OBJECT);
build(EndpointId, #cf_call{authorizing_id=AuthId, inception_during_transfer=IDT}=Call, Properties) when is_binary(EndpointId) ->
    case wh_util:is_false(wh_json:get_value(<<"can_call_self">>, Properties, false))
        andalso EndpointId =:= AuthId andalso (not IDT) of
        true ->
            ?LOG("call is from endpoint ~s, skipping", [EndpointId]),
            {error, called_self};
        false ->
            get_endpoints(EndpointId, Call, Properties)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Collects all the appropriate endpoints to reach this endpoint ID
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/3 :: (EndpointId, Call, Properties) -> {'ok', json_objects()} | {'error', 'db_not_reachable' | 'not_found'} when
      EndpointId :: binary(),
      Call :: #cf_call{},
      Properties :: json_object().
get_endpoints(EndpointId, #cf_call{account_db=Db}=Call, Properties) ->
    Fwd = cf_attributes:call_forward(EndpointId, Call),
    case couch_mgr:open_doc(Db, EndpointId) of
        {ok, Endpoint} ->
            case {Fwd, wh_json:is_false(<<"substitute">>, Fwd)} of
                %% if the call forward object is undefined then there is no fwd'n
                {undefined, _} ->
                    {ok, [create_endpoint(Endpoint, Call, Properties)]};
                %% if the call forwarding was not undefined (see above) and substitute is
                %% not explicitly set to false then only ring the fwd'd number
                {_, false} ->
                    {ok, [create_call_fwd_endpoint(Endpoint, Call, Properties, Fwd)]};
                %% if the call forwarding was not undefined (see above) and substitute is
                %% explicitly set to false then ring the fwd'd number and the device
                {_, true} ->
                    {ok, [create_call_fwd_endpoint(Endpoint, Call, Properties, Fwd)
                          ,create_endpoint(Endpoint, Call, Properties)]}
            end;
        {error, _Reason}=E ->
            ?LOG("failed to load endpoint ~s, ~w", [EndpointId, _Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoint/3 :: (Endpoint, Call, Properties) -> json_object() when
      Endpoint :: json_object(),
      Call :: #cf_call{},
      Properties :: 'undefined' | json_object().
create_endpoint(Endpoint, #cf_call{request_user=ReqUser, inception=Inception}=Call, Properties) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    ?LOG("creating endpoint for ~s", [EndpointId]),

    SIP = wh_json:get_value(<<"sip">>, Endpoint, ?EMPTY_JSON_OBJECT),
    Media = wh_json:get_value(<<"media">>, Endpoint, ?EMPTY_JSON_OBJECT),

    case Inception of
        <<"off-net">> ->
            {CalleeNum, CalleeName} =
                cf_attributes:callee_id(<<"external">>, EndpointId, OwnerId, Call),
            SIPHeaders =
                merge_sip_headers(
                  wh_json:get_value([<<"ringtones">>, <<"external">>], Endpoint)
                  ,wh_json:get_value(<<"custom_sip_headers">>, SIP));
        _ ->
            {CalleeNum, CalleeName} =
                cf_attributes:callee_id(<<"internal">>, EndpointId, OwnerId, Call),
            SIPHeaders =
                merge_sip_headers(
                  wh_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint)
                  ,wh_json:get_value(<<"custom_sip_headers">>, SIP))
    end,

    Prop = [{<<"Invite-Format">>, wh_json:get_value(<<"invite_format">>, SIP, <<"username">>)}
            ,{<<"To-User">>, wh_json:get_value(<<"username">>, SIP)}
            ,{<<"To-Realm">>, wh_json:get_value(<<"realm">>, SIP)}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, SIP, ReqUser)}
            ,{<<"Route">>, wh_json:get_value(<<"route">>, SIP)}
            ,{<<"Callee-ID-Number">>, CalleeNum}
            ,{<<"Callee-ID-Name">>, CalleeName}
            ,{<<"Ignore-Early-Media">>, wh_json:get_binary_boolean(<<"ignore_early_media">>, Media)}
            ,{<<"Bypass-Media">>, wh_json:get_binary_boolean(<<"bypass_media">>, Media)}
            ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_binary_value(<<"progress_timeout">>, Media)}
            ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Media)}
            ,{<<"SIP-Headers">>, SIPHeaders} % converted to json_object() in merge_headers/2
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Endpoint-ID">>, EndpointId}
							    ,{<<"Owner-ID">>, OwnerId}
							   ])}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= undefined ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command when
%% the deivce (or owner) has forwarded their phone.  This endpoint
%% is comprised of a route based on CallFwd, the relevant settings
%% from the actuall endpoint, and the properties of this endpoint in
%% the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_call_fwd_endpoint/4 :: (Endpoint, Call, Properties, CallFwd) -> json_object() when
      Endpoint :: json_object(),
      Call :: #cf_call{},
      Properties :: json_object(),
      CallFwd :: json_object().
create_call_fwd_endpoint(Endpoint, #cf_call{request_user=ReqUser, inception=Inception
					    ,authorizing_id=AuthId, owner_id=AuthOwnerId
					    ,cid_number=CIDNum, cid_name=CIDName}=Call
			 ,Properties, CallFwd) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    ?LOG("creating call forward endpoint for ~s", [EndpointId]),
    ?LOG("call forwarding number set to ~s", [wh_json:get_value(<<"number">>, CallFwd)]),

    {CalleeNum, CalleeName} = cf_attributes:callee_id(<<"external">>, EndpointId, OwnerId, Call),

    {CallerNum, CallerName} = case Inception of
                                  <<"off-net">> -> {CIDNum, CIDName};
                                  _ ->
                                      cf_attributes:callee_id(<<"internal">>, AuthId, AuthOwnerId, Call)
                              end,

    SIP = wh_json:get_value(<<"sip">>, Endpoint, ?EMPTY_JSON_OBJECT),
    Media = wh_json:get_value(<<"media">>, Endpoint, ?EMPTY_JSON_OBJECT),

    CCV1 = case wh_util:is_true(wh_json:get_value(<<"keep_caller_id">>, CallFwd)) of
               true ->
                   ?LOG("call forwarding configured to keep the caller id"),
                   [{<<"Call-Forward">>, <<"true">>}
                    ,{<<"Retain-CID">>, <<"true">>}];
               false ->
                   ?LOG("call forwarding configured use the caller id of the endpoint"),
                   [{<<"Call-Forward">>, <<"true">>}]
           end,

    CCV2 = case wh_util:is_true(wh_json:get_value(<<"require_keypress">>, CallFwd)) of
               true ->
                   ?LOG("call forwarding configured to require key press"),
                   IgnoreEarlyMedia = <<"true">>,
                   [{<<"Confirm-Key">>, <<"1">>}
                    ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                    ,{<<"Confirm-File">>, ?CONFIRM_FILE}] ++ CCV1;
               false ->
                   IgnoreEarlyMedia
                       = wh_json:get_binary_boolean(<<"ignore_early_media">>, Media),
                   CCV1
           end,

    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, ReqUser)}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallFwd, <<"unknown">>))/binary>>}
            ,{<<"Caller-ID-Number">>, CallerNum}
            ,{<<"Caller-ID-Name">>, CallerName}
            ,{<<"Callee-ID-Number">>, CalleeNum}
            ,{<<"Callee-ID-Name">>, CalleeName}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Leg-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Leg-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, SIP)}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Authorizing-ID">>, EndpointId}
							    ,{<<"Owner-ID">>, OwnerId}
							    | CCV2])}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= undefined ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the sip headers json object for the whistle API with by
%% merging the 'ringtones' (alert-info) with any custom sip headers
%% @end
%%--------------------------------------------------------------------
-spec merge_sip_headers/2 :: (AlertInfo, CustomHeaders) -> 'undefined' | json_object() when
      AlertInfo :: 'undefined' | json_object(),
      CustomHeaders :: 'undefined' | json_object().
merge_sip_headers(undefined, undefined) ->
    undefined;
merge_sip_headers(AlertInfo, undefined) ->
    wh_json:set_value(<<"Alert-Info">>, AlertInfo, wh_json:new());
merge_sip_headers(undefined, CustomHeaders) ->
    CustomHeaders;
merge_sip_headers(AlertInfo, JObj) ->
    wh_json:set_value(<<"Alert-Info">>, AlertInfo, JObj).
