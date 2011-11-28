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

-export([build/2, build/3, build/4]).

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
-spec build/4 :: (EndpointId, Call, Properties, RejectSelf) -> {'ok', json_objects()} | {'error', build_errors()} when
      EndpointId :: binary(),
      Call :: #cf_call{},
      Properties :: 'undefined' | json_object(),
      RejectSelf :: boolean().

build(EndpointId, Call) ->
    build(EndpointId, Call, ?EMPTY_JSON_OBJECT).

build(undefined, _Call, _Properties) ->
    {error, endpoint_id_undefined};
build(EndpointId, Call, undefined) ->
    build(EndpointId, Call, ?EMPTY_JSON_OBJECT);
build(EndpointId, Call, Properties) ->
    build(EndpointId, Call, Properties, wh_json:is_false(<<"can_call_self">>, Properties)).

build(EndpointId, #cf_call{authorizing_id=EndpointId}, _Properties, true) ->
        ?LOG("call is from endpoint ~s, skipping", [EndpointId]),
        {error, called_self};
build(EndpointId, #cf_call{account_db=Db}=Call, Properties, false) ->
    case couch_mgr:open_doc(Db, EndpointId) of
        {ok, Endpoint} ->
            OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
            Fwd = cf_attributes:call_forward(EndpointId, OwnerId, Call),
            case {Fwd, wh_json:is_false(<<"substitute">>, Fwd)} of
                %% if the call forward object is undefined then there is no fwd'n
                {undefined, _} ->
                    {ok, [create_sip_endpoint(Endpoint, Call, Properties)]};
                %% if the call forwarding was not undefined (see above) and substitute is
                %% not explicitly set to false then only ring the fwd'd number
                {_, false} ->
                    {ok, [create_call_fwd_endpoint(Endpoint, Call, Properties, Fwd)]};
                %% if the call forwarding was not undefined (see above) and substitute is
                %% explicitly set to false then ring the fwd'd number and the device
                {_, true} ->
                    {ok, [create_call_fwd_endpoint(Endpoint, Call, Properties, Fwd)
                          ,create_sip_endpoint(Endpoint, Call, Properties)]}
            end;
        {error, R}=E ->
            ?LOG("failed to load endpoint ~s, ~p", [EndpointId, R]),
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
-spec create_sip_endpoint/3 :: (Endpoint, Call, Properties) -> json_object() when
      Endpoint :: json_object(),
      Call :: #cf_call{},
      Properties :: 'undefined' | json_object().
create_sip_endpoint(Endpoint, #cf_call{request_user=ReqUser, inception=Inception}=Call, Properties) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    ?LOG("creating endpoint for ~s", [EndpointId]),

    SIP = wh_json:get_value(<<"sip">>, Endpoint, ?EMPTY_JSON_OBJECT),
    Media = wh_json:get_value(<<"media">>, Endpoint, ?EMPTY_JSON_OBJECT),

    HeaderFuns = [fun(J) -> 
                           case wh_json:get_value(<<"custom_sip_headers">>, SIP) of
                               undefined -> J;
                               CustomHeaders ->
                                   wh_json:merge_jobjs(CustomHeaders, J)
                           end
                  end
                  ,fun(J) when Inception =:= <<"off-net">> ->
                           case wh_json:get_value([<<"ringtones">>, <<"external">>], Endpoint) of
                                undefined -> J;
                                Ringtone ->
                                    wh_json:set_value(<<"Alert-Info">>, Ringtone, J)
                           end
                  end
                  ,fun(J) when Inception =/= <<"off-net">> ->
                           case wh_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint) of
                                undefined -> J;
                                Ringtone ->
                                    wh_json:set_value(<<"Alert-Info">>, Ringtone, J)
                           end
                  end
                 ],

   CCVFuns = [fun(J) ->
                        case wh_json:is_true(<<"fax_enabled">>, Endpoint) of
                            false -> J;
                            true -> wh_json:set_value(<<"Fax-Enabled">>, <<"true">>, J)
                        end
              end
              ,fun(J) -> 
                        case EndpointId of
                            undefined -> J;
                            _ ->
                                wh_json:set_value(<<"Endpoint-ID">>, EndpointId, J) 
                        end
              end
              ,fun(J) -> 
                        case OwnerId of
                            undefined -> J;
                            _ -> 
                                wh_json:set_value(<<"Owner-ID">>, OwnerId, J)
                        end
              end
             ],

   {CalleeNum, CalleeName} = case Inception of
                                  <<"off-net">> ->
                                      cf_attributes:callee_id(<<"external">>, EndpointId, OwnerId, Call);
                                  _ ->
                                      cf_attributes:callee_id(<<"internal">>, EndpointId, OwnerId, Call)
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
            ,{<<"Hold-Media">>, cf_attributes:media_attributes(<<"hold_id">>, EndpointId, OwnerId, Call)}
            ,{<<"Codecs">>, cf_attributes:media_attributes(<<"codecs">>, EndpointId, OwnerId, Call)}
            ,{<<"SIP-Headers">>, wh_json:from_list(lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, HeaderFuns))}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, CCVFuns))}
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

    CCVFuns = [fun(J) ->
                        case wh_json:is_true(<<"keep_caller_id">>, CallFwd) of
                            false -> J;
                            true -> 
                                ?LOG("call forwarding configured to keep the caller id"),
                                wh_json:set_value(<<"Retain-CID">>, <<"true">>, J)
                        end
              end
              ,fun(J) -> 
                        case EndpointId of
                            undefined -> J;
                            _ ->
                                wh_json:set_value(<<"Endpoint-ID">>, EndpointId, J) 
                        end
              end
              ,fun(J) -> 
                        case OwnerId of
                            undefined -> J;
                            _ -> 
                                wh_json:set_value(<<"Owner-ID">>, OwnerId, J)
                        end
              end
              ,fun(J) ->
                  wh_json:set_value(<<"Call-Forward">>, <<"true">>, J)
              end
              ,fun(J) -> 
                        case wh_json:is_true(<<"require_keypress">>, CallFwd) of
                            undefined -> J;
                            _ ->
                                ?LOG("call forwarding configured to require key press"),
                                Confirm = [{<<"Confirm-Key">>, <<"1">>}
                                           ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                                           ,{<<"Confirm-File">>, ?CONFIRM_FILE}],
                                wh_json:merge_jobjs(wh_json:from_list(Confirm), J)
                        end
              end
             ],

    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallFwd) of
                           true -> <<"true">>;
                           false -> wh_json:get_binary_boolean(<<"ignore_early_media">>, CallFwd)
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
            ,{<<"Hold-Media">>, cf_attributes:media_attributes(<<"hold_id">>, EndpointId, OwnerId, Call)}
            ,{<<"SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, SIP)}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, CCVFuns))}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= undefined ]).
