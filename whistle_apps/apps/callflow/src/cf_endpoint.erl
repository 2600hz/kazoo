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
-export([get/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates one or more whistle API endpoints for use in a bridge string.
%% Takes into account settings on the callflow, the endpoint, call
%% forwarding, and ringtones.  More functionality to come, but as it is
%% added it will be implicit in all functions that 'ring an endpoing'
%% like devices, ring groups, and resources.
%% @end
%%--------------------------------------------------------------------
-spec build/2 :: (undefined | ne_binary() | json_object(), #cf_call{}) -> {'ok', json_objects()} | {'error', term()}.
-spec build/3 :: (undefined | ne_binary() | json_object(), undefined | json_object(), #cf_call{}) -> {'ok', json_objects()} | {'error', term()}.
-spec build/4 :: (undefined | ne_binary() | json_object()
                  ,undefined | json_object()
                  ,{undefined| ne_binary(), undefined | ne_binary(), undefined | ne_binary()}
                  ,#cf_call{}) -> {'ok', json_objects()} | {'error', term()}.
-spec build/5 :: (undefined | ne_binary() | json_object()
                  ,undefined | json_object()
                  ,{undefined| ne_binary(), undefined | ne_binary(), undefined | ne_binary()}
                  ,boolean()
                  ,#cf_call{}) -> {'ok', json_objects()} | {'error', term()}.

build(EndpointId, Call) ->
    build(EndpointId, ?EMPTY_JSON_OBJECT, Call).

build(undefined, _Properties, _Call) ->
    {error, endpoint_id_undefined};
build(EndpointId, undefined, Call) ->
    build(EndpointId, ?EMPTY_JSON_OBJECT, Call);
build(EndpointId, Properties, Call) when is_binary(EndpointId) ->
    case ?MODULE:get(EndpointId, Call) of
        {ok, Endpoint} ->
            build(Endpoint, Properties, Call);
        {error, _}=E ->
            E
    end;
build(Endpoint, Properties, #cf_call{account_id=AccountId}=Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    build(Endpoint, Properties, {OwnerId, EndpointId, AccountId}, Call).


build(Endpoint, Properties, Hierarchy, Call) ->
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    build(Endpoint, Properties, Hierarchy, CanCallSelf, Call).

build(_Endpoint, _Properties, {OwnerId, _, _}, false, #cf_call{owner_id=OwnerId}) when is_binary(OwnerId) ->
    ?LOG("call originated from same user ~s, skipping", [OwnerId]),
    {error, owner_called_self};
build(_Endpoint, _Properties, {_, EndpointId, _}, false, #cf_call{authorizing_id=EndpointId}) when is_binary(EndpointId) ->
    ?LOG("call originated from same endpoint ~s, skipping", [EndpointId]),
    {error, endpoint_called_self};
build(Endpoint, Properties, {OwnerId, EndpointId, _},  _, Call) ->
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
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get/2 :: (undefined | ne_binary(), #cf_call{}) -> {ok, json_object} | {error, term()}.
get(undefined, _Call) ->
    {error, invalid_endpoint_id};
get(EndpointId, #cf_call{account_db=Db}) ->
    case wh_cache:peek({?MODULE, Db, EndpointId}) of
        {ok, _}=Ok ->
            Ok;
        {error, not_found} ->
            case couch_mgr:open_doc(Db, EndpointId) of
                {ok, JObj}=OK ->
                    wh_cache:store({?MODULE, Db, EndpointId}, JObj, 900),
                    OK;
                {error, R}=E ->
                    ?LOG("unable to fetch endpoint ~s: ~p", [EndpointId, R]),
                    E
            end
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
create_sip_endpoint(Endpoint, #cf_call{request_user=RUser}=Call, Properties) ->
    {CalleeNum, CalleeName} = cf_attributes:callee_id(Endpoint, Call),
    Prop = [{<<"Invite-Format">>, wh_json:get_value([<<"sip">>, <<"invite_format">>], Endpoint, <<"username">>)}
            ,{<<"To-User">>, wh_json:get_value([<<"sip">>, <<"username">>], Endpoint)}
            ,{<<"To-Realm">>, wh_json:get_value([<<"sip">>, <<"realm">>], Endpoint)}
            ,{<<"To-DID">>, wh_json:get_value([<<"sip">>, <<"number">>], Endpoint, RUser)}
            ,{<<"Route">>, wh_json:get_value([<<"sip">>, <<"route">>], Endpoint)}
            ,{<<"Callee-ID-Number">>, CalleeNum}
            ,{<<"Callee-ID-Name">>, CalleeName}
            ,{<<"Ignore-Early-Media">>, wh_json:get_binary_boolean([<<"media">>, <<"ignore_early_media">>], Endpoint)}
            ,{<<"Bypass-Media">>, wh_json:get_binary_boolean([<<"media">>, <<"bypass_media">>], Endpoint)}
            ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_binary_value([<<"media">>, <<"progress_timeout">>], Endpoint)}
            ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"Codecs">>, cf_attributes:media_attributes(Endpoint, <<"codecs">>, Call)}
            ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
            ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
            ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
            ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
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
    ?LOG("call forwarding endpoint to ~s", [wh_json:get_value(<<"number">>, CallFwd)]),
    {CalleeNum, CalleeName} = cf_attributes:callee_id(Endpoint, <<"external">>, Call),
    {CallerNum, CallerName} = case Inception of
                                  <<"off-net">> -> {CIDNum, CIDName};
                                  _ ->
                                      cf_attributes:callee_id(AuthId, AuthOwnerId, <<"external">>, Call)
                              end,
    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallFwd)
                           orelse not wh_json:is_true(<<"substitute">>, CallFwd) of
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
            ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
            ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
            ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
            ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, CallFwd)}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= undefined ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the sip headers that should be set for
%% the endpoint
%% @end
%%--------------------------------------------------------------------
generate_sip_headers(Endpoint, #cf_call{inception=Inception}) ->
    HeaderFuns = [fun(J) ->
                          case wh_json:get_value([<<"sip">>, <<"custom_sip_headers">>], Endpoint) of
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
                           end;
                      (J) ->
                           case wh_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint) of
                               undefined -> J;
                               Ringtone ->
                                   wh_json:set_value(<<"Alert-Info">>, Ringtone, J)
                           end
                   end
                 ],
    lists:foldr(fun(F, JObj) -> F(JObj) end, ?EMPTY_JSON_OBJECT, HeaderFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the custom channel vars that should be
%% set for this endpoint depending on its settings, and the current
%% call.
%% @end
%%--------------------------------------------------------------------
-spec generate_ccvs/2 :: (json_object(), #cf_call{}) -> json_object().
-spec generate_ccvs/3 :: (json_object(), #cf_call{}, undefined | json_object()) -> json_object().

generate_ccvs(Endpoint, Call) ->
    generate_ccvs(Endpoint, Call, undefined).
generate_ccvs(Endpoint, #cf_call{account_id=AccountId}, CallFwd) ->
    CCVFuns = [fun(J) ->
                       case wh_json:is_true(<<"keep_caller_id">>, CallFwd) of
                           false -> J;
                           true ->
                                ?LOG("call forwarding configured to keep the caller id"),
                               wh_json:set_value(<<"Retain-CID">>, <<"true">>, J)
                       end
               end
               ,fun(J) ->
                        case wh_json:get_value(<<"_id">>, Endpoint) of
                            undefined -> J;
                            EndpointId ->
                                wh_json:set_value(<<"Endpoint-ID">>, EndpointId, J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"owner_id">>, Endpoint) of
                            undefined -> J;
                            OwnerId ->
                                wh_json:set_value(<<"Owner-ID">>, OwnerId, J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"pvt_account_id">>, Endpoint) of
                            undefined ->
                                wh_json:set_value(<<"Account-ID">>, AccountId, J);
                            PvtAccountId ->
                                wh_json:set_value(<<"Account-ID">>, PvtAccountId, J)
                        end
                end
               ,fun(J) ->
                        case CallFwd of
                            undefined -> J;
                            _ ->
                                wh_json:set_value(<<"Call-Forward">>, <<"true">>, J)
                        end
                end
               ,fun(J) ->
                        case wh_json:is_true(<<"require_keypress">>, CallFwd) of
                            false -> J;
                            _ ->
                                ?LOG("call forwarding configured to require key press"),
                                Confirm = [{<<"Confirm-Key">>, <<"1">>}
                                           ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                                           ,{<<"Confirm-File">>, ?CONFIRM_FILE}],
                                wh_json:merge_jobjs(wh_json:from_list(Confirm), J)
                        end
                end
               ,fun(J) ->
                       case wh_json:is_false([<<"media">>, <<"fax">>, <<"option">>], Endpoint) of
                           true -> J;
                           false -> wh_json:set_value(<<"Fax-Enabled">>, <<"true">>, J)
                       end
               end
              ],
    lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, CCVFuns).
