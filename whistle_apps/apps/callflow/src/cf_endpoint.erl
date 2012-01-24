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

-define(NON_DIRECT_MODULES, [cf_ring_group]).

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
-spec build/2 :: ('undefined' | ne_binary() | wh_json:json_object(), #cf_call{}) -> {'ok', wh_json:json_objects()} | 
                                                                                    {'error', term()}.
-spec build/3 :: ('undefined' | ne_binary() | wh_json:json_object(), 'undefined' | wh_json:json_object(), #cf_call{}) -> {'ok', wh_json:json_objects()} |
                                                                                                                         {'error', term()}.
build(EndpointId, Call) ->
    build(EndpointId, wh_json:new(), Call).

build(undefined, _Properties, _Call) ->
    {error, endpoint_id_undefined};
build(EndpointId, undefined, Call) ->
    build(EndpointId, wh_json:new(), Call);
build(EndpointId, Properties, Call) when is_binary(EndpointId) ->
    case ?MODULE:get(EndpointId, Call) of
        {ok, Endpoint} ->
            case wh_json:is_false(<<"enabled">>, Endpoint) of
                true -> {error, disabled};
                false ->
                    build(Endpoint, Properties, Call)
            end;
        {error, _}=E ->
            E
    end;
build(Endpoint, Properties, #cf_call{owner_id=OwnerId, authorizing_id=AuthorizingId}=Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    EndpointOwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    case {EndpointId, EndpointOwnerId, CanCallSelf} of
        {_, _, true} -> 
            create_endpoints(Endpoint, Properties, Call);
        {AuthorizingId, _, false} when is_binary(AuthorizingId) -> 
            {error, endpoint_called_self};
        {_, OwnerId, false} when is_binary(OwnerId) -> 
            {error, owner_called_self};
        {_, _, false} -> 
            create_endpoints(Endpoint, Properties, Call)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates the actual endpoint json objects for use in the whistle
%% bridge API.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoints/3 :: (wh_json:json_object(), wh_json:json_object(), #cf_call{}) -> {'ok', wh_json:json_objects()} |
                                                                                          {'error', 'no_endpoints'}.
create_endpoints(Endpoint, Properties, Call) ->
    Fwd = cf_attributes:call_forward(Endpoint, Call),
    Substitue = wh_json:is_false(<<"substitute">>, Fwd),
    IgnoreFwd = wh_json:is_true(<<"direct_calls_only">>, Fwd) 
        andalso lists:member(wh_json:get_value(<<"source">>, Properties), ?NON_DIRECT_MODULES),
    Endpoints = case {IgnoreFwd, Substitue, Fwd} of
                    %% if the call forward object is undefined then there is no fwd'n
                    {_, _, undefined} ->
                        ?LOG("callfwd is undefined, try creating sip endpoint"),
                        [catch(create_sip_endpoint(Endpoint, Properties, Call))];
                    %% if ignore ring groups is true and susbtitues is true (hence false via is_false)
                    %% then there are no endpoints to ring
                    {true, false, _} ->
                        ?LOG("no endpoints to ring"),
                        [];
                    %% if ignore ring groups is true and susbtitues is false (hence true via is_false)
                    %% then try to ring just the device
                    {true, true, _} ->
                        ?LOG("trying to ring just the device"),
                        [catch(create_sip_endpoint(Endpoint, Properties, Call))];
                    %% if we are not ignoring ring groups and and substitute is not set to false
                    %% (hence false via is_false) then only ring the fwd'd number
                    {false, false, _} ->
                        ?LOG("trying to ring the fwd number in ring group"),
                        [catch(create_call_fwd_endpoint(Endpoint, Properties, Fwd, Call))];
                    %% if we are not ignoring ring groups and and substitute is set to false
                    %% (hence true via is_false) then only ring the fwd'd number
                    {false, true, _} ->
                        ?LOG("trying to ring the fwd number only"),
                        [catch(create_call_fwd_endpoint(Endpoint, Properties, Fwd, Call))
                         ,catch(create_sip_endpoint(Endpoint, Properties, Call))]
                end,
    case lists:filter(fun wh_json:is_json_object/1, Endpoints) of
        [] ->
            {error, no_endpoints};
        Else ->
            {ok, Else}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get/2 :: ('undefined' | ne_binary(), #cf_call{}) -> {'ok', wh_json:json_object()} | {'error', term()}.
get(undefined, _Call) ->
    {error, invalid_endpoint_id};
get(EndpointId, #cf_call{account_db=Db}) ->
    case wh_cache:peek({?MODULE, Db, EndpointId}) of
        {ok, _}=Ok ->
            Ok;
        {error, not_found} ->
            case couch_mgr:open_doc(Db, EndpointId) of
                {ok, JObj}=OK ->
                    wh_cache:store({?MODULE, Db, EndpointId}, JObj, 300),
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
-spec create_sip_endpoint/3 :: (wh_json:json_object(), wh_json:json_object(), #cf_call{}) -> wh_json:json_object().
create_sip_endpoint(Endpoint, Properties, #cf_call{authorizing_id=AuthId, owner_id=OwnerId, account_id=AccountId
                                                   ,request_user=RUser, cid_name=CIDName, cid_number=CIDNum}=Call) ->
    {CalleeNum, CalleeName} = cf_attributes:callee_id(Endpoint, Call),
    {IntCIDNumber, IntCIDName} = case cf_attributes:caller_id(AuthId, OwnerId, <<"internal">>, Call) of
                                     %% if both the internal name and number are the same as the current
                                     %% caller id then leave it alone
                                     {CIDNum, CIDName} -> {undefined, undefined};
                                     %% if both the internal name is the same as the current
                                     %% caller id then leave it alone
                                     {AltCIDNum, CIDName} -> {AltCIDNum, undefined};
                                     %% if both the internal number is the same as the current
                                     %% caller id then leave it alone
                                     {CIDNum, AltCIDName} -> {undefined, AltCIDName};
                                     %% if both the internal number and name are different, use them!
                                     {AltCIDNum, AltCIDName} -> {AltCIDNum, AltCIDName}
                                 end,

    Prop = [{<<"Invite-Format">>, wh_json:get_value([<<"sip">>, <<"invite_format">>], Endpoint, <<"username">>)}
            ,{<<"To-User">>, wh_json:get_value([<<"sip">>, <<"username">>], Endpoint)}
            ,{<<"To-Realm">>, cf_util:get_sip_realm(Endpoint, AccountId)}
            ,{<<"To-DID">>, wh_json:get_value([<<"sip">>, <<"number">>], Endpoint, RUser)}
            ,{<<"Route">>, wh_json:get_value([<<"sip">>, <<"route">>], Endpoint)}
            ,{<<"Outgoing-Caller-ID-Number">>, IntCIDNumber}
            ,{<<"Outgoing-Caller-ID-Name">>, IntCIDName}
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
-spec create_call_fwd_endpoint/4 :: (wh_json:json_object(), wh_json:json_object(), wh_json:json_object(), #cf_call{}) -> wh_json:json_object().
create_call_fwd_endpoint(Endpoint, Properties, CallFwd, #cf_call{request_user=ReqUser}=Call) ->
    ?LOG("call forwarding endpoint to ~s", [wh_json:get_value(<<"number">>, CallFwd)]),
    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallFwd)
                           orelse not wh_json:is_true(<<"substitute">>, CallFwd) of
                           true -> <<"true">>;
                           false -> wh_json:get_binary_boolean(<<"ignore_early_media">>, CallFwd)
                       end,
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, ReqUser)}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallFwd, <<"unknown">>))/binary>>}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Leg-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Leg-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
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
    lists:foldr(fun(F, JObj) -> F(JObj) end, wh_json:new(), HeaderFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the custom channel vars that should be
%% set for this endpoint depending on its settings, and the current
%% call.
%% @end
%%--------------------------------------------------------------------
-spec generate_ccvs/2 :: (wh_json:json_object(), #cf_call{}) -> wh_json:json_object().
-spec generate_ccvs/3 :: (wh_json:json_object(), #cf_call{}, 'undefined' | wh_json:json_object()) -> wh_json:json_object().

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
                                wh_json:set_value(<<"Authorizing-ID">>, EndpointId, J)
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
               ,fun(J) ->
                        case wh_json:get_value(<<"pvt_type">>, Endpoint) of
                            <<"device">> ->
                                ?LOG("setting inherit_codec"),
                                wh_json:set_value(<<"Inherit-Codec">>, <<"true">>, J);
                            false -> J
                        end
                end
              ],
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), CCVFuns).
