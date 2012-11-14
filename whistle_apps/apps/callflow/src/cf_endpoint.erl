%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([build/2, build/3]).
-export([get/1, get/2]).
-export([flush/2]).

-define(NON_DIRECT_MODULES, [cf_ring_group, acdc_util]).

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
-type build_errors() :: 'db_not_reachable' | 'disabled' | 'endpoint_called_self'
                      | 'endpoint_id_undefined' | 'invalid_endpoint_id'
                      | 'not_found' | 'owner_called_self'.

-spec build/2 :: (cf_api_binary() | wh_json:json_object(), whapps_call:call()) ->
                         {'ok', wh_json:json_objects()} |
                         {'error', build_errors()}.
-spec build/3 :: (cf_api_binary() | wh_json:json_object(), 'undefined' | wh_json:json_object(), whapps_call:call()) ->
                         {'ok', wh_json:json_objects()} |
                         {'error', build_errors()}.
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
build(Endpoint, Properties, Call) ->
    OwnerId = whapps_call:kvs_fetch(owner_id, Call),
    AuthorizingId = whapps_call:authorizing_id(Call),
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
-spec create_endpoints/3 :: (wh_json:json_object(), wh_json:json_object(), whapps_call:call()) ->
                                    {'ok', wh_json:json_objects()} |
                                    {'error', 'no_endpoints'}.
create_endpoints(Endpoint, Properties, Call) ->
    Fwd = cf_attributes:call_forward(Endpoint, Call),
    Substitue = wh_json:is_false(<<"substitute">>, Fwd),
    IgnoreFwd = wh_json:is_true(<<"direct_calls_only">>, Fwd)
        andalso lists:member(wh_json:get_value(<<"source">>, Properties), ?NON_DIRECT_MODULES),
    Endpoints = case {IgnoreFwd, Substitue, Fwd} of
                    %% if the call forward object is undefined then there is no fwd'n
                    {_, _, undefined} ->
                        lager:debug("callfwd is undefined, try creating sip endpoint"),
                        [catch(create_sip_endpoint(Endpoint, Properties, Call))];
                    %% if ignore ring groups is true and susbtitues is true (hence false via is_false)
                    %% then there are no endpoints to ring
                    {true, false, _} ->
                        lager:debug("no endpoints to ring"),
                        [];
                    %% if ignore ring groups is true and susbtitues is false (hence true via is_false)
                    %% then try to ring just the device
                    {true, true, _} ->
                        lager:debug("trying to ring just the device"),
                        [catch(create_sip_endpoint(Endpoint, Properties, Call))];

                    %% if we are not ignoring ring groups and and substitute is not set to false
                    %% (hence false via is_false) then only ring the fwd'd number
                    {false, false, _} ->
                        lager:debug("trying to ring the fwd number in ring group"),
                        [catch(create_call_fwd_endpoint(Endpoint, Properties, Fwd, Call))];
                    %% if we are not ignoring ring groups and and substitute is set to false
                    %% (hence true via is_false) then only ring the fwd'd number
                    {false, true, _} ->
                        lager:debug("trying to ring the fwd number only"),
                        [catch(create_call_fwd_endpoint(Endpoint, Properties, Fwd, Call))
                         ,catch(create_sip_endpoint(Endpoint, Properties, Call))]
                end,
    case lists:filter(fun wh_json:is_json_object/1, Endpoints) of
        [] -> {error, no_endpoints};
        Else -> {ok, Else}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (whapps_call:call()) ->
                       {'ok', wh_json:json_object()} |
                       {'error', term()}.
-spec get/2 :: ('undefined' | ne_binary(), ne_binary() | whapps_call:call()) ->
                       {'ok', wh_json:json_object()} |
                       {'error', term()}.

get(Call) ->
    get(whapps_call:authorizing_id(Call), Call).

get(undefined, _Call) ->
    {error, invalid_endpoint_id};
get(EndpointId, AccountDb) when is_binary(AccountDb) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, EndpointId}) of
        {ok, _}=Ok -> Ok;
        {error, not_found} ->
            case couch_mgr:open_doc(AccountDb, EndpointId) of
                {ok, JObj}=OK ->
                    wh_cache:store_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, EndpointId}, JObj, 300),
                    OK;
                {error, R}=E ->
                    lager:debug("unable to fetch endpoint ~s: ~p", [EndpointId, R]),
                    E
            end
    end;
get(EndpointId, Call) ->
    get(EndpointId, whapps_call:account_db(Call)).

-spec flush/2 :: (ne_binary(), ne_binary()) -> any().
flush(Db, Id) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, Db, Id}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_sip_endpoint/3 :: (wh_json:json_object(), wh_json:json_object(), whapps_call:call()) ->
                                       wh_json:json_object().
create_sip_endpoint(Endpoint, Properties, Call) ->
    CIDName = whapps_call:caller_id_name(Call),
    CIDNum = whapps_call:caller_id_number(Call),
    {CalleeNum, CalleeName} = cf_attributes:callee_id(Endpoint, Call),

    {IntCIDNumber, IntCIDName} =
        case cf_attributes:caller_id(<<"internal">>, Call) of
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

    OutgoingCIDNum = maybe_format_caller_id_number(Endpoint, IntCIDNumber, Call),

    MediaJObj = wh_json:get_value(<<"media">>, Endpoint),
    SIPJObj = wh_json:get_value(<<"sip">>, Endpoint),

    ForceFax = case wh_json:is_true(<<"fax_option">>, MediaJObj) of
                   false -> undefined;
                   true -> <<"self">>
               end,

    Prop =
        [{<<"Invite-Format">>, invite_format(SIPJObj)}
         ,{<<"To-User">>, to_user(SIPJObj)}
         ,{<<"To-Username">>, to_username(SIPJObj)}
         ,{<<"To-Realm">>, cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call))}
         ,{<<"To-DID">>, to_did(Endpoint, Call)}
         ,{<<"To-IP">>, wh_json:get_value(<<"ip">>, SIPJObj)}
         ,{<<"Route">>, wh_json:get_value(<<"route">>, SIPJObj)}
         ,{<<"Proxy-IP">>, wh_json:get_value(<<"proxy">>, SIPJObj)}
         ,{<<"Forward-IP">>, wh_json:get_value(<<"forward">>, SIPJObj)}
         ,{<<"Outgoing-Caller-ID-Number">>, OutgoingCIDNum}
         ,{<<"Outgoing-Caller-ID-Name">>, IntCIDName}
         ,{<<"Callee-ID-Number">>, CalleeNum}
         ,{<<"Callee-ID-Name">>, CalleeName}
         ,{<<"Ignore-Early-Media">>, wh_json:is_true(<<"ignore_early_media">>, MediaJObj)}
         ,{<<"Bypass-Media">>, wh_json:is_true(<<"bypass_media">>, MediaJObj)}
         ,{<<"Endpoint-Progress-Timeout">>, wh_json:is_true(<<"progress_timeout">>, MediaJObj)}
         ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
         ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
         ,{<<"Codecs">>, cf_attributes:media_attributes(Endpoint, <<"codecs">>, Call)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, wh_json:get_value(<<"outbound_flags">>, Endpoint)}
         ,{<<"Force-Fax">>, ForceFax}
        ],
    wh_json:from_list(props:filter_undefined(Prop)).

-spec invite_format/1 :: (wh_json:json_object()) -> ne_binary().
invite_format(SIPJObj) ->
    wh_json:get_value(<<"invite_format">>, SIPJObj, <<"username">>).

-spec to_did/2 :: (wh_json:json_object(), whapps_call:call()) -> api_binary().
to_did(Endpoint, Call) ->
    wh_json:get_value([<<"sip">>, <<"number">>]
                      ,Endpoint
                      ,whapps_call:request_user(Call)
                     ).

-spec to_user/1 :: (wh_json:json_object()) -> api_binary().
to_user(SIPJObj) ->
    case wh_json:get_ne_value(<<"static_invite">>, SIPJObj) of
        undefined -> wh_json:get_value(<<"username">>, SIPJObj);
        To -> To
    end.

-spec to_username/1 :: (wh_json:json_object()) -> api_binary().
to_username(SIPJObj) ->
    wh_json:get_value(<<"username">>, SIPJObj).

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
-spec create_call_fwd_endpoint/4 :: (wh_json:json_object(), wh_json:json_object(), wh_json:json_object(), whapps_call:call()) -> wh_json:json_object().
create_call_fwd_endpoint(Endpoint, Properties, CallFwd, Call) ->
    lager:debug("call forwarding endpoint to ~s", [wh_json:get_value(<<"number">>, CallFwd)]),
    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallFwd)
                           orelse not wh_json:is_true(<<"substitute">>, CallFwd) of
                           true -> <<"true">>;
                           false -> wh_json:get_binary_boolean(<<"ignore_early_media">>, CallFwd)
                       end,
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, whapps_call:request_user(Call))}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallFwd, <<"unknown">>))/binary>>}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_binary_value([<<"media">>, <<"progress_timeout">>], Endpoint)}
            ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
            ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
            ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, CallFwd)}
           ],
    wh_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the sip headers that should be set for
%% the endpoint
%% @end
%%--------------------------------------------------------------------
generate_sip_headers(Endpoint, Call) ->
    Inception = whapps_call:inception(Call),
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
-spec generate_ccvs/2 :: (wh_json:json_object(), whapps_call:call()) -> wh_json:json_object().
-spec generate_ccvs/3 :: (wh_json:json_object(), whapps_call:call(), 'undefined' | wh_json:json_object()) -> wh_json:json_object().

generate_ccvs(Endpoint, Call) ->
    generate_ccvs(Endpoint, Call, undefined).
generate_ccvs(Endpoint, Call, CallFwd) ->
    CCVFuns = [fun(J) ->
                       case wh_json:is_true(<<"keep_caller_id">>, CallFwd) of
                           false -> J;
                           true ->
                               lager:debug("call forwarding configured to keep the caller id"),
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
                                wh_json:set_value(<<"Account-ID">>, whapps_call:account_id(Call), J);
                            PvtAccountId ->
                                wh_json:set_value(<<"Account-ID">>, PvtAccountId, J)
                        end
                end
               ,fun(J) ->
                        case CallFwd of
                            undefined -> J;
                            _ ->
                                wh_json:set_values([{<<"Call-Forward">>, <<"true">>}
                                                    ,{<<"Authorizing-Type">>, <<"device">>}
                                                   ], J)
                        end
                end
               ,fun(J) ->
                        case wh_json:is_true(<<"require_keypress">>, CallFwd) of
                            false -> J;
                            _ ->
                                lager:debug("call forwarding configured to require key press"),
                                Confirm = [{<<"Confirm-Key">>, <<"1">>}
                                           ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                                           ,{<<"Confirm-File">>, ?CONFIRM_FILE}],
                                wh_json:merge_jobjs(wh_json:from_list(Confirm), J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value([<<"media">>, <<"fax_option">>], Endpoint) of
                            <<"auto">> -> wh_json:set_value(<<"Fax-Enabled">>, <<"true">>, J);
                            _Else -> J
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"pvt_type">>, Endpoint) of
                            <<"device">> ->
                                lager:debug("setting inherit_codec"),
                                wh_json:set_value(<<"Inherit-Codec">>, <<"true">>, J);
                            false -> J
                        end
                end
              ],
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), CCVFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Conditionally formats the caller id number
%% @end
%%--------------------------------------------------------------------
-spec maybe_format_caller_id_number/3 :: (wh_json:json_object(), ne_binary(), whapps_call:call()) -> ne_binary().
maybe_format_caller_id_number(Endpoint, CIDNum, Call) ->
    case cf_attributes:caller_id_attributes(Endpoint, <<"format">>, Call) of
        undefined -> CIDNum;
        FormatObj ->
            case wh_json:is_json_object(FormatObj) of
                false -> CIDNum;
                true -> wh_json:foldl(fun(Key, Value, CIDNum1) ->
                                              format_caller_id_number_flag(Key, Value, CIDNum1)
                                      end, CIDNum, FormatObj)
            end
    end.

-spec format_caller_id_number_flag/3 :: (ne_binary(), term(), ne_binary()) -> ne_binary().
format_caller_id_number_flag(<<"remove_plus">>, true, <<$+, CIDNum/binary>>) -> CIDNum;
format_caller_id_number_flag(_Key, _Value, CIDNum) -> CIDNum.
