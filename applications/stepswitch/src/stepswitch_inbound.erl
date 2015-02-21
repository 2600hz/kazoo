%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% Handle route requests from carrier resources
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_inbound).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    _ = whapps_util:put_callid(JObj),
    case wh_json:get_ne_value(?CCV(<<"Account-ID">>), JObj) of
        'undefined' -> maybe_relay_request(JObj);
        _AcctID -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle a request inbound from offnet
%% @end
%%--------------------------------------------------------------------
-spec maybe_relay_request(wh_json:object()) -> 'ok'.
maybe_relay_request(JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    case stepswitch_util:lookup_number(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for ~s: ~p", [Number, _R]);
        {'ok', _, NumberProps} ->
            Routines = [fun set_account_id/2
                        ,fun set_ignore_display_updates/2
                        ,fun set_inception/2
                        ,fun maybe_find_resource/2
                        ,fun maybe_format_destination/2
                        ,fun maybe_set_ringback/2
                        ,fun maybe_set_transfer_media/2
                        ,fun maybe_lookup_cnam/2
                        ,fun maybe_add_prepend/2
                        ,fun maybe_blacklisted/2
                        ,fun maybe_transition_port_in/2
                       ],
            _ = lists:foldl(fun(F, J) -> F(NumberProps, J) end
                            ,JObj
                            ,Routines
                           ),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(wh_proplist(), wh_json:object()) ->
                            wh_json:object().
set_account_id(NumberProps, JObj) ->
    AccountId = wh_number_properties:account_id(NumberProps),
    wh_json:set_value(?CCV(<<"Account-ID">>), AccountId, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_ignore_display_updates(wh_proplist(), wh_json:object()) ->
                                        wh_json:object().
set_ignore_display_updates(_, JObj) ->
    wh_json:set_value(?CCV(<<"Ignore-Display-Updates">>), <<"true">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_inception(wh_proplist(), wh_json:object()) ->
                           wh_json:object().
set_inception(_, JObj) ->
    Request = wh_json:get_value(<<"Request">>, JObj),
    wh_json:set_value(?CCV(<<"Inception">>), Request, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_find_resource(wh_proplist(), wh_json:object()) ->
                                 wh_json:object().
maybe_find_resource(_NumberProps, JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> JObj;
        {'ok', ResourceProps} ->
            Routines = [fun maybe_add_resource_id/2
                        ,fun maybe_add_t38_settings/2
                       ],
            lists:foldl(fun(F, J) ->  F(J, ResourceProps) end
                        ,JObj
                        ,Routines
                       )
    end.

-spec maybe_add_resource_id(wh_json:object(), wh_proplist()) -> wh_json:object().
maybe_add_resource_id(JObj, ResourceProps) ->
    case props:get_is_true('global', ResourceProps) of
        'false' -> JObj;
        'true' ->
            wh_json:set_value(?CCV(<<"Resource-ID">>)
                              ,props:get_value('resource_id', ResourceProps)
                              ,JObj
                             )
    end.

-spec maybe_add_t38_settings(wh_json:object(), wh_proplist()) -> wh_json:object().
maybe_add_t38_settings(JObj, ResourceProps) ->
    case props:get_value('fax_option', ResourceProps) of
        'true' ->
            wh_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                              ,props:get_value('fax_option', ResourceProps)
                              ,JObj
                             );
        <<"auto">> ->
            wh_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                              ,props:get_value('fax_option', ResourceProps)
                              ,JObj
                             );
        _ -> JObj
    end.

-spec maybe_format_destination(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_format_destination(_NumberProps, JObj) ->
    case wh_json:get_value(?CCV(<<"Resource-ID">>), JObj) of
        'undefined' -> JObj;
        ResourceId ->
            case stepswitch_resources:get_props(ResourceId) of
                'undefined' -> JObj;
                Resource ->
                    stepswitch_formatters:apply(JObj, props:get_value(<<"Formatters">>, Resource, wh_json:new()), 'inbound')
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_ringback(wh_proplist(), wh_json:object()) ->
                                wh_json:object().
maybe_set_ringback(NumberProps, JObj) ->
    case wh_number_properties:ringback_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            wh_json:set_value(?CCV(<<"Ringback-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_transfer_media(wh_proplist(), wh_json:object()) ->
                                      wh_json:object().
maybe_set_transfer_media(NumberProps, JObj) ->
    case wh_number_properties:transfer_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            wh_json:set_value(?CCV(<<"Transfer-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec maybe_lookup_cnam(wh_proplist(), wh_json:object()) ->
                               wh_json:object().
maybe_lookup_cnam(NumberProps, JObj) ->
    case wh_number_properties:inbound_cnam_enabled(NumberProps) of
        'false' -> JObj;
        'true' -> stepswitch_cnam:lookup(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_prepend(wh_number_manager:number_properties(), wh_json:object()) ->
                               wh_json:object().
maybe_add_prepend(NumberProps, JObj) ->
    case wh_number_properties:prepend(NumberProps) of
        'undefined' -> JObj;
        Prepend -> wh_json:set_value(<<"Prepend-CID-Name">>, Prepend, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec maybe_blacklisted(wh_proplist(), wh_json:object()) ->
                           wh_json:object().
maybe_blacklisted(_NumberProps, JObj) ->
    case is_blacklisted(JObj) of
        'true' -> JObj;
        'false' ->
            _ = relay_request(JObj),
            JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_request(wh_json:object()) -> wh_json:object().
relay_request(JObj) ->
    wapi_route:publish_req(JObj),
    lager:debug("relaying route request").


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_transition_port_in(wh_proplist(), wh_json:object()) ->
                                      wh_json:object().
maybe_transition_port_in(NumberProps, JObj) ->
    _ = case wh_number_properties:has_pending_port(NumberProps) of
            'false' -> 'ok';
            'true' ->
                case wh_port_request:get(wh_number_properties:number(NumberProps)) of
                    {'ok', PortReq} ->
                        _ = wh_port_request:transition_to_complete(PortReq);
                    _ ->
                        Number = stepswitch_util:get_inbound_destination(JObj),
                        wh_number_manager:ported(Number)
                end
        end,
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_blacklisted(wh_json:object()) -> boolean().
is_blacklisted(JObj) ->
    AccountId = wh_json:get_ne_value(?CCV(<<"Account-ID">>), JObj),
    case get_blacklists(AccountId) of
        {'error', _R} ->
            lager:debug("not blacklisted ~p", [_R]),
            'false';
        {'ok', Blacklists} ->
            Blacklist = get_blacklist(AccountId, Blacklists),
            Number = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
            case wh_json:get_value(Number, Blacklist) of
                'undefined' ->
                    lager:debug("~p not blacklisted, did not match any rule", [Number]),
                    'false';
                _Rule ->
                    lager:info("~p is blacklisted", [Number]),
                    'true'
            end
    end.

-spec get_blacklists(ne_binary()) ->
                            {'ok', ne_binaries()} |
                            {'error', any()}.
get_blacklists(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _R}=E ->
            lager:error("could not open ~s in ~s: ~p", [AccountId, AccountDb, _R]),
            E;
        {'ok', Doc} ->
            case wh_json:get_value(<<"blacklists">>, Doc, []) of
                [] -> {'error', 'undefined'};
                [_|_]=Blacklists-> {'ok', Blacklists};
                _ -> {'error', 'miss_configured'}
            end
    end.

-spec get_blacklist(ne_binary(), ne_binaries()) -> wh_json:object().
get_blacklist(AccountId, Blacklists) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    lists:foldl(
        fun(BlacklistId, Acc) ->
            case couch_mgr:open_cache_doc(AccountDb, BlacklistId) of
                {'error', _R} ->
                    lager:error("could not open ~s in ~s: ~p", [BlacklistId, AccountDb, _R]),
                    Acc;
                {'ok', Doc} ->
                    Numbers = wh_json:get_value(<<"numbers">>, Doc, wh_json:new()),
                    wh_json:merge_jobjs(Acc, Numbers)
            end
        end
        ,wh_json:new()
        ,Blacklists
    ).
