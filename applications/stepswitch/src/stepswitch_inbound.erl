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
            lager:debug("relaying route request"),
            Routines = [fun set_account_id/2
                        ,fun set_inception/2
                        ,fun maybe_find_resource/2
                        ,fun maybe_format_destination/2
                        ,fun maybe_set_ringback/2
                        ,fun maybe_set_transfer_media/2
                        ,fun maybe_lookup_cnam/2
                        ,fun relay_request/2
                        ,fun maybe_transition_port_in/2
                       ],
            _ = lists:foldl(fun(F, J) ->  F(NumberProps, J) end
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
    AccountId = props:get_value('account_id', NumberProps),
    wh_json:set_value(?CCV(<<"Account-ID">>), AccountId, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_inception(wh_proplist(), wh_json:object()) ->
                           wh_json:object().
set_inception(_, JObj) ->
    wh_json:set_value(?CCV(<<"Inception">>), <<"off-net">>, JObj).

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
            maybe_add_resource_id(JObj, ResourceProps)
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

-spec maybe_format_destination(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_format_destination(_NumberProps, JObj) ->
    case wh_json:get_value(?CCV(<<"Resource-ID">>), JObj) of
        'undefined' -> JObj;
        ResourceId ->
            case stepswitch_resources:get_props(ResourceId) of
                'undefined' -> JObj;
                Resource ->
                    [RequestUser, RequestRealm] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
                    maybe_apply_formatters(JObj
                                           ,RequestUser
                                           ,RequestRealm
                                           ,props:get_value(<<"Destination-Formatters">>, Resource, [])
                                          )
            end
    end.

-spec maybe_apply_formatters(wh_json:object(), ne_binary(), ne_binary(), wh_json:objects()) -> wh_json:object().
maybe_apply_formatters(JObj, _RequestUser, _RequestRealm, []) -> JObj;
maybe_apply_formatters(JObj, RequestUser, RequestRealm, [Formatter|Formatters]) ->
    case wh_json:get_value(<<"regex">>, Formatter) of
        'undefined' -> maybe_apply_formatters(JObj, RequestUser, RequestRealm, Formatters);
        Regex ->
            case re:run(RequestUser, Regex, [{'capture', 'all_but_first', 'binary'}]) of
                'nomatch' -> maybe_apply_formatters(JObj, RequestUser, RequestRealm, Formatters);
                {'match', [Captured |_]} ->
                    lager:debug("formatter ~s captured ~s", [Regex, Captured]),
                    apply_formatter(JObj, Captured, RequestRealm, Formatter);
                {'match', []} ->
                    lager:debug("formatter ~s failed to capture, leaving alone", [Regex]),
                    maybe_apply_formatters(JObj, RequestUser, RequestRealm, Formatters)
            end
    end.

-spec apply_formatter(wh_json:object(), ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
apply_formatter(JObj, Captured, RequestRealm, Formatter) ->
    NewRequest = list_to_binary([wh_json:get_value(<<"prefix">>, Formatter, <<>>)
                                 ,Captured
                                 ,wh_json:get_value(<<"suffix">>, Formatter, <<>>)
                                ]),
    lager:debug("updating request user to '~s'@~s", [NewRequest, RequestRealm]),
    wh_json:set_value(<<"Request">>, list_to_binary([NewRequest, "@", RequestRealm]), JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_ringback(wh_proplist(), wh_json:object()) ->
                                wh_json:object().
maybe_set_ringback(NumberProps, JObj) ->
    case props:get_value('ringback_media', NumberProps) of
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
    case props:get_value('transfer_media', NumberProps) of
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
    case props:get_value('inbound_cnam', NumberProps) of
        'false' -> JObj;
        'true' -> stepswitch_cnam:lookup(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_request(wh_proplist(), wh_json:object()) ->
                           wh_json:object().
relay_request(_NumberProps, JObj) ->
    wapi_route:publish_req(JObj),
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_transition_port_in(wh_proplist(), wh_json:object()) ->
                                      wh_json:object().
maybe_transition_port_in(NumberProps, JObj) ->
    _ = case props:get_value('pending_port', NumberProps) of
            'false' -> 'ok';
            'true' ->
                case wh_port_request:get(props:get_value('number', NumberProps)) of
                    {'ok', PortReq} ->
                        _ = wh_port_request:transition_to_complete(PortReq);
                    _ -> 'ok'
                end
        end,
    JObj.
