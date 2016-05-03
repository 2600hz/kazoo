%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_dash_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

-include("knm.hrl").

-define(KNM_DASH_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".dash_e911">>).

-define(DASH_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(DASH_AUTH_USERNAME, kapps_config:get_binary(?KNM_DASH_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(DASH_AUTH_PASSWORD, kapps_config:get_binary(?KNM_DASH_CONFIG_CAT, <<"auth_password">>, <<>>)).
-define(DASH_EMERG_URL
        ,kapps_config:get_string(?KNM_DASH_CONFIG_CAT
                                  ,<<"emergency_provisioning_url">>
                                  ,<<"https://service.dashcs.com/dash-api/xml/emergencyprovisioning/v1">>
                                 )
       ).

-define(DASH_DEBUG, kapps_config:get_is_true(?KNM_DASH_CONFIG_CAT, <<"debug">>, 'false')).
-define(DASH_DEBUG(Fmt, Args), ?DASH_DEBUG
        andalso file:write_file("/tmp/dash_e911.xml", io_lib:format(Fmt, Args))
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
-spec save(knm_number:knm_number(), api_binary()) ->
                  knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_dash_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_dash_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
     maybe_update_dash_e911(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    knm_number:knm_number().
delete(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?DASH_KEY) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information from ~s"
                        ,[knm_phone_number:number(knm_number:phone_number(Number))]
                       ),
            _ = remove_number(Number),
            knm_services:deactivate_feature(Number, ?DASH_KEY)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:knm_number()) -> boolean().
has_emergency_services(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?DASH_KEY) of
        'undefined' -> 'false';
        _Else -> 'true'
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_dash_e911(knm_number:knm_number()) ->
                                    knm_number:knm_number().
maybe_update_dash_e911(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentE911 = kz_json:get_ne_value(?DASH_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    E911 = kz_json:get_ne_value([?PVT_FEATURES, ?DASH_KEY], Doc),

    NotChanged = kz_json:are_identical(CurrentE911, E911),
    case kz_util:is_empty(E911) of
        'true' ->
            lager:debug("dash e911 information has been removed, updating dash"),
            _ = remove_number(Number),
            knm_services:deactivate_feature(Number, ?DASH_KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?DASH_KEY);
        'false' ->
            lager:debug("e911 information has been changed: ~s", [kz_json:encode(E911)]),
            Number1 = knm_services:activate_feature(Number, ?DASH_KEY),
            UpdatedFeatures = maybe_update_dash_e911(Number1, E911, Features),
            knm_number:set_phone_number(
              Number1
              ,knm_phone_number:set_features(PhoneNumber, UpdatedFeatures)
             )
    end.

-spec maybe_update_dash_e911(knm_number:knm_number(), kz_json:object(), kz_json:object()) ->
                                    kz_json:object().
maybe_update_dash_e911(Number, Address, JObj) ->
    Location = json_address_to_xml_location(Address),
    case is_valid_location(Location) of
        {'error', E} ->
            lager:error("error while checking location ~p", [E]),
            knm_errors:unspecified(E, Number);
        {'invalid', Reason}->
            lager:error("error while checking location ~p", [Reason]),
            Error =
                kz_json:from_list(
                  [{<<"cause">>, Address}
                   ,{<<"message">>, Reason}
                  ]),
            knm_errors:invalid(Number, Error);
        {'provisioned', _} ->
            lager:debug("location seems already provisioned"),
            update_e911(Number, Address, JObj);
        {'geocoded', [_Loc]} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Number, Address, JObj);
        {'geocoded', [_|_]=Addresses} ->
            lager:warning("location could correspond to multiple addresses"),
            Update =
                kz_json:from_list(
                  [{<<"cause">>, Address}
                   ,{<<"details">>, Addresses}
                   ,{<<"message">>, <<"more than one address found">>}
                  ]),
            knm_errors:multiple_choice(Number, Update);
        {'geocoded', _Loc} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Number, Address, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_e911(knm_number:knm_number(), kz_json:object(), kz_json:object()) ->
                         kz_json:object().
-spec update_e911(knm_number:knm_number(), kz_json:object(), kz_json:object(), boolean()) ->
                         kz_json:object().
update_e911(Number, Address, JObj) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    update_e911(Number, Address, JObj, DryRun).

update_e911(_Number, Address, JObj, 'true') ->
    kz_json:set_value(?DASH_KEY, Address, JObj);
update_e911(Number, Address, JObj, 'false') ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    Location = json_address_to_xml_location(Address),
    CallerName = kz_json:get_ne_value(<<"caller_name">>, Address, <<"Valued Customer">>),
    case add_location(Num, Location, CallerName) of
        {'error', E} ->
            lager:debug("error provisioning dash e911 address: ~p", [E]),
            knm_errors:unspecified(E, Number);
        {'provisioned', E911} ->
            lager:debug("provisioned dash e911 address"),
            kz_json:set_value(?DASH_KEY, E911, JObj);
        {'geocoded', E911} ->
            provision_geocoded(JObj, E911)
    end.

-spec provision_geocoded(kz_json:object(), kz_json:object()) ->
                                kz_json:object().
provision_geocoded(JObj, E911) ->
    lager:debug("added location to dash e911, attempting to provision new location"),
    case provision_location(kz_json:get_value(<<"location_id">>, E911)) of
        'undefined' ->
            lager:debug("provisioning attempt moved location to status: undefined"),
            kz_json:set_value(?DASH_KEY, E911, JObj);
        Status ->
            lager:debug("provisioning attempt moved location to status: ~s", [Status]),
            kz_json:set_value(?DASH_KEY
                              ,kz_json:set_value(<<"status">>, Status, E911)
                              ,JObj
                             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_location([xml_location()]) ->
                               {'geocoded', kz_json:object() | kz_json:objects()} |
                               {'provisioned', kz_json:object() | kz_json:objects()} |
                               {'invalid', binary()} |
                               {'error', binary()}.
is_valid_location(Location) ->
    case emergency_provisioning_request('validateLocation', Location) of
        {'error', Reason} ->
            {'error', kz_util:to_binary(Reason)};
        {'ok', Response} ->
            case kz_util:get_xml_value("//Location/status/code/text()", Response) of
                <<"GEOCODED">> ->
                    {'geocoded', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"PROVISIONED">> ->
                    {'provisioned', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"INVALID">> ->
                    {'invalid', kz_util:get_xml_value("//Location/status/description/text()", Response)};
                <<"ERROR">> ->
                    {'error', kz_util:get_xml_value("//Location/status/description/text()", Response)};
                Else ->
                    {'error', kz_util:to_binary(Else)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_location(ne_binary(), [xml_location()], ne_binary()) ->
                          {'geocoded', kz_json:object()} |
                          {'provisioned', kz_json:object()} |
                          {'error', binary()}.
add_location(Number, Location, CallerName) ->
    Props = [{'uri', [{'uri', [kz_util:to_list(<<"tel:", (knm_converters:to_1npan(Number))/binary>>)]}
                      ,{'callername', [kz_util:to_list(CallerName)]}
                     ]
             }
             | Location
            ],
    case emergency_provisioning_request('addLocation', Props) of
        {'error', Reason} -> {'error', kz_util:to_binary(Reason)};
        {'ok', Response} ->
            case kz_util:get_xml_value("//Location/status/code/text()", Response) of
                <<"GEOCODED">> ->
                    {'geocoded', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"PROVISIONED">> ->
                    {'provisioned', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"INVALID">> ->
                    {'error', kz_util:get_xml_value("//Location/status/description/text()", Response)};
                <<"ERROR">> ->
                    {'error', kz_util:get_xml_value("//Location/status/description/text()", Response)};
                Else ->
                    {'error', kz_util:to_binary(Else)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec provision_location(ne_binary()) -> api_binary().
provision_location(LocationId) ->
    Props = [{'locationid', [kz_util:to_list(LocationId)]}],
    case emergency_provisioning_request('provisionLocation', Props) of
        {'error', _} -> 'undefined';
        {'ok', Response} ->
            kz_util:get_xml_value("//LocationStatus/code/text()", Response)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number(knm_number:knm_number()) -> api_binary().
remove_number(Number) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    lager:debug("removing dash e911 number '~s'", [Num]),
    Props = [{'uri', [kz_util:to_list(<<"tel:", (knm_converters:to_1npan(Num))/binary>>)]}],
    case emergency_provisioning_request('removeURI', Props) of
        {'error', 'server_error'} ->
            lager:debug("removed number from dash e911"),
            <<"REMOVED">>;
        {'error', _E} ->
            lager:debug("removed number from dash e911: ~p", [_E]),
            <<"REMOVED">>;
        {'ok', Response} ->
            case kz_util:get_xml_value("//URIStatus/code/text()", Response) of
                <<"REMOVED">> = R ->
                    lager:debug("removed number from dash e911"),
                    R;
                Else ->
                    lager:debug("failed to remove number from dash e911: ~p", [Else]),
                    Else
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to dash e911 emergency provisiong API to preform
%% the given verb (validatelocation, addlocation, ect).
%% @end
%%--------------------------------------------------------------------
-type emergency_provisioning_error() :: 'authentication' |
                                        'authorization' |
                                        'not_found' |
                                        'server_error' |
                                        'empty_response' |
                                        'unreachable'.

-type request_prop() :: xml_location() |
                        {'uri', list()} |
                        {'locationid', list()}.
-type request_props() :: [request_prop()].

-spec emergency_provisioning_request(atom(), request_props()) ->
                                            {'ok', xml_el()} |
                                            {'error', emergency_provisioning_error()}.
emergency_provisioning_request(Verb, Props) ->
    URL = list_to_binary([?DASH_EMERG_URL, "/", kz_util:to_lower_binary(Verb)]),
    Body = unicode:characters_to_binary(
             xmerl:export_simple([{Verb, Props}]
                                 ,'xmerl_xml'
                                 ,[{'prolog', ?DASH_XML_PROLOG}]
                                )
            ),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?KNM_USER_AGENT}
               ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}]}
                   ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'basic_auth', {?DASH_AUTH_USERNAME, ?DASH_AUTH_PASSWORD}}
                  ],
    lager:debug("making ~s request to dash e911 ~s", [Verb, URL]),
    ?DASH_DEBUG("Request:~n~s ~s~n~s~n", ['post', URL, Body]),
    case kz_http:post(kz_util:to_list(URL), Headers, Body, HTTPOptions) of
        {'ok', 401, _, _Response} ->
            ?DASH_DEBUG("Response:~n401~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', 403, _, _Response} ->
            ?DASH_DEBUG("Response:~n403~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', 404, _, _Response} ->
            ?DASH_DEBUG("Response:~n404~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', 500, _, _Response} ->
            ?DASH_DEBUG("Response:~n500~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', 503, _, _Response} ->
            ?DASH_DEBUG("Response:~n503~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 503"),
            {'error', 'server_error'};
        {'ok', Code, _, Response} ->
            ?DASH_DEBUG("Response:~n~p~n~s~n", [Code, Response]),
            lager:debug("received response from dash e911"),
            try xmerl_scan:string(Response) of
                {Xml, _} -> {'ok', Xml}
            catch
                _:R ->
                    lager:debug("failed to decode xml: ~p", [R]),
                    {'error', 'empty_response'}
            end;
        {'error', _Reason} ->
            lager:debug("dash e911 request error: ~p", [_Reason]),
            {'error', 'unreachable'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type xml_location_property() :: {'address1', list()} |
                                 {'address2', list()} |
                                 {'community', list()} |
                                 {'state', list()} |
                                 {'postalcode', list()} |
                                 {'type', list()}.
-type xml_location() :: {'location', [xml_location_property()]}.

-spec json_address_to_xml_location(kz_json:object()) -> [xml_location()].
json_address_to_xml_location(JObj) ->
    Props = [{'address1', [kz_json:get_string_value(<<"street_address">>, JObj)]}
             ,{'address2', [kz_json:get_string_value(<<"extended_address">>, JObj)]}
             ,{'community', [kz_json:get_string_value(<<"locality">>, JObj)]}
             ,{'state', [kz_json:get_string_value(<<"region">>, JObj)]}
             ,{'postalcode', [kz_json:get_string_value(<<"postal_code">>, JObj)]}
             ,{'type', ["ADDRESS"]}
            ],
    [{'location', [KV || {_, V}=KV <- Props, V =/= ['undefined']]}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec location_xml_to_json_address(list()) -> kz_json:object() | kz_json:objects().
location_xml_to_json_address([]) ->
    kz_json:new();
location_xml_to_json_address([Xml]) ->
    location_xml_to_json_address(Xml);
location_xml_to_json_address(Xml) when is_list(Xml) ->
    [location_xml_to_json_address(X) || X <- Xml];
location_xml_to_json_address(Xml) ->
    Props =
        [{<<"street_address">>, kz_util:get_xml_value("address1/text()", Xml)}
         ,{<<"extended_address">>, kz_util:get_xml_value("address2/text()", Xml)}
         ,{<<"activated_time">>, kz_util:get_xml_value("activated_time/text()", Xml)}
         ,{<<"caller_name">>, kz_util:get_xml_value("callername/text()", Xml)}
         ,{<<"comments">>, kz_util:get_xml_value("comments/text()", Xml)}
         ,{<<"locality">>, kz_util:get_xml_value("community/text()", Xml)}
         ,{<<"order_id">>, kz_util:get_xml_value("customerorderid/text()", Xml)}
         ,{<<"latitude">>, kz_util:get_xml_value("latitude/text()", Xml)}
         ,{<<"longitude">>, kz_util:get_xml_value("longitude/text()", Xml)}
         ,{<<"location_id">>, kz_util:get_xml_value("locationid/text()", Xml)}
         ,{<<"plus_four">>, kz_util:get_xml_value("plusfour/text()", Xml)}
         ,{<<"postal_code">>, kz_util:get_xml_value("postalcode/text()", Xml)}
         ,{<<"region">>, kz_util:get_xml_value("state/text()", Xml)}
         ,{<<"status">>, kz_util:get_xml_value("status/code/text()", Xml)}
         ,{<<"legacy_data">>, legacy_data_xml_to_json(xmerl_xpath:string("legacydata", Xml))}
        ],
    kz_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec legacy_data_xml_to_json(term()) -> kz_json:object().
legacy_data_xml_to_json([]) ->
    kz_json:new();
legacy_data_xml_to_json([Xml]) ->
    legacy_data_xml_to_json(Xml);
legacy_data_xml_to_json(Xml) when is_list(Xml) ->
    [legacy_data_xml_to_json(X) || X <- Xml];
legacy_data_xml_to_json(Xml) ->
    Props = [{<<"house_number">>, kz_util:get_xml_value("housenumber/text()", Xml)}
             ,{<<"predirectional">>, kz_util:get_xml_value("predirectional/text()", Xml)}
             ,{<<"streetname">>, kz_util:get_xml_value("streetname/text()", Xml)}
             ,{<<"suite">>, kz_util:get_xml_value("suite/text()", Xml)}
            ],
    kz_json:from_list(props:filter_undefined(Props)).
