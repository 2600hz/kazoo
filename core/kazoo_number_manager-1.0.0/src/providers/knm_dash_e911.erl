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

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(KNM_DASH_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".dash_e911">>).
-define(DASH_KEY, <<"dash_e911">>).
-define(DASH_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(DASH_AUTH_USERNAME, whapps_config:get_string(?KNM_DASH_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(DASH_AUTH_PASSWORD, whapps_config:get_string(?KNM_DASH_CONFIG_CAT, <<"auth_password">>, <<>>)).
-define(DASH_EMERG_URL
        ,whapps_config:get_string(?KNM_DASH_CONFIG_CAT
                                  ,<<"emergency_provisioning_url">>
                                  ,<<"https://service.dashcs.com/dash-api/xml/emergencyprovisioning/v1">>
                                 )
       ).

-define(DASH_DEBUG, whapps_config:get_is_true(?KNM_DASH_CONFIG_CAT, <<"debug">>, 'false')).
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
-spec save(number()) -> number_return().
-spec save(number(), ne_binary()) -> number_return().
save(Number) ->
    State = knm_phone_number:state(Number),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_dash_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_dash_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
     maybe_update_dash_e911(Number);
save(Number, _State) -> delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(number()) -> number_return().
delete(Number) ->
    Features = knm_phone_number:features(Number),
    case wh_json:get_ne_value(?DASH_KEY, Features) of
        'undefined' -> {'ok', Number};
        _Else ->
            Num = knm_phone_number:number(Number),
            lager:debug("removing e911 information from ~s", [Num]),
            _ = remove_number(Num),
            Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(?DASH_KEY, Features)),
            {'ok', Number1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_dash_e911(number()) ->
                        number_return()
                        | {'invalid', _}
                        | {'multiple_choice', wh_json:object()}.
maybe_update_dash_e911(Number) ->
    Num = knm_phone_number:number(Number),

    Features = knm_phone_number:features(Number),
    CurrentE911 = wh_json:get_ne_value(?DASH_KEY, Features),

    Doc = knm_phone_number:doc(Number),
    E911 = wh_json:get_ne_value([<<"features">>, ?DASH_KEY], Doc),

    NotChanged = wnm_util:are_jobjs_identical(CurrentE911, E911),
    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("dash e911 information has been removed, updating dash"),
            _ = remove_number(Num),
            Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(?DASH_KEY, Features)),
            {'ok', Number1};
        'false' when NotChanged  ->
            Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(?DASH_KEY, Features)),
            {'ok', Number1};
        'false' ->
            lager:debug("e911 information has been changed: ~s", [wh_json:encode(E911)]),
            Number1 = knm_services:activate_feature(?DASH_KEY, Number),
            case maybe_update_dash_e911(Num, E911, Features) of
                {'ok', UpdatedFeatures} -> knm_phone_number:set_features(Number1, UpdatedFeatures);
                Else -> Else
            end
    end.

-spec maybe_update_dash_e911(ne_binary(), wh_json:object(), wh_json:object()) ->
                        {'ok', wh_json:object()}
                        | {'error', _}
                        | {'invalid', _}
                        | {'multiple_choice', wh_json:object()}.
maybe_update_dash_e911(Num, Address, JObj) when is_binary(Num) ->
    Location = json_address_to_xml_location(Address),
    case is_valid_location(Location) of
        {'error', _R}=Error->
            lager:error("error while checking location ~p", [_R]),
            Error;
        {'invalid', Reason}->
            lager:error("error while checking location ~p", [Reason]),
            Error =
                wh_json:from_list(
                  [{<<"cause">>, Address}
                   ,{<<"message">>, Reason}
                  ]),
            {'invalid', Error};
        {'provisioned', _} ->
            lager:debug("location seems already provisioned"),
            update_e911(Num, Address, JObj);
        {'geocoded', [_Loc]} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Num, Address, JObj);
        {'geocoded', [_|_]=Addresses} ->
            lager:warning("location could correspond to multiple addresses"),
            Update =
                wh_json:from_list(
                  [{<<"cause">>, Address}
                   ,{<<"details">>, Addresses}
                   ,{<<"message">>, <<"more than one address found">>}
                  ]),
            {'multiple_choice', Update};
        {'geocoded', _Loc} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Num, Address, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_e911(ne_binary(), wh_json:object(), wh_json:object()) ->
                         {'ok', wh_json:object()} |
                         {'error', _}.
update_e911(Num, Address, JObj) ->
    Location = json_address_to_xml_location(Address),
    CallerName = wh_json:get_ne_value(<<"caller_name">>, Address, <<"Valued Customer">>),
    case add_location(Num, Location, CallerName) of
        {'error', _R}=E ->
            lager:debug("error provisioning dash e911 address: ~p", [_R]),
            E;
        {'provisioned', E911} ->
            lager:debug("provisioned dash e911 address"),
            {'ok', wh_json:set_value(?DASH_KEY, E911, JObj)};
        {'geocoded', E911} ->
            provision_geocoded(JObj, E911)
    end.

-spec provision_geocoded(wh_json:object(), wh_json:object()) -> {'ok', wh_json:object()}.
provision_geocoded(JObj, E911) ->
    lager:debug("added location to dash e911, attempting to provision new location"),
    case provision_location(wh_json:get_value(<<"location_id">>, E911)) of
        'undefined' ->
            lager:debug("provisioning attempt moved location to status: undefined"),
            {'ok', wh_json:set_value(?DASH_KEY, E911, JObj)};
        Status ->
            lager:debug("provisioning attempt moved location to status: ~s", [Status]),
            {'ok', wh_json:set_value(?DASH_KEY, wh_json:set_value(<<"status">>, Status, E911), JObj)}
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
-spec emergency_provisioning_request(atom(), wh_proplist()) ->
                                            {'ok', xml_el()} |
                                            {'error', emergency_provisioning_error()}.
emergency_provisioning_request(Verb, Props) ->
    URL = list_to_binary([?DASH_EMERG_URL, "/", wh_util:to_lower_binary(Verb)]),
    Body = xmerl:export_simple([{Verb, Props}]
                               ,'xmerl_xml'
                               ,[{'prolog', ?DASH_XML_PROLOG}]
                              ),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?KNM_USER_AGENT}
               ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl',[{'verify',0}]}
                   ,{'inactivity_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'basic_auth', {?DASH_AUTH_USERNAME, ?DASH_AUTH_PASSWORD}}
                  ],
    lager:debug("making ~s request to dash e911 ~s", [Verb, URL]),
    ?DASH_DEBUG("Request:~n~s ~s~n~s~n", ['post', URL, Body]),
    case ibrowse:send_req(wh_util:to_list(URL)
                          ,Headers
                          ,'post'
                          ,unicode:characters_to_binary(Body)
                          ,HTTPOptions
                          ,180 * ?MILLISECONDS_IN_SECOND
                         )
    of
        {'ok', "401", _, _Response} ->
            ?DASH_DEBUG("Response:~n401~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', "403", _, _Response} ->
            ?DASH_DEBUG("Response:~n403~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', "404", _, _Response} ->
            ?DASH_DEBUG("Response:~n404~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', "500", _, _Response} ->
            ?DASH_DEBUG("Response:~n500~n~s~n", [_Response]),
            lager:debug("dash e911 request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', "503", _, _Response} ->
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
        {'error', _}=E ->
            lager:debug("dash e911 request error: ~p", [E]),
            {'error', 'unreachable'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_location(term()) ->
                               {'geocoded', wh_json:object() | wh_json:objects()} |
                               {'provisioned', wh_json:object() | wh_json:objects()} |
                               {'invalid', binary()} |
                               {'error', binary()}.
is_valid_location(Location) ->
    case emergency_provisioning_request('validateLocation', Location) of
        {'error', Reason} -> {'error', wh_util:to_binary(Reason)};
        {'ok', Response} ->
            case wh_util:get_xml_value("//Location/status/code/text()", Response) of
                <<"GEOCODED">> ->
                    {'geocoded', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"PROVISIONED">> ->
                    {'provisioned', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"INVALID">> ->
                    {'invalid', wh_util:get_xml_value("//Location/status/description/text()", Response)};
                <<"ERROR">> ->
                    {'error', wh_util:get_xml_value("//Location/status/description/text()", Response)};
                Else ->
                    {'error', wh_util:to_binary(Else)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_location(ne_binary(), term(), ne_binary()) ->
                          {'geocoded', wh_json:object()} |
                          {'provisioned', wh_json:object()} |
                          {'error', binary()}.
add_location(Number, Location, CallerName) ->
    Props = [{'uri', [{'uri', [wh_util:to_list(<<"tel:", (wnm_util:to_1npan(Number))/binary>>)]}
                      ,{'callername', [wh_util:to_list(CallerName)]}
                     ]
             }
             | Location
            ],
    case emergency_provisioning_request('addLocation', Props) of
        {'error', Reason} -> {'error', wh_util:to_binary(Reason)};
        {'ok', Response} ->
            case wh_util:get_xml_value("//Location/status/code/text()", Response) of
                <<"GEOCODED">> ->
                    {'geocoded', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"PROVISIONED">> ->
                    {'provisioned', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
                <<"INVALID">> ->
                    {'error', wh_util:get_xml_value("//Location/status/description/text()", Response)};
                <<"ERROR">> ->
                    {'error', wh_util:get_xml_value("//Location/status/description/text()", Response)};
                Else ->
                    {'error', wh_util:to_binary(Else)}
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
    Props = [{'locationid', [wh_util:to_list(LocationId)]}],
    case emergency_provisioning_request('provisionLocation', Props) of
        {'error', _} -> 'undefined';
        {'ok', Response} ->
            wh_util:get_xml_value("//LocationStatus/code/text()", Response)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number(ne_binary()) -> api_binary().
remove_number(Number) ->
    lager:debug("removing dash e911 number '~s'", [Number]),
    Props = [{'uri', [wh_util:to_list(<<"tel:", (wnm_util:to_1npan(Number))/binary>>)]}],
    case emergency_provisioning_request('removeURI', Props) of
        {'error', 'server_error'} ->
            lager:debug("removed number from dash e911"),
            <<"REMOVED">>;
        {'error', _E} ->
            lager:debug("removed number from dash e911: ~p", [_E]),
            <<"REMOVED">>;
        {'ok', Response} ->
            case wh_util:get_xml_value("//URIStatus/code/text()", Response) of
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
%%
%% @end
%%--------------------------------------------------------------------
-spec json_address_to_xml_location(wh_json:object()) -> wh_proplist().
json_address_to_xml_location(JObj) ->
    Props = [{'address1', [wh_json:get_string_value(<<"street_address">>, JObj)]}
             ,{'address2', [wh_json:get_string_value(<<"extended_address">>, JObj)]}
             ,{'community', [wh_json:get_string_value(<<"locality">>, JObj)]}
             ,{'state', [wh_json:get_string_value(<<"region">>, JObj)]}
             ,{'postalcode', [wh_json:get_string_value(<<"postal_code">>, JObj)]}
             ,{'type', ["ADDRESS"]}
            ],
    [{'location', [KV || {_, V}=KV <- Props, V =/= ['undefined']]}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec location_xml_to_json_address(term()) -> wh_json:object() | wh_json:objects().
location_xml_to_json_address([]) ->
    wh_json:new();
location_xml_to_json_address([Xml]) ->
    location_xml_to_json_address(Xml);
location_xml_to_json_address(Xml) when is_list(Xml) ->
    [location_xml_to_json_address(X) || X <- Xml];
location_xml_to_json_address(Xml) ->
    Props =
        [{<<"street_address">>, wh_util:get_xml_value("address1/text()", Xml)}
         ,{<<"extended_address">>, wh_util:get_xml_value("address2/text()", Xml)}
         ,{<<"activated_time">>, wh_util:get_xml_value("activated_time/text()", Xml)}
         ,{<<"caller_name">>, wh_util:get_xml_value("callername/text()", Xml)}
         ,{<<"comments">>, wh_util:get_xml_value("comments/text()", Xml)}
         ,{<<"locality">>, wh_util:get_xml_value("community/text()", Xml)}
         ,{<<"order_id">>, wh_util:get_xml_value("customerorderid/text()", Xml)}
         ,{<<"latitude">>, wh_util:get_xml_value("latitude/text()", Xml)}
         ,{<<"longitude">>, wh_util:get_xml_value("longitude/text()", Xml)}
         ,{<<"location_id">>, wh_util:get_xml_value("locationid/text()", Xml)}
         ,{<<"plus_four">>, wh_util:get_xml_value("plusfour/text()", Xml)}
         ,{<<"postal_code">>, wh_util:get_xml_value("postalcode/text()", Xml)}
         ,{<<"region">>, wh_util:get_xml_value("state/text()", Xml)}
         ,{<<"status">>, wh_util:get_xml_value("status/code/text()", Xml)}
         ,{<<"legacy_data">>, legacy_data_xml_to_json(xmerl_xpath:string("legacydata", Xml))}
        ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec legacy_data_xml_to_json(term()) -> wh_json:object().
legacy_data_xml_to_json([]) ->
    wh_json:new();
legacy_data_xml_to_json([Xml]) ->
    legacy_data_xml_to_json(Xml);
legacy_data_xml_to_json(Xml) when is_list(Xml) ->
    [legacy_data_xml_to_json(X) || X <- Xml];
legacy_data_xml_to_json(Xml) ->
    Props = [{<<"house_number">>, wh_util:get_xml_value("housenumber/text()", Xml)}
             ,{<<"predirectional">>, wh_util:get_xml_value("predirectional/text()", Xml)}
             ,{<<"streetname">>, wh_util:get_xml_value("streetname/text()", Xml)}
             ,{<<"suite">>, wh_util:get_xml_value("suite/text()", Xml)}
            ],
    wh_json:from_list(props:filter_undefined(Props)).
