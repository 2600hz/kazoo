%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_dash_e911).

-export([save/1]).
-export([delete/1]).
-export([is_valid_location/1]).

-include("../wnm.hrl").

-define(WNM_DASH_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".dash_e911">>).

-define(DASH_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(DASH_AUTH_USERNAME, whapps_config:get_string(?WNM_DASH_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(DASH_AUTH_PASSWORD, whapps_config:get_string(?WNM_DASH_CONFIG_CAT, <<"auth_password">>, <<>>)).
-define(DASH_EMERG_URL, whapps_config:get_string(?WNM_DASH_CONFIG_CAT
                                                 ,<<"emergency_provisioning_url">>
                                                 ,<<"https://service.dashcs.com/dash-api/xml/emergencyprovisioning/v1">>)).
-define(DASH_DEBUG, whapps_config:get_is_true(?WNM_DASH_CONFIG_CAT, <<"debug">>, 'false')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{state = <<"port_in">>} = Number) ->
    maybe_update_dash_e911(Number);
save(#number{state = <<"reserved">>} = Number) ->
    maybe_update_dash_e911(Number);
save(#number{state = <<"in_service">>} = Number) ->
    maybe_update_dash_e911(Number);
save(Number) -> delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(#number{features=Features
               ,number=Num
               ,current_number_doc=CurrentDoc
               ,number_doc=Doc
              }=Number) ->
    case wh_json:get_ne_value(<<"dash_e911">>, CurrentDoc) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            _ = remove_number(Num),
            Number#number{features=sets:del_element(<<"dash_e911">>, Features)
                          ,number_doc=wh_json:delete_key(<<"dash_e911">>, Doc)
                         }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_dash_e911(wnm_number()) -> wnm_number().
maybe_update_dash_e911(#number{current_number_doc=CurrentJObj
                               ,number_doc=JObj
                               ,features=Features
                               ,number=Number
                              }=N) ->
    CurrentE911 = wh_json:get_ne_value(<<"dash_e911">>, CurrentJObj),
    E911 = wh_json:get_ne_value(<<"dash_e911">>, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentE911, E911),
    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("dash e911 information has been removed, updating dash"),
            _ = remove_number(Number),
            N#number{features=sets:del_element(<<"dash_e911">>, Features)};
        'false' when NotChanged  -> N#number{features=sets:add_element(<<"dash_e911">>, Features)};
        'false' ->
            lager:debug("e911 information has been changed: ~s", [wh_json:encode(E911)]),
            N1 = wnm_number:activate_feature(<<"dash_e911">>, N),
            case update_e911(Number, E911, JObj) of
                {'error', _}=E -> E;
                {'ok', J} -> N1#number{number_doc=J}
            end
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
update_e911(Number, Address, JObj) ->
    Location = json_address_to_xml_location(Address),
    CallerName = wh_json:get_ne_value(<<"caller_name">>, Address, <<"Valued Customer">>),
    case add_location(Number, Location, CallerName) of
        {'error', R}=E ->
            lager:debug("error provisioning dash e911 address: ~p", [R]),
            E;
        {'provisioned', E911} ->
            lager:debug("provisioned dash e911 address"),
            {'ok', wh_json:set_value(<<"dash_e911">>, E911, JObj)};
        {'geocoded', E911} ->
            lager:debug("added location to dash e911, attempting to provision new location"),
            case provision_location(wh_json:get_value(<<"location_id">>, E911)) of
                'undefined' ->
                    lager:debug("provisioning attempt moved location to status: undefined"),
                    {'ok', wh_json:set_value(<<"dash_e911">>, E911, JObj)};
                Status ->
                    lager:debug("provisioning attempt moved location to status: ~s", [Status]),
                    {'ok', wh_json:set_value(<<"dash_e911">>, wh_json:set_value(<<"status">>, Status, E911), JObj)}
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
-spec emergency_provisioning_request(atom(), wh_proplist()) ->
                                            {'ok', xml_el()} |
                                            {'error', emergency_provisioning_error()}.
emergency_provisioning_request(Verb, Props) ->
    URL = list_to_binary([?DASH_EMERG_URL, "/", wh_util:to_lower_binary(Verb)]),
    Body = xmerl:export_simple([{Verb, Props}]
                               ,'xmerl_xml'
                               ,[{'prolog', ?DASH_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl',[{'verify',0}]}
                   ,{'inactivity_timeout', 180000}
                   ,{'connect_timeout', 180000}
                   ,{'basic_auth', {?DASH_AUTH_USERNAME, ?DASH_AUTH_PASSWORD}}
                  ],
    lager:debug("making ~s request to dash e911 ~s", [Verb, URL]),
    ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                        ,io_lib:format("Request:~n~s ~s~n~s~n", ['post', URL, Body])),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post'
                          ,unicode:characters_to_binary(Body), HTTPOptions, 180000
                         )
    of
        {'ok', "401", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                                ,['append']),
            lager:debug("dash e911 request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', "403", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                                ,['append']),
            lager:debug("dash e911 request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', "404", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                                ,['append']),
            lager:debug("dash e911 request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', "500", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                                ,['append']),
            lager:debug("dash e911 request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', "503", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                                ,['append']),
            lager:debug("dash e911 request error: 503"),
            {'error', 'server_error'};
        {'ok', Code, _, Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                                ,['append']),
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
                               {'geocoded', wh_json:object()} |
                               {'provisioned', wh_json:object()} |
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
-spec add_location(ne_binary(), term(), ne_binary()) ->
                          {'geocoded', wh_json:object()} |
                          {'provisioned', wh_json:object()} |
                          {'error', binary()}.
add_location(Number, Location, CallerName) ->
    Props = [{'uri', [{'uri', [wh_util:to_list(<<"tel:", (wnm_util:to_1npan(Number))/binary>>)]}
                      ,{'callername', [wh_util:to_list(CallerName)]}]}
             |Location
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
        Response ->
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
-spec location_xml_to_json_address(term()) -> wh_json:object().
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
