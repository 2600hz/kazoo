%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_dash_e911).

-export([save/3]).
-export([delete/3]).
-export([is_valid_location/1]).

-include("../../include/wh_number_manager.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(WNM_DASH_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".dash_e911">>).

-define(SERVER, ?MODULE).

-define(DASH_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(DASH_AUTH_USERNAME, whapps_config:get_string(?WNM_DASH_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(DASH_AUTH_PASSWORD, whapps_config:get_string(?WNM_DASH_CONFIG_CAT, <<"auth_password">>, <<>>)).
-define(DASH_EMERG_URL, whapps_config:get_string(?WNM_DASH_CONFIG_CAT
                                                 ,<<"emergency_provisioning_url">>
                                                 ,<<"https://service.dashcs.com/dash-api/xml/emergencyprovisioning/v1">>)).
-define(DASH_DEBUG, whapps_config:get_is_true(?WNM_DASH_CONFIG_CAT, <<"debug">>, false)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will 
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, binary()}.
save(JObj, Number, <<"in_service">>) ->
    ?LOG("provisioning dash e911 address for '~s'", [Number]),
    Address = wh_json:get_value(<<"dash_e911">>, JObj),
    Location = json_address_to_xml_location(Address),
    case not is_atom(Address) andalso add_location(Number, Location) of
        false -> 
            ?LOG("dash_e911 address not provided"),
            {error, <<"address for dash_e911 is required">>};
        {error, R}=E -> 
            ?LOG("error provisioning dash e911 address: ~p", [R]),
            E;
        {provisioned, E911} -> 
            ?LOG("provisioned dash e911 address"),
            {ok, wh_json:set_value(<<"dash_e911">>, E911, JObj)};
        {geocoded, E911} ->
            ?LOG("added location to dash e911, attempting to provision new location"),
            case provision_location(wh_json:get_value(<<"location_id">>, E911)) of
                undefined -> 
                    ?LOG("provisioning attempt moved location to status: undefined"),
                    {ok, wh_json:set_value(<<"dash_e911">>, E911, JObj)};
                Status -> 
                    ?LOG("provisioning attempt moved location to status: ~s", [Status]),
                    {ok, wh_json:set_value(<<"dash_e911">>, wh_json:set_value(<<"status">>, Status, E911), JObj)}
            end
    end;
save(JObj, _, <<"reserved">>) ->
    {ok, JObj};
save(JObj, _, <<"discovery">>) ->
    {ok, JObj};
save(JObj, Number, _) ->
    ?LOG("removing dash e911 number '~s'", [Number]),
    case remove_number(Number) of
        <<"REMOVED">> -> 
            ?LOG("removed number from dash e911"),
            {ok, JObj};
        Error -> 
            ?LOG("failed to remove number from dash e911: ~p", [Error]),
            {ok, JObj}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will 
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, binary()}.
delete(JObj, Number, _) ->
    ?LOG("removing dash e911 number '~s'", [Number]),
    case remove_number(Number) of
        <<"REMOVED">> -> 
            ?LOG("removed number from dash e911"),
            {ok, JObj};
        Error -> 
            ?LOG("failed to remove number from dash e911: ~p", [Error]),
            {ok, JObj}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to dash e911 emergency provisiong API to preform
%% the given verb (validatelocation, addlocation, ect).
%% @end
%%--------------------------------------------------------------------
-spec emergency_provisioning_request/2 :: (atom(), proplist()) -> {ok, term()} | {error, term()}.
emergency_provisioning_request(Verb, Props) ->
    URL = list_to_binary([?DASH_EMERG_URL, "/", wh_util:to_lower_binary(Verb)]),
    Body = xmerl:export_simple([{Verb, Props}]
                               ,xmerl_xml
                               ,[{prolog, ?DASH_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"Content-Type", "text/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{inactivity_timeout, 180000}
                   ,{connect_timeout, 180000}
                   ,{basic_auth, {?DASH_AUTH_USERNAME, ?DASH_AUTH_PASSWORD}}
                  ],
    ?LOG("making ~s request to dash e911 ~s", [Verb, URL]),
    ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                        ,io_lib:format("Request:~n~s ~s~n~s~n", [post, URL, Body])),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, post, unicode:characters_to_binary(Body), HTTPOptions, 180000) of
        {ok, "401", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                                ,[append]),
            ?LOG("dash e911 request error: 401 (unauthenticated)"),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                                ,[append]),
            ?LOG("dash e911 request error: 403 (unauthorized)"),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                                ,[append]),
            ?LOG("dash e911 request error: 404 (not found)"),
            {error, not_found};
        {ok, "500", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                                ,[append]),
            ?LOG("dash e911 request error: 500 (server error)"),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                                ,[append]),
            ?LOG("dash e911 request error: 503"),
            {error, server_error};
        {ok, Code, _, Response} ->
            ?DASH_DEBUG andalso file:write_file("/tmp/dash_e911.xml"
                                                ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                                ,[append]),
            ?LOG("received response from dash e911"),
            try
                {Xml, _} = xmerl_scan:string(Response),
                Xml
            catch
                _:R ->
                    ?LOG("failed to decode xml: ~p", [R]),
                    {error, empty_response}
            end;
        {error, _}=E ->
            ?LOG("dash e911 request error: ~p", [E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_location/1 :: (term()) -> {geocoded, wh_json:json_object()} | 
                                                 {provisioned, wh_json:json_object()} |
                                                 {error, binary()}.
is_valid_location(Location) ->
    Response = emergency_provisioning_request('validateLocation', Location),
    case wh_util:get_xml_value("//Location/status/code/text()", Response) of
        <<"GEOCODED">> ->
            {geocoded, location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
        <<"PROVISIONED">> ->
            {provisioned, location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
        <<"INVALID">> ->
            {error, wh_util:get_xml_value("//Location/status/description/text()", Response)};
        <<"ERROR">> ->
            {error, wh_util:get_xml_value("//Location/status/description/text()", Response)};
        Else ->
            {error, wh_util:to_binary(Else)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_location/2 :: (ne_binary(), term()) -> {geocoded, wh_json:json_object()} | 
                                                 {provisioned, wh_json:json_object()} |
                                                 {error, binary()}.
add_location(Number, Location) ->
    Props = [{'uri', [{'uri', [wh_util:to_list(<<"tel:", (wnm_util:to_1npan(Number))/binary>>)]}
                      ,{'callername', ["Valued Customer"]}]}
             |Location
            ],
    Response = emergency_provisioning_request('addLocation', Props),
    case wh_util:get_xml_value("//Location/status/code/text()", Response) of
        <<"GEOCODED">> ->
            {geocoded, location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
        <<"PROVISIONED">> ->
            {provisioned, location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
        <<"INVALID">> ->
            {error, wh_util:get_xml_value("//Location/status/description/text()", Response)};
        <<"ERROR">> ->
            {error, wh_util:get_xml_value("//Location/status/description/text()", Response)};
        Else ->
            {error, wh_util:to_binary(Else)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec provision_location/1 :: (ne_binary()) -> undefined | ne_binary().
provision_location(LocationId) ->
    Props = [{'locationid', [wh_util:to_list(LocationId)]}],
    Response = emergency_provisioning_request('provisionLocation', Props),
    wh_util:get_xml_value("//LocationStatus/code/text()", Response).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number/1 :: (ne_binary()) -> undefined | ne_binary().
remove_number(Number) ->
    Props = [{'uri', [wh_util:to_list(<<"tel:", (wnm_util:to_1npan(Number))/binary>>)]}],
    Response = emergency_provisioning_request('removeURI', Props),
    wh_util:get_xml_value("//URIStatus/code/text()", Response).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_address_to_xml_location/1 :: (wh_json:json_object()) -> proplist().
json_address_to_xml_location(JObj) ->
    Props = [{'address1', [wh_json:get_string_value(<<"street_address">>, JObj)]}
             ,{'address2', [wh_json:get_string_value(<<"extended_address">>, JObj)]}
             ,{'community', [wh_json:get_string_value(<<"locality">>, JObj)]}
             ,{'state', [wh_json:get_string_value(<<"region">>, JObj)]}
             ,{'postalcode', [wh_json:get_string_value(<<"postal_code">>, JObj)]}
             ,{'type', ["ADDRESS"]}
            ],
    [{'location', [{K, V} || {K, V} <- Props, V =/= [undefined]]}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec location_xml_to_json_address/1 :: (term()) -> wh_json:json_object().
location_xml_to_json_address([]) ->
    wh_json:new();
location_xml_to_json_address([Xml]) ->
    location_xml_to_json_address(Xml);
location_xml_to_json_address(Xml) when is_list(Xml) ->
    [location_xml_to_json_address(X) || X <- Xml];
location_xml_to_json_address(Xml) ->
    Props = [{<<"street_address">>, wh_util:get_xml_value("address1/text()", Xml)}
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
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec legacy_data_xml_to_json/1 :: (term()) -> wh_json:json_object().
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
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).
