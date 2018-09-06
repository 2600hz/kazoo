%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Handle e911 provisioning
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_dash_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".dash_e911">>).

-define(XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(AUTH_USERNAME, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(AUTH_PASSWORD, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"auth_password">>, <<>>)).
-define(EMERG_URL
       ,kapps_config:get_string(?MOD_CONFIG_CAT
                               ,<<"emergency_provisioning_url">>
                               ,<<"https://service.dashcs.com/dash-api/xml/emergencyprovisioning/v1">>
                               )
       ).

-define(DEBUG, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"debug">>, 'false')).
-define(DEBUG(Fmt, Args),
        ?DEBUG
        andalso file:write_file("/tmp/dash_e911.xml", io_lib:format(Fmt, Args))
       ).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

-spec save(knm_number:knm_number(), kz_term:api_binary()) ->
                  knm_number:knm_number().
save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(Number);
save(Number, _State) ->
    delete(Number).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information from ~s"
                       ,[knm_phone_number:number(knm_number:phone_number(Number))]),
            _ = remove_number(Number),
            knm_providers:deactivate_feature(Number, ?FEATURE_E911)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?FEATURE_E911).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_e911(knm_number:knm_number()) -> knm_number:knm_number().
maybe_update_e911(N) ->
    PN = knm_number:phone_number(N),
    CurrentE911 = feature(N),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            _ = remove_number(N),
            knm_providers:deactivate_feature(N, ?FEATURE_E911);
        'false' when NotChanged  ->
            N;
        'false' ->
            lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
            NewE911 = maybe_update_e911(N, E911),
            lager:debug("using address ~p", [NewE911]),
            NewDoc = kz_json:set_value(?FEATURE_E911, NewE911, knm_phone_number:doc(PN)),
            NewPN = knm_phone_number:reset_doc(PN, NewDoc),
            NewN = knm_number:set_phone_number(N, NewPN),
            knm_providers:activate_feature(NewN, {?FEATURE_E911, NewE911})
    end.

-spec maybe_update_e911(knm_number:knm_number(), kz_json:object()) -> kz_json:object().
maybe_update_e911(Number, Address) ->
    Location = json_address_to_xml_location(Address),
    case is_valid_location(Location) of
        {'error', E} ->
            lager:error("error while checking location ~p", [E]),
            knm_errors:unspecified(E, Number);
        {'invalid', Reason}->
            lager:error("error while checking location ~p", [Reason]),
            Error = <<Reason/binary, " (", (kz_json:encode(Address))/binary, ")">>,
            knm_errors:invalid(Number, Error);
        {'provisioned', _} ->
            lager:debug("location seems already provisioned"),
            update_e911(Number, Address);
        {'geocoded', [_Loc]} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Number, Address);
        {'geocoded', [_|_]=Addresses} ->
            lager:warning("location could correspond to multiple addresses"),
            Msg = <<"more than one address found">>,
            Update =
                kz_json:from_list([{<<"cause">>, Address}
                                  ,{<<"details">>, Addresses}
                                  ,{<<"message">>, Msg}
                                  ]),
            knm_errors:multiple_choice(Number, Update);
        {'geocoded', _Loc} ->
            lager:debug("location seems geocoded to only one address"),
            update_e911(Number, Address)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec update_e911(knm_number:knm_number(), kz_json:object()) -> kz_json:object().
update_e911(Number, Address) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    update_e911(Number, Address, DryRun).

-spec update_e911(knm_number:knm_number(), kz_json:object(), boolean()) -> kz_json:object().
update_e911(_Number, Address, 'true') -> Address;
update_e911(Number, Address, 'false') ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    Location = json_address_to_xml_location(Address),
    E911Name = kz_json:get_ne_binary_value(?E911_NAME, Address),
    CallerName = knm_providers:e911_caller_name(Number, E911Name),
    case add_location(Num, Location, CallerName) of
        {'provisioned', E911} ->
            lager:debug("provisioned address"),
            E911;
        {'geocoded', E911} ->
            provision_geocoded(E911);
        {_E, Reason} ->
            lager:debug("~s provisioning address: ~p", [_E, Reason]),
            knm_errors:unspecified(Reason, Number)
    end.

-spec provision_geocoded(kz_json:object()) -> kz_json:object().
provision_geocoded(E911) ->
    lager:debug("added location, attempting to provision new location"),
    case provision_location(kz_json:get_value(<<"location_id">>, E911)) of
        'undefined'=Status ->
            lager:debug("provisioning attempt moved location to status: ~s", [Status]),
            E911;
        Status ->
            lager:debug("provisioning attempt moved location to status: ~s", [Status]),
            kz_json:set_value(<<"status">>, Status, E911)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type location_response() :: {'geocoded', kz_json:object() | kz_json:objects()} |
                             {'provisioned', kz_json:object() | kz_json:objects()} |
                             {'invalid', binary()} |
                             {'error', binary()}.
-spec is_valid_location([xml_location()]) -> location_response().
is_valid_location(Location) ->
    case emergency_provisioning_request('validateLocation', Location) of
        {'ok', Response} -> parse_response(Response);
        {'error', Reason} -> {'error', kz_term:to_binary(Reason)}
    end.


-spec parse_response(kz_types:xml_el()) -> location_response().
parse_response(Response) ->
    StatusCode = kz_xml:get_value("//Location/status/code/text()", Response),
    parse_response(StatusCode, Response).

-spec parse_response(kz_term:ne_binary(), kz_types:xml_el()) -> location_response().
parse_response(<<"GEOCODED">>, Response) ->
    {'geocoded',    location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
parse_response(<<"PROVISIONED">>, Response) ->
    {'provisioned', location_xml_to_json_address(xmerl_xpath:string("//Location", Response))};
parse_response(<<"INVALID">>, Response) ->
    {'invalid', kz_xml:get_value("//Location/status/description/text()", Response)};
parse_response(<<"ERROR">>, Response) ->
    {'error', kz_xml:get_value("//Location/status/description/text()", Response)};
parse_response(Else, _) ->
    {'error', kz_term:to_binary(Else)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_location(kz_term:ne_binary(), [xml_location()], kz_term:ne_binary()) ->
                          {'geocoded', kz_json:object()} |
                          {'provisioned', kz_json:object()} |
                          {'error', binary()}.
add_location(Number, Location, CallerName) ->
    Props = [{'uri', [{'uri', [kz_term:to_list(<<"tel:", (knm_converters:to_1npan(Number))/binary>>)]}
                     ,{'callername', [kz_term:to_list(CallerName)]}
                     ]
             }
             | Location
            ],
    case emergency_provisioning_request('addLocation', Props) of
        {'ok', Response} -> parse_response(Response);
        {'error', Reason} -> {'error', kz_term:to_binary(Reason)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec provision_location(kz_term:ne_binary()) -> kz_term:api_binary().
provision_location(LocationId) ->
    Props = [{'locationid', [kz_term:to_list(LocationId)]}],
    case emergency_provisioning_request('provisionLocation', Props) of
        {'error', _} -> 'undefined';
        {'ok', Response} ->
            kz_xml:get_value("//LocationStatus/code/text()", Response)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_number(knm_number:knm_number()) -> kz_term:api_binary().
remove_number(Number) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    lager:debug("removing from upstream '~s'", [Num]),
    Props = [{'uri', [kz_term:to_list(<<"tel:", (knm_converters:to_1npan(Num))/binary>>)]}],
    case emergency_provisioning_request('removeURI', Props) of
        {'error', 'server_error'} ->
            lager:debug("removed number from upstream"),
            <<"REMOVED">>;
        {'error', _E} ->
            lager:debug("removed number from upstream: ~p", [_E]),
            <<"REMOVED">>;
        {'ok', Response} ->
            case kz_xml:get_value("//URIStatus/code/text()", Response) of
                <<"REMOVED">> = R ->
                    lager:debug("removed number from upstream"),
                    R;
                Else ->
                    lager:debug("failed to remove number from upstream: ~p", [Else]),
                    Else
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Make a REST request to dash e911 emergency provisioning API to preform
%% the given verb (validatelocation, addlocation, etc).
%% @end
%%------------------------------------------------------------------------------
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
                                            {'ok', kz_types:xml_el()} |
                                            {'error', emergency_provisioning_error()}.
emergency_provisioning_request(Verb, Props) ->
    URL = list_to_binary([?EMERG_URL, "/", kz_term:to_lower_binary(Verb)]),
    Body = unicode:characters_to_binary(
             xmerl:export_simple([{Verb, Props}]
                                ,'xmerl_xml'
                                ,[{'prolog', ?XML_PROLOG}]
                                )
            ),
    Headers = [{"Accept", "*/*"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'basic_auth', {?AUTH_USERNAME, ?AUTH_PASSWORD}}
                  ],
    lager:debug("making ~s request to upstream ~s", [Verb, URL]),
    ?DEBUG("Request:~n~s ~s~n~s~n", ['post', URL, Body]),
    case kz_http:post(kz_term:to_list(URL), Headers, Body, HTTPOptions) of
        {'ok', 401, _, _Response} ->
            ?DEBUG("Response:~n401~n~s~n", [_Response]),
            lager:debug("request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', 403, _, _Response} ->
            ?DEBUG("Response:~n403~n~s~n", [_Response]),
            lager:debug("request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', 404, _, _Response} ->
            ?DEBUG("Response:~n404~n~s~n", [_Response]),
            lager:debug("request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', 500, _, _Response} ->
            ?DEBUG("Response:~n500~n~s~n", [_Response]),
            lager:debug("request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', 503, _, _Response} ->
            ?DEBUG("Response:~n503~n~s~n", [_Response]),
            lager:debug("request error: 503"),
            {'error', 'server_error'};
        {'ok', Code, _, Response} ->
            ?DEBUG("Response:~n~p~n~s~n", [Code, Response]),
            lager:debug("received response from upstream"),
            try xmerl_scan:string(kz_term:to_list(Response)) of
                {Xml, _} -> {'ok', Xml}
            catch
                _:R ->
                    lager:debug("failed to decode xml: ~p", [R]),
                    {'error', 'empty_response'}
            end;
        {'error', _Reason} ->
            lager:debug("request error: ~p", [_Reason]),
            {'error', 'unreachable'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type xml_location_property() :: {'address1', list()} |
                                 {'address2', list()} |
                                 {'community', list()} |
                                 {'state', list()} |
                                 {'postalcode', list()} |
                                 {'type', list()}.
-type xml_location() :: {'location', [xml_location_property()]}.

-spec json_address_to_xml_location(kz_json:object()) -> [xml_location()].
json_address_to_xml_location(JObj) ->
    Props = [{'address1', [kz_json:get_string_value(?E911_STREET1, JObj)]}
            ,{'address2', [kz_json:get_string_value(?E911_STREET2, JObj)]}
            ,{'community', [kz_json:get_string_value(?E911_CITY, JObj)]}
            ,{'state', [kz_json:get_string_value(?E911_STATE, JObj)]}
            ,{'postalcode', [kz_json:get_string_value(?E911_ZIP, JObj)]}
            ,{'type', ["ADDRESS"]}
            ],
    [{'location', [KV || {_, V}=KV <- Props, V =/= ['undefined']]}].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec location_xml_to_json_address(kz_types:xml_el() | kz_types:xml_els()) -> kz_json:object() | kz_json:objects().
location_xml_to_json_address([]) ->
    kz_json:new();
location_xml_to_json_address([Xml]) ->
    location_xml_to_json_address(Xml);
location_xml_to_json_address(Xml) when is_list(Xml) ->
    [location_xml_to_json_address(X) || X <- Xml];
location_xml_to_json_address(Xml) ->
    kz_json:from_list(
      [{?E911_STREET1, kz_xml:get_value("address1/text()", Xml)}
      ,{?E911_STREET2, kz_xml:get_value("address2/text()", Xml)}
      ,{<<"activated_time">>, kz_xml:get_value("activated_time/text()", Xml)}
      ,{?E911_NAME, kz_xml:get_value("callername/text()", Xml)}
      ,{<<"comments">>, kz_xml:get_value("comments/text()", Xml)}
      ,{?E911_CITY, kz_xml:get_value("community/text()", Xml)}
      ,{<<"order_id">>, kz_xml:get_value("customerorderid/text()", Xml)}
      ,{<<"latitude">>, kz_xml:get_value("latitude/text()", Xml)}
      ,{<<"longitude">>, kz_xml:get_value("longitude/text()", Xml)}
      ,{<<"location_id">>, kz_xml:get_value("locationid/text()", Xml)}
      ,{<<"plus_four">>, kz_xml:get_value("plusfour/text()", Xml)}
      ,{?E911_ZIP, kz_xml:get_value("postalcode/text()", Xml)}
      ,{?E911_STATE, kz_xml:get_value("state/text()", Xml)}
      ,{<<"status">>, kz_xml:get_value("status/code/text()", Xml)}
      ,{<<"legacy_data">>, legacy_data_xml_to_json(xmerl_xpath:string("legacydata", Xml))}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec legacy_data_xml_to_json(term()) -> kz_json:object().
legacy_data_xml_to_json([]) ->
    kz_json:new();
legacy_data_xml_to_json([Xml]) ->
    legacy_data_xml_to_json(Xml);
legacy_data_xml_to_json(Xml) when is_list(Xml) ->
    [legacy_data_xml_to_json(X) || X <- Xml];
legacy_data_xml_to_json(Xml) ->
    kz_json:from_list(
      [{<<"house_number">>, kz_xml:get_value("housenumber/text()", Xml)}
      ,{<<"predirectional">>, kz_xml:get_value("predirectional/text()", Xml)}
      ,{<<"streetname">>, kz_xml:get_value("streetname/text()", Xml)}
      ,{<<"suite">>, kz_xml:get_value("suite/text()", Xml)}
      ]).
