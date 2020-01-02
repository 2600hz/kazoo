%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc Handle e911 provisioning
%%% @author James Aimonetti
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_vitelity_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([is_valid_location/1]).
-export([get_location/1]).

-include("knm.hrl").

-define(CUSTOMER_NAME, <<"customer_name">>).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    State = knm_phone_number:state(PN),
    save(PN, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(PN);
save(PN, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(PN);
save(PN, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(PN);
save(PN, _State) ->
    delete(PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    case knm_phone_number:feature(PN, ?FEATURE_E911) of
        'undefined' -> PN;
        _Else ->
            lager:debug("removing e911 information"),
            _ = remove_number(PN),
            knm_providers:deactivate_feature(PN, ?FEATURE_E911)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_valid_location(kz_json:object()) -> {'ok', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
is_valid_location(Location) ->
    URL = knm_vitelity_util:build_uri(location_options(Location)),
    case knm_vitelity_util:query_vitelity(URL) of
        {'error', _}=E -> E;
        {'ok', XML} -> process_xml_resp(XML)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_location(kz_term:ne_binary() | knm_phone_number:record()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
get_location(?NE_BINARY=DID) ->
    URI = knm_vitelity_util:build_uri(get_location_options(DID)),
    case knm_vitelity_util:query_vitelity(URI) of
        {'ok', RespXML} -> process_xml_resp(RespXML);
        {'error', _}=E -> E
    end;
get_location(PN) ->
    DID = knm_phone_number:number(PN),
    get_location(DID).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_e911(knm_phone_number:record()) -> knm_phone_number:record().
maybe_update_e911(PN) ->
    IsDryRun = knm_phone_number:dry_run(PN),
    maybe_update_e911(PN, IsDryRun).

-spec maybe_update_e911(knm_phone_number:record(), boolean()) -> knm_phone_number:record().
maybe_update_e911(PN, 'true') ->
    CurrentE911 = knm_phone_number:feature(PN, ?FEATURE_E911),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_providers:deactivate_feature(PN, ?FEATURE_E911);
        'false' when NotChanged  ->
            PN;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_providers:activate_feature(PN, {?FEATURE_E911, E911})
    end;

maybe_update_e911(PN, 'false') ->
    CurrentE911 = knm_phone_number:feature(PN, ?FEATURE_E911),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            _ = remove_number(PN),
            knm_providers:deactivate_feature(PN, ?FEATURE_E911);
        'false' when NotChanged  ->
            PN;
        'false' ->
            lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
            case update_e911(PN, E911) of
                {'ok', Data} ->
                    knm_providers:activate_feature(PN, {?FEATURE_E911, Data});
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, PN)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_number(knm_phone_number:record()) ->
          {'ok', kz_json:object() | kz_term:ne_binary()} |
          {'error', kz_term:ne_binary()}.
remove_number(PN) ->
    DID = knm_phone_number:number(PN),
    URI = knm_vitelity_util:build_uri(remove_e911_options(DID)),
    case knm_vitelity_util:query_vitelity(URI) of
        {'error', _}=E -> E;
        {'ok', RespXML} -> process_xml_resp(RespXML)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_e911_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
remove_e911_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'xml', <<"yes">>}
            ,{'cmd', <<"e911delete">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_location_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
get_location_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'xml', <<"yes">>}
            ,{'cmd', <<"e911getinfo">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_e911(knm_phone_number:record(), kz_json:object()) ->
          {'ok', kz_json:object() | kz_term:ne_binary()} |
          {'error', kz_term:ne_binary()}.
update_e911(PN, Address) ->
    URI = knm_vitelity_util:build_uri(e911_options(PN, Address)),
    case knm_vitelity_util:query_vitelity(URI) of
        {'ok', XML} -> process_xml_resp(XML);
        {'error', _E}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec e911_options(knm_phone_number:record(), kz_json:object()) ->
          knm_vitelity_util:query_options().
e911_options(PN, AddressJObj) ->
    DID = knm_phone_number:number(PN),
    State = knm_vitelity_util:get_short_state(kz_json:get_value(?E911_STATE, AddressJObj)),
    {UnitType, UnitNumber} = get_unit(kz_json:get_value(?E911_STREET2, AddressJObj)),
    [{'qs', props:filter_undefined(
              [{'did', knm_converters:to_npan(DID)}
              ,{'name', get_caller_name(PN, AddressJObj)}
              ,{'address', kz_json:get_value(?E911_STREET1, AddressJObj)}
              ,{'unittype', UnitType}
              ,{'unitnumber', UnitNumber}
              ,{'city', kz_json:get_value(?E911_CITY, AddressJObj)}
              ,{'state', State}
              ,{'zip', kz_json:get_value(?E911_ZIP, AddressJObj)}
              ,{'xml', <<"yes">>}
              ,{'cmd', <<"e911send">>}
               | knm_vitelity_util:default_options()
              ])
     }
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_unit(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_unit(ExtendedAddress) ->
    case binary:split(ExtendedAddress, <<" ">>) of
        [UnitType, UnitNumber|_] -> {UnitType, UnitNumber};
        [UnitType] -> {UnitType, 'undefined'};
        _ -> {'undefined', 'undefined'}
    end.

-spec get_caller_name(knm_phone_number:record(), kz_json:object()) -> kz_term:ne_binary().
get_caller_name(PN, AddressJObj) ->
    case kz_json:get_ne_binary_value(?CUSTOMER_NAME, AddressJObj) of
        ?NE_BINARY=Name -> Name;
        'undefined' ->
            E911Name = kz_json:get_ne_binary_value(?E911_NAME, AddressJObj),
            knm_providers:e911_caller_name(PN, E911Name)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec location_options(kz_json:object()) -> knm_vitelity_util:query_options().
location_options(AddressJObj) ->
    State = knm_vitelity_util:get_short_state(kz_json:get_value(?E911_STATE, AddressJObj)),
    CallerName = case kz_json:get_ne_binary_value(?CUSTOMER_NAME, AddressJObj) of
                     ?NE_BINARY=Name -> Name;
                     'undefined' ->
                         kz_json:get_ne_binary_value(?E911_NAME, AddressJObj, ?E911_NAME_DEFAULT)
                 end,
    [{'qs', [{'name', CallerName}
            ,{'address', kz_json:get_value(?E911_STREET1, AddressJObj)}
            ,{'city', kz_json:get_value(?E911_CITY, AddressJObj)}
            ,{'state', State}
            ,{'zip', kz_json:get_value(?E911_ZIP, AddressJObj)}
            ,{'xml', <<"yes">>}
            ,{'cmd', <<"e911checkaddress">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_resp(kz_term:text()) -> {'ok', kz_json:object() | kz_term:ne_binary()} |
          {'error', kz_term:ne_binary()}.
process_xml_resp(RespXML_binary) ->
    RespXML = unicode:characters_to_list( RespXML_binary),
    try xmerl_scan:string(RespXML) of
        {XmlEl, _} -> process_xml_content_tag(XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process xml: ~s: ~p", [_E, _R]),
            {'error', 'invalid_resp_from_server'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_content_tag(kz_types:xml_el()) -> {'ok', kz_json:object() | kz_term:ne_binary()} |
          {'error', kz_term:ne_binary()}.
process_xml_content_tag(#xmlElement{name='content'
                                   ,content=Children
                                   }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"ok">> -> {'ok', xml_resp(Els)};
        <<"fail">> -> {'error', knm_vitelity_util:xml_resp_error_msg(Els)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec xml_resp(kz_types:xml_els()) -> kz_json:object() | kz_term:ne_binary().
xml_resp([#xmlElement{name='info'
                     ,content=Content
                     }
          |_]) ->
    kz_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(Content)
       ));
xml_resp([#xmlElement{name='response'
                     ,content=Content
                     }
          |_]) ->
    kz_xml:texts_to_binary(Content);
xml_resp([_|T]) ->
    xml_resp(T).
