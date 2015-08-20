%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_vitelity_e911).

-export([save/1]).
-export([delete/1]).
-export([is_valid_location/1]).
-export([get_location/1]).

-include("../knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number_return().
-spec save(knm_number:knm_number(), ne_binary()) ->
                  knm_number_return().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
     maybe_update_e911(Number);
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
                    {'ok', knm_number:knm_number()}.
delete(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?VITELITY_KEY) of
        'undefined' -> {'ok', Number};
        _Else ->
            lager:debug("removing e911 information"),
            _ = remove_number(Number),
            knm_services:deactivate_feature(Number, ?VITELITY_KEY)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_location(wh_json:object()) ->
                               {'ok', wh_json:object()} |
                               {'error', ne_binary()}.
is_valid_location(Location) ->
    case knm_vitelity_util:query_vitelity(
           knm_vitelity_util:build_uri(location_options(Location))
          )
    of
        {'error', _}=E -> E;
        {'ok', XML} -> process_xml_resp(XML)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_location(ne_binary() | knm_number:knm_number()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
get_location(DID) when is_binary(DID) ->
    case knm_vitelity_util:query_vitelity(
           knm_vitelity_util:build_uri(get_location_options(DID))
          )
    of
        {'ok', RespXML} -> process_xml_resp(RespXML);
        {'error', _}=E -> E
    end;
get_location(Number) ->
    get_location(
      knm_phone_number:number(knm_number:phone_number(Number))
     ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_e911(knm_number:knm_number()) ->
                               knm_number_return().
-spec maybe_update_e911(knm_number:knm_number(), boolean()) ->
                               knm_number_return().
maybe_update_e911(Number) ->
    maybe_update_e911(Number
                      ,knm_phone_number:dry_run(knm_number:phone_number(Number))
                     ).

maybe_update_e911(Number, 'true') ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentE911 = wh_json:get_ne_value(?VITELITY_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    E911 = wh_json:get_ne_value([?PVT_FEATURES, ?VITELITY_KEY], Doc),

    NotChanged = wh_json:are_identical(CurrentE911, E911),

    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("dry run: remove vitelity e911 information"),
            knm_services:deactivate_feature(Number, ?VITELITY_KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?VITELITY_KEY);
        'false' ->
            lager:debug("dry run: change vitelity e911 information: ~s", [wh_json:encode(E911)]),
            knm_services:activate_feature(Number, ?VITELITY_KEY)
    end;
maybe_update_e911(Number, 'false') ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentE911 = wh_json:get_ne_value(?VITELITY_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    E911 = wh_json:get_ne_value([?PVT_FEATURES, ?VITELITY_KEY], Doc),

    NotChanged = wh_json:are_identical(CurrentE911, E911),

    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("vitelity e911 information has been removed, updating vitelity"),
            _ = remove_number(Number),
            knm_services:deactivate_feature(Number, ?VITELITY_KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?VITELITY_KEY);
        'false' ->
            lager:debug("vitelity e911 information has been changed: ~s", [wh_json:encode(E911)]),
            {'ok', Number1} = knm_services:activate_feature(Number, ?VITELITY_KEY),
            case update_e911(Number1, E911) of
                {'ok', Data} ->
                    {'ok'
                     ,knm_phone_number:set_feature(knm_number:phone_number(Number1)
                                                   ,?VITELITY_KEY
                                                   ,Data
                                                  )
                    };
                {'error', _R}=Error ->
                    lager:error("vitelity e911 information update failed: ~p", [_R]),
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number(knm_number:knm_number()) ->
                           {'ok', wh_json:object() | ne_binary()} |
                           {'error', ne_binary()}.
remove_number(Number) ->
    DID = knm_phone_number:number(knm_number:phone_number(Number)),
    case knm_vitelity_util:query_vitelity(
           knm_vitelity_util:build_uri(remove_e911_options(DID))
          )
    of
        {'error', _}=E -> E;
        {'ok', RespXML} -> process_xml_resp(RespXML)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_e911_options(ne_binary()) ->
                                 knm_vitelity_util:query_options().
remove_e911_options(DID) ->
    [{'qs', [{'did', (knm_converters:default()):to_npan(DID)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911delete">>}
             | knm_vitelity_util:default_options()
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_location_options(ne_binary()) ->
                                  knm_vitelity_util:query_options().
get_location_options(DID) ->
    [{'qs', [{'did',  (knm_converters:default()):to_npan(DID)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911getinfo">>}
             | knm_vitelity_util:default_options()
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_e911(knm_number:knm_number(), wh_json:object()) ->
                         {'ok', wh_json:object() | ne_binary()} |
                         {'error', ne_binary()}.
update_e911(Number, Address) ->
    case knm_vitelity_util:query_vitelity(
           knm_vitelity_util:build_uri(e911_options(Number, Address))
          )
    of
        {'ok', XML} -> process_xml_resp(XML);
        {'error', _E}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec e911_options(knm_number:knm_number(), wh_json:object()) ->
                          knm_vitelity_util:query_options().
e911_options(Number, AddressJObj) ->
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    DID = knm_phone_number:number(PhoneNumber),
    State = knm_vitelity_util:get_short_state(wh_json:get_value(<<"region">>, AddressJObj)),
    {UnitType, UnitNumber} = get_unit(wh_json:get_value(<<"extended_address">>, AddressJObj)),
    [{'qs', props:filter_undefined(
              [{'did',  (knm_converters:default()):to_npan(DID)}
               ,{'name', wh_json:get_value(<<"customer_name">>, AddressJObj, get_account_name(AccountId))}
               ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
               ,{'unittype', UnitType}
               ,{'unitnumber', UnitNumber}
               ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
               ,{'state', State}
               ,{'zip', wh_json:get_value(<<"postal_code">>, AddressJObj)}
               ,{'xml', <<"yes">>}
               ,{'cmd', <<"e911send">>}
               | knm_vitelity_util:default_options()
              ])
     }
     ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_unit(ne_binary()) -> {api_binary(), api_binary()}.
get_unit(ExtendedAddress) ->
    case binary:split(ExtendedAddress, <<" ">>) of
        [UnitType, UnitNumber|_] -> {UnitType, UnitNumber};
        [UnitType] -> {UnitType, 'undefined'};
        _ -> {'undefined', 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_account_name(ne_binary()) -> api_binary().
get_account_name(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _Error} ->
            lager:error('error opening ~p in ~p', [AccountId, AccountDb]),
            'undefined';
        {'ok', JObj} -> wh_json:get_value(<<"name">>, JObj, 'undefined')
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec location_options(wh_json:object()) ->
                              knm_vitelity_util:query_options().
location_options(AddressJObj) ->
    State = knm_vitelity_util:get_short_state(wh_json:get_value(<<"region">>, AddressJObj)),
    [{'qs', [{'name', wh_json:get_value(<<"customer_name">>, AddressJObj)}
             ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
             ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
             ,{'state', State}
             ,{'zip', wh_json:get_value(<<"postal_code">>, AddressJObj)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911checkaddress">>}
             | knm_vitelity_util:default_options()
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(text()) ->
                              {'ok', wh_json:object() | ne_binary()} |
                              {'error', ne_binary()}.
process_xml_resp(RespXML) ->
    try xmerl_scan:string(RespXML) of
        {XmlEl, _} ->
            process_xml_content_tag(XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process XML: ~s: ~p", [_E, _R]),
            {'error', 'invalid_resp_from_server'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(xml_el()) ->
                                     {'ok', wh_json:object() | ne_binary()} |
                                     {'error', ne_binary()}.
process_xml_content_tag(#xmlElement{name='content'
                                    ,content=Children
                                   }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', knm_vitelity_util:xml_resp_error_msg(Els)};
        <<"ok">> ->
            {'ok', xml_resp(Els)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec xml_resp(xml_els()) -> wh_json:object() | ne_binary().
xml_resp([#xmlElement{name='info'
                     ,content=Content
                    }
          |_]) ->
    wh_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(Content)
       ));
xml_resp([#xmlElement{name='response'
                      ,content=Content
                     }
          |_]) ->
    kz_xml:texts_to_binary(Content);
xml_resp([_|T]) -> xml_resp(T).
