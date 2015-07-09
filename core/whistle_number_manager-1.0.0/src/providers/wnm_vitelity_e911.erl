%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_vitelity_e911).

-export([save/1
         ,delete/1
         ,is_valid_location/1
         ,get_location/1
        ]).

-include("../wnm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{state = ?NUMBER_STATE_PORT_IN} = Number) ->
    maybe_update_e911(Number);
save(#number{state = ?NUMBER_STATE_RESERVED} = Number) ->
    maybe_update_e911(Number);
save(#number{state = ?NUMBER_STATE_IN_SERVICE} = Number) ->
    maybe_update_e911(Number);
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
    case wh_json:get_ne_value(?VITELITY_KEY, CurrentDoc) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            _ = remove_number(Num),
            Number#number{features=sets:del_element(?VITELITY_KEY, Features)
                          ,number_doc=wh_json:delete_key(?VITELITY_KEY, Doc)
                         }
    end.

-spec maybe_update_e911(wnm_number()) -> wnm_number().
maybe_update_e911(#number{current_number_doc=CurrentJObj
                          ,features=Features
                          ,number_doc=JObj
                          ,dry_run='true'
                         }=N) ->
    CurrentE911 = wh_json:get_ne_value(?VITELITY_KEY, CurrentJObj),
    E911 = wh_json:get_ne_value(?VITELITY_KEY, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentE911, E911),
    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("dry run: remove vitelity e911 information"),
            N#number{features=sets:del_element(?VITELITY_KEY, Features)};
        'false' when NotChanged  ->
            N#number{features=sets:add_element(?VITELITY_KEY, Features)};
        'false' ->
            lager:debug("dry run: change vitelity e911 information: ~s", [wh_json:encode(E911)]),
            wnm_number:activate_feature(?VITELITY_KEY, N)
    end;
maybe_update_e911(#number{current_number_doc=CurrentJObj
                          ,features=Features
                          ,number=Number
                          ,number_doc=JObj
                         }=N) ->
    CurrentE911 = wh_json:get_ne_value(?VITELITY_KEY, CurrentJObj),
    E911 = wh_json:get_ne_value(?VITELITY_KEY, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentE911, E911),
    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("vitelity e911 information has been removed, updating vitelity"),
            _ = remove_number(Number),
            N#number{features=sets:del_element(?VITELITY_KEY, Features)};
        'false' when NotChanged  ->
            N#number{features=sets:add_element(?VITELITY_KEY, Features)};
        'false' ->
            lager:debug("vitelity e911 information has been changed: ~s", [wh_json:encode(E911)]),
            N1 = wnm_number:activate_feature(?VITELITY_KEY, N),
            N1#number{number_doc=wh_json:set_value(?VITELITY_KEY, update_e911(N1, E911), JObj)}
    end.

-spec remove_number(ne_binary()) ->
                           {'ok', _} |
                           {'error', _}.
remove_number(DID) ->
    case query_vitelity(wnm_vitelity_util:build_uri(remove_e911_options(DID))) of
        {'error', _}=E -> E;
        {'ok', RespXML} -> process_xml_resp(RespXML)
    end.

-spec remove_e911_options(ne_binary()) -> list().
remove_e911_options(DID) ->
    [{'qs', [{'did', wnm_util:to_npan(DID)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911delete">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec get_location(ne_binary() | wnm_number()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
get_location(#number{number=DID}) -> get_location(DID);
get_location(DID) ->
    case query_vitelity(wnm_vitelity_util:build_uri(get_location_options(DID))) of
        {'ok', RespXML} -> process_xml_resp(RespXML);
        {'error', _}=E -> E
    end.

-spec get_location_options(ne_binary()) -> list().
get_location_options(DID) ->
    [{'qs', [{'did',  wnm_util:to_npan(DID)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911getinfo">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec update_e911(wnm_number(), wh_json:object()) -> wh_json:object().
update_e911(Number, Address) ->
    query_vitelity(Number, wnm_vitelity_util:build_uri(e911_options(Number, Address))).

-spec e911_options(wnm_number(), wh_json:object()) -> list().
e911_options(#number{number=Number
                     ,assigned_to=AccountId
                    }, AddressJObj) ->
    State = wnm_vitelity_util:get_short_state(wh_json:get_value(<<"region">>, AddressJObj)),
    {UnitType, UnitNumber} = get_unit(wh_json:get_value(<<"extended_address">>, AddressJObj)),
    [{'qs', props:filter_undefined(
                [{'did',  wnm_util:to_npan(Number)}
                 ,{'name', wh_json:get_value(<<"customer_name">>, AddressJObj, get_account_name(AccountId))}
                 ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
                 ,{'unittype', UnitType}
                 ,{'unitnumber', UnitNumber}
                 ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
                 ,{'state', State}
                 ,{'zip', wh_json:get_value(<<"postal_code">>, AddressJObj)}
                 ,{'xml', <<"yes">>}
                 ,{'cmd', <<"e911send">>}
                 | wnm_vitelity_util:default_options()
                ])
     }
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec get_unit(ne_binary()) -> {api_binary(),  api_binary()}.
get_unit(ExtendedAddress) ->
    case binary:split(ExtendedAddress, <<" ">>) of
        [UnitType, UnitNumber|_] -> {UnitType, UnitNumber};
        [UnitType] -> {UnitType, 'undefined'};
        _ -> {'undefined', 'undefined'}
    end.

-spec get_account_name(ne_binary()) -> api_binary().
get_account_name(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'error', _Error} ->
            lager:error('error opening account doc ~s', [AccountId]),
            'undefined';
        {'ok', JObj} -> kz_account:name(JObj)
    end.

-spec is_valid_location(wh_json:object()) ->
                               {'ok', wh_json:object()} |
                               {'error', ne_binary()}.
is_valid_location(Location) ->
    case query_vitelity(wnm_vitelity_util:build_uri(location_options(Location))) of
        {'error', _}=E -> E;
        {'ok', XML} -> process_xml_resp(XML)
    end.

-spec location_options(wh_json:object()) -> list().
location_options(AddressJObj) ->
    State = wnm_vitelity_util:get_short_state(wh_json:get_value(<<"region">>, AddressJObj)),
    [{'qs', [{'name', wh_json:get_value(<<"customer_name">>, AddressJObj)}
             ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
             ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
             ,{'state', State}
             ,{'zip', wh_json:get_value(<<"postal_code">>, AddressJObj)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911checkaddress">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec query_vitelity(wnm_number(), ne_binary()) ->
                            wh_json:object().
query_vitelity(N, URI) ->
    case query_vitelity(URI) of
        {'ok', XML} -> process_xml_resp(N, XML);
        {'error', _E} ->
            wnm_number:error_carrier_fault(<<"failed to query API">>, N)
    end.

-spec query_vitelity(ne_binary()) ->
                            {'ok', text()} |
                            {'error', _}.
query_vitelity(URI) ->
    lager:debug("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~s: ~s", [_RespCode, RespXML]),
            {'ok', RespXML};
        {'error', _R}=E ->
            lager:debug("error querying: ~p", [_R]),
            E
    end.

-spec process_xml_resp(wnm_number(), text()) -> wh_json:object().
process_xml_resp(N, RespXML) ->
    case process_xml_resp(RespXML) of
        {'ok', JObj} -> JObj;
        {'error', E} ->
            wnm_number:error_carrier_fault(E, N)
    end.

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
            {'error', <<"invalid response from server">>}
    end.

-spec process_xml_content_tag(xml_el()) ->
                                     {'ok', wh_json:object() | ne_binary()} |
                                     {'error', ne_binary()}.
process_xml_content_tag(#xmlElement{name='content'
                                    ,content=Children
                                   }) ->
    Els = kz_xml:elements(Children),
    case wnm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', wnm_vitelity_util:xml_resp_error_msg(Els)};
        <<"ok">> ->
            {'ok', xml_resp(Els)}
    end.

-spec xml_resp(xml_els()) -> wh_json:object() | ne_binary().
xml_resp([#xmlElement{name='info'
                     ,content=Content
                    }
          |_]) ->
    wh_json:from_list(
      wnm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(Content)
       ));
xml_resp([#xmlElement{name='response'
                      ,content=Content
                     }
          |_]) ->
    kz_xml:texts_to_binary(Content);
xml_resp([_|T]) -> xml_resp(T).
