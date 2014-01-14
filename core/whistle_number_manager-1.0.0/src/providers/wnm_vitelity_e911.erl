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
save(#number{state = <<"port_in">>} = Number) ->
    maybe_update_e911(Number);
save(#number{state = <<"reserved">>} = Number) ->
    maybe_update_e911(Number);
save(#number{state = <<"in_service">>} = Number) ->
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
    case wh_json:get_ne_value(<<"vitelity_e911">>, CurrentDoc) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            _ = remove_number(Num),
            Number#number{features=sets:del_element(<<"vitelity_e911">>, Features)
                          ,number_doc=wh_json:delete_key(<<"vitelity_e911">>, Doc)
                         }
    end.

-spec maybe_update_e911(wnm_number()) -> wnm_number().
maybe_update_e911(#number{current_number_doc=CurrentJObj
                          ,features=Features
                          ,number=Number
                          ,number_doc=JObj
                         }=N) ->
    CurrentE911 = wh_json:get_ne_value(<<"vitelity_e911">>, CurrentJObj),
    E911 = wh_json:get_ne_value(<<"vitelity_e911">>, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentE911, E911),
    case wh_util:is_empty(E911) of
        'true' ->
            lager:debug("vitelity e911 information has been removed, updating vitelity"),
            _ = remove_number(Number),
            N#number{features=sets:del_element(<<"vitelity_e911">>, Features)};
        'false' when NotChanged  ->
            N#number{features=sets:add_element(<<"vitelity_e911">>, Features)};
        'false' ->
            lager:debug("vitelity e911 information has been changed: ~s", [wh_json:encode(E911)]),
            N1 = wnm_number:activate_feature(<<"vitelity_e911">>, N),
            N1#number{number_doc=update_e911(N1, E911)}
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
    [{'qs', [{'did', DID}
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
    [{'qs', [{'did', DID}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911getinfo">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec update_e911(wnm_number(), wh_json:object()) -> wh_json:object().
update_e911(#number{number=Number}=N, Address) ->
    query_vitelity(N, wnm_vitelity_util:build_uri(e911_options(Number, Address))).

-spec e911_options(ne_binary(), wh_json:object()) -> list().
e911_options(Number, AddressJObj) ->
    [{'qs', [{'did', Number}
             ,{'name', wh_json:get_value(<<"customer_name">>, AddressJObj)}
             ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
             ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
             ,{'state', wh_json:get_value(<<"region">>, AddressJObj)}
             ,{'zip', wh_json:get_value(<<"postal_code">>, AddressJObj)}
             ,{'xml', <<"yes">>}
             ,{'cmd', <<"e911send">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

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
    [{'qs', [{'name', wh_json:get_value(<<"customer_name">>, AddressJObj)}
             ,{'address', wh_json:get_value(<<"street_address">>, AddressJObj)}
             ,{'city', wh_json:get_value(<<"locality">>, AddressJObj)}
             ,{'state', wh_json:get_value(<<"region">>, AddressJObj)}
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
