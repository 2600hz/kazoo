%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_vitelity).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
        ]).

-include("../wnm.hrl").

-define(WNM_VITELITY_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".vitelity">>).

-define(TOLLFREE_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"tollfree_uri">>
                                                 ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}&xml={xml}">>)).

-define(NPA_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"npa_uri">>
                                                ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}&npa={npa}&xml={xml}">>)).

-define(NPAXX_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"npaxx_uri">>
                                                  ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}&npanxx={npanxx}&xml={xml}">>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:objects()} |
                          {'error', _}.
find_numbers(Prefix, Quantity, Opts) ->
    case props:get_value('tollfree', Opts, 'false') of
        'true' -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        'false' ->
            classify_and_find(Prefix, Quantity, Opts)
    end.

-spec classify_and_find(ne_binary(), pos_integer(), wh_proplist()) ->
                               {'ok', wh_json:objects()} |
                               {'error', _}.
classify_and_find(Prefix, Quantity, Opts) ->
    case wnm_util:classify_number(Prefix) of
        <<"tollfree_us">> -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        <<"tollfree">> -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        _ -> find(Prefix, Quantity, add_local_options(Prefix, Opts))
    end.

-spec add_tollfree_options(pos_integer(), wh_proplist()) -> wh_proplist().
add_tollfree_options(Quantity, Opts) ->
    TollFreeOpts = [{'cmd', <<"listtollfree">>}
                    ,{'limit', Quantity}
                    ,{'xml', <<"yes">>}
                    ,{'uri_template', ?TOLLFREE_URI_TEMPLATE}
                    | default_options()
                   ],
    lists:foldl(fun add_options_fold/2, Opts, TollFreeOpts).

-spec add_local_options(ne_binary(), wh_proplist()) -> wh_proplist().
add_local_options(Prefix, Opts) when byte_size(Prefix) =< 3 ->
    LocalOpts = [{'npa', Prefix}
                 ,{'cmd', <<"listnpa">>}
                 ,{'withrates', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"withrates">>)}
                 ,{'type', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"type">>)}
                 ,{'provider', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"provider">>)}
                 ,{'xml', <<"yes">>}
                 ,{'cnam', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"require_cnam">>)}
                 ,{'uri_template', ?NPA_URI_TEMPLATE}
                 | default_options()
                ],
    lists:foldl(fun add_options_fold/2, Opts, LocalOpts);
add_local_options(Prefix, Opts) ->
    LocalOpts = [{'npanxx', Prefix}
                 ,{'cmd', <<"listnpanxx">>}
                 ,{'withrates', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"withrates">>)}
                 ,{'type', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"type">>)}
                 ,{'provider', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"provider">>)}
                 ,{'xml', <<"yes">>}
                 ,{'cnam', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"require_cnam">>)}
                 ,{'uri_template', ?NPAXX_URI_TEMPLATE}
                 | default_options()
                ],
    lists:foldl(fun add_options_fold/2, Opts, LocalOpts).

-spec default_options() -> wh_proplist().
default_options() ->
    [{'login', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"login">>, <<>>)}
     ,{'password', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"password">>, <<>>)}
    ].

-spec add_options_fold({atom(), api_binary()}, wh_proplist()) -> wh_proplist().
add_options_fold({_K, 'undefined'}, Opts) -> Opts;
add_options_fold({K, V}, Opts) ->
    props:insert_value(K, V, Opts).

-spec find(ne_binary(), pos_integer(), wh_proplist()) ->
                  {'ok', wh_json:objects()} |
                  {'error', _}.
find(Prefix, Quantity, Opts) ->
    query_vitelity(Prefix, Quantity, build_uri(Opts)).

-spec build_uri(wh_proplist()) -> ne_binary().
build_uri(Opts) ->
    lists:foldl(fun build_uri_fold/2, props:get_value('uri_template', Opts), Opts).

-spec build_uri_fold({atom(), ne_binary()}, ne_binary()) -> ne_binary().
build_uri_fold({Key, Replace}, T) ->
    Search = <<"{", (wh_util:to_binary(Key))/binary, "}">>,
    binary:replace(T, Search, wh_util:to_binary(Replace), ['global']).

-spec query_vitelity(ne_binary(), pos_integer(), ne_binary()) ->
                            {'ok', wh_json:objects()} |
                            {'error', _}.
query_vitelity(Prefix, Quantity, URI) ->
    lager:debug("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', "200", _RespHeaders, RespXML} ->
            lager:debug("recv 200: ~s", [RespXML]),
            process_xml_resp(Prefix, Quantity, RespXML);
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~s: ~s", [_RespCode, RespXML]),
            process_xml_resp(Prefix, Quantity, RespXML);
        {'error', _R}=E ->
            lager:debug("error querying: ~p", [_R]),
            E
    end.

-spec process_xml_resp(ne_binary(), pos_integer(), text()) ->
                              {'ok', wh_json:object()} |
                              {'error', _}.
process_xml_resp(Prefix, Quantity, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Prefix, Quantity, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

-spec process_xml_content_tag(ne_binary(), pos_integer(), xml_el()) ->
                                     {'ok', wh_json:object()} |
                                     {'error', _}.
process_xml_content_tag(Prefix, Quantity, #xmlElement{name='content'
                                                      ,content=Children
                                                     }) ->
    Els = kz_xml:elements(Children),
    case xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', xml_resp_error_msg(Els)};
        Status when Status =:= <<"ok">>;
                    Status =:= 'undefined' ->
            process_xml_numbers(Prefix, Quantity, xml_resp_numbers(Els))
    end.

-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_el()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', _}.
-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_el(), wh_proplist()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', _}.
process_xml_numbers(_Prefix, _Quantity, 'undefined') ->
    {'error', 'no_numbers'};
process_xml_numbers(Prefix, Quantity, #xmlElement{name='numbers'
                                                  ,content=Content
                                                 }) ->
    process_xml_numbers(Prefix, Quantity, kz_xml:elements(Content), []).

process_xml_numbers(_Prefix, 0, _Els, Acc) ->
    lager:debug("reached quantity requested"),
    {'ok', wh_json:from_list(Acc)};
process_xml_numbers(_Prefix, _Quantity, [#xmlElement{name='response'
                                                     ,content=Reason
                                                     }
                                         |_], _Acc) ->
    lager:debug("response tag found, error!"),
    {'error', kz_xml:texts_to_binary(Reason)};
process_xml_numbers(_Prefix, _Quantity, [], Acc) ->
    lager:debug("no more results: ~p", [_Quantity]),
    {'ok', wh_json:from_list(Acc)};
process_xml_numbers(Prefix, Quantity, [El|Els], Acc) ->
    JObj = xml_did_to_json(El),
    case number_matches_prefix(JObj, Prefix) of
        'true' -> process_xml_numbers(Prefix, Quantity-1, Els, [{wh_json:get_value(<<"number">>, JObj), JObj}|Acc]);
        'false' -> process_xml_numbers(Prefix, Quantity, Els, Acc)
    end.

-spec number_matches_prefix(wh_json:object(), ne_binary()) -> boolean().
number_matches_prefix(JObj, Prefix) ->
    PrefixLen = byte_size(Prefix),
    case wh_json:get_value(<<"number">>, JObj) of
        <<Prefix:PrefixLen/binary, _/binary>> -> 'true';
        _N -> 'false'
    end.

-spec xml_did_to_json(xml_el()) -> wh_json:object().
xml_did_to_json(#xmlElement{name='did'
                            ,content=[#xmlText{}]=DID
                           }) ->
    wh_json:from_list([{<<"number">>, kz_xml:texts_to_binary(DID)}]);
xml_did_to_json(#xmlElement{name='did'
                            ,content=DIDInfo
                           }) ->
    wh_json:from_list(xml_els_to_proplist(kz_xml:elements(DIDInfo)));
xml_did_to_json(#xmlElement{name='number'
                           ,content=DIDInfo
                           }) ->
    wh_json:from_list(xml_els_to_proplist(kz_xml:elements(DIDInfo))).

-spec xml_els_to_proplist(xml_els()) -> wh_proplist().
xml_els_to_proplist(Els) ->
    [KV || El <- Els,
           begin
               {_, V}=KV = xml_el_to_kv_pair(El),
               V =/= 'undefined'
           end
    ].

-spec xml_el_to_kv_pair(xml_el()) -> {ne_binary(), ne_binary()}.
xml_el_to_kv_pair(#xmlElement{name='did'
                              ,content=Value
                             }) ->
    %% due to inconsistency in listdids
    {<<"number">>
     ,kz_xml:texts_to_binary(Value)
    };
xml_el_to_kv_pair(#xmlElement{name=Name
                              ,content=[]
                             }) ->
    {Name, 'undefined'};
xml_el_to_kv_pair(#xmlElement{name=Name
                              ,content=Value
                             }) ->
    case kz_xml:elements(Value) of
        [] ->
            {wh_util:to_binary(Name)
             ,kz_xml:texts_to_binary(Value)
            };
        Els ->
            {wh_util:to_binary(Name)
             ,wh_json:from_list(xml_els_to_proplist(Els))
            }
    end.

-spec xml_resp_status_msg(xml_els()) -> api_binary().
xml_resp_status_msg(XmlEls) ->
    xml_el_to_binary(xml_resp_tag(XmlEls, 'status')).

-spec xml_resp_error_msg(xml_els()) -> api_binary().
xml_resp_error_msg(XmlEls) ->
    xml_el_to_binary(xml_resp_tag(XmlEls, 'error')).

-spec xml_resp_numbers(xml_els()) -> xml_el() | 'undefined'.
xml_resp_numbers(XmlEls) ->
    xml_resp_tag(XmlEls, 'numbers').

-spec xml_resp_tag(xml_els(), atom()) -> xml_el() | 'undefined'.
xml_resp_tag([#xmlElement{name=Name}=El|_], Name) -> El;
xml_resp_tag([_|Els], Name) ->
    xml_resp_tag(Els, Name);
xml_resp_tag([], _Name) ->
    'undefined'.

-spec xml_el_to_binary('undefined' | xml_el()) -> api_binary().
xml_el_to_binary('undefined') -> 'undefined';
xml_el_to_binary(#xmlElement{content=Content}) ->
    kz_xml:texts_to_binary(Content).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{auth_by=AuthBy
                       ,assigned_to=AssignedTo
                       ,module_data=Data
                      }=N) ->
    N.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.
