%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_vitelity).

-behaviour(knm_gen_carrier).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
         ,should_lookup_cnam/0
         ,is_number_billable/1
        ]).

-include("../knm.hrl").
-include("../knm_vitelity.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Vitelity system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', _}.
find_numbers(Prefix, Quantity, Options) ->
    case props:is_true(<<"tollfree">>, Options, 'false') of
        'true' ->
            find(Prefix
                 ,Quantity
                 ,Options
                 ,tollfree_options(Quantity, Options)
                );
        'false' ->
            classify_and_find(Prefix, Quantity, Options)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PhoneNumber),
    case knm_converters:classify(DID) of
        <<"tollfree_us">> ->
            query_vitelity(
              Number
              ,knm_vitelity_util:build_uri(
                 purchase_tollfree_options(DID)
                )
             );
        <<"tollfree">> ->
            query_vitelity(
              Number
              ,knm_vitelity_util:build_uri(
                 purchase_tollfree_options(DID)
                )
             );
        _ ->
            query_vitelity(
              Number
              ,knm_vitelity_util:build_uri(
                 purchase_local_options(DID)
                )
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PhoneNumber),
    lager:debug("attempting to disconnect ~s", [DID]),
    query_vitelity(
      Number
      ,knm_vitelity_util:build_uri(
         release_did_options(DID)
        )
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'false'.
should_lookup_cnam() -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(_) -> 'true'.
is_number_billable(_) -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec classify_and_find(ne_binary(), pos_integer(), knm_vitelity_util:query_options()) ->
                               {'ok', knm_number:knm_numbers()} |
                               {'error', _}.
classify_and_find(Prefix, Quantity, Options) ->
    case knm_converters:classify(Prefix) of
        <<"tollfree_us">> ->
            find(Prefix, Quantity, Options, tollfree_options(Quantity, Options));
        <<"tollfree">> ->
            find(Prefix, Quantity, Options, tollfree_options(Quantity, Options));
        _Classification ->
            find(Prefix, Quantity, Options, local_options(Prefix, Options))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec tollfree_options(pos_integer(), knm_vitelity_util:query_options()) ->
                              knm_vitelity_util:query_options().
tollfree_options(Quantity, Options) ->
    TollFreeOptions = [{'qs', [{'cmd', <<"listtollfree">>}
                               ,{'limit', Quantity}
                               ,{'xml', <<"yes">>}
                               | knm_vitelity_util:default_options(Options)
                              ]}
                       ,{'uri', knm_vitelity_util:api_uri()}
                      ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2
                ,[]
                ,TollFreeOptions
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_options(ne_binary(), knm_vitelity_util:query_options()) ->
                           knm_vitelity_util:query_options().
local_options(Prefix, Options) when byte_size(Prefix) =< 3 ->
    LocalOptions = [{'qs', [{'npa', Prefix}
                            ,{'cmd', <<"listnpa">>}
                            ,{'withrates', knm_vitelity_util:get_query_value(<<"withrates">>, Options)}
                            ,{'type', knm_vitelity_util:get_query_value(<<"type">>, Options)}
                            ,{'provider', knm_vitelity_util:get_query_value(<<"provider">>, Options)}
                            ,{'xml', <<"yes">>}
                            ,{'cnam', knm_vitelity_util:get_query_value(<<"cnam">>, Options)}
                            | knm_vitelity_util:default_options(Options)
                           ]}
                    ,{'uri', knm_vitelity_util:api_uri()}
                   ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2
                ,[]
                ,LocalOptions
               );
local_options(Prefix, Options) ->
    LocalOptions = [{'qs', [{'npanxx', Prefix}
                            ,{'cmd', <<"listnpanxx">>}
                            ,{'withrates', knm_vitelity_util:get_query_value(<<"withrates">>, Options)}
                            ,{'type', knm_vitelity_util:get_query_value(<<"type">>, Options)}
                            ,{'provider', knm_vitelity_util:get_query_value(<<"provider">>, Options)}
                            ,{'xml', <<"yes">>}
                            ,{'cnam', knm_vitelity_util:get_query_value(<<"cnam">>, Options)}
                            | knm_vitelity_util:default_options(Options)
                           ]}
                    ,{'uri', knm_vitelity_util:api_uri()}
                   ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2
                ,[]
                ,LocalOptions
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary(), pos_integer(), wh_proplist(), knm_vitelity_util:query_options()) ->
                  {'ok', knm_number:knm_numbers()} |
                  {'error', _}.
find(Prefix, Quantity, Options, VitelityOptions) ->
    case query_vitelity(Prefix
                        ,Quantity
                        ,knm_vitelity_util:build_uri(VitelityOptions)
                       )
    of
        {'error', _}=Error -> Error;
        {'ok', JObj} -> response_to_numbers(JObj, Options)
    end.

-spec response_to_numbers(wh_json:object(), wh_proplist()) ->
                                 {'ok', knm_number:knm_numbers()}.
response_to_numbers(JObj, Options) ->
    AccountId = props:get_value(<<"account_id">>, Options),
    {'ok'
     ,wh_json:foldl(fun(K, V, Acc) -> response_pair_to_number(K, V, Acc, AccountId) end
                    ,[]
                    ,JObj
                   )
    }.

-spec response_pair_to_number(ne_binary(), wh_json:object(), knm_number:knm_numbers(), api_binary()) ->
                                     knm_number:knm_numbers().
response_pair_to_number(DID, CarrierData, Acc, AccountId) ->
    NormalizedNum = knm_converters:normalize(DID),
    NumberDb = knm_converters:to_db(NormalizedNum),

    Updates = [{fun knm_phone_number:set_number/2, NormalizedNum}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
               ,{fun knm_phone_number:set_carrier_data/2, CarrierData}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
               ,{fun knm_phone_number:set_assign_to/2, AccountId}
              ],
    PhoneNumber = knm_phone_number:setters(knm_phone_number:new(), Updates),
    [knm_number:set_phone_number(
      knm_number:new()
      ,PhoneNumber
     )
     | Acc
    ].



%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-ifdef(TEST).
query_vitelity(Prefix, Quantity, URI) ->
    {'ok'
     ,{'http', [], _Host, _Port, _Path, [$? | QueryString]}
    } = http_uri:parse(wh_util:to_list(URI)),
    Options = cow_qs:parse_qs(wh_util:to_binary(QueryString)),

    XML =
        case props:get_value(<<"cmd">>, Options) of
            ?PREFIX_SEARCH_CMD -> ?PREFIX_SEARCH_RESP;
            ?NUMBER_SEARCH_CMD -> ?NUMBER_SEARCH_RESP;
            ?TOLLFREE_SEARCH_CMD -> ?TOLLFREE_SEARCH_RESP
        end,
    ?LOG_DEBUG("for ~s, use ~p", [URI, XML]),
    process_xml_resp(Prefix, Quantity, XML).
-else.
-spec query_vitelity(ne_binary(), pos_integer(), ne_binary()) ->
                            {'ok', wh_json:object()} |
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
        {'error', _R} ->
            lager:debug("error querying: ~p", [_R]),
            {'error', 'not_available'}
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(ne_binary(), pos_integer(), text()) ->
                              {'ok', wh_json:object()} |
                              {'error', _}.
process_xml_resp(Prefix, Quantity, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Prefix, Quantity, XmlEl)
    catch
        _E:_R ->
            ?LOG_DEBUG("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(ne_binary(), pos_integer(), xml_el()) ->
                                     {'ok', wh_json:object()} |
                                     {'error', _}.
process_xml_content_tag(Prefix, Quantity, #xmlElement{name='content'
                                                      ,content=Children
                                                     }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', knm_vitelity_util:xml_resp_error_msg(Els)};
        Status when Status =:= <<"ok">>;
                    Status =:= 'undefined' ->
            ?LOG_DEBUG("response status: ~p", [Status]),
            process_xml_numbers(Prefix, Quantity, knm_vitelity_util:xml_resp_numbers(Els))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_el()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', _}.
-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_els(), wh_proplist()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', _}.
process_xml_numbers(_Prefix, _Quantity, 'undefined') ->
    ?LOG_DEBUG("no numbers in xml response", []),
    {'error', 'no_numbers'};
process_xml_numbers(Prefix, Quantity, #xmlElement{name='numbers'
                                                  ,content=Content
                                                 }) ->
    process_xml_numbers(Prefix, Quantity, kz_xml:elements(Content), []).

process_xml_numbers(_Prefix, 0, _Els, Acc) ->
    ?LOG_DEBUG("reached quantity requested"),
    {'ok', wh_json:from_list(Acc)};
process_xml_numbers(_Prefix, _Quantity, [#xmlElement{name='response'
                                                     ,content=Reason
                                                    }
                                         |_], _Acc) ->
    ?LOG_DEBUG("response tag found, error!"),
    {'error', kz_xml:texts_to_binary(Reason)};
process_xml_numbers(_Prefix, _Quantity, [], Acc) ->
    ?LOG_DEBUG("no more results: ~p", [_Quantity]),
    {'ok', wh_json:from_list(Acc)};
process_xml_numbers(Prefix, Quantity, [El|Els], Acc) ->
    JObj = xml_did_to_json(El),
    ?LOG_DEBUG("el: ~p jobj: ~p", [El, JObj]),
    case number_matches_prefix(JObj, Prefix) of
        'true' ->
            N = wh_json:get_value(<<"number">>, JObj),
            ?LOG_DEBUG("adding number ~s to accumulator", [N]),
            process_xml_numbers(Prefix, Quantity-1, Els, [{N, JObj}|Acc]);
        'false' ->
            ?LOG_DEBUG("failed to match number ~p to prefix ~p", [JObj, Prefix]),
            process_xml_numbers(Prefix, Quantity, Els, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec number_matches_prefix(wh_json:object(), ne_binary()) -> boolean().
number_matches_prefix(JObj, Prefix) ->
    PrefixLen = byte_size(Prefix),
    CountryCode = wh_json:get_value(<<"country_code">>, JObj, <<"+1">>),
    CountryCodeLen = byte_size(CountryCode),
    case wh_json:get_value(<<"number">>, JObj) of
        <<Prefix:PrefixLen/binary, _/binary>> -> 'true';
        <<CountryCode:CountryCodeLen/binary, Prefix:PrefixLen/binary, _/binary>> -> 'true';
        _N -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec xml_did_to_json(xml_el()) -> wh_json:object().
xml_did_to_json(#xmlElement{name='did'
                            ,content=[#xmlText{}]=DID
                           }) ->
    wh_json:from_list([{<<"number">>
                        ,knm_converters:normalize(
                           kz_xml:texts_to_binary(DID)
                          )
                       }
                      ]);
xml_did_to_json(#xmlElement{name='did'
                            ,content=DIDInfo
                           }) ->
    wh_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(DIDInfo)
       ));
xml_did_to_json(#xmlElement{name='number'
                           ,content=DIDInfo
                           }) ->
    wh_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(DIDInfo)
       )).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec purchase_local_options(ne_binary()) -> knm_vitelity_util:query_options().
purchase_local_options(DID) ->
    [{'qs', [{'did', (knm_converters:default_converter()):to_npan(DID)}
             ,{'cmd', <<"getlocaldid">>}
             ,{'xml', <<"yes">>}
             ,{'routesip', get_routesip()}
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
-spec purchase_tollfree_options(ne_binary()) -> knm_vitelity_util:query_options().
purchase_tollfree_options(DID) ->
    [{'qs', [{'did', (knm_converters:default_converter()):to_npan(DID)}
             ,{'cmd', <<"gettollfree">>}
             ,{'xml', <<"yes">>}
             ,{'routesip', get_routesip()}
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
-ifdef(TEST).
get_routesip() ->
    <<"1.2.3.4">>.
-else.
-spec get_routesip() -> api_binary().
get_routesip() ->
    case whapps_config:get(knm_vitelity_util:config_cat(), <<"routesip">>) of
        [Route|_] -> Route;
        Route -> Route
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec query_vitelity(knm_number:knm_number(), ne_binary()) ->
                            knm_number:knm_number().
query_vitelity(Number, URI) ->
    ?LOG_DEBUG("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            ?LOG_DEBUG("recv ~s: ~s", [_RespCode, RespXML]),
            process_xml_resp(Number, RespXML);
        {'error', Error} ->
            ?LOG_DEBUG("error querying: ~p", [Error]),
            knm_errors:unspecified(Error, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(knm_number:knm_number(), text()) ->
                              knm_number:knm_number().
process_xml_resp(Number, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Number, XmlEl)
    catch
        _E:_R ->
            ?LOG_DEBUG("failed to decode xml: ~s: ~p", [_E, _R]),
            knm_errors:unspecified('failed_decode_resp', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(knm_number:knm_number(), xml_el()) ->
                                     knm_number:knm_number().
process_xml_content_tag(Number, #xmlElement{name='content'
                                            ,content=Children
                                           }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Els),
            ?LOG_DEBUG("xml status is 'fail': ~s", [Msg]),
            knm_errors:unspecified(Msg, Number);
        <<"ok">> ->
            ?LOG_DEBUG("successful provisioning"),
            Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec release_did_options(ne_binary()) -> knm_vitelity_util:query_options().
release_did_options(DID) ->
    [{'qs', [{'did', (knm_converters:default_converter()):to_npan(DID)}
             ,{'cmd', <<"removedid">>}
             ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].
