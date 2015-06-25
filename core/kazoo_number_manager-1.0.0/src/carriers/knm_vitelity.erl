%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_vitelity).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
         ,should_lookup_cnam/0
         ,is_number_billable/1
        ]).

-include("../knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Vitelity system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:objects()} |
                          {'error', _}.
find_numbers(Prefix, Quantity, Opts) when not is_integer(Quantity) ->
    find_numbers(Prefix, wh_util:to_integer(Quantity), Opts);
find_numbers(Prefix, Quantity, Opts) ->
    case props:get_value('tollfree', Opts, 'false') of
        'true' -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        'false' ->
            classify_and_find(Prefix, Quantity, Opts)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(number()) -> number_return().
-spec acquire_number(number(), boolean()) -> number_return().

acquire_number(Number) ->
    acquire_number(Number, knm_phone_number:dry_run(Number)).

acquire_number(Number, 'true') -> {'ok', Number};
acquire_number(Number, 'false') ->
    DID = knm_phone_number:number(Number),
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
-spec disconnect_number(number()) -> number_return().
disconnect_number(Number) ->
    DID = knm_phone_number:number(Number),
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
-spec classify_and_find(ne_binary(), pos_integer(), wh_proplist()) -> {'ok', wh_json:objects()} | {'error', _}.
classify_and_find(Prefix, Quantity, Opts) ->
    case knm_converters:classify(Prefix) of
        <<"tollfree_us">> -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        <<"tollfree">> -> find(Prefix, Quantity, add_tollfree_options(Quantity, Opts));
        _ -> find(Prefix, Quantity, add_local_options(Prefix, Opts))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_tollfree_options(pos_integer(), wh_proplist()) -> wh_proplist().
add_tollfree_options(Quantity, Opts) ->
    TollFreeOpts = [{'qs', [{'cmd', <<"listtollfree">>}
                            ,{'limit', Quantity}
                            ,{'xml', <<"yes">>}
                            | knm_vitelity_util:default_options(Opts)
                           ]}
                     ,{'uri', knm_vitelity_util:api_uri()}
                    ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2, Opts, TollFreeOpts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_local_options(ne_binary(), wh_proplist()) -> wh_proplist().
add_local_options(Prefix, Opts) when byte_size(Prefix) =< 3 ->
    LocalOpts = [{'qs', [{'npa', Prefix}
                         ,{'cmd', <<"listnpa">>}
                         ,{'withrates', knm_vitelity_util:get_query_value('withrates', Opts)}
                         ,{'type', knm_vitelity_util:get_query_value('type', Opts)}
                         ,{'provider', knm_vitelity_util:get_query_value('provider', Opts)}
                         ,{'xml', <<"yes">>}
                         ,{'cnam', knm_vitelity_util:get_query_value('cnam', Opts)}
                         | knm_vitelity_util:default_options(Opts)
                        ]}
                 ,{'uri', knm_vitelity_util:api_uri()}
                ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2, Opts, LocalOpts);
add_local_options(Prefix, Opts) ->
    LocalOpts = [{'qs', [{'npanxx', Prefix}
                         ,{'cmd', <<"listnpanxx">>}
                         ,{'withrates', knm_vitelity_util:get_query_value('withrates', Opts)}
                         ,{'type', knm_vitelity_util:get_query_value('type', Opts)}
                         ,{'provider', knm_vitelity_util:get_query_value('provider', Opts)}
                         ,{'xml', <<"yes">>}
                         ,{'cnam', knm_vitelity_util:get_query_value('cnam', Opts)}
                         | knm_vitelity_util:default_options(Opts)
                        ]}
                 ,{'uri', knm_vitelity_util:api_uri()}
                ],
    lists:foldl(fun knm_vitelity_util:add_options_fold/2, Opts, LocalOpts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary(), pos_integer(), wh_proplist()) -> {'ok', wh_json:objects()} | {'error', _}.
find(Prefix, Quantity, Opts) ->
    query_vitelity(Prefix, Quantity, knm_vitelity_util:build_uri(Opts)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec query_vitelity(ne_binary(), pos_integer(), ne_binary()) -> {'ok', wh_json:object()} | {'error', _}.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(ne_binary(), pos_integer(), text()) -> {'ok', wh_json:object()} | {'error', _}.
process_xml_resp(Prefix, Quantity, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Prefix, Quantity, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(ne_binary(), pos_integer(), xml_el()) -> {'ok', wh_json:object()} | {'error', _}.
process_xml_content_tag(Prefix, Quantity, #xmlElement{name='content'
                                                      ,content=Children
                                                     }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', knm_vitelity_util:xml_resp_error_msg(Els)};
        Status when Status =:= <<"ok">>;
                    Status =:= 'undefined' ->
            process_xml_numbers(Prefix, Quantity, knm_vitelity_util:xml_resp_numbers(Els))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_el()) -> {'ok', wh_json:object()} | {'error', _}.
-spec process_xml_numbers(ne_binary(), pos_integer(), 'undefined' | xml_els(), wh_proplist()) ->  {'ok', wh_json:object()} | {'error', _}.
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
        'true' ->
            N = wh_json:get_value(<<"number">>, JObj),
            lager:debug("adding number ~s to accumulator", [N]),
            process_xml_numbers(Prefix, Quantity-1, Els, [{N, JObj}|Acc]);
        'false' -> process_xml_numbers(Prefix, Quantity, Els, Acc)
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
    wh_json:from_list([{<<"number">>, kz_xml:texts_to_binary(DID)}]);
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
purchase_local_options(DID) ->
    [{'qs', [{'did', knm_converter_regex:to_npan(DID)}
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
purchase_tollfree_options(DID) ->
    [{'qs', [{'did', knm_converter_regex:to_npan(DID)}
             ,{'cmd', <<"gettollfree">>}
             ,{'xml', <<"yes">>}
             ,{'routesip', get_routesip()}
             | knm_vitelity_util:default_options([])
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_routesip() -> api_binary().
get_routesip() ->
    case whapps_config:get(knm_vitelity_util:config_cat(), <<"routesip">>) of
        [Route|_] -> Route;
        Route -> Route
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec query_vitelity(number(), ne_binary()) -> number_return().
query_vitelity(Number, URI) ->
    lager:debug("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~s: ~s", [_RespCode, RespXML]),
            process_xml_resp(Number, RespXML);
        {'error', _R} ->
            lager:debug("error querying: ~p", [_R]),
            {'error', 'failed_query_carrier'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(number(), text()) -> number_return().
process_xml_resp(Number, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Number, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'failed_decode_resp'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(number(), xml_el()) -> number_return().
process_xml_content_tag(Number, #xmlElement{name='content'
                                            ,content=Children
                                           }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Els),
            lager:debug("xml status is 'fail': ~s", [Msg]),
            {'error', Msg};
        <<"ok">> ->
            lager:debug("successful provisioning"),
            {'ok', Number}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
release_did_options(DID) ->
    [{'qs', [{'did', knm_converter_regex:to_npan(DID)}
             ,{'cmd', <<"removedid">>}
             ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options([])
            ]}
     ,{'uri', knm_vitelity_util:api_uri()}
    ].