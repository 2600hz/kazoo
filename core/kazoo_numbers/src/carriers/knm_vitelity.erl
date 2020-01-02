%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_vitelity).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([should_lookup_cnam/0]).
-export([is_number_billable/1]).
-export([check_numbers/1]).

-include("knm.hrl").
-include("knm_vitelity.hrl").


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the Vitelity system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
          knm_search:mod_response().
find_numbers(<<"+1",Prefix/binary>>, Quantity, Options) ->
    find_numbers(Prefix, Quantity, Options);
find_numbers(Prefix, Quantity, Options) ->
    case props:is_true('tollfree', Options, 'false') of
        'false' -> classify_and_find(Prefix, Quantity, Options);
        'true' ->
            TFOpts = tollfree_options(Quantity, Options),
            find(Prefix, Quantity, Options, TFOpts)
    end.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) ->
          knm_phone_number:record().
acquire_number(PN) ->
    DID = knm_phone_number:number(PN),
    case knm_converters:classify(DID) of
        <<"tollfree_us">> ->
            query_vitelity(PN, purchase_tollfree_options(DID));
        <<"tollfree">> ->
            query_vitelity(PN, purchase_tollfree_options(DID));
        _ ->
            query_vitelity(PN, purchase_local_options(DID))
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) ->
          knm_phone_number:record().
disconnect_number(PN) ->
    DID = knm_phone_number:number(PN),
    lager:debug("attempting to disconnect ~s", [DID]),
    query_vitelity(PN, release_did_options(DID)).

%%------------------------------------------------------------------------------
%% @doc Check to see if use_stepswitch_cnam is defined in the couchdoc. If it is
%% set to true, then incoming calls will use stepswitch for cnam
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() ->
    kapps_config:get_is_true(?KNM_VITELITY_CONFIG_CAT, <<"use_stepswitch_cnam">>, false).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_) -> 'true'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec classify_and_find(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
          knm_search:mod_response().
classify_and_find(Prefix, Quantity, Options) ->
    case knm_converters:classify(Prefix) of
        <<"tollfree_us">> ->
            find(Prefix, Quantity, Options, tollfree_options(Quantity, Options));
        <<"tollfree">> ->
            find(Prefix, Quantity, Options, tollfree_options(Quantity, Options));
        _Classification ->
            find(Prefix, Quantity, Options, local_options(Prefix, Options))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec tollfree_options(pos_integer(), knm_carriers:options()) ->
          knm_vitelity_util:query_options().
tollfree_options(Quantity, Options) ->
    TollFreeOptions = [{'qs', [{'cmd', <<"listtollfree">>}
                              ,{'limit', Quantity}
                              ,{'xml', <<"yes">>}
                               | knm_vitelity_util:default_options(Options)
                              ]}
                      ,{'uri', knm_vitelity_util:api_uri()}
                      ],
    F = fun knm_vitelity_util:add_options_fold/2,
    lists:foldl(F, [], TollFreeOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec local_options(kz_term:ne_binary(), knm_carriers:options()) ->
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
    F = fun knm_vitelity_util:add_options_fold/2,
    lists:foldl(F, [], LocalOptions);

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
    F = fun knm_vitelity_util:add_options_fold/2,
    lists:foldl(F, [], LocalOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find(kz_term:ne_binary(), pos_integer(), knm_carriers:options(), knm_vitelity_util:query_options()) ->
          knm_search:mod_response().
find(Prefix, Quantity, Options, VitelityOptions) ->
    case query_vitelity(Prefix, Quantity, VitelityOptions) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            QID = knm_search:query_id(Options),
            Results = [{QID, {DID, ?MODULE, ?NUMBER_STATE_DISCOVERY, CarrierData}}
                       || {DID, CarrierData} <- kz_json:to_proplist(JObj)
                      ],
            {'ok', Results}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec query_vitelity(kz_term:ne_binary(), pos_integer(), knm_vitelity_util:query_options()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:api_binary()}.
-ifdef(TEST).
query_vitelity(Prefix, Quantity, QOptions) ->
    URI = knm_vitelity_util:build_uri(QOptions),
    {'ok'
    ,{'http', [], _Host, _Port, _Path, [$? | QueryString]}
    } = http_uri:parse(kz_term:to_list(URI)),
    Options = cow_qs:parse_qs(kz_term:to_binary(QueryString)),
    XML =
        case props:get_value(<<"cmd">>, Options) of
            ?PREFIX_SEARCH_CMD -> ?PREFIX_SEARCH_RESP;
            ?NUMBER_SEARCH_CMD -> ?NUMBER_SEARCH_RESP;
            ?TOLLFREE_SEARCH_CMD -> ?TOLLFREE_SEARCH_RESP
        end,
    process_xml_resp(Prefix, Quantity, XML).

-else.
query_vitelity(Prefix, Quantity, QOptions) ->
    URI = knm_vitelity_util:build_uri(QOptions),
    lager:debug("querying ~s", [URI]),
    case kz_http:post(kz_term:to_list(URI)) of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~p: ~s", [_RespCode, RespXML]),
            process_xml_resp(Prefix, Quantity, RespXML);
        {'error', _R} ->
            lager:debug("error querying: ~p", [_R]),
            {'error', 'not_available'}
    end.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_resp(kz_term:ne_binary(), pos_integer(), kz_term:text()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
process_xml_resp(Prefix, Quantity, XML_binary) ->
    XML = unicode:characters_to_list(XML_binary),
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Prefix, Quantity, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_content_tag(kz_term:ne_binary(), pos_integer(), kz_types:xml_el()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:api_binary()}.
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec process_xml_numbers(kz_term:ne_binary(), pos_integer(), 'undefined' | kz_types:xml_el()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:api_binary()}.
process_xml_numbers(_Prefix, _Quantity, 'undefined') ->
    {'error', 'no_numbers'};
process_xml_numbers(Prefix, Quantity, #xmlElement{name='numbers'
                                                 ,content=Content
                                                 }) ->
    process_xml_numbers(Prefix, Quantity, kz_xml:elements(Content), []).

-spec process_xml_numbers(kz_term:ne_binary(), pos_integer(), 'undefined' | kz_types:xml_els(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:api_binary()}.
process_xml_numbers(_Prefix, 0, _Els, Acc) ->
    {'ok', kz_json:from_list(Acc)};
process_xml_numbers(_Prefix, _Quantity, [#xmlElement{name='response'
                                                    ,content=Reason
                                                    }
                                         |_], _Acc) ->
    {'error', kz_xml:texts_to_binary(Reason)};
process_xml_numbers(_Prefix, _Quantity, [], Acc) ->
    {'ok', kz_json:from_list(Acc)};
process_xml_numbers(Prefix, Quantity, [El|Els], Acc) ->
    JObj = xml_did_to_json(El),

    case number_matches_prefix(JObj, Prefix) of
        'true' ->
            N = kz_json:get_value(<<"number">>, JObj),
            process_xml_numbers(Prefix, Quantity-1, Els, [{N, JObj}|Acc]);
        'false' ->
            process_xml_numbers(Prefix, Quantity, Els, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec number_matches_prefix(kz_json:object(), kz_term:ne_binary()) -> boolean().
number_matches_prefix(JObj, Prefix) ->
    PrefixLen = byte_size(Prefix),
    CountryCode = kz_json:get_value(<<"country_code">>, JObj, <<"+1">>),
    CountryCodeLen = byte_size(CountryCode),
    case kz_json:get_value(<<"number">>, JObj) of
        <<Prefix:PrefixLen/binary, _/binary>> -> 'true';
        <<CountryCode:CountryCodeLen/binary, Prefix:PrefixLen/binary, _/binary>> -> 'true';
        _N -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec xml_did_to_json(kz_types:xml_el()) -> kz_json:object().
xml_did_to_json(#xmlElement{name='did'
                           ,content=[#xmlText{}]=DID
                           }) ->
    kz_json:from_list([{<<"number">>
                       ,knm_converters:normalize(
                          kz_xml:texts_to_binary(DID)
                         )
                       }
                      ]);
xml_did_to_json(#xmlElement{name='did'
                           ,content=DIDInfo
                           }) ->
    kz_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(DIDInfo)
       ));
xml_did_to_json(#xmlElement{name='number'
                           ,content=DIDInfo
                           }) ->
    kz_json:from_list(
      knm_vitelity_util:xml_els_to_proplist(
        kz_xml:elements(DIDInfo)
       )).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec purchase_local_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
purchase_local_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'cmd', <<"getlocaldid">>}
            ,{'xml', <<"yes">>}
            ,{'routesip', knm_vitelity_util:get_routesip()}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec purchase_tollfree_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
purchase_tollfree_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'cmd', <<"gettollfree">>}
            ,{'xml', <<"yes">>}
            ,{'routesip', knm_vitelity_util:get_routesip()}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec query_vitelity(knm_phone_number:record(), knm_vitelity_util:query_options()) ->
          knm_phone_number:record().
query_vitelity(PN, QOptions) ->
    URI = knm_vitelity_util:build_uri(QOptions),
    lager:debug("querying ~s", [URI]),
    case kz_http:post(kz_term:to_list(URI)) of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~p: ~s", [_RespCode, RespXML]),
            process_xml_resp(PN, RespXML);
        {'error', Error} ->
            lager:debug("error querying: ~p", [Error]),
            knm_errors:by_carrier(?MODULE, Error, PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_resp(knm_phone_number:record(), kz_term:text()) ->
          knm_phone_number:record().
process_xml_resp(PN, XML_binary) ->
    XML = unicode:characters_to_list(XML_binary),
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(PN, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            knm_errors:by_carrier(?MODULE, 'failed_decode_resp', PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_content_tag(knm_phone_number:record(), kz_types:xml_el()) ->
          knm_phone_number:record().
process_xml_content_tag(PN, #xmlElement{name='content'
                                       ,content=Children
                                       }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Els),
            lager:debug("xml status is 'fail': ~s", [Msg]),
            knm_errors:by_carrier(?MODULE, Msg, PN);
        <<"ok">> ->
            lager:debug("successful provisioning"),
            PN
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release_did_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
release_did_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'cmd', <<"removedid">>}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].
