%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents
%%% @author Karl Anderson
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_other).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([is_number_billable/1]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

-define(KNM_OTHER_CONFIG_CAT, <<?KNM_CONFIG_CAT/binary, ".other">>).

-define(COUNTRY, kapps_config:get_ne_binary(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?KNM_DEFAULT_COUNTRY)).

-define(PHONEBOOK_URL, kapps_config:get_ne_binary(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>)).

-ifdef(TEST).
-define(PHONEBOOK_URL(Options), props:get_value('phonebook_url', Options)).
-else.
-define(PHONEBOOK_URL(_Options), ?PHONEBOOK_URL).
-endif.

-ifdef(TEST).
-define(BLOCKS_RESP, kz_json:from_list(
                       [{<<"status">>, <<"success">>}
                       ,{<<"data">>
                        ,[kz_json:from_list(
                            [{<<"start_number">>, ?START_BLOCK}
                            ,{<<"end_number">>, ?END_BLOCK}
                            ])
                         ]
                        }
                       ])
       ).

-define(NUMBERS_DATA, kz_json:from_list(
                        [{<<"+1415886790", (D + $0)>>, Ext}
                         || D <- lists:seq(0, 9),
                            Ext <- [kz_json:from_list([{<<"extension">>, D}])]
                        ])
       ).

-define(NUMBERS_RESPONSE
       ,kz_json:from_list([{<<"status">>, <<"success">>}
                          ,{<<"data">>, ?NUMBERS_DATA}
                          ])
       ).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 10
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
%% @doc Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
          knm_search:mod_response().
find_numbers(Prefix, Quantity, Options) ->
    case ?PHONEBOOK_URL(Options) of
        'undefined' -> {'error', 'not_available'};
        Url ->
            case props:is_defined('blocks', Options) of
                'false' -> get_numbers(Url, Prefix, Quantity, Options);
                'true' -> get_blocks(Url, Prefix, Quantity, Options)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(Numbers) ->
    FormatedNumbers = [knm_converters:to_npan(Number) || Number <- Numbers],
    case ?PHONEBOOK_URL of
        'undefined' -> {'error', 'not_available'};
        Url ->
            ReqBody = kz_json:set_value(<<"data">>, FormatedNumbers, kz_json:new()),
            Uri = <<Url/binary,  "/numbers/", (?COUNTRY)/binary, "/status">>,
            lager:debug("making request to ~s with body ~p", [Uri, ReqBody]),
            case kz_http:post(binary:bin_to_list(Uri), [], kz_json:encode(ReqBody)) of
                {'ok', 200, _Headers, Body} ->
                    format_check_numbers(kz_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("numbers check failed: ~p", [Body]),
                    {'error', Body};
                E ->
                    lager:error("numbers check failed: error ~p", [E]),
                    E
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_PN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PN) ->
    Num = knm_phone_number:number(PN),
    case ?PHONEBOOK_URL of
        'undefined' ->
            knm_errors:unspecified('missing_provider_url', Num);
        Url ->
            Hosts = case kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"endpoints">>) of
                        'undefined' -> [];
                        Endpoint when is_binary(Endpoint) ->
                            [Endpoint];
                        Endpoints -> Endpoints
                    end,

            ReqBody = kz_json:from_list_recursive(
                        [{<<"data">>
                         ,[{<<"numbers">>, [Num]}
                          ,{<<"gateways">>, Hosts}
                          ]
                         }
                        ]),

            Uri = <<Url/binary,  "/numbers/", (?COUNTRY)/binary, "/order">>,
            case kz_http:put(binary:bin_to_list(Uri), [], kz_json:encode(ReqBody)) of
                {'ok', 200, _Headers, Body} ->
                    format_acquire_resp(PN, kz_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("number lookup failed to ~s with ~p: ~s"
                               ,[Uri, _Status, Body]),
                    knm_errors:by_carrier(?MODULE, 'lookup_failed', Num);
                {'error', Reason} ->
                    lager:debug("failed to acquire number ~s: ~p", [Num, Reason]),
                    knm_errors:by_carrier(?MODULE, 'lookup_failed', Num)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(PN) -> PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_check_numbers(kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', 'resp_error'}.
format_check_numbers(Body) ->
    case kz_json:get_value(<<"status">>, Body) of
        <<"success">> ->
            format_check_numbers_success(Body);
        _Error ->
            lager:error("number check resp error: ~p", [_Error]),
            {'error', 'resp_error'}
    end.

-spec format_check_numbers_success(kz_json:object()) ->
          {'ok', kz_json:object()}.
format_check_numbers_success(Body) ->
    F = fun(NumberJObj, Acc) ->
                Number = kz_json:get_value(<<"number">>, NumberJObj),
                Status = kz_json:get_value(<<"status">>, NumberJObj),
                kz_json:set_value(Number, Status, Acc)
        end,
    JObj = lists:foldl(F
                      ,kz_json:new()
                      ,kz_json:get_value(<<"data">>, Body, [])
                      ),
    {'ok', JObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_numbers(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), knm_search:options()) ->
          {'ok', knm_search:results()} |
          {'error', 'not_available'}.
get_numbers(Url, Prefix, Quantity, Options) ->
    Offset = props:get_binary_value('offset', Options, <<"0">>),
    ReqBody = <<"?prefix=", Prefix/binary, "&limit=", (kz_term:to_binary(Quantity))/binary, "&offset=", Offset/binary>>,
    Uri = <<Url/binary, "/numbers/", (?COUNTRY)/binary, "/search", ReqBody/binary>>,
    Results = query_for_numbers(Uri),
    handle_number_query_results(Results, Options).

-spec query_for_numbers(kz_term:ne_binary()) -> kz_http:http_ret().
-ifdef(TEST).
query_for_numbers(<<?NUMBER_PHONEBOOK_URL_L, _/binary>>=URI) ->
    lager:debug("number pb url ~s resp: ~s", [URI, kz_json:encode(?NUMBERS_RESPONSE)]),
    {'ok', 200, [], kz_json:encode(?NUMBERS_RESPONSE)}.
-else.
query_for_numbers(Uri) ->
    lager:debug("querying ~s for numbers", [Uri]),
    kz_http:get(binary:bin_to_list(Uri)).
-endif.

-spec handle_number_query_results(kz_http:http_ret(), knm_search:options()) ->
          {'ok', list()} |
          {'error', 'not_available'}.
handle_number_query_results({'error', _Reason}, _Options) ->
    lager:error("number query failed: ~p", [_Reason]),
    {'error', 'not_available'};
handle_number_query_results({'ok', 200, _Headers, Body}, Options) ->
    format_numbers_resp(kz_json:decode(Body), Options);
handle_number_query_results({'ok', _Status, _Headers, _Body}, _Options) ->
    lager:error("number query failed with ~p: ~s", [_Status, _Body]),
    {'error', 'not_available'}.

-spec format_numbers_resp(kz_json:object(), knm_search:options()) ->
          {'ok', knm_search:results()} |
          {'error', 'not_available'}.
format_numbers_resp(JObj, Options) ->
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            DataJObj = kz_json:get_value(<<"data">>, JObj),
            QID = knm_search:query_id(Options),
            Numbers = [format_found(QID, DID, CarrierData)
                       || {DID, CarrierData} <- kz_json:to_proplist(DataJObj)
                      ],
            {'ok', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [_Error]),
            {'error', 'not_available'}
    end.

-spec format_found(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> knm_search:result().
format_found(QID, DID, CarrierData) ->
    {QID, {DID, ?MODULE, ?NUMBER_STATE_DISCOVERY, CarrierData}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_blocks(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), knm_search:options()) ->
          {'bulk', knm_search:results()} |
          {'error', 'not_available'}.
-ifdef(TEST).
get_blocks(?BLOCK_PHONEBOOK_URL, _Prefix, _Quantity, Options) ->
    format_blocks_resp(?BLOCKS_RESP, Options).
-else.
get_blocks(Url, Prefix, Quantity, Options) ->
    Offset = props:get_binary_value('offset', Options, <<"0">>),
    Limit = props:get_binary_value('blocks', Options, <<"0">>),
    ReqBody = list_to_binary(["?prefix=", kz_http_util:urlencode(Prefix)
                             ,"&size=", kz_term:to_binary(Quantity)
                             ,"&offset=", Offset
                             ,"&limit=", Limit
                             ]),
    Uri = list_to_binary([Url
                         ,"/blocks/", (?COUNTRY)
                         ,"/search", ReqBody
                         ]),
    lager:debug("making request to ~s", [Uri]),
    case kz_http:get(binary:bin_to_list(Uri)) of
        {'ok', 200, _Headers, Body} ->
            format_blocks_resp(kz_json:decode(Body), Options);
        {'ok', _Status, _Headers, Body} ->
            lager:error("block lookup failed: ~p ~p", [_Status, Body]),
            {'error', 'not_available'};
        {'error', Reason} ->
            lager:error("block lookup error: ~p", [Reason]),
            {'error', 'not_available'}
    end.
-endif.

-spec format_blocks_resp(kz_json:object(), knm_search:options()) ->
          {'bulk', knm_search:results()} |
          {'error', 'not_available'}.
format_blocks_resp(JObj, Options) ->
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            QID = knm_search:query_id(Options),
            Numbers =
                lists:flatmap(fun(I) -> format_block_resp_fold(I, QID) end
                             ,kz_json:get_value(<<"data">>, JObj, [])
                             ),
            {'bulk', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [JObj]),
            {'error', 'not_available'}
    end.

-spec format_block_resp_fold(kz_json:object(), kz_term:ne_binary()) -> knm_search:results().
format_block_resp_fold(Block, QID) ->
    StartNumber = kz_json:get_value(<<"start_number">>, Block),
    EndNumber = kz_json:get_value(<<"end_number">>, Block),
    [format_found(QID, StartNumber, Block)
    ,format_found(QID, EndNumber, Block)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_acquire_resp(knm_phone_number:record(), kz_json:object()) ->
          knm_phone_number:record().
format_acquire_resp(PN, Body) ->
    Num = knm_phone_number:number(PN),
    JObj = kz_json:get_value([<<"data">>, Num], Body, kz_json:new()),
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Routines = [fun maybe_merge_opaque/2
                       ,fun maybe_merge_locality/2
                       ],
            lists:foldl(fun(F, N) -> F(JObj, N) end, PN, Routines);
        Error ->
            lager:error("number lookup resp error: ~p", [Error]),
            knm_errors:by_carrier(?MODULE, 'lookup_resp_error', Num)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_merge_opaque(kz_json:object(), knm_phone_number:record()) ->
          knm_phone_number:record().
maybe_merge_opaque(JObj, PN) ->
    case kz_json:get_ne_value(<<"opaque">>, JObj) of
        'undefined' -> PN;
        Opaque ->
            knm_phone_number:set_carrier_data(PN, Opaque)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_merge_locality(kz_json:object(), knm_phone_number:record()) ->
          knm_phone_number:record().
maybe_merge_locality(JObj, PN) ->
    case kz_json:get_ne_value(<<"locality">>,  JObj) of
        'undefined' -> PN;
        Locality ->
            knm_phone_number:set_feature(PN, <<"locality">>, Locality)
    end.
