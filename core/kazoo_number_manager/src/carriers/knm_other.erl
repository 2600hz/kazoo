%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_other).

-behaviour(knm_gen_carrier).

-export([find_numbers/3]).
-export([check_numbers/2]).
-export([is_number_billable/1]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

-define(DEFAULT_COUNTRY, <<"US">>).
-define(KNM_OTHER_CONFIG_CAT, <<?KNM_CONFIG_CAT/binary, ".other">>).

-ifdef(TEST).
-define(COUNTRY, ?DEFAULT_COUNTRY).
-else.
-define(COUNTRY
        ,kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY)
       ).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-ifdef(TEST).
-define(PHONEBOOK_URL(Options)
        ,props:get_value(<<"phonebook_url">>, Options)
       ).
-else.
-define(PHONEBOOK_URL(_Options)
        ,kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>)
       ).
-endif.

-spec find_numbers(ne_binary(), pos_integer(), kz_proplist()) ->
                          {'error', any()} |
                          {'bulk', knm_number:knm_numbers()} |
                          {'ok', knm_number:knm_numbers()}.
find_numbers(Number, Quantity, Options) ->
    case ?PHONEBOOK_URL(Options) of
        'undefined' -> {'error', 'not_available'};
        Url ->
            case props:is_defined(<<"blocks">>, Options) of
                'false' -> get_numbers(Url, Number, Quantity, Options);
                'true' ->  get_blocks(Url, Number, Quantity, Options)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries(), kz_proplist()) ->
                           {'error', any()} |
                           {'ok', kz_json:object()}.
check_numbers(Numbers, _Props) ->
    FormatedNumbers = [knm_converters:to_npan(Number) || Number <- Numbers],
    case kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            {'error', 'not_available'};
        Url ->
            DefaultCountry = kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
            ReqBody = kz_json:set_value(<<"data">>, FormatedNumbers, kz_json:new()),
            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/status">>,
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_number:knm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    DefaultCountry = kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            knm_errors:unspecified('missing_provider_url', Number);
        Url ->
            Hosts = case kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"endpoints">>) of
                        'undefined' -> [];
                        Endpoint when is_binary(Endpoint) ->
                            [Endpoint];
                        Endpoints -> Endpoints
                    end,

            ReqBody = kz_json:set_values([{[<<"data">>, <<"numbers">>], [Num]}
                                          ,{[<<"data">>, <<"gateways">>], Hosts}
                                         ]
                                         ,kz_json:new()
                                        ),

            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/order">>,
            case kz_http:put(binary:bin_to_list(Uri), [], kz_json:encode(ReqBody)) of
                {'ok', 200, _Headers, Body} ->
                    format_acquire_resp(Number, kz_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("number lookup failed to ~s with ~p: ~s"
                                ,[Uri, _Status, Body]),
                    knm_errors:by_carrier(?MODULE, 'lookup_failed', Number);
                {'error', Reason} ->
                    knm_errors:by_carrier(?MODULE, Reason, Number)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_check_numbers(kz_json:object()) ->
                                  {'error', 'resp_error'} |
                                  {'ok', kz_json:object()}.
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
    JObj = lists:foldl(
             fun(NumberJObj, Acc) ->
                     Number = kz_json:get_value(<<"number">>, NumberJObj),
                     Status = kz_json:get_value(<<"status">>, NumberJObj),
                     kz_json:set_value(Number, Status, Acc)
             end
             ,kz_json:new()
             ,kz_json:get_value(<<"data">>, Body, [])
            ),
    {'ok', JObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_numbers(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                         {'error', 'not_available'} |
                         {'ok', knm_number:knm_numbers()}.
get_numbers(Url, Number, Quantity, Options) ->
    Offset = props:get_binary_value(<<"offset">>, Options, <<"0">>),
    Country = ?COUNTRY,
    ReqBody = <<"?prefix=", Number/binary, "&limit=", (kz_util:to_binary(Quantity))/binary, "&offset=", Offset/binary>>,
    Uri = <<Url/binary, "/numbers/", Country/binary, "/search", ReqBody/binary>>,

    Results = query_for_numbers(Uri),
    handle_number_query_results(Results, Options).

-spec query_for_numbers(ne_binary()) -> kz_http:http_ret().
-ifdef(TEST).
query_for_numbers(<<?NUMBER_PHONEBOOK_URL_L, _/binary>>) ->
    {'ok', 200, [], kz_json:encode(?NUMBERS_RESPONSE)}.
-else.
query_for_numbers(Uri) ->
    lager:debug("querying ~s for numbers", [Uri]),
    kz_http:get(binary:bin_to_list(Uri)).
-endif.

-spec handle_number_query_results(kz_http:http_ret(), kz_proplist()) ->
                                         {'error', 'not_available'} |
                                         {'ok', knm_number:knm_numbers()}.
handle_number_query_results({'error', _Reason}, _Options) ->
    lager:error("number query failed: ~p", [_Reason]),
    {'error', 'not_available'};
handle_number_query_results({'ok', 200, _Headers, Body}, Options) ->
    format_numbers_resp(kz_json:decode(Body), Options);
handle_number_query_results({'ok', _Status, _Headers, _Body}, _Options) ->
    lager:error("number query failed with ~p: ~s", [_Status, _Body]),
    {'error', 'not_available'}.

-spec format_numbers_resp(kz_json:object(), kz_proplist()) ->
                                 {'error', 'not_available'} |
                                 {'ok', knm_number:knm_numbers()}.
format_numbers_resp(JObj, Options) ->
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            DataJObj = kz_json:get_value(<<"data">>, JObj),
            AccountId = props:get_value(?KNM_ACCOUNTID_CARRIER, Options),

            {'ok'
             ,lists:reverse(
                kz_json:foldl(fun(K, V, Acc) ->
                                      format_number_resp(K, V, AccountId, Acc)
                              end
                              ,[]
                              ,DataJObj
                             )
               )
            };
        _Error ->
            lager:error("block lookup resp error: ~p", [_Error]),
            {'error', 'not_available'}
    end.

-spec format_number_resp(ne_binary(), kz_json:object(), ne_binary(), knm_number:knm_numbers()) ->
                                knm_number:knm_number_return().
format_number_resp(DID, CarrierData, AccountId, Acc) ->
    case knm_carriers:create_discovery(DID, ?MODULE, AccountId, CarrierData) of
        {'ok', N} -> [N | Acc];
        _ -> Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_blocks(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                        {'error', 'not_available'} |
                        {'ok', knm_number:knm_numbers()}.
-ifdef(TEST).
get_blocks(?BLOCK_PHONEBOOK_URL, _Number, _Quantity, Props) ->
    format_blocks_resp(?BLOCKS_RESP, Props).
-else.
get_blocks(Url, Number, Quantity, Props) ->
    Offset = props:get_binary_value(<<"offset">>, Props, <<"0">>),
    Limit = props:get_binary_value(<<"blocks">>, Props, <<"0">>),
    Country = kapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", (kz_util:uri_encode(Number))/binary
                ,"&size=", (kz_util:to_binary(Quantity))/binary
                ,"&offset=", Offset/binary
                ,"&limit=", Limit/binary
              >>,
    Uri = <<Url/binary
            ,"/blocks/", Country/binary
            ,"/search", ReqBody/binary
          >>,
    lager:debug("making request to ~s", [Uri]),
    case kz_http:get(binary:bin_to_list(Uri)) of
        {'ok', 200, _Headers, Body} ->
            format_blocks_resp(kz_json:decode(Body), Props);
        {'ok', _Status, _Headers, Body} ->
            lager:error("block lookup failed: ~p ~p", [_Status, Body]),
            {'error', 'not_available'};
        {'error', Reason} ->
            lager:error("block lookup error: ~p", [Reason]),
            {'error', 'not_available'}
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_blocks_resp(kz_json:object(), kz_proplist()) ->
                                {'error', 'not_available'} |
                                {'bulk', knm_number:knm_numbers()}.
format_blocks_resp(JObj, Options) ->
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            AccountId = props:get_value(?KNM_ACCOUNTID_CARRIER, Options),
            Numbers =
                lists:flatmap(
                  fun(I) -> format_block_resp_fold(I, AccountId) end
                  ,kz_json:get_value(<<"data">>, JObj, [])
                 ),
            {'bulk', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [JObj]),
            {'error', 'not_available'}
    end.

-spec format_block_resp_fold(kz_json:object(), api_binary()) -> knm_number:knm_numbers().
format_block_resp_fold(Block, AccountId) ->
    StartNumber = kz_json:get_value(<<"start_number">>, Block),
    EndNumber = kz_json:get_value(<<"end_number">>, Block),
    [N || {'ok', N} <- [block_resp(Block, AccountId, StartNumber)
                       ,block_resp(Block, AccountId, EndNumber)
                       ]
    ].

-spec block_resp(kz_json:object(), api_binary(), ne_binary()) ->
                        knm_number:knm_number_return().
block_resp(JObj, AccountId, Num) ->
    knm_carriers:create_discovery(Num, ?MODULE, AccountId, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_acquire_resp(knm_number:knm_number(), kz_json:object()) ->
                                 knm_number:knm_number().
format_acquire_resp(Number, Body) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    JObj = kz_json:get_value([<<"data">>, Num], Body, kz_json:new()),
    case kz_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Routines = [fun maybe_merge_opaque/2
                        ,fun maybe_merge_locality/2
                       ],
            lists:foldl(fun(F, N) -> F(JObj, N) end
                        ,Number
                        ,Routines
                       );
        Error ->
            lager:error("number lookup resp error: ~p", [Error]),
            knm_errors:by_carrier(?MODULE, 'lookup_resp_error', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_merge_opaque(kz_json:object(), knm_number:knm_number()) ->
                                knm_number:knm_number().
maybe_merge_opaque(JObj, Number) ->
    case kz_json:get_ne_value(<<"opaque">>, JObj) of
        'undefined' -> Number;
        Opaque ->
            knm_number:set_phone_number(
              Number
              ,knm_phone_number:set_carrier_data(knm_number:phone_number(Number), Opaque)
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_merge_locality(kz_json:object(), knm_number:knm_number()) ->
                                  knm_number:knm_number().
maybe_merge_locality(JObj, Number) ->
    case kz_json:get_ne_value(<<"locality">>,  JObj) of
        'undefined' -> Number;
        Locality ->
            knm_number:set_phone_number(
              Number
              ,knm_phone_number:set_feature(knm_number:phone_number(Number)
                                            ,<<"locality">>
                                            ,Locality
                                           )
             )
    end.
