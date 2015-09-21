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

-include("../knm.hrl").

-define(DEFAULT_COUNTRY, <<"US">>).
-define(KNM_OTHER_CONFIG_CAT, <<?KNM_CONFIG_CAT/binary, ".other">>).

-ifdef(TEST).
-define(COUNTRY, ?DEFAULT_COUNTRY).
-else.
-define(COUNTRY
        ,whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY)
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
-define(PHONEBOOK_URL(Options)
        ,whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>)
       ).
-endif.

-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'error', _} |
                          {'bulk', knm_number:knm_numbers()} |
                          {'ok', knm_number:knm_numbers()}.
find_numbers(Number, Quantity, Options) ->
    case ?PHONEBOOK_URL(Options) of
        'undefined' -> {'error', 'non_available'};
        Url ->
            case props:is_defined(<<"blocks">>, Options) of
                'false' ->
                    get_numbers(Url, Number, Quantity, Options);
                'true' ->
                    get_blocks(Url, Number, Quantity, Options)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries(), wh_proplist()) ->
                           {'error', _} |
                           {'ok', wh_json:object()}.
check_numbers(Numbers, _Props) ->
    FormatedNumbers = [(knm_converters:default_converter()):to_npan(Number)
                       || Number <- Numbers
                      ],
    case whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            {'error', 'non_available'};
        Url ->
            DefaultCountry = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
            ReqBody = wh_json:set_value(<<"data">>, FormatedNumbers, wh_json:new()),
            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/status">>,
            lager:debug("making request to ~s with body ~p", [Uri, ReqBody]),
            case ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("numbers check failed: ~p", [Reason]),
                    {'error', Reason};
                {'ok', "200", _Headers, Body} ->
                    format_check_numbers(wh_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("numbers check failed: ~p", [Body]),
                    {'error', Body}
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
    DefaultCountry = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            knm_errors:unspecified('missing_provider_url', Number);
        Url ->
            Hosts = case whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"endpoints">>) of
                        'undefined' -> [];
                        Endpoint when is_binary(Endpoint) ->
                            [Endpoint];
                        Endpoints -> Endpoints
                    end,

            ReqBody = wh_json:set_values([{[<<"data">>, <<"numbers">>], [Num]}
                                          ,{[<<"data">>, <<"gateways">>], Hosts}
                                         ]
                                         ,wh_json:new()
                                        ),

            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/order">>,
            case ibrowse:send_req(wh_util:to_list(Uri), [], 'put', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    knm_errors:unspecified(Reason, Number);
                {'ok', "200", _Headers, Body} ->
                    format_acquire_resp(Number, wh_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("number lookup failed to ~s with ~s: ~s"
                                ,[Uri, _Status, Body]
                               ),
                    knm_errors:unspecified('lookup_failed', Number)
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
-spec format_check_numbers(wh_json:object()) ->
                                  {'error', 'resp_error'} |
                                  {'ok', wh_json:object()}.
format_check_numbers(Body) ->
    case wh_json:get_value(<<"status">>, Body) of
        <<"success">> ->
            format_check_numbers_success(Body);
        _Error ->
            lager:error("number check resp error: ~p", [_Error]),
            {'error', 'resp_error'}
    end.

-spec format_check_numbers_success(wh_json:object()) ->
                                          {'ok', wh_json:object()}.
format_check_numbers_success(Body) ->
    JObj = lists:foldl(
             fun(NumberJObj, Acc) ->
                     Number = wh_json:get_value(<<"number">>, NumberJObj),
                     Status = wh_json:get_value(<<"status">>, NumberJObj),
                     wh_json:set_value(Number, Status, Acc)
             end
             ,wh_json:new()
             ,wh_json:get_value(<<"data">>, Body, [])
            ),
    {'ok', JObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_numbers(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                         {'error', 'non_available'} |
                         {'ok', knm_number:knm_numbers()}.
get_numbers(Url, Number, Quantity, Options) ->
    Offset = props:get_binary_value(<<"offset">>, Options, <<"0">>),
    Country = ?COUNTRY,
    ReqBody = <<"?prefix=", Number/binary, "&limit=", (wh_util:to_binary(Quantity))/binary, "&offset=", Offset/binary>>,
    Uri = <<Url/binary, "/numbers/", Country/binary, "/search", ReqBody/binary>>,

    Results = query_for_numbers(Uri),
    handle_number_query_results(Results, Options).

-spec query_for_numbers(ne_binary()) -> ibrowse_ret().
-ifdef(TEST).
query_for_numbers(<<?NUMBER_PHONEBOOK_URL_L, _/binary>>) ->
    {'ok', "200", [], wh_json:encode(?NUMBERS_RESPONSE)}.
-else.
query_for_numbers(Uri) ->
    lager:debug("querying ~s for numbers", [Uri]),
    ibrowse:send_req(wh_util:to_list(Uri), [], 'get').
-endif.

-spec handle_number_query_results(ibrowse_ret(), wh_proplist()) ->
                                         {'error', 'non_available'} |
                                         {'ok', knm_number:knm_numbers()}.
handle_number_query_results({'error', _Reason}, _Options) ->
    lager:error("number query failed: ~p", [_Reason]),
    {'error', 'non_available'};
handle_number_query_results({'ok', "200", _Headers, Body}, Options) ->
    format_numbers_resp(wh_json:decode(Body), Options);
handle_number_query_results({'ok', _Status, _Headers, _Body}, _Options) ->
    lager:error("number query failed with ~s: ~s", [_Status, _Body]),
    {'error', 'non_available'}.

-spec format_numbers_resp(wh_json:object(), wh_proplist()) ->
                                 {'error', 'non_available'} |
                                 {'ok', knm_number:knm_numbers()}.
format_numbers_resp(JObj, Options) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            DataJObj = wh_json:get_value(<<"data">>, JObj),
            AccountId = props:get_value(<<"account_id">>, Options),

            {'ok'
             ,lists:reverse(
                wh_json:foldl(fun(K, V, Acc) ->
                                      [format_number_resp(K, V, AccountId) | Acc]
                              end
                              ,[]
                              ,DataJObj
                             )
               )
            };
        _Error ->
            lager:error("block lookup resp error: ~p", [_Error]),
            {'error', 'non_available'}
    end.

-spec format_number_resp(ne_binary(), wh_json:object(), knm_number:knm_numbers()) ->
                                knm_number:knm_number().
format_number_resp(DID, CarrierData, AccountId) ->
    NormalizedNum = knm_converters:normalize(DID),
    NumberDb = knm_converters:to_db(NormalizedNum),

    Updates = [{fun knm_phone_number:set_number/2, NormalizedNum}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
               ,{fun knm_phone_number:set_carrier_data/2, CarrierData}
               ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
               ,{fun knm_phone_number:set_assign_to/2, AccountId}
              ],
    PhoneNumber = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_number:set_phone_number(knm_number:new(), PhoneNumber).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_blocks(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                        {'error', 'non_available'} |
                        {'ok', knm_number:knm_numbers()}.
-ifdef(TEST).
get_blocks(?BLOCK_PHONEBOOK_URL, _Number, _Quantity, Props) ->
    format_blocks_resp(?BLOCKS_RESP, Props).
-else.
get_blocks(Url, Number, Quantity, Props) ->
    Offset = props:get_binary_value(<<"offset">>, Props, <<"0">>),
    Limit = props:get_binary_value(<<"blocks">>, Props, <<"0">>),
    Country = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", (wh_util:uri_encode(Number))/binary
                ,"&size=", (wh_util:to_binary(Quantity))/binary
                ,"&offset=", Offset/binary
                ,"&limit=", Limit/binary
              >>,
    Uri = <<Url/binary
            ,"/blocks/", Country/binary
            ,"/search", ReqBody/binary
          >>,
    lager:debug("making request to ~s", [Uri]),
    case ibrowse:send_req(binary:bin_to_list(Uri), [], 'get') of
        {'error', Reason} ->
            lager:error("block lookup error: ~p", [Reason]),
            {'error', 'non_available'};
        {'ok', "200", _Headers, Body} ->
            format_blocks_resp(wh_json:decode(Body), Props);
        {'ok', _Status, _Headers, Body} ->
            lager:error("block lookup failed: ~p ~p", [_Status, Body]),
            {'error', 'non_available'}
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_blocks_resp(wh_json:object(), wh_proplist()) ->
                                {'error', 'non_available'} |
                                {'bulk', knm_number:knm_numbers()}.
format_blocks_resp(JObj, Options) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            AccountId = props:get_value(<<"account_id">>, Options),
            Numbers =
                lists:foldl(
                  fun(I, Acc) -> format_block_resp_fold(I, Acc, AccountId) end
                  ,[]
                  ,wh_json:get_value(<<"data">>, JObj, [])
                 ),
            {'bulk', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [JObj]),
            {'error', 'non_available'}
    end.

-spec format_block_resp_fold(wh_json:object(), knm_number:knm_numbers(), api_binary()) ->
                                    knm_number:knm_numbers().
format_block_resp_fold(Block, Numbers, AccountId) ->
    StartNumber = wh_json:get_value(<<"start_number">>, Block),
    EndNumber = wh_json:get_value(<<"end_number">>, Block),
    format_block_resp(Block, Numbers, AccountId, StartNumber, EndNumber).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_block_resp(wh_json:object(), knm_number:knm_numbers(), api_binary(), ne_binary(), ne_binary()) ->
                               knm_number:knm_numbers().
format_block_resp(JObj, Numbers, AccountId, Start, End) ->
    [block_resp(JObj, AccountId, Start)
     ,block_resp(JObj, AccountId, End)
     | Numbers
    ].

-spec block_resp(wh_json:object(), api_binary(), ne_binary()) ->
                        knm_number:knm_number().
block_resp(JObj, AccountId, Num) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates = [{fun knm_phone_number:set_number/2, NormalizedNum}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
               ,{fun knm_phone_number:set_carrier_data/2, JObj}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_assign_to/2, AccountId}
              ],
    PhoneNumber = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_number:set_phone_number(knm_number:new(), PhoneNumber).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_acquire_resp(knm_number:knm_number(), wh_json:object()) ->
                                 knm_number:knm_number().
format_acquire_resp(Number, Body) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    JObj = wh_json:get_value([<<"data">>, Num], Body, wh_json:new()),
    case wh_json:get_value(<<"status">>, JObj) of
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
            knm_errors:unspecified('lookup_resp_error', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_merge_opaque(wh_json:object(), knm_number:knm_number()) ->
                                knm_number:knm_number().
maybe_merge_opaque(JObj, Number) ->
    case wh_json:get_ne_value(<<"opaque">>, JObj) of
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
-spec maybe_merge_locality(wh_json:object(), knm_number:knm_number()) ->
                                  knm_number:knm_number().
maybe_merge_locality(JObj, Number) ->
    case wh_json:get_ne_value(<<"locality">>,  JObj) of
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
