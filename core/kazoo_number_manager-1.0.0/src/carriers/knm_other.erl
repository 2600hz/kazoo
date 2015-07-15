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

-export([find_numbers/3]).
-export([check_numbers/2]).
-export([is_number_billable/1]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([should_lookup_cnam/0]).

-include("../knm.hrl").

-define(DEFAULT_COUNTRY, <<"US">>).
-define(KNM_OTHER_CONFIG_CAT, <<"number_manager.other">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) -> {'error', _} | {'ok', numbers()}.
find_numbers(Number, Quantity, Props) ->
    case whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' -> {'error', 'non_available'};
        Url ->
            case props:get_value(<<"blocks">>, Props) of
                'undefined' ->
                    get_numbers(Url, Number, Quantity, Props);
                _ ->
                    get_blocks(Url, Number, Quantity, Props)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries(), wh_proplist()) -> {'error', any()} | {'ok', any()}.
check_numbers(Numbers, _Props) ->
    FormatedNumbers = [(knm_converters:default()):to_npan(Number) || Number <- Numbers],
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
-spec is_number_billable(number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

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
    Num = knm_phone_number:number(Number),
    DefaultCountry = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            {'error', 'missing_provider_url'};
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
                    {'error', Reason};
                {'ok', "200", _Headers, Body} ->
                    format_acquire_resp(wh_json:decode(Body), Number);
                {'ok', _Status, _Headers, Body} ->
                    lager:error("number lookup failed to ~s with ~s: ~s", [Uri, _Status, Body]),
                    {'error', 'lookup_failed'}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(number()) -> number().
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
-spec format_check_numbers(wh_json:object()) -> {'error', any()} | {'ok', any()}.
format_check_numbers(Body) ->
    case wh_json:get_value(<<"status">>, Body) of
        <<"success">> ->
            JObj =  lists:foldl(
                        fun(NumberJObj, Acc) ->
                            Number = wh_json:get_value(<<"number">>, NumberJObj),
                            Status = wh_json:get_value(<<"status">>, NumberJObj),
                            wh_json:set_value(Number, Status, Acc)
                        end
                        ,wh_json:new()
                        ,wh_json:get_value(<<"data">>, Body, [])
                    ),
            {'ok', JObj};
        _Error ->
            lager:error("number check resp error: ~p", [_Error]),
            {'error', 'resp_error'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_numbers(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> {'error', 'non_available'} | {'ok', numbers()}.
get_numbers(Url, Number, Quantity, Props) ->
    Offset = props:get_binary_value(<<"offset">>, Props, <<"0">>),
    Country = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", Number/binary, "&limit=", (wh_util:to_binary(Quantity))/binary, "&offset=", Offset/binary>>,
    Uri = <<Url/binary, "/numbers/", Country/binary, "/search", ReqBody/binary>>,
    case ibrowse:send_req(wh_util:to_list(Uri), [], 'get') of
        {'error', _Reason} ->
            lager:error("number lookup error to ~s: ~p", [Uri, _Reason]),
            {'error', 'non_available'};
        {'ok', "200", _Headers, Body} ->
            format_numbers_resp(wh_json:decode(Body));
        {'ok', _Status, _Headers, _Body} ->
            lager:error("number lookup failed to ~s with ~s: ~s", [Uri, _Status, _Body]),
            {'error', 'non_available'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_numbers_resp(wh_json:object()) -> {'error', 'non_available'} | {'ok', numbers()}.
-spec format_numbers_resp(wh_json:object(), numbers()) -> numbers().
format_numbers_resp(JObj) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            DataJObj = wh_json:get_value(<<"data">>, JObj),
            Props = wh_json:to_proplist(DataJObj),
            {'ok', format_numbers_resp(Props, [])};
        _Error ->
            lager:error("block lookup resp error: ~p", [_Error]),
            {'error', 'non_available'}
    end.

format_numbers_resp([], Numbers) -> Numbers;
format_numbers_resp([{Num, JObj}|T], Numbers) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates = [
        {fun knm_phone_number:set_number/2, NormalizedNum}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
        ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
        ,{fun knm_phone_number:set_carrier_data/2, JObj}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
        ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
    ],
    Number = knm_phone_number:setters(knm_phone_number:new(), Updates),
    format_numbers_resp(T, [Number|Numbers]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_blocks(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> {'error', 'non_available'} | {'ok', numbers()}.
get_blocks(Url, Number, Quantity, Props) ->
    Offset = props:get_binary_value(<<"offset">>, Props, <<"0">>),
    Limit = props:get_binary_value(<<"blocks">>, Props, <<"0">>),
    Country = whapps_config:get(?KNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", (wh_util:uri_encode(Number))/binary
                            ,"&size=", (wh_util:to_binary(Quantity))/binary
                            ,"&offset=", Offset/binary
                            ,"&limit=", Limit/binary>>,
    Uri = <<Url/binary, "/blocks/", Country/binary, "/search", ReqBody/binary>>,
    lager:debug("making request to ~s", [Uri]),
    case ibrowse:send_req(binary:bin_to_list(Uri), [], 'get') of
        {'error', Reason} ->
            lager:error("block lookup error: ~p", [Reason]),
            {'error', 'non_available'};
        {'ok', "200", _Headers, Body} ->
            format_blocks_resp(wh_json:decode(Body));
        {'ok', _Status, _Headers, Body} ->
            lager:error("block lookup failed: ~p ~p", [_Status, Body]),
            {'error', 'non_available'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_blocks_resp(wh_json:object()) ->  {'error', 'non_available'} | {'ok', wh_json:objects()}.
format_blocks_resp(JObj) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Numbers =
                lists:foldl(
                    fun(Block, Numbers) ->
                        StartNumber = wh_json:get_value(<<"start_number">>, Block),
                        EndNumber = wh_json:get_value(<<"end_number">>, Block),
                        format_blocks_resp_fold([{StartNumber, Block}, {EndNumber, Block}], Numbers)
                    end
                    ,[]
                    ,wh_json:get_value(<<"data">>, JObj, [])
                ),
            {'ok', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [JObj]),
            {'error', 'non_available'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_blocks_resp_fold(wh_json:object(), numbers()) -> numbers().
format_blocks_resp_fold([], Numbers) -> Numbers;
format_blocks_resp_fold([{Num, JObj}|T], Numbers) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates = [
        {fun knm_phone_number:set_number/2, NormalizedNum}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
        ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
        ,{fun knm_phone_number:set_carrier_data/2, JObj}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
    ],
    Number = knm_phone_number:setters(knm_phone_number:new(), Updates),
    format_blocks_resp_fold(T, [Number|Numbers]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_acquire_resp(wh_json:object(), number()) -> number_return().
format_acquire_resp(Body, Number) ->
    Num = knm_phone_number:number(Number),
    JObj = wh_json:get_value([<<"data">>, Num], Body, wh_json:new()),
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Routines = [fun maybe_merge_opaque/2
                        ,fun maybe_merge_locality/2
                       ],
            Number1 = lists:foldl(fun(F, N) -> F(JObj, N) end, Number, Routines),
            {'ok', Number1};
        Error ->
            lager:error("number lookup resp error: ~p", [Error]),
            {'error', 'lookup_resp_error'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_merge_opaque(wh_json:object(), number()) -> number().
maybe_merge_opaque(JObj, Number) ->
    case wh_json:get_ne_value(<<"opaque">>, JObj) of
        'undefined' -> Number;
        Opaque ->
            knm_phone_number:set_carrier_data(Number, Opaque)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_merge_locality(wh_json:object(), number()) -> number().
maybe_merge_locality(JObj, Number) ->
    case wh_json:get_ne_value(<<"locality">>,  JObj) of
        'undefined' -> Number;
        Locality ->
            knm_phone_number:set_feature(Number, <<"locality">>, Locality)
    end.