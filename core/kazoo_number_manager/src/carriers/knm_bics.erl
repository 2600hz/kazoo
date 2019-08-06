%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle client requests for phone_number documents using BICS api
%%% @author Kyle Diedrick
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bics).
-behaviour(knm_gen_carrier).

-include("knm.hrl").
-include("../knm_bics.hrl").

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

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
-spec check_numbers(kz_term:ne_binaries()) -> {ok, kz_json:object()} |
                                              {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%------------------------------------------------------------------------------
%% @doc Query the Bics API for a quantity of available numbers
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(_Search, _Quantity, Options) ->
    Country = knm_iso3166_util:country(proplists:get_value('country', Options)),
    Resp = make_inventory_request(maps:from_list(Options), Country),
    handle_inventory_response(Resp, Options).
handle_inventory_response({'ok', _Resp, _RespHeaders, Body}, Options) ->
    lager:debug("BICS response ~p: ~p", [_Resp, Body]),
    {'ok', process_numbers_search_resp(Body, Options)};
handle_inventory_response({'error', _R}, _Options) ->
    lager:debug("BICS responded with error: ~p", [_R]),
    {'error', 'not_available'}.
%%------------------------------------------------------------------------------
%% @doc Attempt to acquire the number from Bics
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
    knm_number:knm_number().
acquire_number(Number) ->
    Resp = make_acquisition_request(Number),
    handle_acquistion_response(Number, Resp).

-spec disconnect_number(knm_number:knm_number()) ->
    knm_number:knm_number().
disconnect_number(Number) -> Number.

-spec should_lookup_cnam() ->
    boolean().
should_lookup_cnam() -> 'true'.

-spec is_number_billable(knm_phone_number:knm_phone_number()) ->
    boolean().
is_number_billable(_Number) -> 'true'.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Convert a knm_number type to the raw binary e164 string (minus +) for sending to Bics
%% @end
%%------------------------------------------------------------------------------
-spec to_bics(knm_number:number()) -> binary().
to_bics(KnmNumber) ->
    <<"+", Number/binary>> = knm_phone_number:number(knm_number:phone_number(KnmNumber)),
    Number.

%%------------------------------------------------------------------------------
%% @doc Take a binary number from the Bics API and make it e164
%% @end
%%------------------------------------------------------------------------------
-spec from_bics(binary()) -> binary().
from_bics(Number) -> <<"+", Number/binary>>.

%%%=============================================================================
%%% Search API Helpers
%%%=============================================================================
process_numbers_search_resp(Json, Options) ->
    JObjs = kz_json:decode(Json),
    QID = knm_search:query_id(Options),
    [N || JObj <- JObjs,
             N <- [search_response_to_knm_number(JObj, QID)]
    ].

-spec search_response_to_knm_number(kz_json:object(), knm_search:options()) -> knm_number:knm_number_return().
search_response_to_knm_number(JObj, QID) ->
    Number = from_bics(kz_json:get_binary_value(<<"number">>, JObj)),
    {QID, {Number, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.

-spec classify_query(map(), binary()) -> binary().
classify_query(#{'dialcode' := <<"+1">>, 'prefix' := Prefix}, _A3)
  when ?IS_US_TOLLFREE(Prefix)
       orelse ?IS_US_TOLLFREE_WILDCARD(Prefix) ->
    <<"ITFS">>;
classify_query(#{'prefix' := Prefix}, _A3)
  when ?IS_UIFN_TOLLFREE(Prefix) ->
    <<"ITFS">>;
classify_query(_Options, _A3) ->
    <<"IBN">>.

-ifndef(TEST).
-spec make_inventory_request(map(), knm_iso3166_util:country()) -> kz_http:ret().
make_inventory_request(Options, #{a3 := A3}= _Country) ->
    Product = classify_query(Options, A3),
    Url = get_inventory_url(Product, A3),
    ?LOG_DEBUG("Sending bics request url: ~p", [Url]),
    kz_http:get(Url, headers()).
-else.
make_inventory_request(#{'prefix' := Prefix} = _Options, #{a3 := _A3} = _Country) ->
    case Prefix of
        {'ok', <<"855">>} ->
            {'ok', 200, [{}], "[{\"number\": \"+18551231234\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}]"};
        _Else ->
            {'ok', 200, [{}], "[{\"number\": \"+19731231234\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}, {\"number\": \"+19734564567\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}]"}
    end.
-endif.

%%%=============================================================================
%%% Number Purchase API Helpers
%%%=============================================================================
-spec acquisition_payload(binary()) -> binary().
acquisition_payload(Number) -> 
    <<"{\"orderRequestItems\": [{\"number\": \"", Number/binary, "\"}]}">>.

-spec process_acquisition_error(binary()) -> binary().
process_acquisition_error(Json) ->
    JObjs = kz_json:decode(Json),
    acquisition_jobj_to_error(JObjs).

-spec acquisition_jobj_to_error(list()) -> binary().
acquisition_jobj_to_error([First|_Rest]) ->
    kz_json:get_binary_value(<<"description">>, First);
acquisition_jobj_to_error(_Any) ->
    generic_acquisition_error().

generic_acquisition_error() -> <<"Unable to purchase number">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handle_acquistion_response(Number, {'ok', _, _, Body}) ->
    JResp = kz_json:decode(Body),
    verify_acquired_number(Number, JResp);
handle_acquistion_response(Number, {'error', Res}) ->
    ?LOG_DEBUG("BICS failed to purchase number ~p", [Res]),
    knm_errors:by_carrier(?MODULE, process_acquisition_error(Res), Number).

verify_acquired_number(Number, [JResp | _]) -> verify_acquired_number(Number, JResp);
verify_acquired_number(Number, JResp) -> 
    OrderItems = kz_json:get_list_value(<<"orderItems">>, JResp),
    RawNumber = to_bics(Number),
    NumberInResponse = lists:any(fun(OrderItem) ->
        OrderNumber = kz_json:get_json_value(<<"number">>, OrderItem),
        ResponseNumber = kz_json:get_binary_value(<<"number">>, OrderNumber),
        RawNumber == ResponseNumber
    end, OrderItems),
    verify_acquired_number_status(Number, kz_json:get_binary_value(<<"status">>, JResp), NumberInResponse).

verify_acquired_number_status(Number, <<"Delivered">>, 'true') -> Number;
verify_acquired_number_status(Number, _, _) -> knm_errors:by_carrier(?MODULE, generic_acquisition_error(), Number).

-ifndef(TEST).
-spec make_acquisition_request(knm_number:knm_number()) -> kz_http:ret().
make_acquisition_request(Number) ->
    Payload = acquisition_payload(to_bics(Number)),
    kz_http:post(get_acquisition_url(), headers(), Payload).
-else.
make_acquisition_request(Number) -> 
    _Payload = acquisition_payload(raw_number(Number)),
    _Url = get_acquisition_url(),
    ?LOG_DEBUG("In test make acquisition"),
    {
        'ok', 
        200, 
        [], 
        <<"[{\"status\": \"Delivered\",\"product\": \"IBN\",\"country\": \"USA\",\"orderItems\": [{\"number\": \"", 
            to_bics(Number), 
            "\",\"location\": \"CA-PITTSBURG\",\"addressReference\": null,\"routing\": [{\"accessType\": \"fixMobPay\",\"accessNetwork\": null,\"crn\": \"14986\",\"pop\": \"\",\"crnType\": \"copy\",\"crnValue\": null}]}]}]">>
    }.
-endif.

%%%=============================================================================
%%% API Helper functions
%%%=============================================================================
-spec headers() -> list().
headers() -> 
    [{"Accept", "*/*"}
        ,{"Authorization", bearer_token()}
        ,{"Content-Type", "application/json"}
    ].

-spec bearer_token() -> binary().
bearer_token() ->
    Token = ?BICS_BEARER_TOKEN,
    <<"Bearer", " ", Token/binary>>.

-spec get_inventory_url(binary(), binary()) -> binary().
get_inventory_url(Product, Country) ->
    <<(?BICS_BASE_URL), "/availablenumbers?product=", Product/binary, "&country=", Country/binary>>.

-spec get_acquisition_url() -> binary().
get_acquisition_url() ->
    <<(?BICS_BASE_URL), "/order/bulk">>.