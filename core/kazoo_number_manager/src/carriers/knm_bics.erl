%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle client requests for phone_number documents using BICS api
%%% @author Kyle Diedrick
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bics).
-behaviour(knm_gen_carrier).

-include("knm.hrl").
-include("knm_bics.hrl").

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
%% @doc Query the BICS system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(_Search, _Quantity, Options) ->
    Country = knm_iso3166_util:country(proplists:get_value('country', Options)),
    case make_inventory_request(maps:from_list(Options), Country) of
        {'ok', _Resp, _RespHeaders, Body} ->
            lager:debug("BICS response ~p: ~p", [_Resp, Body]),
            {'ok', process_numbers_search_resp(Body, Options)};
        {'error', _R} ->
            lager:debug("BICS responded with error: ~p", [_R]),
            {'error', 'not_available'}
    end.

%% @todo --- below here
-spec acquire_number(knm_number:knm_number()) ->
    knm_number:knm_number().
acquire_number(Number) -> Number.

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
process_numbers_search_resp(Json, Options) ->
    JObjs = kz_json:decode(Json),
    process_response(JObjs, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_response(kz_json:objects(), knm_carriers:options()) ->
                              {'ok', knm_number:knm_numbers()}.
process_response(JObjs, Options) ->
    QID = knm_search:query_id(Options),

    [N || JObj <- JObjs,
             N <- [response_jobj_to_number(JObj, QID)]
    ].

response_jobj_to_number(JObj, QID) ->
    Num = kz_json:get_binary_value(<<"number">>, JObj),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.

-spec classify_query(map(), kz_term:ne_binary()) -> kz_term:ne_binary().
classify_query(#{'dialcode' := <<"+1">>, 'prefix' := Prefix}, _A3)
  when ?IS_US_TOLLFREE(Prefix)
       orelse ?IS_US_TOLLFREE_WILDCARD(Prefix) ->
    <<"ITFS">>;
classify_query(#{'prefix' := Prefix}, _A3)
  when ?IS_UIFN_TOLLFREE(Prefix) ->
    <<"ITFS">>;
classify_query(_Options, _A3) ->
    <<"IBN">>.

%%------------------------------------------------------------------------------
%% @doc Get the URL for requesting the number inventory
%% @end
%%------------------------------------------------------------------------------
-spec get_inventory_url(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_inventory_url(Product, Country) ->
    list_to_binary([?BICS_BASE_URL, <<"/availablenumbers?product=">>, Product, <<"&country=">>, Country]).

-spec make_inventory_request(map(), knm_iso3166_util:country()) -> kz_http:ret().
make_inventory_request(#{'test' := 'true'} = Options, #{a3 := _A3}= _Country) ->
    Prefix = maps:find('prefix', Options),
    case Prefix of
        {'ok', <<"855">>} ->
            {'ok', 200, [{}], "[{\"number\": \"+18551231234\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}]"};
        _Else ->
            {'ok', 200, [{}], "[{\"number\": \"+19731231234\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}, {\"number\": \"+19734564567\",\"product\": \"IBN\",\"country\": \"USA\",\"location\": \"CA-PITTSBURG\"}]"}
    end;
make_inventory_request(Options, #{a3 := A3}= _Country) ->
    Product = classify_query(Options, A3),
    Headers = [{"Accept", "*/*"}
              ,{"Authorization", bearer_token()}
              ,{"Content-Type", "application/json"}
              ],
    Url = get_inventory_url(Product, A3),
    lager:debug("Sending bics request url: ~p", [Url]),
    kz_http:get(Url, Headers).

-spec bearer_token() -> kz_term:ne_binary().
bearer_token() ->
    list_to_binary([<<"Bearer">>, <<" ">>, ?BICS_BEARER_TOKEN]).