%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle client requests for phone_number documents using BICS api
%%% @author Kyle Diedrick
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bics).
-behaviour(knm_gen_carrier).

-include("knm.hrl").

% -export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-define(BICS_BASE_URL, "https://mynumbers.api.bics.com").

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
    case make_inventory_request("FRA") of
        {'ok', _Resp, _RespHeaders, Body} ->
            lager:debug("BICS response ~p: ~p", [_Resp, Body]),
            {'ok', process_numbers_search_resp(Body, Options)};
        {'error', _R} ->
            lager:debug("BICS response: ~p", [_R]),
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
    lager:error("received response from bics ~p", [JObjs]),
    process_response(JObjs, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_response(kz_json:objects(), knm_carriers:options()) ->
                              {'ok', knm_number:knm_numbers()}.
process_response(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    Resp = [N || JObj <- JObjs,
                 N <- [response_jobj_to_number(JObj, QID)]
           ],
    lager:error("res ~p", [Resp]),
    Resp.

response_jobj_to_number(JObj, QID) ->
    % Num = kz_json:get_binary_value(<<"number">>, JObj),
    Num = list_to_binary([<<"1">>
                         ,kz_json:get_binary_value(<<"number">>, JObj)
                         ]),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.


%%------------------------------------------------------------------------------
%% @doc Get the URL for requesting the number inventory
%% @end
%%------------------------------------------------------------------------------
-spec get_inventory_url(string(), string()) -> string().
get_inventory_url(Product, Country) ->
    ?BICS_BASE_URL ++ "/availablenumbers?product=" ++ Product ++ "&country=" ++ Country.

make_inventory_request(Country) ->
    make_inventory_request("IBN", Country).
make_inventory_request(Product, Country) ->
    Headers = [{"Accept", "*/*"}
              
              ,{"Content-Type", "application/json"}
              ],
    kz_http:get(get_inventory_url(Product, Country), Headers).