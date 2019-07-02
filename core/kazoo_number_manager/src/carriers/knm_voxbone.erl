%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle client requests for phone_number documents using the voxbone api
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voxbone).

-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").
-include("knm_voxbone.hrl").

%%% API

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
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} | {'error', any()}.
check_numbers(_Numbers) ->
    {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the voxbone system for a quantity of available numbers
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), kz_term:api_pos_integer(), knm_carriers:options()) -> {'ok', knm_number:numbers()} | {'error', any()}.
find_numbers(Number, Quantity, Options) ->
    Country = knm_iso3166_util:country(proplists:get_value('country', Options)),
    find_numbers(Number, Quantity, Options, Country).

-spec find_numbers(kz_term:ne_binary(), kz_term:api_pos_integer(), knm_carriers:options(), knm_iso3166_util:country()) -> {'ok', knm_number:phone_numbers()} | {'error', any()}.
find_numbers(_Number, _Quantity, Options, #{a3 := A3}=_Country) ->
    Qty = lists:min([Q || {'quantity', Q} <- Options]),
    SearchOptions = [{'didType', classify_query(maps:from_list(Options), A3)}
                    ,{'countryCodeA3', A3}
                    ,{'areaCode', knm_search:prefix(Options)}
                    ],
    search(Qty, Options, SearchOptions).

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) -> Number.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number:knm_number().
disconnect_number(Number) ->
    {'ok', Response} = fetch_did(Number),
    Numbers = kz_json:get_values(<<"dids">>, Response),
    release(Numbers),
    Number.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

%%% Internal Functions

%%------------------------------------------------------------------------------
%% @doc Determine Voxbone didType and availability
%% @end
%%------------------------------------------------------------------------------
-spec classify_query(map(), kz_term:ne_binary()) -> kz_term:ne_binary().
classify_query(#{'dialcode' := <<"+1">>, 'prefix' := Prefix}, _A3)
    when ?IS_US_TOLLFREE(Prefix)
        orelse ?IS_US_TOLLFREE_WILDCARD(Prefix) ->
    <<"TOLL_FREE">>;
classify_query(#{'prefix' := Prefix}, _A3)
        when ?IS_UIFN_TOLLFREE(Prefix) ->
    <<"TOLL_FREE">>;
classify_query(_Options, _A3) ->
    <<"GEOGRAPHIC">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_cart(kz_term:proplist()) -> map().
create_cart(Options) ->
    lager:debug("[~s] initializing Cart", [?MODULE]),
    Body = kz_json:from_list([{<<"customerReference">>, knm_search:account_id(Options)}
                             ,{<<"description">>, knm_search:query_id(Options)}
                             ]),
    {'ok', Response} = knm_voxbone_util:voxbone_request('put', <<"ordering/cart">>, [], Body),
    Cart = kz_json:get_value(<<"cart">>, Response),
    #{id => kz_json:get_value(<<"cartIdentifier">>, Cart), query_id => knm_search:query_id(Options)}.

%%------------------------------------------------------------------------------
%% @doc verify order can be fulfilled and initialize cart
%% @end
%%------------------------------------------------------------------------------
-spec search(kz_term:api_pos_integer(), kz_term:proplist(), kz_term:proplist()) -> {'error', any()} | {'ok', knm_number:numbers()}.
search(Quantity, Options, SearchOptions) ->
    Balance = account_balance(),
    #{total := Total, order :=  Order, cost := Cost} = do_search(Quantity, SearchOptions),
    case {Total < Quantity, Balance}  of
        %% We can't fulfill the quantity so return
        {_, {'error', _}} -> knm_errors:unspecified("please contact the carrier administrator", -61);
        {_, Balance} when Balance < Cost -> knm_errors:unspecified("please contact the carrier administrator", -61);
        {'true', Balance} when Balance >= Cost -> knm_errors:unspecified("insufficient inventory to sastisfy request", 404);
        {'false', Balance} when Balance >= Cost ->
            Cart = create_cart(Options),
            maybe_start_order(Cart, Order)
    end.

%%------------------------------------------------------------------------------
%% @doc get and process available DID quantities
%% @end
%%------------------------------------------------------------------------------
-spec do_search(kz_term:api_pos_integer(), kz_term:proplist()) -> {'error', any()} | map().
do_search(Quantity, SearchOptions) ->
    QueryString = knm_voxbone_util:required_params() ++  SearchOptions,
    case knm_voxbone_util:voxbone_request('get', <<"inventory/didgroup">>, QueryString) of
        {'error', Reason} -> {'error',  Reason};
        {'ok', DidGroups} -> process_search_results(Quantity, DidGroups)
    end.

%%------------------------------------------------------------------------------
%% @doc reserve quantities from DID groups and build order
%% @end
%%------------------------------------------------------------------------------
-spec process_search_results(kz_term:api_pos_integer(), kz_json:object()) -> map().
process_search_results(Quantity, Response) ->
    DidGroups = kz_json:get_value(<<"didGroups">>, Response, kz_json:new()),
    resolve_order_quantities(DidGroups, Quantity, #{total => 0, order => [], cost => 0}).

%%------------------------------------------------------------------------------
%% @doc reconcile a quantity of DIDs to purchase from a given DIDGroup
%% @end
%%------------------------------------------------------------------------------
-spec resolve_order_quantities(kz_json:objects(), kz_term:api_pos_integer(), kz_term:proplist()) ->  map().
resolve_order_quantities([], _Remaining, Acc) ->
    Acc;
resolve_order_quantities(_DidGroups, 0=Remaining, Acc) ->
    resolve_order_quantities([], Remaining, Acc);
resolve_order_quantities([DidGroup | Rest], Remaining, #{total :=Total, order := Order, cost := Cost}=_Acc) ->
    DidGroupId = kz_json:get_value(<<"didGroupId">>, DidGroup),
    Available = kz_json:get_value(<<"stock">>, DidGroup),
    UnitPrice = kz_json:get_value(<<"setup100">>, DidGroup) + kz_json:get_value(<<"monthly100">>, DidGroup),
    {Remaining2, Reserve} = maybe_reserve_did_qty(Available, Remaining),
    NewCost = Reserve * (UnitPrice / 100),
    lager:debug("ordering ~p of ~p dids from didGroup ~p.", [Reserve, Available, DidGroupId]),
    resolve_order_quantities(Rest, Remaining2, #{total => Total+Reserve, order => Order ++ [{DidGroupId, Reserve}], cost => NewCost + Cost}).

%%------------------------------------------------------------------------------
%% @doc determine what amount from available inventory can be reserved
%% @end
%%------------------------------------------------------------------------------
-spec maybe_reserve_did_qty(kz_term:api_pos_integer(), kz_term:api_pos_integer()) -> {kz_term:api_pos_integer(), kz_term:api_pos_integer()}.
maybe_reserve_did_qty(Available, Need) when Available < Need->
    {Need - Available, Available};
maybe_reserve_did_qty(_Available, Need) ->
     {0, Need}.

%%------------------------------------------------------------------------------
%% @doc attempt to add quantities to cart and checkout
%% @end
%%------------------------------------------------------------------------------
-spec maybe_start_order(map(), [{kz_term:api_pos_integer(), kz_term:api_pos_integer()}]) -> {'error', any()} | {'ok', knm_number:phone_numbers()}.
maybe_start_order(#{id := CartId, query_id := QueryId}=Cart, Order) ->
    lager:debug("initializing order for query ~p with cart ~p", [QueryId, CartId]),
    case maybe_add_to_cart(Order, Cart) of
        {'error', Msg} -> {'error', Msg};
        _ -> checkout(Cart)
    end.

%%------------------------------------------------------------------------------
%% @doc add did group quantities to cart and checkout.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_to_cart([{kz_term:api_pos_integer(), kz_term:api_pos_integer()}], map()) -> 'ok' | {'error', any()}.
maybe_add_to_cart([], _Cart) ->
    'ok';
maybe_add_to_cart([{DidGroupId, Quantity} | Rest], #{id := CartId, query_id := _QueryId}=Cart) ->
    Item = [{<<"didGroupId">>, DidGroupId}
           ,{<<"quantity">>, Quantity}
           ],
    Body = kz_json:from_list_recursive([{<<"cartIdentifier">>, CartId}
                                       ,{<<"didCartItem">>, Item}
                                       ]),
    case knm_voxbone_util:voxbone_request('post', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary,"/product">>, [], Body) of
        {'ok', _} -> maybe_add_to_cart(Rest, Cart);
        {'error', Message} ->
            maybe_destroy_cart(Cart),
            knm_errors:unspecified(Message, 503)
    end.

%%------------------------------------------------------------------------------
%% @doc checkout the order and generate the DID summary
%% @end
%%------------------------------------------------------------------------------
-spec checkout(map()) -> {'error', any()} | {'ok', knm_number:phone_numbers()}.
checkout(#{id := CartId, query_id := _QueryId}=Cart) ->
    case knm_voxbone_util:voxbone_request('get', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary,"/checkout">>, []) of
        {'error', Message} ->
            maybe_destroy_cart(Cart),
            knm_errors:unspecified(Message, 503);
        {'ok',  Response} ->
            generate_number_summary(Cart, kz_json:get_value(<<"productCheckoutList">>, Response))
    end.

%%------------------------------------------------------------------------------
%% @doc extract actual DIDs from order and normalize to knm search results
%% @end
%%------------------------------------------------------------------------------
-spec generate_number_summary(map(), kz_json:object()) -> {'error', any()} | {'ok', knm_number:numbers()}.
generate_number_summary(#{id := _CartId, query_id := _QueryId}=Cart, OrderSummary) ->
    generate_number_summary(Cart, OrderSummary, []).

-spec generate_number_summary(map(), kz_json:object(), kz_term:objects()) -> {'error', any()} | {'ok', knm_number:numbers()}.
generate_number_summary(#{id := _CartId, query_id := _QueryId}=_Cart, [], Acc) ->
    {'ok', Acc};
generate_number_summary(#{id := _CartId, query_id := QueryId}=Cart, [Order | Rest], Acc) ->
    case kz_json:get_value(<<"productType">>, Order) =:= <<"DID">> of
        'false' ->
            generate_number_summary(Cart, Rest, Acc);
        'true' ->
            Options = [{<<"orderReference">>, kz_json:get_value(<<"orderReference">>, Order)}
                      ] ++ knm_voxbone_util:required_params(),
            {'ok', DIDs} = knm_voxbone_util:voxbone_request('get', <<"inventory/did">>, Options),
            KNMs = order_to_KNM(QueryId, DIDs),
            generate_number_summary(Cart, Rest, KNMs ++ Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc convert voxbone dids into knm phone numbers
%% @end
%%--------------------------------------------------
-spec order_to_KNM(kz_term:ne_binary(), kz_json:objects()) -> {'ok', knm_number:numbers()}.
order_to_KNM(QueryId, DIDs) ->
    JObjs = kz_json:get_value(<<"dids">>, DIDs),
   order_to_KNM(QueryId, JObjs, []).

-spec order_to_KNM(kz_term:ne_binary(), kz_json:objects(), knm_number:phone_numbers()) -> {'ok', knm_number:numbers()}.
order_to_KNM(_QueryId, [], Acc) ->
    Acc;
order_to_KNM(QueryId, [DID | Rest], Acc) ->
    MasterAccountId = kapps_config:get(<<"accounts">>, <<"master_account_id">>),
    Options = [{'auth_by', MasterAccountId}
              ,{'assign_to', MasterAccountId}
              ,{'state', ?NUMBER_STATE_AVAILABLE}
              ,{'carrier_data', DID}
              ,{'module_name', <<"knm_voxbone">>}],
    DID2 = kz_json:get_value(<<"e164">>, DID),
    Number = {DID2, ?MODULE, ?NUMBER_STATE_AVAILABLE, DID},
    % Add number to database to ensure inventory is updated before acquire_number stub
    {'ok', _} = knm_number:create(DID2, Options),
    order_to_KNM(QueryId, Rest, [{QueryId, Number} | Acc]).

%%------------------------------------------------------------------------------
%% @doc cleanup voxbone cart artifact when order can't be satisfied.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_destroy_cart(map()) -> none.
maybe_destroy_cart(#{id := CartId, query_id := _QueryId}=_Cart) ->
    {'ok', _} = knm_voxbone_util:voxbone_request('delete', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary>>, [], []).

%%------------------------------------------------------------------------------
%% @doc fetches DID metadata from voxbone portal
%% @end
%%------------------------------------------------------------------------------
-spec fetch_did(knm_number:phone_number()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch_did(Number) ->
    Options = kz_json:from_list([{<<"e164pattern">>, knm_voxbone_util:to_voxbone_pattern(Number)}
                                ]) ++ knm_voxbone_util:required_params(),
    knm_voxbone_util:voxbone_request('get', <<"inventory/did">>, Options).

%%------------------------------------------------------------------------------
%% @doc release number back to voxbone
%% @end
%%------------------------------------------------------------------------------
-spec release(kz_json:object()) -> 'ok'.
release(DID) ->
    NumberId = kz_json:get_value(<<"didId">>,  DID),
    Body = kz_json:from_list([{'didIds', [NumberId]}]),
    knm_voxbone_util:voxbone_request('delete', <<"ordering/cancel">>, knm_voxbone_util:required_params(), Body).

%%------------------------------------------------------------------------------
%% @doc fetch account balance
%% @end
%%------------------------------------------------------------------------------
-spec account_balance() -> kz_term:api_float() | {'error', any()}.
account_balance() ->
    case knm_voxbone_util:voxbone_request('get', <<"ordering/accountbalance">>, []) of
        {'error', Msg} -> knm_errors:unspecified(Msg, 500);
        {'ok', Response} ->
            JObj = kz_json:get_value(<<"accountBalance">>, Response),
            kz_json:get_value(<<"balance">>, JObj)
    end.
