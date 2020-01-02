%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents using the voxbone api
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    #{?CARRIER_INFO_MAX_PREFIX => 3}.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} | {'error', any()}.
check_numbers(_Numbers) ->
    {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc
%% Query the voxbone system for a quantity of available numbers
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), kz_term:api_pos_integer(), knm_search:options()) ->
          {'ok', knm_search:results()}.
find_numbers(Number, Quantity, Options) ->
    Country = knm_iso3166_util:country(proplists:get_value('country', Options)),
    find_numbers(Number, Quantity, Options, Country).

-spec find_numbers(kz_term:ne_binary(), kz_term:api_pos_integer(), knm_search:options(), knm_iso3166_util:country()) ->
          {'ok', knm_search:results()}.
find_numbers(_Number, _Quantity, Options, #{a3 := A3}=_Country) ->
    Qty = lists:min([Q || {'quantity', Q} <- Options]),
    Prefix = maybe_escape_wildcard(knm_search:prefix(Options)),
    SearchOptions = [{'didType', classify_query(maps:from_list(Options), A3)}
                    ,{'countryCodeA3', A3}
                    ,{'areaCode', Prefix}
                    ],
    search(Qty, Options, SearchOptions).

%%------------------------------------------------------------------------------
%% @doc
%% Convert kazoo * wildcard to voxbone %
%% @end
%%------------------------------------------------------------------------------
-spec maybe_escape_wildcard(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_escape_wildcard(<<A:1/binary,B:1/binary,"*">>) ->
    <<A/binary,B/binary,"%25">>;
maybe_escape_wildcard(Prefix) ->
    Prefix.

%%------------------------------------------------------------------------------
%% @doc
%% Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PN) -> PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(PN) ->
    {'ok', Response} = fetch_did(PN),
    {Numbers, _Keys} = kz_json:get_values(<<"dids">>, Response),
    _ = release(Numbers),
    PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_PN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

%%% Internal Functions

%%------------------------------------------------------------------------------
%% @doc
%% Determine Voxbone didType and availability
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
%% Create voxbone cart object to start order.
%% @end
%%------------------------------------------------------------------------------
-spec create_cart(kz_term:proplist()) -> map().
create_cart(Options) ->
    lager:debug("[~s] initializing Cart", [?MODULE]),
    Body = kz_json:from_list([{<<"customerReference">>, knm_search:account_id(Options)}
                             ,{<<"description">>, knm_search:query_id(Options)}
                             ]),
    case knm_voxbone_util:voxbone_request('put', <<"ordering/cart">>, [], kz_json:encode(Body)) of
        {'ok', Response} ->
            Cart = kz_json:get_value(<<"cart">>, Response),
            #{id => kz_json:get_value(<<"cartIdentifier">>, Cart), query_id => knm_search:query_id(Options)};
        {'error', Msg} -> knm_errors:unspecified(Msg, 500)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Verify order can be fulfilled and initialize cart
%% @end
%%------------------------------------------------------------------------------
-spec search(kz_term:api_pos_integer(), kz_search:options(), kz_term:proplist()) ->
          {'ok', knm_search:results()}.
search(Quantity, Options, SearchOptions) ->
    Balance = account_balance(),
    #{total := Total
     ,order := Order
     , cost := Cost
     } = do_search(Quantity, SearchOptions),

    case {Total < Quantity, Balance}  of
        %% We can't fulfill the quantity so return
        {_, Balance} when Balance < Cost -> knm_errors:unspecified("please contact the carrier administrator", -61);
        {'true', Balance} when Balance >= Cost -> knm_errors:unspecified("insufficient inventory to satisfy request", 404);
        {'false', Balance} when Balance >= Cost ->
            Cart = create_cart(Options),
            maybe_start_order(Cart, Order)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Fetch and process available DID quantities
%% @end
%%------------------------------------------------------------------------------
-spec do_search(kz_term:api_pos_integer(), kz_term:proplist()) -> {'error', any()} | map().
do_search(Quantity, SearchOptions) ->
    QueryString = knm_voxbone_util:required_params() ++ SearchOptions,
    case knm_voxbone_util:voxbone_request('get', <<"inventory/didgroup">>, QueryString) of
        {'error', Reason} -> {'error',  Reason};
        {'ok', DidGroups} -> process_search_results(Quantity, DidGroups)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Reserve quantities from DID groups and build order
%% @end
%%------------------------------------------------------------------------------
-spec process_search_results(kz_term:api_pos_integer(), kz_json:object()) -> map().
process_search_results(Quantity, Response) ->
    DidGroups = kz_json:get_value(<<"didGroups">>, Response, kz_json:new()),
    resolve_order_quantities(DidGroups, Quantity, #{total => 0, order => [], cost => 0}).

%%------------------------------------------------------------------------------
%% @doc
%% Reconcile a quantity of DIDs to purchase from a given DIDGroup
%% @end
%%------------------------------------------------------------------------------
-spec resolve_order_quantities(kz_json:objects(), kz_term:api_pos_integer(), map()) ->  map().
resolve_order_quantities([], _Remaining, Acc) -> Acc;
resolve_order_quantities(_DidGroups, 0, Acc) -> Acc;
resolve_order_quantities([DidGroup | Rest], Remaining, #{total :=Total, order := Order, cost := Cost}=_Acc) ->
    DidGroupId = kz_json:get_value(<<"didGroupId">>, DidGroup),
    Available = kz_json:get_value(<<"stock">>, DidGroup),
    UnitPrice = kz_json:get_value(<<"setup100">>, DidGroup) + kz_json:get_value(<<"monthly100">>, DidGroup),
    {Remaining2, Reserve} = maybe_reserve_did_qty(Available, Remaining),
    NewCost = Reserve * (UnitPrice / 100),
    lager:debug("ordering ~p of ~p dids from didGroup ~p.", [Reserve, Available, DidGroupId]),
    resolve_order_quantities(Rest, Remaining2, #{total => Total+Reserve
                                                ,order => Order ++ [{DidGroupId, Reserve}]
                                                ,cost => NewCost + Cost
                                                }).

%%------------------------------------------------------------------------------
%% @doc
%% Determine what amount from available inventory can be reserved
%% @end
%%------------------------------------------------------------------------------
-spec maybe_reserve_did_qty(kz_term:api_pos_integer(), pos_integer()) ->
          {non_neg_integer(), non_neg_integer()}.
maybe_reserve_did_qty('undefined', Need) -> {0, Need};
maybe_reserve_did_qty(Available, Need) when Available < Need ->
    {Need - Available, Available};
maybe_reserve_did_qty(_Available, Need) ->
    {0, Need}.

%%------------------------------------------------------------------------------
%% @doc Attempt to add quantities to cart and checkout
%% @end
%%------------------------------------------------------------------------------
-spec maybe_start_order(map(), [{kz_term:api_pos_integer(), kz_term:api_pos_integer()}]) ->
          {'ok', knm_search:results()}.
maybe_start_order(#{id := _CartId, query_id := _QueryId}=Cart, Order) ->
    lager:debug("initializing order for query ~s with cart ~s", [_QueryId, _CartId]),
    maybe_add_to_cart(Order, Cart),
    checkout(Cart).

%%------------------------------------------------------------------------------
%% @doc
%% Add did group quantities to cart and checkout.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_to_cart([{kz_term:api_pos_integer(), kz_term:api_pos_integer()}], map()) -> 'ok'.
maybe_add_to_cart([], _Cart) -> 'ok';
maybe_add_to_cart([{DidGroupId, Quantity} | Rest], #{id := CartId, query_id := _QueryId}=Cart) ->
    Item = [{<<"didGroupId">>, DidGroupId}
           ,{<<"quantity">>, Quantity}
           ],
    Body = kz_json:from_list_recursive([{<<"cartIdentifier">>, CartId}
                                       ,{<<"didCartItem">>, Item}
                                       ]),
    case knm_voxbone_util:voxbone_request('post'
                                         ,<<"ordering/cart/",(kz_term:to_binary(CartId))/binary,"/product">>
                                         ,[]
                                         ,kz_json:encode(Body)
                                         )
    of
        {'ok', _} -> maybe_add_to_cart(Rest, Cart);
        {'error', Message} ->
            maybe_destroy_cart(Cart),
            knm_errors:unspecified(Message, 503)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Checkout the order and generate the DID summary
%% @end
%%------------------------------------------------------------------------------
-spec checkout(map()) -> {'ok', knm_search:results()}.
checkout(#{id := CartId, query_id := _QueryId}=Cart) ->
    case knm_voxbone_util:voxbone_request('get'
                                         ,<<"ordering/cart/", (kz_term:to_binary(CartId))/binary, "/checkout">>
                                         ,[]
                                         )
    of
        {'error', Message} ->
            maybe_destroy_cart(Cart),
            knm_errors:unspecified(Message, 503);
        {'ok',  Response} ->
            generate_number_summary(Cart, kz_json:get_value(<<"productCheckoutList">>, Response))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Extract actual DIDs from order and normalize to knm search results
%% @end
%%------------------------------------------------------------------------------
-spec generate_number_summary(map(), kz_json:objects()) ->
          {'ok', knm_search:results()}.
generate_number_summary(#{id := _CartId, query_id := _QueryId}=Cart, OrderSummary) ->
    generate_number_summary(Cart, OrderSummary, []).

-spec generate_number_summary(map(), kz_json:objects(), knm_number:numbers()) ->
          {'ok', knm_search:results()}.
generate_number_summary(#{id := _CartId, query_id := _QueryId}=_Cart, [], Acc) ->
    {'ok', Acc};
generate_number_summary(#{id := _CartId, query_id := QueryId}=Cart, [Order | Rest], Acc) ->
    case kz_json:get_value(<<"productType">>, Order) =:= <<"DID">> of
        'false' ->
            generate_number_summary(Cart, Rest, Acc);
        'true' ->
            Options = [{'orderReference', kz_json:get_value(<<"orderReference">>, Order)}
                       | knm_voxbone_util:required_params()
                      ],
            {'ok', DIDs} = knm_voxbone_util:voxbone_request('get', <<"inventory/did">>, Options),
            KNMs = order_to_search_result(QueryId, DIDs),
            generate_number_summary(Cart, Rest, KNMs ++ Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Convert voxbone dids into knm phone numbers
%% @end
%%------------------------------------------------------------------------------
-spec order_to_search_result(kz_term:ne_binary(), kz_json:object()) -> knm_search:results().
order_to_search_result(QueryId, DIDs) ->
    JObjs = kz_json:get_value(<<"dids">>, DIDs),
    order_to_search_result(QueryId, JObjs, []).

-spec order_to_search_result(kz_term:ne_binary(), kz_json:objects(), knm_search:results()) ->
          knm_search:results().
-ifndef(TEST).
order_to_search_result(_QueryId, [], Acc) ->
    Acc;
order_to_search_result(QueryId, [DID | Rest], Acc) ->
    MasterAccountId = kapps_config:get(<<"accounts">>, <<"master_account_id">>),
    Options = [{'auth_by', MasterAccountId}
              ,{'assign_to', MasterAccountId}
              ,{'state', ?NUMBER_STATE_AVAILABLE}
              ,{'carrier_data', DID}
              ,{'module_name', <<"knm_voxbone">>}],
    E164 = kz_json:get_ne_binary_value(<<"e164">>, DID),

    %% Add number to database to ensure inventory is updated before acquire_number stub
    {'ok', _} = knm_number:create(E164, Options),

    NumberReturn = {E164, ?MODULE, ?NUMBER_STATE_AVAILABLE, DID},

    order_to_search_result(QueryId, Rest, [{QueryId, NumberReturn} | Acc]).
-else.
order_to_search_result(_QueryId, [], Acc) ->
    Acc;
order_to_search_result(QueryId, [DID | Rest], Acc) ->
    DID2 = kz_json:get_value(<<"e164">>, DID),
    Number = {DID2, ?MODULE, ?NUMBER_STATE_AVAILABLE, DID},
    order_to_search_result(QueryId, Rest, [{QueryId, Number} | Acc]).
-endif.
%%------------------------------------------------------------------------------
%% @doc
%% Cleanup voxbone cart artifact when order can't be satisfied.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_destroy_cart(map()) -> 'ok'.
maybe_destroy_cart(#{id := CartId, query_id := _QueryId}=_Cart) ->
    case knm_voxbone_util:voxbone_request('delete', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary>>, []) of
        {'error', _Msg} -> knm_errors:unspecified("there was an error destroying cart", 500);
        _ -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Fetches DID metadata from voxbone portal
%% @end
%%------------------------------------------------------------------------------
-spec fetch_did(knm_phone_number:record()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch_did(PN) ->
    Options = [{'e164Pattern', knm_voxbone_util:to_voxbone_pattern(knm_phone_number:number(PN))}
               | knm_voxbone_util:required_params()
              ],
    case knm_voxbone_util:voxbone_request('get', <<"inventory/did">>, Options) of
        {'error', _Msg} -> knm_errors:by_carrier(?MODULE, "there was an error fetching metadata for number ", PN);
        Response -> Response
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Release number back to voxbone
%% @end
%%------------------------------------------------------------------------------
-spec release(kz_json:objects() | kz_json:object()) -> 'ok'.
release(DIDs) when is_list(DIDs) ->
    lists:foreach(fun release/1, DIDs);
release(DID) ->
    NumberId = kz_json:get_value(<<"didId">>, DID),
    Body = kz_json:from_list([{<<"didIds">>, [NumberId]}]),
    knm_voxbone_util:voxbone_request('delete'
                                    ,<<"ordering/cancel">>
                                    ,knm_voxbone_util:required_params()
                                    ,kz_json:encode(Body)
                                    ).

%%------------------------------------------------------------------------------
%% @doc
%% Fetch account balance
%% @end
%%------------------------------------------------------------------------------
-spec account_balance() -> kz_term:api_float().
account_balance() ->
    case knm_voxbone_util:voxbone_request('get', <<"ordering/accountbalance">>, []) of
        {'error', Msg} -> knm_errors:unspecified(Msg, 500);
        {'ok', Response} ->
            JObj = kz_json:get_value(<<"accountBalance">>, Response),
            kz_json:get_float_value(<<"balance">>, JObj)
    end.
