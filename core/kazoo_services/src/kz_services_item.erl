%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_item).

-export([category_name/1
        ,set_category_name/2
        ]).
-export([item_name/1
        ,set_item_name/2
        ]).
-export([display_name/1
        ,set_display_name/2
        ]).
-export([quantity/1
        ,set_quantity/2
        ]).
-export([billable_quantity/1
        ,set_billable_quantity/2
        ]).
-export([rate/1
        ,set_rate/2
        ]).
-export([single_discount_rate/1
        ,set_single_discount_rate/2
        ]).
-export([cumulative_discount_quantity/1
        ,set_cumulative_discount_quantity/2
        ]).
-export([cumulative_discount_rate/1
        ,set_cumulative_discount_rate/2
        ]).
-export([activation_charge/1
        ,set_activation_charge/2
        ]).
-export([minimum/1
        ,set_minimum/2
        ]).
-export([exceptions/1
        ,set_exceptions/2
        ]).
-export([changes/1
        ,set_changes/2
        ]).
-export([taxes/1
        ,set_taxes/2
        ]).
-export([item_plan/1
        ,set_item_plan/2
        ]).
-export([masqueraded/1
        ,set_masqueraded/2
        ]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).

-export([create/4]).
-export([reset/1]).
-export([has_changes/1]).

-include("services.hrl").

-record(kz_service_item, {category_name :: kz_term:api_binary()
                         ,item_name :: kz_term:api_binary()
                         ,display_name :: kz_term:api_binary()
                         ,quantity = 0 :: non_neg_integer()
                         ,billable_quantity = 0 :: non_neg_integer()
                         ,rate = 0.0 :: float()
                         ,single_discount_rate = 0.00 :: float()
                         ,cumulative_discount_quantity = 0 :: non_neg_integer()
                         ,cumulative_discount_rate = 0.00 :: float()
                         ,activation_charge = 0.00 :: float()
                         ,minimum = 0 :: non_neg_integer()
                         ,exceptions = [] :: kz_term:ne_binaries()
                         ,taxes = kz_json:new() :: kz_json:object()
                         ,changes = 'undefined'
                         ,item_plan = kz_json:new() :: kz_json:object()
                         ,masqueraded = 'false' :: boolean()
                         }).

-opaque item() :: #kz_service_item{}.
-type setter_fun() :: {fun((item(), Value) -> item()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([item/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category_name(item()) -> kz_term:api_binary().
category_name(#kz_service_item{category_name=CategoryName}) ->
    CategoryName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_category_name(item(), kz_term:ne_binary()) -> item().
set_category_name(#kz_service_item{}=Item, CategoryName) ->
    Item#kz_service_item{category_name=CategoryName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item_name(item()) -> kz_term:api_binary().
item_name(#kz_service_item{item_name=ItemName}) ->
    ItemName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_item_name(item(), kz_term:ne_binary()) -> item().
set_item_name(#kz_service_item{}=Item, ItemName) ->
    Item#kz_service_item{item_name=ItemName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec display_name(item()) -> kz_term:api_binary().
display_name(#kz_service_item{item_name=ItemName, display_name=ItemName}) ->
    'undefined';
display_name(#kz_service_item{display_name=DisplayName}) ->
    DisplayName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_display_name(item(), kz_term:ne_binary()) -> item().
set_display_name(#kz_service_item{}=Item, DisplayName) ->
    Item#kz_service_item{display_name=DisplayName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quantity(item()) -> non_neg_integer().
quantity(#kz_service_item{quantity=Quantity}) ->
    Quantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_quantity(item(), kz_term:api_binary() | non_neg_integer()) -> item().
set_quantity(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{quantity=0};
set_quantity(#kz_service_item{}=Item, Quantity) ->
    Item#kz_service_item{quantity=kz_term:to_integer(Quantity)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billable_quantity(item()) -> non_neg_integer().
billable_quantity(#kz_service_item{billable_quantity=BillableQuantity}) ->
    BillableQuantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_billable_quantity(item(), kz_term:api_binary() | non_neg_integer()) -> item().
set_billable_quantity(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{billable_quantity=0};
set_billable_quantity(#kz_service_item{}=Item, BillableQuantity) ->
    Item#kz_service_item{billable_quantity=kz_term:to_integer(BillableQuantity)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rate(item()) -> float().
rate(#kz_service_item{rate=Rate}) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_rate(item(), kz_term:api_binary() | float()) -> item().
set_rate(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{rate=0.0};
set_rate(#kz_service_item{}=Item, Rate) ->
    Item#kz_service_item{rate=kz_term:to_float(Rate)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec single_discount_rate(item()) -> float().
single_discount_rate(#kz_service_item{single_discount_rate=Rate}) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_single_discount_rate(item(), kz_term:api_binary() | float()) -> item().
set_single_discount_rate(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{single_discount_rate=0.0};
set_single_discount_rate(#kz_service_item{}=Item, Rate) ->
    Item#kz_service_item{single_discount_rate=kz_term:to_float(Rate)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cumulative_discount_quantity(item()) -> non_neg_integer().
cumulative_discount_quantity(#kz_service_item{cumulative_discount_quantity=Quantity}) ->
    Quantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_cumulative_discount_quantity(item(), kz_term:api_binary() | non_neg_integer()) -> item().
set_cumulative_discount_quantity(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{cumulative_discount_quantity=0};
set_cumulative_discount_quantity(#kz_service_item{}=Item, Quantity) ->
    Item#kz_service_item{cumulative_discount_quantity=kz_term:to_integer(Quantity)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cumulative_discount_rate(item()) -> float().
cumulative_discount_rate(#kz_service_item{cumulative_discount_rate=Rate}) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_cumulative_discount_rate(item(), kz_term:api_binary() | float()) -> item().
set_cumulative_discount_rate(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{cumulative_discount_rate=0.0};
set_cumulative_discount_rate(#kz_service_item{}=Item, Rate) ->
    Item#kz_service_item{cumulative_discount_rate=kz_term:to_float(Rate)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activation_charge(item()) -> float().
activation_charge(#kz_service_item{activation_charge=Charge}) -> Charge.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_activation_charge(item(), kz_term:api_binary() | float()) -> item().
set_activation_charge(Item, 'undefined') ->
    Item#kz_service_item{activation_charge=0.0};
set_activation_charge(Item, Charge) ->
    Item#kz_service_item{activation_charge=kz_term:to_float(Charge)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec minimum(item()) -> non_neg_integer().
minimum(#kz_service_item{minimum=Minimum}) -> Minimum.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_minimum(item(), kz_term:api_binary() | non_neg_integer()) -> item().
set_minimum(Item, 'undefined') ->
    Item#kz_service_item{minimum=0};
set_minimum(Item, Minimum) ->
    Item#kz_service_item{minimum=kz_term:to_integer(Minimum)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec exceptions(item()) -> kz_term:ne_binaries().
exceptions(#kz_service_item{exceptions=Exceptions}) -> Exceptions.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_exceptions(item(), kz_term:ne_binaries()) -> item().
set_exceptions(Item, Exceptions) ->
    Item#kz_service_item{exceptions=Exceptions}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec changes(item()) -> kz_term:ne_binaries().
changes(#kz_service_item{changes=Changes}) -> Changes.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_changes(item(), kz_term:ne_binaries()) -> item().
set_changes(Item, Changes) ->
    Item#kz_service_item{changes=Changes}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec taxes(item()) -> kz_json:object().
taxes(#kz_service_item{taxes=Taxes}) -> Taxes.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_taxes(item(), kz_json:object()) -> item().
set_taxes(Item, Taxes) ->
    Item#kz_service_item{taxes=Taxes}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item_plan(item()) -> kz_json:object().
item_plan(#kz_service_item{item_plan=ItemPlan}) -> ItemPlan.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_item_plan(item(), kz_json:object()) -> item().
set_item_plan(Item, ItemPlan) ->
    Item#kz_service_item{item_plan=ItemPlan}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec masqueraded(item()) -> boolean().
masqueraded(#kz_service_item{masqueraded=Masqueraded}) ->
    Masqueraded.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_masqueraded(item(), boolean()) -> item().
set_masqueraded(Item, Masqueraded) ->
    Item#kz_service_item{masqueraded=Masqueraded}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> item().
empty() ->
    #kz_service_item{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> item().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(item(), setter_funs()) -> item().
setters(Item, Routines) ->
    lists:foldl(fun({Setter, Value}, I) ->
                        Setter(I, Value)
                end
               ,Item
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(item()) -> kz_json:object().
public_json(Item) ->
    Props =
        props:filter_undefined(
          [{<<"category">>, category_name(Item)}
          ,{<<"item">>, item_name(Item)}
          ,{<<"name">>, display_name(Item)}
          ,{<<"quantity">>, quantity(Item)}
          ,{<<"billable">>, billable_quantity(Item)}
          ,{<<"rate">>, rate(Item)}
          ,{<<"discounts">>, undefine_empty(discounts(Item))}
          ,{<<"minimum">>, undefine_empty(minimum(Item))}
          ,{<<"taxes">>, undefine_empty(taxes(Item))}
          ,{<<"total">>, calculate_total(Item)}
          ,{<<"changes">>, undefine_empty(changes(Item))}
          ]
         ),
    kz_json:from_list_recursive(Props).

-spec undefine_empty(Thing) -> 'undefined'|Thing.
undefine_empty(Thing) ->
    case kz_term:is_empty(Thing) of
        'true' -> 'undefined';
        'false' -> Thing
    end.

-spec discounts(item()) -> kz_term:api_proplist().
discounts(Item) ->
    SingleDiscounts =
        props:filter_empty(
          [{<<"rate">>, single_discount_rate(Item)}]
         ),
    CumulativeDiscounts =
        props:filter_empty(
          [{<<"rate">>, cumulative_discount_rate(Item)}
          ,{<<"quantity">>, cumulative_discount_quantity(Item)}
          ]
         ),
    props:filter_empty(
      [{<<"total">>, calculate_discount_total(Item)}
      ,{<<"single">>, SingleDiscounts}
      ,{<<"cumulative">>, CumulativeDiscounts}
      ]
     ).

-spec calculate_discount_total(item()) -> float().
calculate_discount_total(Item) ->
    single_discount_rate(Item)
        + (cumulative_discount_quantity(Item) * cumulative_discount_rate(Item)).

-spec calculate_total(item()) -> float().
calculate_total(Item) ->
    (billable_quantity(Item) * rate(Item)) - calculate_discount_total(Item).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> item().
create(Services, ItemPlan, CategoryName, ItemName) ->
    Quantity = get_quantity(Services, ItemPlan, CategoryName, ItemName),
    {Rate, BillableQuantity} = get_rate_at_quantity(ItemPlan, CategoryName, ItemName, Quantity),
    ActivationCharge = kzd_item_plan:activation_charge(ItemPlan),
    Minimum =  kzd_item_plan:minimum(ItemPlan),
    DisplayName = kzd_item_plan:name(ItemPlan),
    Exceptions = kzd_item_plan:exceptions(ItemPlan),
    %% allow service plans to re-map item names (IE: softphone items "as" sip_device)
    As = kzd_item_plan:masquerade_as(ItemPlan, ItemName),
    Setters = [{fun set_item_plan/2, ItemPlan}
              ,{fun set_category_name/2, CategoryName}
              ,{fun set_item_name/2, As}
              ,{fun set_display_name/2, DisplayName}
              ,{fun set_quantity/2, Quantity}
              ,{fun set_billable_quantity/2, BillableQuantity}
              ,{fun set_rate/2, Rate}
              ,{fun set_activation_charge/2, ActivationCharge}
              ,{fun set_minimum/2, Minimum}
              ,{fun set_exceptions/2, Exceptions}
               %% NOTE: Ensure rate has been set in item before this is executed!
              ,{fun maybe_set_discounts/2, ItemPlan}
              ,{fun set_masqueraded/2, ItemName =/= As}
              ],
    log_item(setters(Setters)).

-spec log_item(item()) -> item().
log_item(Item) ->
    case billable_quantity(Item) > 0 of
        'false' -> Item;
        'true' ->
            lager:debug("~p billable ~s/~s at ~p each with ~p discounts"
                       ,[billable_quantity(Item)
                        ,category_name(Item)
                        ,item_name(Item)
                        ,rate(Item)
                        ,calculate_discount_total(Item)
                        ]),
            Item
    end.

-spec get_quantity(kz_services:services(), kzd_item_plan:doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
get_quantity(Services, ItemPlan, CategoryName, ItemName) ->
    case kzd_service_plan:all_items_key() =:= ItemName of
        'true' -> get_category_quantity(Services, ItemPlan, CategoryName);
        'false' -> get_item_quantity(Services, ItemPlan, CategoryName, ItemName)
    end.

-spec get_category_quantity(kz_services:services(), kzd_item_plan:doc(), kz_term:ne_binary()) -> non_neg_integer().
get_category_quantity(Services, ItemPlan, CategoryName) ->
    Exceptions = kzd_item_plan:exceptions(ItemPlan),
    case kzd_item_plan:should_cascade(ItemPlan) of
        'false' -> kz_services_quantities:category(Services, CategoryName, Exceptions);
        'true' -> kz_services_quantities:cascade_category(Services, CategoryName, Exceptions)
    end.

-spec get_item_quantity(kz_services:services(), kzd_item_plan:doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
get_item_quantity(Services, ItemPlan, CategoryName, ItemName) ->
    case kzd_item_plan:should_cascade(ItemPlan) of
        'false' -> kz_services_quantities:item(Services, CategoryName, ItemName);
        'true' -> kz_services_quantities:cascade_item(Services, CategoryName, ItemName)
    end.

-spec get_billable_quantity(kzd_item_plan:doc(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> non_neg_integer().
get_billable_quantity(ItemPlan, CategoryName, ItemName, Quantity) ->
    case kzd_item_plan:minimum(ItemPlan) of
        Minimum when Minimum > Quantity ->
            lager:debug("minimum '~s/~s' not met with ~p, enforcing quantity ~p"
                       ,[CategoryName, ItemName, Quantity, Minimum]
                       ),
            Minimum;
        _ -> Quantity
    end.

-spec get_rate_at_quantity(kzd_item_plan:doc(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> {float(), non_neg_integer()}.
get_rate_at_quantity(ItemPlan, CategoryName, ItemName, Quantity) ->
    BillableQuantity = get_billable_quantity(ItemPlan, CategoryName, ItemName, Quantity),
    case get_rate(ItemPlan, BillableQuantity) of
        {'false', Rate} -> {Rate, BillableQuantity};
        {'true', Rate} ->
            lager:debug("~s/~s should be billed as a flat rate"
                       ,[CategoryName
                        ,ItemName
                        ]
                       ),
            {Rate, 1}
    end.

-spec get_rate(kzd_item_plan:doc(), non_neg_integer()) -> {boolean(), float()}.
get_rate(ItemPlan, BillableQuantity) ->
    case maybe_get_flat_rate(ItemPlan, BillableQuantity) of
        'undefined' -> {'false', get_quantity_rate(ItemPlan, BillableQuantity)};
        Rate -> {'true', Rate}
    end.

-spec maybe_get_flat_rate(kzd_item_plan:doc(), non_neg_integer()) -> kz_term:api_float().
maybe_get_flat_rate(ItemPlan, BillableQuantity) ->
    find_tiered_rate(kzd_item_plan:flat_rates(ItemPlan), BillableQuantity).

-spec get_quantity_rate(kzd_item_plan:doc(), non_neg_integer()) -> float().
get_quantity_rate(ItemPlan, BillableQuantity) ->
    case find_tiered_rate(kzd_item_plan:rates(ItemPlan), BillableQuantity) of
        'undefined' -> kzd_item_plan:rate(ItemPlan);
        Rate -> Rate
    end.

-type discount_routine() :: fun((kzd_item_plan:doc(), item()) ->
                                       item()).

-spec maybe_set_discounts(item(), kzd_item_plan:doc()) -> item().
maybe_set_discounts(Item, ItemPlan) ->
    Routines = [fun maybe_set_single_discount/2
               ,fun maybe_set_cumulative_discount/2
               ],
    lists:foldl(maybe_set_discounts_fold(ItemPlan)
               ,Item
               ,Routines
               ).

-spec maybe_set_discounts_fold(kzd_item_plan:doc()) ->
                                      fun((discount_routine(), item()) ->
                                                 item()).
maybe_set_discounts_fold(ItemPlan) ->
    fun(Routine, Item) ->
            Routine(Item, ItemPlan)
    end.

-spec maybe_set_single_discount(item(), kzd_item_plan:doc()) ->
                                       item().
maybe_set_single_discount(Item, ItemPlan) ->
    DiscountPlan = kzd_item_plan:single_discount(ItemPlan),
    case should_set_discount(Item, DiscountPlan) of
        'false' -> Item;
        'true' ->
            Rate = kz_json:get_float_value(<<"rate">>, DiscountPlan, rate(Item)),
            set_single_discount_rate(Item, Rate)
    end.

-spec maybe_set_cumulative_discount(item(), kzd_item_plan:doc()) -> item().
maybe_set_cumulative_discount(Item, ItemPlan) ->
    DiscountPlan = kzd_item_plan:cumulative_discount(ItemPlan),
    case should_set_discount(Item, DiscountPlan) of
        'false' -> Item;
        'true' -> set_cumulative_discount(Item, DiscountPlan)
    end.


-spec set_cumulative_discount(item(), kz_json:object()) -> item().
set_cumulative_discount(Item, DiscountPlan) ->
    BillableQuantity = billable_quantity(Item),
    CumulativeQuantity = cumulative_quantity(Item, DiscountPlan, BillableQuantity),
    CumulativeRate = cumulative_rate(Item, DiscountPlan, CumulativeQuantity),
    Routines = [{fun set_cumulative_discount_quantity/2, CumulativeQuantity}
               ,{fun set_cumulative_discount_rate/2, CumulativeRate}
               ],
    lists:foldl(fun({Setter, Value}, I) -> Setter(I, Value) end, Item, Routines).

-spec cumulative_rate(item(), kz_json:object(), non_neg_integer()) -> float().
cumulative_rate(Item, DiscountPlan, BillableQuantity) ->
    Rates = kz_json:get_value(<<"rates">>, DiscountPlan, []),
    case find_tiered_rate(Rates, BillableQuantity) of
        'undefined' -> rate(Item);
        Rate -> Rate
    end.

-spec cumulative_quantity(item(), kz_json:object(), non_neg_integer()) -> non_neg_integer().
cumulative_quantity(Item, DiscountPlan, BillableQuantity) ->
    case kz_json:get_integer_value(<<"maximum">>, DiscountPlan, 0) of
        Maximum when Maximum < BillableQuantity ->
            lager:debug("item '~s/~s' quantity ~p exceeds cumulative discount max, using ~p"
                       ,[category_name(Item)
                        ,item_name(Item)
                        ,BillableQuantity
                        ,Maximum
                        ]
                       ),
            Maximum;
        _ -> BillableQuantity
    end.

-spec should_set_discount(item(), kz_term:api_object()) -> boolean().
should_set_discount(_, 'undefined') -> 'false';
should_set_discount(Item, DiscountPlan) ->
    Rate = rate(Item),
    BillableQuantity = billable_quantity(Item),
    Minimum = kz_json:get_integer_value(<<"minimum">>, DiscountPlan, 1),
    Rate > 0
        andalso
        BillableQuantity >= Minimum.

-spec find_tiered_rate(kz_json:object(), non_neg_integer()) -> kz_term:api_float().
find_tiered_rate(Rates, Quantity) ->
    L1 = [kz_term:to_integer(K) || K <- kz_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] -> 'undefined';
        Range ->
            kz_json:get_float_value(kz_term:to_binary(hd(Range)), Rates)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset(item()) -> item().
reset(Item) ->
    Setters = [{fun set_billable_quantity/2, 0}
              ,{fun maybe_set_discounts/2, item_plan(Item)}
              ],
    setters(Item, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(item()) -> boolean().
has_changes(Item) ->
    changes(Item) =/= 'undefined'.
