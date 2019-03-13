%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_item).

-export([item_plan/1
        ,set_item_plan/2
        ]).
-export([category_name/1
        ,set_category_name/2
        ]).
-export([item_name/1
        ,set_item_name/2
        ]).
-export([quantity/1
        ,set_quantity/2
        ]).
-export([billable_quantity/1
        ,set_billable_quantity/2
        ]).
-export([billing_rate/1
        ,set_billing_rate/2
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
-export([changes/1
        ,set_changes/2
        ]).
-export([taxes/1
        ,set_taxes/2
        ]).
-export([display_name/1]).
-export([rate/1]).
-export([minimum/1
        ,maximum/1
        ]).
-export([masquerade_as/1]).
-export([should_cascade/1]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).

-export([create/4]).
-export([reset/1]).
-export([is_masquerading/1]).
-export([has_changes/1]).
-export([has_additions/1]).
-export([has_billable_additions/1]).

-include("services.hrl").

-record(kz_service_item, {category_name :: kz_term:api_binary()
                         ,item_name :: kz_term:api_binary()
                         ,quantity = 0 :: non_neg_integer()
                         ,billable_quantity = 0 :: non_neg_integer()
                         ,billing_rate = 0.0 :: float()
                         ,single_discount_rate = 0.00 :: float()
                         ,cumulative_discount_quantity = 0 :: non_neg_integer()
                         ,cumulative_discount_rate = 0.00 :: float()
                         ,taxes = kz_json:new() :: kz_json:object()
                         ,changes = 'undefined' :: kz_term:api_object()
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
-spec billing_rate(item()) -> float().
billing_rate(#kz_service_item{billing_rate=Rate}) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_billing_rate(item(), kz_term:api_binary() | float()) -> item().
set_billing_rate(#kz_service_item{}=Item, 'undefined') ->
    Item#kz_service_item{billing_rate=0.0};
set_billing_rate(#kz_service_item{}=Item, Rate) ->
    Item#kz_service_item{billing_rate=kz_term:to_float(Rate)}.

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
-spec changes(item()) -> kz_term:api_ne_binaries().
changes(#kz_service_item{changes=Changes}) -> Changes.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_changes(item(), kz_term:api_object()) -> item().
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
-spec display_name(item()) -> kz_term:api_binary().
display_name(Item) ->
    kzd_item_plan:name(item_plan(Item), default_display_name(Item)).

-spec default_display_name(item()) -> kz_term:ne_binary().
default_display_name(Item) ->
    CategoryName =
        kz_term:to_binary(
          category_name(Item)
         ),
    ItemName =
        kz_term:to_binary(
          masquerade_as(Item)
         ),
    case kzd_service_plan:all_items_key() =:= item_name(Item) of
        'true' -> CategoryName;
        'false' -> <<CategoryName/binary, "/", ItemName/binary>>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rate(item()) -> float().
rate(Item) ->
    kzd_item_plan:rate(item_plan(Item)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec minimum(item()) -> non_neg_integer().
minimum(Item) ->
    kzd_item_plan:minimum(item_plan(Item), 0).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maximum(item()) -> non_neg_integer().
maximum(Item) ->
    kzd_item_plan:maximum(item_plan(Item), quantity(Item)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec masquerade_as(item()) -> kz_term:ne_binary().
masquerade_as(Item) ->
    kzd_item_plan:masquerade_as(item_plan(Item)
                               ,item_name(Item)
                               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_cascade(item()) -> boolean().
should_cascade(Item) ->
    kzd_item_plan:should_cascade(item_plan(Item)).

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
          ,{<<"item">>, masquerade_as(Item)}
          ,{<<"name">>, display_name(Item)}
          ,{<<"quantity">>, quantity(Item)}
          ,{<<"billable">>, billable_quantity(Item)}
          ,{<<"rate">>, billing_rate(Item)}
          ,{<<"discounts">>, undefine_empty(discounts(Item))}
          ,{<<"minimum">>, undefine_empty(minimum(Item))}
          ,{<<"maximum">>, undefine_empty(maximum(Item))}
          ,{<<"prorate">>, kzd_item_plan:prorate(item_plan(Item))}
          ,{<<"unprorate">>, kzd_item_plan:unprorate(item_plan(Item))}
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

-spec discounts(item()) -> kz_term:proplist().
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
    (billable_quantity(Item) * billing_rate(Item)) - calculate_discount_total(Item).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> item().
create(Services, ItemPlan, CategoryName, ItemName) ->
    Setters = [{fun set_item_plan/2, ItemPlan}
              ,{fun set_category_name/2, CategoryName}
              ,{fun set_item_name/2, ItemName}
              ],
    Routines = [fun calculate_quantity/2
               ,fun calculate_billable_quantity/2
               ,fun calculate_rate/2
               ,fun calculate_discounts/2
               ,fun log_item/2
               ],
    lists:foldl(fun(F, I) -> F(Services, I) end
               ,setters(Setters)
               ,Routines
               ).

-spec log_item(kz_services:services(), item()) -> item().
log_item(_Services, Item) ->
    case billable_quantity(Item) > 0 of
        'false' -> Item;
        'true' ->
            lager:debug("~p billable ~s/~s at ~p in ~.2f total amount with ~.2f total discounts"
                       ,[billable_quantity(Item)
                        ,category_name(Item)
                        ,item_name(Item)
                        ,rate(Item)
                        ,calculate_total(Item)
                        ,calculate_discount_total(Item)
                        ]),
            Item
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_quantity(kz_services:services(), item()) -> item().
calculate_quantity(Services, Item) ->
    ItemName = item_name(Item),
    Quantity =
        case kzd_service_plan:all_items_key() =:= ItemName of
            'true' -> get_category_quantity(Services, Item);
            'false' -> get_item_quantity(Services, Item)
        end,
    set_quantity(Item, Quantity).

-spec get_category_quantity(kz_services:services(), item()) -> non_neg_integer().
get_category_quantity(Services, Item) ->
    CategoryName = category_name(Item),
    Exceptions = kzd_item_plan:exceptions(item_plan(Item)),
    case should_cascade(Item) of
        'false' ->
            kz_services_quantities:category(Services, CategoryName, Exceptions);
        'true' ->
            kz_services_quantities:cascade_category(Services, CategoryName, Exceptions)
    end.

-spec get_item_quantity(kz_services:services(), item()) -> non_neg_integer().
get_item_quantity(Services, Item) ->
    CategoryName = category_name(Item),
    ItemName = item_name(Item),
    case should_cascade(Item) of
        'false' ->
            kz_services_quantities:item(Services, CategoryName, ItemName);
        'true' ->
            kz_services_quantities:cascade_item(Services, CategoryName, ItemName)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_billable_quantity(kz_services:services(), item()) -> item().
calculate_billable_quantity(_Services, Item) ->
    Step = kzd_item_plan:step(item_plan(Item)),
    Minimum = handle_step(minimum(Item), Step),
    Maximum = handle_step(maximum(Item), Step),
    case handle_step(quantity(Item), Step) of
        BillableQuantity when BillableQuantity < Minimum ->
            lager:debug("minimum '~s' not met with ~p, enforcing quantity ~p"
                       ,[display_name(Item), BillableQuantity, Minimum]
                       ),
            set_billable_quantity(Item, Minimum);
        BillableQuantity when BillableQuantity > Maximum ->
            lager:debug("maximum '~s' exceeded with ~p, enforcing quantity ~p"
                       ,[display_name(Item), BillableQuantity, Maximum]
                       ),
            set_billable_quantity(Item, Maximum);
        BillableQuantity ->
            set_billable_quantity(Item, BillableQuantity)
    end.


-spec handle_step(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
handle_step(Quantity, Step) ->
    kz_term:ceiling(Quantity / Step) * Step.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_rate(kz_services:services(), item()) -> item().
calculate_rate(Services, Item) ->
    Routines = [fun calculate_flat_rates/2
               ,fun calculate_flat_rate/2
               ,fun calculate_quantity_rates/2
               ,fun calculate_quantity_rate/2
               ],
    calculate_rate(Services, Item, Routines).

calculate_rate(_Services, Item, []) ->
    Item;
calculate_rate(Services, Item, [Routine|Routines]) ->
    case Routine(Services, Item) of
        'undefined' ->
            calculate_rate(Services, Item, Routines);
        UpdatedItem ->
            UpdatedItem
    end.

-spec calculate_flat_rates(kz_services:services(), item()) -> item() | 'undefined'.
calculate_flat_rates(_Services, Item) ->
    Rates = kzd_item_plan:flat_rates(item_plan(Item)),
    case find_tiered_rate(Rates, billable_quantity(Item)) of
        'undefined' -> 'undefined';
        Rate ->
            lager:debug("~s should be billed as a flat rate"
                       ,[display_name(Item)]
                       ),
            Setters = [{fun set_billing_rate/2, Rate}
                      ,{fun set_billable_quantity/2, 1}
                      ],
            setters(Item, Setters)
    end.

-spec calculate_flat_rate(kz_services:services(), item()) -> item() | 'undefined'.
calculate_flat_rate(_Services, Item) ->
    case kzd_item_plan:flat_rate(item_plan(Item), 0) of
        Rate when Rate =< 0 -> 'undefined';
        Rate ->
            lager:debug("~s should be billed as a flat rate"
                       ,[display_name(Item)]
                       ),
            Setters = [{fun set_billing_rate/2, Rate}
                      ,{fun set_billable_quantity/2, 1}
                      ],
            setters(Item, Setters)
    end.

-spec calculate_quantity_rates(kz_services:services(), item()) -> item().
calculate_quantity_rates(_Services, Item) ->
    Rates = kzd_item_plan:rates(item_plan(Item)),
    case find_tiered_rate(Rates, billable_quantity(Item)) of
        'undefined' -> 'undefined';
        Rate -> set_billing_rate(Item, Rate)
    end.

-spec calculate_quantity_rate(kz_services:services(), item()) -> item().
calculate_quantity_rate(_Services, Item) ->
    set_billing_rate(Item, rate(Item)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_discounts(kz_services:services(), item()) -> item().
calculate_discounts(Services, Item) ->
    calculate_single_discount(Services, Item).

-spec calculate_single_discount(kz_services:services(), item()) -> item().
calculate_single_discount(_Services, Item) ->
    DiscountPlan = kzd_item_plan:single_discount(item_plan(Item)),
    case should_set_discount(Item, DiscountPlan) of
        'false' -> Item;
        'true' ->
            Rate = kz_json:get_float_value(<<"rate">>, DiscountPlan, rate(Item)),
            set_single_discount_rate(Item, Rate)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

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
        andalso BillableQuantity >= Minimum.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
-spec is_masquerading(item()) -> boolean().
is_masquerading(Item) ->
    masquerade_as(Item) =:= item_name(Item).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(item()) -> boolean().
has_changes(Item) ->
    changes(Item) =/= 'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_additions(item()) -> boolean().
has_additions(Item) ->
    case changes(Item) of
        'undefined' -> 'false';
        Changes ->
            Key = [<<"difference">>, <<"quantity">>],
            kz_json:get_integer_value(Key, Changes, 0) > 0
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_billable_additions(item()) -> boolean().
has_billable_additions(Item) ->
    case changes(Item) of
        'undefined' -> 'false';
        Changes ->
            Key = [<<"difference">>, <<"billable">>],
            kz_json:get_integer_value(Key, Changes, 0) > 0
    end.
