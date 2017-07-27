%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_plan).

-export([fetch/2]).
-export([activation_charges/3]).
-export([create_items/3]).

-include("services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a vendor database and service plan id, fetch the document.
%% Merge any plan overrides into the plan property.
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), ne_binary()) -> kzd_service_plan:api_doc().
fetch(PlanId, VendorId) ->
    VendorDb = kz_util:format_account_db(VendorId),
    case fetch_plan(VendorDb, PlanId) of
        {'ok', ServicePlan} ->
            ?LOG_DEBUG("found service plan ~s/~s", [VendorDb, PlanId]),
            ServicePlan;
        {'error', _R} ->
            lager:debug("unable to open service plan ~s/~s: ~p", [VendorDb, PlanId, _R]),
            'undefined'
    end.

-ifdef(TEST).
fetch_plan(?A_MASTER_ACCOUNT_DB, ?A_MASTER_PLAN_ID) ->
    {ok, kz_services_test:fixture("a_master_plans.json")}.
-else.
fetch_plan(VendorDb, PlanId) ->
    kz_datamgr:open_cache_doc(VendorDb, PlanId).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), kzd_service_plan:doc()) -> float().
activation_charges(CategoryId, ItemId, ServicePlan) ->
    case kzd_service_plan:item_activation_charge(ServicePlan, CategoryId, ItemId, 'undefined') of
        'undefined' ->
            kzd_service_plan:category_activation_charge(ServicePlan, CategoryId, 0.0);
        Charge -> Charge
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_items(kzd_service_plan:doc(), kz_service_items:items(), kz_services:services()) ->
                          kz_service_items:items().
-spec create_items(kzd_service_plan:doc(), kz_service_items:items(), kz_services:services()
                  ,ne_binary(), ne_binary()
                  ) ->
                          kz_service_items:items().
create_items(ServicePlan, ServiceItems, Services) ->
    lists:foldl(fun({CategoryId, ItemId}, SIs) ->
                        create_items(ServicePlan, SIs, Services, CategoryId, ItemId)
                end
               ,ServiceItems
               ,get_plan_items(ServicePlan, Services)
               ).

create_items(ServicePlan, ServiceItems, Services, CategoryId, ItemId) ->
    ItemPlan = kzd_service_plan:item(ServicePlan, CategoryId, ItemId),
    create_items(ServicePlan, ServiceItems, Services, CategoryId, ItemId, ItemPlan).

create_items(ServicePlan, ServiceItems, Services, CategoryId, _ItemId, 'undefined') ->
    case kz_services:select_bookkeeper(Services) of
        'kz_bookkeeper_http' ->
            ItemPlan = get_generic_item_plan(ServicePlan, CategoryId),
            create_items(ServicePlan, ServiceItems, Services, CategoryId, _ItemId, ItemPlan);
        _Bookkeeper ->
            AccountId = kzd_service_plan:account_id(ServicePlan),
            ServicePlanId = kz_doc:id(ServicePlan),
            lager:warning("unable to create service plan item ~s/~s from ~s/~s for bookkeeper ~s"
                         ,[CategoryId, _ItemId, AccountId, ServicePlanId, _Bookkeeper]
                         ),
            ServiceItems
    end;
create_items(ServicePlan, ServiceItems, Services, CategoryId, ItemId, ItemPlan) ->
    {Rate, Quantity} = get_rate_at_quantity(CategoryId, ItemId, ItemPlan, Services),
    lager:debug("for ~s/~s, found rate ~p for quantity ~p", [CategoryId, ItemId, Rate, Quantity]),

    Charge = activation_charges(CategoryId, ItemId, ServicePlan),
    Min = kzd_service_plan:item_minimum(ServicePlan, CategoryId, ItemId),
    Name = kzd_service_plan:item_name(ServicePlan, CategoryId, ItemId),
    Exc = kzd_service_plan:item_exceptions(ServicePlan, CategoryId, ItemId),

    %% allow service plans to re-map item names (IE: softphone items "as" sip_device)
    As = kzd_item_plan:masquerade_as(ItemPlan, ItemId),
    lager:debug("item ~s masquerades as ~s", [ItemId, As]),

    Routines = [fun(I) -> kz_service_item:set_category(CategoryId, I) end
               ,fun(I) -> kz_service_item:set_item(As, I) end
               ,fun(I) -> kz_service_item:set_name(Name, I) end
               ,fun(I) -> kz_service_item:set_quantity(Quantity, I) end
               ,fun(I) -> kz_service_item:set_rate(Rate, I) end
               ,fun(I) -> maybe_set_discounts(I, ItemPlan) end
               ,fun(I) -> kz_service_item:set_bookkeepers(bookkeeper_jobj(CategoryId, As, ServicePlan), I) end
               ,fun(I) -> kz_service_item:set_activation_charge(Charge, I) end
               ,fun(I) -> kz_service_item:set_minimum(Min, I) end
               ,fun(I) -> kz_service_item:set_exceptions(Exc, I) end
               ],
    ServiceItem = lists:foldl(fun(F, I) -> F(I) end
                             ,kz_service_items:find(CategoryId, As, ServiceItems)
                             ,Routines
                             ),
    kz_service_items:update(ServiceItem, ServiceItems).

-spec get_generic_item_plan(kzd_service_plan:doc(), ne_binary()) -> kz_json:object().
get_generic_item_plan(ServicePlan, CategoryId) ->
    case kzd_service_plan:item(ServicePlan, CategoryId, <<"_all">>) of
        'undefined' -> kz_json:new();
        ItemPlan -> ItemPlan
    end.
-spec get_plan_items(kzd_service_plan:doc(), kz_services:services()) -> kz_proplist().
get_plan_items(ServicePlan, Services) ->
    case kz_services:select_bookkeeper(Services) of
        'kz_bookkeeper_http' ->
            [{CategoryId, ItemId}
             || CategoryId <- kz_services:list_categories(Services),
                ItemId <- kz_services:list_items(Services, CategoryId)
            ];
        _Else ->
            [{CategoryId, ItemId}
             || CategoryId <- kzd_service_plan:categories(ServicePlan),
                ItemId <- kzd_service_plan:items(ServicePlan, CategoryId)
            ]
    end.

-spec maybe_set_discounts(kz_service_item:item(), kzd_item_plan:doc()) ->
                                 kz_service_item:item().
maybe_set_discounts(Item, ItemPlan) ->
    lists:foldl(fun(F, I) -> F(I, ItemPlan) end
               ,Item
               ,[fun maybe_set_single_discount/2
                ,fun maybe_set_cumulative_discount/2
                ]
               ).

-spec maybe_set_single_discount(kz_service_item:item(), kzd_item_plan:doc()) ->
                                       kz_service_item:item().
maybe_set_single_discount(Item, ItemPlan) ->
    case kzd_item_plan:single_discount(ItemPlan) of
        'undefined' -> Item;
        SingleDiscount ->
            SingleRate = kz_json:get_float_value(<<"rate">>, SingleDiscount, kz_service_item:rate(Item)),
            lager:debug("setting single discount rate ~p", [SingleRate]),
            kz_service_item:set_single_discount_rate(SingleRate, Item)
    end.

-spec maybe_set_cumulative_discount(kz_service_item:item(), kzd_item_plan:doc()) ->
                                           kz_service_item:item().
maybe_set_cumulative_discount(Item, ItemPlan) ->
    case kzd_item_plan:cumulative_discount(ItemPlan) of
        'undefined' -> Item;
        CumulativeDiscount ->
            set_cumulative_discount(Item, CumulativeDiscount)
    end.

-spec set_cumulative_discount(kz_service_item:item(), kz_json:object()) ->
                                     kz_service_item:item().
set_cumulative_discount(Item, CumulativeDiscount) ->
    Quantity = kz_service_item:quantity(Item),

    CumulativeQuantity = cumulative_quantity(Item, CumulativeDiscount, Quantity),

    CumulativeRate = case get_quantity_rate(Quantity, CumulativeDiscount) of
                         'undefined' -> kz_service_item:rate(Item);
                         Else -> Else
                     end,

    lager:debug("setting cumulative discount ~p @ ~p", [CumulativeQuantity, CumulativeRate]),

    Item1 = kz_service_item:set_cumulative_discount(CumulativeQuantity, Item),
    kz_service_item:set_cumulative_discount_rate(CumulativeRate, Item1).

-spec cumulative_quantity(kz_service_item:item(), kz_json:object(), integer()) -> integer().
cumulative_quantity(Item, CumulativeDiscount, Quantity) ->
    case kz_json:get_integer_value(<<"maximum">>, CumulativeDiscount, 0) of
        Max when Max < Quantity ->
            lager:debug("item '~s/~s' quantity ~p exceeds cumulative discount max, using ~p"
                       ,[kz_service_item:category(Item)
                        ,kz_service_item:item(Item)
                        ,Quantity
                        ,Max
                        ]
                       ),
            Max;
        _ -> Quantity
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bookkeeper_jobj(ne_binary(), ne_binary(), kzd_service_plan:doc()) -> kz_json:object().
bookkeeper_jobj(CategoryId, ItemId, ServicePlan) ->
    lists:foldl(fun(Bookkeeper, J) ->
                        Mapping = kz_json:get_value([CategoryId, ItemId]
                                                   ,kzd_service_plan:bookkeeper(ServicePlan, Bookkeeper)
                                                   ,kz_json:new()
                                                   ),
                        kz_json:set_value(Bookkeeper, Mapping, J)
                end
               ,kz_json:new()
               ,kzd_service_plan:bookkeeper_ids(ServicePlan)
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_rate_at_quantity(ne_binary(), ne_binary(), kzd_service_plan:doc(), kz_services:services()) ->
                                  {float(), integer()}.
get_rate_at_quantity(CategoryId, ItemId, ItemPlan, Services) ->
    Quantity = get_quantity(CategoryId, ItemId, ItemPlan, Services),
    case get_flat_rate(Quantity, ItemPlan) of
        'undefined' -> {get_quantity_rate(Quantity, ItemPlan), Quantity};
        FlatRate -> {FlatRate, 1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If tiered flate rates are provided, find the value to use given the
%% current quantity.
%% @end
%%--------------------------------------------------------------------
-spec get_quantity(ne_binary(), ne_binary(), kzd_item_plan:doc(), kz_services:services()) -> integer().
get_quantity(CategoryId, ItemId, ItemPlan, Services) ->
    ItemQuantity = get_item_quantity(CategoryId, ItemId, ItemPlan, Services),
    case kzd_item_plan:minimum(ItemPlan) of
        Min when Min > ItemQuantity ->
            lager:debug("minimum '~s/~s' not met with ~p, enforcing quantity ~p"
                       ,[CategoryId, ItemId, ItemQuantity, Min]
                       ),
            Min;
        _ -> ItemQuantity
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If tiered flate rates are provided, find the value to use given the
%% current quantity.
%% @end
%%--------------------------------------------------------------------
-spec get_flat_rate(non_neg_integer(), kzd_item_plan:doc()) -> api_float().
get_flat_rate(Quantity, ItemPlan) ->
    Rates = kzd_item_plan:flat_rates(ItemPlan),
    L1 = [kz_term:to_integer(K) || K <- kz_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] -> 'undefined';
        Range ->
            kz_json:get_float_value(kz_term:to_binary(hd(Range)), Rates)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If tiered rates are provided, find the value to use given the current
%% quantity.  If no rates are viable attempt to use the "rate" property.
%% @end
%%--------------------------------------------------------------------
-spec get_quantity_rate(non_neg_integer(), kzd_item_plan:doc()) -> api_float().
get_quantity_rate(Quantity, ItemPlan) ->
    Rates = kzd_item_plan:rates(ItemPlan),
    L1 = [kz_term:to_integer(K) || K <- kz_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] ->
            kzd_item_plan:rate(ItemPlan);
        Range ->
            kz_json:get_float_value(kz_term:to_binary(hd(Range)), Rates)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the item quantity, drawing solely from the provided account or
%% (when the service plan dictates) the summed (cascaded) decendants.
%% Also handle the special case were we should sum all items in a
%% given category, except a list of item names to ignore during
%% summation.
%% @end
%%--------------------------------------------------------------------
-spec get_item_quantity(ne_binary(), ne_binary(), kzd_item_plan:doc(), kz_services:services()) ->
                               integer().
-spec get_item_quantity(ne_binary(), ne_binary(), kzd_item_plan:doc(), kz_services:services(), ne_binary()) ->
                               integer().

get_item_quantity(CategoryId, ItemId, ItemPlan, Services) ->
    get_item_quantity(CategoryId, ItemId, ItemPlan, Services, kzd_service_plan:all_items_key()).

get_item_quantity(CategoryId, AllItems, ItemPlan, Services, AllItems) ->
    Exceptions = kzd_item_plan:exceptions(ItemPlan),

    case kzd_item_plan:should_cascade(ItemPlan) of
        'false' -> kz_services:category_quantity(CategoryId, Exceptions, Services);
        'true' ->
            lager:debug("collecting '~s' as a cascaded sum", [CategoryId]),
            kz_services:cascade_category_quantity(CategoryId, Exceptions, Services)
    end;
get_item_quantity(CategoryId, ItemId, ItemPlan, Services, _AllItems) ->
    case kzd_item_plan:should_cascade(ItemPlan) of
        'false' -> kz_services:quantity(CategoryId, ItemId, Services);
        'true' ->
            lager:debug("collecting '~s/~s' as a cascaded quantity", [CategoryId, ItemId]),
            kz_services:cascade_quantity(CategoryId, ItemId, Services)
    end.
