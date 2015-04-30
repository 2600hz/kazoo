%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plan).

-export([fetch/3]).
-export([activation_charges/3]).
-export([create_items/3]).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a vendor database and service plan id, fetch the document.
%% Merge any plan overrides into the plan property.
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), ne_binary(), wh_json:object()) -> kzd_service_plan:api_doc().
fetch(PlanId, VendorId, Overrides) ->
    VendorDb = wh_util:format_account_id(VendorId, 'encoded'),
    case couch_mgr:open_cache_doc(VendorDb, PlanId) of
        {'ok', JObj} ->
            lager:debug("found service plan ~s/~s", [VendorDb, PlanId]),
            kzd_service_plan:merge_overrides(JObj, Overrides);
        {'error', _R} ->
            lager:debug("unable to open service plan ~s/~s: ~p", [VendorDb, PlanId, _R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), kzd_service_plan:doc()) -> float().
activation_charges(Category, Item, ServicePlan) ->
    case kzd_service_plan:item_activation_charge(ServicePlan, Category, Item) of
        'undefined' ->
            kzd_service_plan:category_activation_charge(ServicePlan, Category, 0.0);
        Charge -> Charge
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_items(kzd_service_plan:doc(), wh_service_items:items(), wh_services:services()) ->
                          wh_service_items:items().
-spec create_items(kzd_service_plan:doc(), wh_service_items:items(), wh_services:services()
                   ,ne_binary(), ne_binary()
                  ) ->
                          wh_service_items:items().

create_items(ServicePlan, ServiceItems, Services) ->
    Plans = [{Category, Item}
             || Category <- kzd_service_plan:categories(ServicePlan),
                Item <- kzd_service_plan:items(ServicePlan, Category)
            ],
    lists:foldl(fun({Category, Item}, SIs) ->
                        create_items(ServicePlan, SIs, Services, Category, Item)
                end
                ,ServiceItems
                ,Plans
               ).

create_items(ServicePlan, ServiceItems, Services, Category, Item) ->
    ItemPlan = kzd_service_plan:item(ServicePlan, Category, Item),

    {Rate, Quantity} = get_rate_at_quantity(Category, Item, ItemPlan, Services),
    %% allow service plans to re-map item names (IE: softphone items "as" sip_device)
    As = wh_json:get_ne_value(<<"as">>, ItemPlan, Item),
    Routines = [fun(I) -> wh_service_item:set_category(Category, I) end
                ,fun(I) -> wh_service_item:set_item(As, I) end
                ,fun(I) -> wh_service_item:set_quantity(Quantity, I) end
                ,fun(I) -> wh_service_item:set_rate(Rate, I) end
                ,fun(I) -> maybe_set_discounts(I, ItemPlan) end
                ,fun(I) -> wh_service_item:set_bookkeepers(bookkeeper_jobj(Category, As, ServicePlan), I) end
               ],
    ServiceItem = lists:foldl(fun(F, I) -> F(I) end
                              ,wh_service_items:find(Category, As, ServiceItems)
                              ,Routines
                             ),
    wh_service_items:update(ServiceItem, ServiceItems).

-spec maybe_set_discounts(wh_service_item:item(), wh_json:object()) -> wh_service_item:item().
maybe_set_discounts(Item, ItemPlan) ->
    lists:foldl(fun(F, I) -> F(I, ItemPlan) end
                ,Item
                ,[fun maybe_set_single_discount/2
                  ,fun maybe_set_cumulative_discount/2
                 ]
               ).

-spec maybe_set_single_discount(wh_service_item:item(), wh_json:object()) -> wh_service_item:item().
maybe_set_single_discount(Item, ItemPlan) ->
    case wh_json:get_value([<<"discounts">>, <<"single">>], ItemPlan) of
        'undefined' -> Item;
        SingleDiscount ->
            SingleRate = wh_json:get_float_value(<<"rate">>, SingleDiscount, wh_service_item:rate(Item)),
            lager:debug("setting single discount rate ~p", [SingleRate]),
            wh_service_item:set_single_discount_rate(SingleRate, Item)
    end.

-spec maybe_set_cumulative_discount(wh_service_item:item(), wh_json:object()) -> wh_service_item:item().
maybe_set_cumulative_discount(Item, ItemPlan) ->
    case wh_json:get_value([<<"discounts">>, <<"cumulative">>], ItemPlan) of
        'undefined' -> Item;
        CumulativeDiscount ->
            set_cumulative_discount(Item, CumulativeDiscount)
    end.

-spec set_cumulative_discount(wh_service_item:item(), wh_json:object()) -> wh_service_item:item().
set_cumulative_discount(Item, CumulativeDiscount) ->
    Quantity = wh_service_item:quantity(Item),

    CumulativeQuantity = cumulative_quantity(Item, CumulativeDiscount, Quantity),

    CumulativeRate = case get_quantity_rate(Quantity, CumulativeDiscount) of
                         'undefined' -> wh_service_item:rate(Item);
                         Else -> Else
                     end,

    lager:debug("setting cumulative discount ~p @ ~p", [CumulativeQuantity, CumulativeRate]),

    Item1 = wh_service_item:set_cumulative_discount(CumulativeQuantity, Item),
    wh_service_item:set_cumulative_discount_rate(CumulativeRate, Item1).

-spec cumulative_quantity(wh_service_item:item(), wh_json:object(), integer()) -> integer().
cumulative_quantity(Item, CumulativeDiscount, Quantity) ->
    case wh_json:get_integer_value(<<"maximum">>, CumulativeDiscount, 0) of
        Max when Max < Quantity ->
            lager:debug("item '~s/~s' quantity ~p exceeds cumulative discount max, using ~p"
                        ,[wh_service_item:category(Item)
                          ,wh_service_item:item(Item)
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
-spec bookkeeper_jobj(ne_binary(), ne_binary(), kzd_service_plan:doc()) -> wh_json:object().
bookkeeper_jobj(Category, Item, ServicePlan) ->
    lists:foldl(fun(Bookkeeper, J) ->
                        Mapping = wh_json:get_value([Category, Item]
                                                    ,kzd_service_plan:bookkeeper(ServicePlan, Bookkeeper)
                                                    ,wh_json:new()
                                                   ),
                        wh_json:set_value(Bookkeeper, Mapping, J)
                end
                ,wh_json:new()
                ,kzd_service_plan:bookkeeper_ids(ServicePlan)
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_rate_at_quantity(ne_binary(), ne_binary(), kzd_service_plan:doc(), wh_services:services()) ->
                                  {float(), integer()}.
get_rate_at_quantity(Category, Item, ItemPlan, Services) ->
    Quantity = get_quantity(Category, Item, ItemPlan, Services),
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
-spec get_quantity(ne_binary(), ne_binary(), kzd_service_plan:doc(), wh_services:services()) -> integer().
get_quantity(Category, Item, ItemPlan, Services) ->
    ItemQuantity = get_item_quantity(Category, Item, ItemPlan, Services),
    case kzd_service_plan:item_minimum(ItemPlan, Category, ItemPlan) of
        Min when Min > ItemQuantity ->
            lager:debug("minimum '~s/~s' not met with ~p, enforcing quantity ~p"
                        ,[Category, Item, ItemQuantity, Min]
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
-spec get_flat_rate(non_neg_integer(), wh_json:object()) -> api_float().
get_flat_rate(Quantity, ItemPlan) ->
    Rates = wh_json:get_value(<<"flat_rates">>, ItemPlan, wh_json:new()),
    L1 = [wh_util:to_integer(K) || K <- wh_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] -> 'undefined';
        Range ->
            wh_json:get_float_value(wh_util:to_binary(hd(Range)), Rates)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If tiered rates are provided, find the value to use given the current
%% quantity.  If no rates are viable attempt to use the "rate" property.
%% @end
%%--------------------------------------------------------------------
-spec get_quantity_rate(non_neg_integer(), wh_json:object()) -> api_float().
get_quantity_rate(Quantity, ItemPlan) ->
    Rates = wh_json:get_value(<<"rates">>, ItemPlan, wh_json:new()),
    L1 = [wh_util:to_integer(K) || K <- wh_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] ->
            wh_json:get_float_value(<<"rate">>, ItemPlan);
        Range ->
            wh_json:get_float_value(wh_util:to_binary(hd(Range)), Rates)
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
-spec get_item_quantity(ne_binary(), ne_binary(), wh_json:object(), wh_services:services()) -> integer().
get_item_quantity(Category, <<"_all">>, ItemPlan, Services) ->
    Exceptions = wh_json:get_value(<<"exceptions">>, ItemPlan, []),
    case wh_json:is_true(<<"cascade">>, ItemPlan) of
        'false' -> wh_services:category_quantity(Category, Exceptions, Services);
        'true' ->
            lager:debug("collecting '~s' as a cascaded sum", [Category]),
            wh_services:cascade_category_quantity(Category, Exceptions, Services)
    end;
get_item_quantity(Category, Item, ItemPlan, Services) ->
    case wh_json:is_true(<<"cascade">>, ItemPlan) of
        'false' -> wh_services:quantity(Category, Item, Services);
        'true' ->
            lager:debug("collecting '~s/~s' as a cascaded quantity", [Category, Item]),
            wh_services:cascade_quantity(Category, Item, Services)
    end.
