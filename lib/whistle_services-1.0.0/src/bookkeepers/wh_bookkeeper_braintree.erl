%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_braintree).

-include_lib("whistle_services/src/whistle_services.hrl").

-export([sync/2]).

-record(wh_service_update, {bt_subscription
                            ,plan_id
                           }).

-record(wh_service_updates, {bt_subscriptions = []
                             ,billing_id
                             ,bt_customer
                            }).
                             
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
sync(Items, BillingId) ->
    Customer = fetch_or_create_customer(BillingId),
    sync(wh_service_items:to_list(Items), BillingId, #wh_service_updates{bt_customer=Customer}).

sync([], _BillingId, #wh_service_updates{bt_subscriptions=Subscriptions}) ->
    _ = [braintree_subscription:update(Subscription) 
         || #wh_service_update{bt_subscription=Subscription} <- Subscriptions
        ],
    ok;
sync([ServiceItem|ServiceItems], BillingId, Updates) ->
    case braintree_plan_addon_id(ServiceItem) of
        {undefined, _} -> sync(ServiceItems, BillingId, Updates);
        {_, undefined} -> sync(ServiceItems, BillingId, Updates);
        {PlanId, AddOnId}->
            Quantity = wh_service_item:quantity(ServiceItem),
            Routines = [fun(S) -> braintree_subscription:update_addon_quantity(S, AddOnId, Quantity) end
                        ,fun(S) -> handle_single_discount(ServiceItem, S) end
                        ,fun(S) -> handle_cumulative_discounts(ServiceItem, S) end
                       ],
            Subscription = lists:foldl(fun(F, S) -> F(S) end, fetch_or_create_subscription(PlanId, Updates), Routines),
            sync(ServiceItems, BillingId, update_subscriptions(PlanId, Subscription, Updates))
    end.

handle_single_discount(ServiceItem, Subscription) ->
    DiscountId = braintree_single_discount_id(ServiceItem),
    SingleDiscount = wh_service_item:single_discount(ServiceItem),
    case wh_util:is_empty(SingleDiscount) orelse wh_util:is_empty(DiscountId) of
        true -> Subscription;
        false ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, 1),
            case wh_service_item:single_discount_rate(ServiceItem) of
                undefined -> S;
                Rate -> braintree_subscription:update_discount_amount(S, DiscountId, Rate)
            end
    end.
    
handle_cumulative_discounts(ServiceItem, Subscription) ->
    DiscountId = braintree_cumulative_discount_id(ServiceItem),
    CumulativeDiscount = wh_service_item:cumulative_discount(ServiceItem),
    case wh_util:is_empty(CumulativeDiscount) orelse wh_util:is_empty(DiscountId) of
        true -> Subscription;
        false ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, CumulativeDiscount),
            case wh_service_item:cumulative_discount_rate(ServiceItem) of
                undefined -> S;
                Rate -> braintree_subscription:update_discount_amount(S, DiscountId, Rate)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_or_create_customer/1 :: (ne_binary()) -> braintree_customer:customer().
fetch_or_create_customer(BillingId) ->
    lager:debug("requesting braintree customer ~s", [BillingId]),
    try braintree_customer:find(BillingId) of
        Customer -> Customer
    catch
        throw:{not_found, _} ->
            lager:debug("creating new braintree customer ~s", [BillingId]),
            braintree_customer:create(BillingId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_subscriptions/3 :: (ne_binary(), #wh_service_update{}, #wh_service_updates{}) -> #wh_service_updates{}.
update_subscriptions(PlanId, Subscription, #wh_service_updates{bt_subscriptions=Subscriptions}=Updates) ->
    Update = #wh_service_update{bt_subscription=Subscription, plan_id=PlanId},
    Updates#wh_service_updates{bt_subscriptions=lists:keystore(PlanId, #wh_service_update.plan_id, Subscriptions, Update)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_or_create_subscription/2 :: (ne_binary(), #wh_service_updates{}) -> braintree_subscription:subscription().
fetch_or_create_subscription(PlanId, #wh_service_updates{bt_subscriptions=Subscriptions
                                                         ,bt_customer=Customer}) ->
    case lists:keyfind(PlanId, #wh_service_update.plan_id, Subscriptions) of
        false ->
            try braintree_customer:get_subscription(PlanId, Customer) of
                Subscription -> 
                    lager:debug("found subscription ~s for plan id ~s"
                                ,[braintree_subscription:get_id(Subscription), PlanId]),
                    braintree_subscription:reset(Subscription)
            catch
                throw:{not_found, _} ->
                    lager:debug("creating new subscription for plan id ~s", [PlanId]),
                    braintree_customer:new_subscription(PlanId, Customer)
            end;
        #wh_service_update{bt_subscription=Subscription} -> Subscription
    end.                

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_plan_addon_id/1 :: (wh_service_items:item()) -> {'undefined' | ne_binary(), 'undefined' | ne_binary()}.
braintree_plan_addon_id(ServiceItem) ->
    JObj = wh_service_item:bookkeeper(<<"braintree">>, ServiceItem),
    {wh_json:get_value(<<"plan">>, JObj), wh_json:get_value(<<"addon">>, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_cumulative_discount_id/1 :: (wh_service_items:item()) -> {'undefined' | ne_binary(), 'undefined' | ne_binary()}.
braintree_cumulative_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"cumulative">>], ServiceItem).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_single_discount_id/1 :: (wh_service_items:item()) -> {'undefined' | ne_binary(), 'undefined' | ne_binary()}.
braintree_single_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"single">>], ServiceItem).
