%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_braintree).

-export([sync/2]).
-export([is_good_standing/1]).
-export([transactions/1]).
-export([subscriptions/1]).

-include("../whistle_services.hrl").

-record(wh_service_update, {bt_subscription
                            ,plan_id
                           }).

-record(wh_service_updates, {bt_subscriptions = []
                             ,account_id
                             ,bt_customer
                            }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
is_good_standing(AccountId) ->
    try braintree_customer:default_payment_token(AccountId) of
        _ ->
            lager:debug("braintree customer ~s is believed to be in good standing", [AccountId]),
            true
    catch
        throw:_R ->
            lager:debug("braintree customer ~s is not in good standing: ~p", [AccountId, _R]),
            false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
sync(Items, AccountId) ->
    ItemList = wh_service_items:to_list(Items),
    case fetch_bt_customer(AccountId, ItemList =/= []) of
        undefined -> ok;
        Customer ->
            sync(ItemList, AccountId, #wh_service_updates{bt_customer=Customer})
    end.

sync([], _AccountId, #wh_service_updates{bt_subscriptions=Subscriptions}) ->
    _ = [braintree_subscription:update(Subscription)
         || #wh_service_update{bt_subscription=Subscription} <- Subscriptions
        ],
    ok;
sync([ServiceItem|ServiceItems], AccountId, Updates) ->
    case braintree_plan_addon_id(ServiceItem) of
        {undefined, _} ->
            lager:debug("service item had no plan id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Updates);
        {_, undefined} ->
            lager:debug("service item had no add on id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Updates);
        {PlanId, AddOnId}->
            Quantity = wh_service_item:quantity(ServiceItem),
            Routines = [fun(S) -> braintree_subscription:update_addon_quantity(S, AddOnId, Quantity) end
                        ,fun(S) ->
                                 Rate = wh_service_item:rate(ServiceItem),
                                 braintree_subscription:update_addon_amount(S, AddOnId, Rate)
                         end
                        ,fun(S) -> handle_single_discount(ServiceItem, S) end
                        ,fun(S) -> handle_cumulative_discounts(ServiceItem, S) end
                       ],
            Subscription = lists:foldl(fun(F, S) -> F(S) end, fetch_or_create_subscription(PlanId, Updates), Routines),
            sync(ServiceItems, AccountId, update_subscriptions(PlanId, Subscription, Updates))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary()) -> atom() | [wh_json:object(), ...].
transactions(AccountId) ->
    try braintree_transaction:find_by_customer(AccountId) of
        Transactions -> 
            [braintree_transaction:record_to_json(Tr) || Tr <- Transactions]
    catch
        throw:{'not_found', _} -> 'not_found';
        _:_ -> 'unknow_error'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------s
-spec subscriptions(ne_binary()) -> atom() | [wh_json:object(), ...].
subscriptions(AccountId) ->
    try braintree_customer:find(AccountId) of
        Customer -> 
            [braintree_subscription:record_to_json(Sub) || Sub <-  braintree_customer:get_subscriptions(Customer)]
    catch
        throw:{'not_found', _} -> 'not_found';
        _:_ -> 'unknow_error'
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_single_discount/2 :: (wh_service_item:item(), braintree_subscription:subscription()) -> braintree_subscription:subscription().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cumulative_discounts/2 :: (wh_service_item:item(), braintree_subscription:subscription()) -> braintree_subscription:subscription().
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
-spec fetch_bt_customer/2 :: (ne_binary(), boolean()) -> 'undefined' | braintree_customer:customer().
fetch_bt_customer(AccountId, NewItems) ->
    lager:debug("requesting braintree customer ~s", [AccountId]),
    try braintree_customer:find(AccountId) of
        Customer -> Customer
    catch
        throw:{not_found, Error} when NewItems ->
            throw({no_payment_token, wh_json:from_list([{<<"no_payment_token">>, Error}])});
        throw:{not_found, _} -> undefined
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
-spec braintree_plan_addon_id/1 :: (wh_service_items:item()) -> {api_binary(), api_binary()}.
braintree_plan_addon_id(ServiceItem) ->
    JObj = wh_service_item:bookkeeper(<<"braintree">>, ServiceItem),
    {wh_json:get_value(<<"plan">>, JObj), wh_json:get_value(<<"addon">>, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_cumulative_discount_id/1 :: (wh_service_item:item()) -> api_binary().
braintree_cumulative_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"cumulative">>], ServiceItem).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_single_discount_id/1 :: (wh_service_items:item()) -> {api_binary(), api_binary()}.
braintree_single_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"single">>], ServiceItem).
