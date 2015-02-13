%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_braintree).

-export([sync/2]).
-export([is_good_standing/1]).
-export([transactions/2]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).
-export([timestamp_to_braintree/1]).

-include_lib("braintree/include/braintree.hrl").
-include("../whistle_services.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-record(wh_service_update, {bt_subscription
                            ,plan_id :: api_binary()
                           }).

-record(wh_service_updates, {bt_subscriptions = [] :: [update(),...] | []
                             ,account_id :: api_binary()
                             ,bt_customer
                            }).

-type update() :: #wh_service_update{}.
-type updates() :: #wh_service_updates{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_good_standing(ne_binary()) -> boolean().
is_good_standing(AccountId) ->
    try braintree_customer:find(AccountId) of
        Customer -> customer_has_card(Customer, AccountId)
    catch
        'throw':_R ->
            lager:debug("braintree customer ~s is not in good standing: ~p", [AccountId, _R]),
            'false'
    end.

customer_has_card(Customer, AccountId) ->
    try braintree_customer:default_payment_card(Customer) of
        Card ->
            case braintree_card:expired(Card) of
                'false' ->
                    lager:debug("braintree customer ~s has a valid credit card on file", [AccountId]),
                    'true';
                'true' ->
                    lager:debug("braintree customer ~s has an expired credit card", [AccountId]),
                    'false'
            end
    catch
        'throw':_R ->
            lager:debug("braintree customer ~s is not in good standing: ~p", [AccountId, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(dict(), ne_binary()) -> 'ok'.
-spec sync(wh_service_item:items(), ne_binary(), updates()) -> 'ok'.
sync(Items, AccountId) ->
    ItemList = wh_service_items:to_list(Items),
    case fetch_bt_customer(AccountId, ItemList =/= []) of
        'undefined' -> 'ok';
        Customer ->
            sync(ItemList, AccountId, #wh_service_updates{bt_customer=Customer})
    end.

sync([], _AccountId, #wh_service_updates{bt_subscriptions=Subscriptions}) ->
    _ = [braintree_subscription:update(Subscription)
         || #wh_service_update{bt_subscription=Subscription} <- Subscriptions
        ],
    'ok';
sync([ServiceItem|ServiceItems], AccountId, Updates) ->
    case braintree_plan_addon_id(ServiceItem) of
        {'undefined', _} ->
            lager:debug("service item had no plan id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Updates);
        {_, 'undefined'} ->
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
            Subscription = lists:foldl(fun(F, S) -> F(S) end
                                       ,fetch_or_create_subscription(PlanId, Updates)
                                       ,Routines
                                      ),
            sync(ServiceItems, AccountId, update_subscriptions(PlanId, Subscription, Updates))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), wh_proplist()) ->
                          {'error', 'not_found'} |
                          {'error', 'unknown_error'} |
                          {'ok', wh_transaction:transactions()}.
transactions(Account, Options) ->
    From = timestamp_to_braintree(props:get_value('from', Options)),
    To = timestamp_to_braintree(props:get_value('to', Options)),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    try braintree_transaction:find_by_customer(AccountId, From, To) of
        BTTransactions ->
            JObjs = [braintree_transaction:record_to_json(Tr) || Tr <- BTTransactions],
            Filtered = filter_prorated(JObjs, props:get_value('prorated', Options, 'true')),
            Transactions = convert_transactions(Filtered),
            {'ok', Transactions}
    catch
        'throw':{'not_found', _} -> {'error', 'not_found'};
        _:_ -> {'error', 'unknown_error'}
    end.

filter_prorated(Transactions, 'true') ->
    [Tr || Tr <- Transactions, is_prorated(Tr)];
filter_prorated(Transactions, 'false') ->
    [Tr || Tr <- Transactions, not(is_prorated(Tr))].


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_prorated(wh_json:object()) -> boolean().
is_prorated(BTransaction) ->
    case wh_json:get_value(<<"subscription_id">>, BTransaction) of
        'undefined' -> 'true';
        _Id ->
            Addon = calculate_addon(BTransaction),
            Discount = calculate_discount(BTransaction),
            Amount = wh_json:get_number_value(<<"amount">>, BTransaction, 0),
            (Addon - Discount) =/= Amount
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_addon(wh_json:object()) -> number().
calculate_addon(BTransaction) ->
    Addons = wh_json:get_value(<<"add_ons">>, BTransaction, []),
    calculate(Addons, 0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_discount(wh_json:object()) -> number().
calculate_discount(BTransaction) ->
    Addons = wh_json:get_value(<<"discounts">>, BTransaction, []),
    calculate(Addons, 0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate(wh_json:objects(), number()) -> number().
calculate([], Acc) -> Acc/100;
calculate([Addon|Addons], Acc) ->
    Amount = wh_json:get_number_value(<<"amount">>, Addon, 0)*100,
    Quantity = wh_json:get_number_value(<<"quantity">>, Addon, 0),
    calculate(Addons, (Amount*Quantity+Acc)).


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscriptions(ne_binary()) -> atom() | wh_json:objects().
subscriptions(AccountId) ->
    try braintree_customer:find(AccountId) of
        Customer ->
            [braintree_subscription:record_to_json(Sub) || Sub <-  braintree_customer:get_subscriptions(Customer)]
    catch
        'throw':{'not_found', _} -> 'not_found';
        _:_ -> 'unknow_error'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(ne_binary(),wh_transactions:wh_transactions()) -> 'ok' | 'error'.
-spec commit_transactions(ne_binary(), wh_transactions:wh_transactions(), integer()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions) ->
    commit_transactions(BillingId, Transactions, 3).

commit_transactions(BillingId, Transactions, Try) when Try > 0 ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, BillingId) of
        {'error', _E} ->
            lager:error("could not open services for ~p : ~p retrying...", [BillingId, _E]),
            commit_transactions(BillingId, Transactions, Try-1);
        {'ok', JObj} ->
            NewTransactions = wh_json:get_value(<<"transactions">>, JObj, [])
                ++ wh_transactions:to_json(Transactions),
            JObj1 = wh_json:set_values([{<<"pvt_dirty">>, 'true'}
                                        ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                        ,{<<"transactions">>, NewTransactions}
                                       ], JObj),
            case couch_mgr:save_doc(?WH_SERVICES_DB, JObj1) of
                {'error', _E} ->
                    lager:error("could not save services for ~p : ~p retrying...", [BillingId, _E]),
                    commit_transactions(BillingId, Transactions, Try-1);
                {'ok', _} -> 'ok'
            end
    end;
commit_transactions(BillingId, _Transactions, _Try) ->
    lager:error("too many attempts writing transaction to services in ~p", [BillingId]),
    'error'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(ne_binary(), wh_json:objects()) -> wh_json:objects().
-spec charge_transactions(ne_binary(), wh_json:objects(), dict()) -> wh_json:objects().
charge_transactions(BillingId, Transactions) ->
    charge_transactions(BillingId, Transactions, dict:new()).

charge_transactions(BillingId, [], Dict) ->
    dict:fold(
        fun(Code, JObjs, Acc) when Code =:= ?CODE_TOPUP ->
            case handle_topup(BillingId, JObjs) of
                'true' -> Acc;
                'false' -> JObjs ++ Acc
            end;
        (Code, JObjs, Acc) ->
             case handle_charged_transactions(BillingId, Code, JObjs) of
                'true' -> Acc;
                'false' -> JObjs ++ Acc
            end
        end
        ,[]
        ,Dict
    );
charge_transactions(BillingId, [Transaction|Transactions], Dict) ->
    Code = wh_json:get_value(<<"pvt_code">>, Transaction),
    L = case dict:find(Code, Dict) of
            'error' -> [Transaction];
            {'ok', Value} -> [Transaction | Value]
        end,
    charge_transactions(BillingId
                        ,Transactions
                        ,dict:store(Code, L, Dict)
                       ).

-spec handle_charged_transactions(ne_binary(), pos_integer(), wh_json:objects()) -> boolean().
handle_charged_transactions(BillingId, Code, []) ->
    lager:debug("no transaction found for ~s", [{BillingId, Code}]),
    'true';
handle_charged_transactions(BillingId, Code, JObjs) ->
    Props = [{<<"purchase_order">>, Code}],
    Amount = calculate_amount(JObjs),
    BT = braintree_transaction:quick_sale(
           BillingId
           ,wht_util:units_to_dollars(Amount)
           ,Props
          ),
    handle_quick_sale_response(BT).

-spec handle_topup(ne_binary(), wh_json:objects()) -> boolean().
handle_topup(BillingId, []) ->
    lager:debug("no top-up transaction found for ~s", [BillingId]),
    'true';
handle_topup(BillingId, JObjs) ->
    case already_charged(BillingId, ?CODE_TOPUP) of
        'true' -> 'true';
        'false' ->
            Amount = calculate_amount(JObjs),
            Props = [{<<"purchase_order">>, ?CODE_TOPUP}],
            BT = braintree_transaction:quick_sale(
                   BillingId
                   ,wht_util:units_to_dollars(Amount)
                   ,Props
                  ),
            handle_quick_sale_response(BT)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_transactions(wh_json:objects()) ->
                                  wh_transaction:transactions().
-spec convert_transactions(wh_json:objects(), wh_transaction:transactions()) ->
                                  wh_transaction:transactions().
convert_transactions(BTTransactions) ->
    convert_transactions(BTTransactions, []).

convert_transactions([], Transactions) -> Transactions;
convert_transactions([BTTransaction|BTTransactions], Transactions) ->
    Transaction = convert_transaction(BTTransaction),
    convert_transactions(BTTransactions, [Transaction|Transactions]).

-spec convert_transaction(wh_json:object()) -> wh_transaction:transaction().
convert_transaction(BTTransaction) ->
    Routines = [fun set_description/2
                ,fun set_bookkeeper_info/2
                ,fun set_metadata/2
                ,fun set_reason/2
                ,fun set_amount/2
                ,fun set_created/2
                ,fun set_modified/2
                ,fun set_account_id/2
                ,fun set_account_db/2
               ],
    lists:foldl(fun(F, T) -> F(BTTransaction, T) end, wh_transaction:new(), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_description(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_description(_BTTransaction, Transaction) ->
    wh_transaction:set_description(?TR_DESCRIPTION, Transaction).

-spec set_bookkeeper_info(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_bookkeeper_info(BTTransaction, Transaction) ->
    wh_transaction:set_bookkeeper_info(
        wh_json:from_list([
            {<<"id">>, wh_json:get_value(<<"id">>, BTTransaction)}
            ,{<<"merchant_account_id">>, wh_json:get_value(<<"merchant_account_id">>, BTTransaction)}
        ])
        ,Transaction
    ).

-spec set_metadata(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_metadata(BTTransaction, Transaction) ->
    wh_transaction:set_metadata(BTTransaction, Transaction).

-spec set_reason(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_reason(_, Transaction) ->
    wh_transaction:set_reason(<<"unknown">>, Transaction).

-spec set_amount(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_amount(BTTransaction, Transaction) ->
    Amount = wh_json:get_value(<<"amount">>, BTTransaction),
    wh_transaction:set_amount(wht_util:dollars_to_units(Amount), Transaction).

-spec set_created(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_created(BTTransaction, Transaction) ->
    Created = utc_to_gregorian_seconds(wh_json:get_value(<<"created_at">>, BTTransaction)),
    wh_transaction:set_created(Created, Transaction).

-spec set_modified(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_modified(BTTransaction, Transaction) ->
    Modified = utc_to_gregorian_seconds(wh_json:get_value(<<"update_at">>, BTTransaction)),
    wh_transaction:set_modified(Modified, Transaction).

-spec set_account_id(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_account_id(BTTransaction, Transaction) ->
    AccountId = wh_util:format_account_id(wh_json:get_value([<<"customer">>, <<"id">>], BTTransaction), 'raw'),
    wh_transaction:set_account_id(AccountId, Transaction).

-spec set_account_db(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_account_db(BTTransaction, Transaction) ->
    AccountDb = wh_util:format_account_id(wh_json:get_value([<<"customer">>, <<"id">>], BTTransaction), 'encoded'),
    wh_transaction:set_account_db(AccountDb, Transaction).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_braintree('undefined' | gregorian_seconds()) -> ne_binary().
timestamp_to_braintree('undefined') ->
    lager:debug("timestamp undefined using current_tstamp"),
    timestamp_to_braintree(wh_util:current_tstamp());
timestamp_to_braintree(Timestamp) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(wh_util:pad_month(M))/binary, "/"
      ,(wh_util:pad_month(D))/binary, "/"
      ,(wh_util:to_binary(Y))/binary
    >>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec utc_to_gregorian_seconds(ne_binary()) -> 'undefined' | gregorian_seconds().
utc_to_gregorian_seconds(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T"
                          ,H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary>>
                        ) ->
    Date = {
      {wh_util:to_integer(Y), wh_util:to_integer(M), wh_util:to_integer(D)}
      ,{wh_util:to_integer(H), wh_util:to_integer(Mi), wh_util:to_integer(S)}
     },
    calendar:datetime_to_gregorian_seconds(Date);
utc_to_gregorian_seconds(UTC) ->
    lager:warning("unknown UTC date format ~s", [UTC]),
    'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_amount(wh_json:objects()) -> integer().
-spec calculate_amount(wh_json:objects(), integer()) -> integer().
calculate_amount(JObjs) ->
    calculate_amount(JObjs, 0).

calculate_amount([], Amount) -> Amount;
calculate_amount([JObj|JObjs], Amount) ->
    calculate_amount(JObjs
                     ,wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0) + Amount
                    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_quick_sale_response(bt_transaction()) -> boolean().
handle_quick_sale_response(BtTransaction) ->
    Transaction = braintree_transaction:record_to_json(BtTransaction),
    RespCode = wh_json:get_value(<<"processor_response_code">>, Transaction, ?CODE_UNKNOWN),
    % https://www.braintreepayments.com/docs/ruby/reference/processor_responses
    wh_util:to_integer(RespCode) < 2000.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec already_charged(ne_binary() | integer() , integer() | wh_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
    lager:debug("checking if ~s has been charge for transaction of type ~p today", [BillingId, Code]),
    BtTransactions = braintree_transaction:find_by_customer(BillingId),
    Transactions = [braintree_transaction:record_to_json(BtTransaction) || BtTransaction <- BtTransactions],
    already_charged(wh_util:to_binary(Code), Transactions);

already_charged(_, []) ->
    lager:debug("no transactions found matching code or made today"),
    'false';
already_charged(Code, [Transaction|Transactions]) ->
    case
        already_charged_transaction(
            Code
            ,wh_json:get_value(<<"status">>, Transaction)
            ,wh_json:get_value(<<"purchase_order">>, Transaction)
            ,Transaction
        )
    of
        'true' -> 'true';
        'false' ->
            already_charged(Code, Transactions)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec already_charged_transaction(integer(), ne_binary(), integer(), wh_json:object()) -> boolean().
already_charged_transaction(_ , <<"voided">>, _, Transaction) ->
    Id = wh_json:get_value(<<"id">>, Transaction),
    lager:debug("transaction was voided (~s)", [Id]),
    'false';
already_charged_transaction(Code , _, Code, Transaction) ->
    <<Year:4/binary, _:1/binary
      ,Month:2/binary, _:1/binary
      ,Day:2/binary
      ,_/binary
    >> = wh_json:get_value(<<"created_at">>, Transaction),
    Id = wh_json:get_value(<<"id">>, Transaction),
    {YearNow, M, D} = erlang:date(),
    case {wh_util:to_binary(YearNow), wh_util:pad_month(M), wh_util:pad_month(D)} of
        {Year, Month, Day} ->
            lager:debug("found transaction matching code and date (~s)", [Id]),
            'true';
        _Now ->
            lager:debug("found transaction matching code but not date (~s)", [Id]),
            'false'
    end;
already_charged_transaction(_, _, _, _) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_single_discount(wh_service_item:item(), braintree_subscription:subscription()) ->
                                    braintree_subscription:subscription().
handle_single_discount(ServiceItem, Subscription) ->
    DiscountId = braintree_single_discount_id(ServiceItem),
    SingleDiscount = wh_service_item:single_discount(ServiceItem),
    case wh_util:is_empty(SingleDiscount) orelse wh_util:is_empty(DiscountId) of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, 1),
            Rate = wh_service_item:single_discount_rate(ServiceItem),
            braintree_subscription:update_discount_amount(S, DiscountId, Rate)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cumulative_discounts(wh_service_item:item(), braintree_subscription:subscription()) ->
                                         braintree_subscription:subscription().
handle_cumulative_discounts(ServiceItem, Subscription) ->
    DiscountId = braintree_cumulative_discount_id(ServiceItem),
    CumulativeDiscount = wh_service_item:cumulative_discount(ServiceItem),
    case wh_util:is_empty(CumulativeDiscount) orelse wh_util:is_empty(DiscountId) of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, CumulativeDiscount),
            Rate = wh_service_item:cumulative_discount_rate(ServiceItem),
            braintree_subscription:update_discount_amount(S, DiscountId, Rate)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_bt_customer(ne_binary(), boolean()) ->
                               'undefined' | braintree_customer:customer().
fetch_bt_customer(AccountId, NewItems) ->
    lager:debug("requesting braintree customer ~s", [AccountId]),
    try braintree_customer:find(AccountId) of
        Customer -> Customer
    catch
        'throw':{'not_found', Error} when NewItems ->
            throw({'no_payment_token', wh_json:from_list([{<<"no_payment_token">>, Error}])});
        'throw':{'not_found', _} -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_subscriptions(ne_binary(), update(), updates()) ->
                                  updates().
update_subscriptions(PlanId, Subscription, #wh_service_updates{bt_subscriptions=Subscriptions}=Updates) ->
    Update = #wh_service_update{bt_subscription=Subscription, plan_id=PlanId},
    Updates#wh_service_updates{bt_subscriptions=lists:keystore(PlanId, #wh_service_update.plan_id, Subscriptions, Update)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_or_create_subscription(ne_binary(), updates()) -> braintree_subscription:subscription().
fetch_or_create_subscription(PlanId, #wh_service_updates{bt_subscriptions=Subscriptions
                                                         ,bt_customer=Customer
                                                        }) ->
    case lists:keyfind(PlanId, #wh_service_update.plan_id, Subscriptions) of
        'false' ->
            try braintree_customer:get_subscription(PlanId, Customer) of
                Subscription ->
                    lager:debug("found subscription ~s for plan id ~s"
                                ,[braintree_subscription:get_id(Subscription), PlanId]),
                    braintree_subscription:reset(Subscription)
            catch
                'throw':{'not_found', _} ->
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
-spec braintree_plan_addon_id(wh_service_item:item()) ->
                                     {api_binary(), api_binary()}.
braintree_plan_addon_id(ServiceItem) ->
    JObj = wh_service_item:bookkeeper(<<"braintree">>, ServiceItem),
    {wh_json:get_value(<<"plan">>, JObj), wh_json:get_value(<<"addon">>, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_cumulative_discount_id(wh_service_item:item()) ->
                                              api_binary().
braintree_cumulative_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"cumulative">>], ServiceItem).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_single_discount_id(wh_service_item:item()) ->
                                          api_binary().
braintree_single_discount_id(ServiceItem) ->
    wh_service_item:bookkeeper([<<"braintree">>, <<"discounts">>, <<"single">>], ServiceItem).
