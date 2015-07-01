%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_braintree).

-export([sync/2]).
-export([is_good_standing/1]).
-export([transactions/3]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).
-export([timestamp_to_braintree/1]).

-include_lib("braintree/include/braintree.hrl").
-include("../whistle_services.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-record(wh_service_update, {bt_subscription :: braintree_subscription:subscription()
                            ,plan_id :: ne_binary()
                           }).

-record(wh_service_updates, {bt_subscriptions = [] :: [update(),...] | []
                             ,account_id :: api_binary()
                             ,bt_customer :: braintree_customer:customer()
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

-spec customer_has_card(braintree_customer:customer(), ne_binary()) -> boolean().
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
-spec sync(wh_service_items:items(), ne_binary()) -> 'ok'.
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
    JObj = wh_service_item:bookkeeper(<<"braintree">>, ServiceItem),
    case {wh_json:get_value(<<"plan">>, JObj), wh_json:get_value(<<"addon">>, JObj)} of
        {'undefined', _} ->
            lager:debug("service item had no plan id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Updates);
        {_, 'undefined'} ->
            lager:debug("service item had no add on id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Updates);
        {PlanId, AddOnId}->
            Subscription = prepare_subscription(ServiceItem, AddOnId, PlanId, Updates),
            NewUpdates = update_subscriptions(PlanId, Subscription, Updates),
            sync(ServiceItems, AccountId, NewUpdates)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', wh_transaction:transactions()} |
                          {'error', 'not_found'} |
                          {'error', 'unknown_error'}.
transactions(AccountId, From0, To0) ->
    From = timestamp_to_braintree(From0),
    To   = timestamp_to_braintree(To0),
    try braintree_transaction:find_by_customer(AccountId, From, To) of
        BTTransactions ->
            JObjs = [braintree_transaction:record_to_json(Tr) || Tr <- BTTransactions],
            {'ok', convert_transactions(JObjs)}
    catch
        'throw':{'not_found', _} -> {'error', 'not_found'};
        _:_ -> {'error', 'unknown_error'}
    end.

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
            [braintree_subscription:record_to_json(Sub)
             || Sub <- braintree_customer:get_subscriptions(Customer)
            ]
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
    charge_transactions(BillingId
                        ,Transactions
                        ,dict:append(Code, Transaction, Dict)
                       ).

-spec handle_charged_transactions(ne_binary(), pos_integer(), wh_json:objects()) -> boolean().
handle_charged_transactions(BillingId, Code, []) ->
    lager:debug("no transaction found for ~p", [{BillingId, Code}]),
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
            Success = handle_quick_sale_response(BT),
            _ = send_topup_notification(Success, BillingId, BT),
            Success
    end.

-spec send_topup_notification(boolean(), ne_binary(), bt_transaction()) -> boolean().
send_topup_notification(Success, BillingId, BtTransaction) ->
    Transaction = braintree_transaction:record_to_json(BtTransaction),
    Amount = wht_util:dollars_to_units(wh_json:get_float_value(<<"amount">>, Transaction, 0.0)),
    Props = [{<<"Account-ID">>, BillingId}
             ,{<<"Amount">>, Amount}
             ,{<<"Success">>, Success}
             ,{<<"Response">>, wh_json:get_value(<<"processor_response_text">>, Transaction)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = case
            whapps_util:amqp_pool_send(
              Props
              ,fun wapi_notifications:publish_topup/1
             )
        of
            'ok' ->
                lager:debug("topup notification sent for ~s", [BillingId]);
            {'error', _R} ->
                lager:error(
                  "failed to send topup notification for ~s : ~p"
                           ,[BillingId, _R]
                 )
        end,
    Success.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_transactions(wh_json:objects()) -> wh_transaction:transactions().
convert_transactions(BTTransactions) ->
    [convert_transaction(Tr) || Tr <- BTTransactions].

-spec convert_transaction(wh_json:object()) -> wh_transaction:transaction().
convert_transaction(BTTransaction) ->
    Routines = [fun set_description/2
                ,fun set_bookkeeper_info/2
                ,fun set_metadata/2
                ,fun set_reason/2
                ,fun set_status/2
                ,fun set_amount/2
                ,fun set_type/2  %% Make sure type is set /after/ amount
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
            {<<"id">>, wh_doc:id(BTTransaction)}
            ,{<<"merchant_account_id">>, wh_json:get_value(<<"merchant_account_id">>, BTTransaction)}
        ])
        ,Transaction
    ).

-spec set_metadata(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_metadata(BTTransaction, Transaction) ->
    wh_transaction:set_metadata(BTTransaction, Transaction).

-spec set_reason(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
-spec set_reason(wh_json:object(), wh_transaction:transaction(), pos_integer() | 'undefined') -> wh_transaction:transaction().
set_reason(BTTransaction, Transaction) ->
    Code = wh_json:get_integer_value(<<"purchase_order">>, BTTransaction),
    set_reason(BTTransaction, Transaction, Code).

set_reason(BTTransaction, Transaction, 'undefined') ->
    IsApi = wh_json:is_true(<<"is_api">>, BTTransaction),
    IsRecurring = wh_json:is_true(<<"is_recurring">>, BTTransaction),
    IsProrated = transaction_is_prorated(BTTransaction),
    if
        IsProrated, IsRecurring ->
            wh_transaction:set_reason(<<"recurring_prorate">>, Transaction);
        IsApi, IsRecurring ->
            wh_transaction:set_reason(<<"recurring_prorate">>, Transaction);
        IsRecurring ->
            wh_transaction:set_reason(<<"monthly_recurring">>, Transaction);
        IsApi ->
            wh_transaction:set_reason(<<"manual_addition">>, Transaction);
        'true' ->
            wh_transaction:set_reason(<<"unknown">>, Transaction)
    end;
set_reason(_BTTransaction, Transaction, Code) ->
    wh_transaction:set_code(Code, Transaction).

-spec set_type(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_type(BTTransaction, Transaction) ->
    case wh_json:get_ne_value(<<"type">>, BTTransaction) =:= ?BT_TRANS_SALE of
        'true'  -> wh_transaction:set_type(<<"debit">>, Transaction);
        'false' -> wh_transaction:set_type(<<"credit">>, Transaction)
    end.

-spec set_status(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_status(BTTransaction, Transaction) ->
    Status = wh_json:get_value(<<"status">>, BTTransaction),
    wh_transaction:set_status(Status, Transaction).

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
    CustomerId = wh_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    AccountId = wh_util:format_account_id(CustomerId, 'raw'),
    wh_transaction:set_account_id(AccountId, Transaction).

-spec set_account_db(wh_json:object(), wh_transaction:transaction()) -> wh_transaction:transaction().
set_account_db(BTTransaction, Transaction) ->
    CustormerId = wh_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    AccountDb = wh_util:format_account_id(CustormerId, 'encoded'),
    wh_transaction:set_account_db(AccountDb, Transaction).

-spec transaction_is_prorated(wh_json:object()) -> boolean().
transaction_is_prorated(BTransaction) ->
    Addon = calculate_addon(BTransaction),
    Discount = calculate_discount(BTransaction),
    Amount = wh_json:get_number_value(<<"amount">>, BTransaction, 0),
    (Addon - Discount) =/= Amount.

-spec calculate_addon(wh_json:object()) -> number().
calculate_addon(BTransaction) ->
    Addons = wh_json:get_value(<<"add_ons">>, BTransaction, []),
    calculate(Addons, 0).

-spec calculate_discount(wh_json:object()) -> number().
calculate_discount(BTransaction) ->
    Addons = wh_json:get_value(<<"discounts">>, BTransaction, []),
    calculate(Addons, 0).

-spec calculate(wh_json:objects(), number()) -> number().
calculate([], Acc) ->
    Acc/100;
calculate([Addon|Addons], Acc) ->
    Amount = wh_json:get_number_value(<<"amount">>, Addon, 0)*100,
    Quantity = wh_json:get_number_value(<<"quantity">>, Addon, 0),
    calculate(Addons, (Amount*Quantity+Acc)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_braintree(api_seconds()) -> ne_binary().
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
-spec utc_to_gregorian_seconds(ne_binary()) -> api_seconds().
utc_to_gregorian_seconds(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T"
                           ,H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary
                         >>
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
calculate_amount(JObjs) ->
    Adder = fun (JObj, Amount) ->
                    Amount + wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
            end,
    lists:foldl(Adder, 0, JObjs).

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
    %% https://www.braintreepayments.com/docs/ruby/reference/processor_responses
    wh_util:to_integer(RespCode) < 2000.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec already_charged(ne_binary() | integer() , integer() | wh_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
    lager:debug("checking if ~s has been charged for transaction of type ~p today", [BillingId, Code]),
    BtTransactions = braintree_transaction:find_by_customer(BillingId),
    Transactions = [braintree_transaction:record_to_json(BtTransaction)
                    || BtTransaction <- BtTransactions
                   ],
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
already_charged_transaction(_ , ?BT_TRANS_VOIDED, _, Transaction) ->
    _Id = wh_doc:id(Transaction),
    lager:debug("transaction was voided (~s)", [_Id]),
    'false';
already_charged_transaction(Code , _, Code, Transaction) ->
    <<Year:4/binary, _:1/binary
      ,Month:2/binary, _:1/binary
      ,Day:2/binary
      ,_/binary
    >> = wh_json:get_value(<<"created_at">>, Transaction),
    Id = wh_doc:id(Transaction),
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
    KeySingle = [<<"braintree">>, <<"discounts">>, <<"single">>],
    DiscountId = wh_service_item:bookkeeper(KeySingle, ServiceItem),
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
    KeyCumulative = [<<"braintree">>, <<"discounts">>, <<"cumulative">>],
    DiscountId = wh_service_item:bookkeeper(KeyCumulative, ServiceItem),
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
-spec update_subscriptions(ne_binary(), braintree_subscription:subscription(), updates()) ->
                                  updates().
update_subscriptions(PlanId, Subscription, #wh_service_updates{bt_subscriptions=Subscriptions}=Updates) ->
    Update = #wh_service_update{bt_subscription = Subscription
                                ,plan_id = PlanId
                               },
    Replaced = lists:keystore(PlanId, #wh_service_update.plan_id, Subscriptions, Update),
    Updates#wh_service_updates{bt_subscriptions = Replaced}.

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
-spec fetch_or_create_subscription(ne_binary(), updates() | braintree_customer:customer()) ->
                                          braintree_subscription:subscription().
fetch_or_create_subscription(PlanId, #wh_service_updates{bt_subscriptions=[]
                                                         ,bt_customer=Customer
                                                        }) ->
    fetch_or_create_subscription(PlanId, Customer);
fetch_or_create_subscription(PlanId, #wh_service_updates{bt_subscriptions=Subscriptions
                                                         ,bt_customer=Customer
                                                        }) ->
    case lists:keyfind(PlanId, #wh_service_update.plan_id, Subscriptions) of
        'false' ->
            fetch_or_create_subscription(PlanId, Customer);
        #wh_service_update{bt_subscription=Subscription} -> Subscription
    end;
fetch_or_create_subscription(PlanId, #bt_customer{}=Customer) ->
    try braintree_customer:get_subscription(PlanId, Customer) of
        Subscription ->
            lager:debug("found subscription ~s for plan id ~s"
                        ,[braintree_subscription:get_id(Subscription), PlanId]
                       ),
            braintree_subscription:reset(Subscription)
    catch
        'throw':{'not_found', _} ->
            lager:debug("creating new subscription for plan id ~s", [PlanId]),
            braintree_customer:new_subscription(PlanId, Customer)
    end.

%% @private
-spec prepare_subscription(wh_service_item:item(), ne_binary(), ne_binary(), updates()) ->
                                  braintree_subscription:subscription().
prepare_subscription(ServiceItem, AddOnId, PlanId, Updates) ->
    Routines = [fun(S) ->
                        Quantity = wh_service_item:quantity(ServiceItem),
                        braintree_subscription:update_addon_quantity(S, AddOnId, Quantity)
                end
               ,fun(S) ->
                        Rate = wh_service_item:rate(ServiceItem),
                        braintree_subscription:update_addon_amount(S, AddOnId, Rate)
                end
               ,fun(S) -> handle_single_discount(ServiceItem, S) end
               ,fun(S) -> handle_cumulative_discounts(ServiceItem, S) end
               ],
    Subscription = fetch_or_create_subscription(PlanId, Updates),
    lists:foldl(fun(F, S) -> F(S) end, Subscription, Routines).
