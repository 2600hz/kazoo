%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_bookkeeper_braintree).
-behaviour(kz_gen_bookkeeper).

-export([sync/2]).
-export([is_good_standing/2]).
-export([transactions/3]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).
-export([timestamp_to_braintree/1]).

-include_lib("braintree/include/braintree.hrl").
-include("services.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-record(kz_service_update, {bt_subscription :: braintree_subscription:subscription()
                           ,plan_id :: kz_term:api_ne_binary()
                           }).

-record(kz_service_updates, {bt_subscriptions = [] :: [update()]
                            ,account_id :: kz_term:api_ne_binary()
                            ,bt_customer :: braintree_customer:customer()
                            }).

-type update() :: #kz_service_update{}.
-type updates() :: #kz_service_updates{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_good_standing(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_good_standing(AccountId, _Status) ->
    try braintree_customer:find(AccountId) of
        Customer -> customer_has_card(Customer, AccountId)
    catch
        'throw':_R ->
            lager:debug("braintree customer ~s is not in good standing: ~p", [AccountId, _R]),
            'false'
    end.

-spec customer_has_card(braintree_customer:customer(), kz_term:ne_binary()) -> boolean().
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec sync(kz_service_items:items(), kz_term:ne_binary()) -> bookkeeper_sync_result().
sync(Items, AccountId) ->
    ItemList = kz_service_items:to_list(Items),
    case fetch_bt_customer(AccountId, ItemList =/= []) of
        'undefined' -> 'ok';
        Customer ->
            sync(ItemList, AccountId, #kz_service_updates{bt_customer=Customer})
    end.

-spec sync(kz_service_item:items(), kz_term:ne_binary(), updates()) -> bookkeeper_sync_result().
sync([], _AccountId, #kz_service_updates{bt_subscriptions=Subscriptions}) ->
    _ = [braintree_subscription:update(Subscription)
         || #kz_service_update{bt_subscription=Subscription} <- Subscriptions
        ],
    'ok';
sync([ServiceItem|ServiceItems], AccountId, Updates) ->
    JObj = kz_service_item:bookkeeper(<<"braintree">>, ServiceItem),
    case {kz_json:get_value(<<"plan">>, JObj), kz_json:get_value(<<"addon">>, JObj)} of
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transactions(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec subscriptions(kz_term:ne_binary()) -> atom() | kz_json:objects().
subscriptions(AccountId) ->
    try braintree_customer:find(AccountId) of
        Customer ->
            [braintree_subscription:record_to_json(Sub)
             || Sub <- braintree_customer:get_subscriptions(Customer)
            ]
    catch
        'throw':{'not_found', _} -> 'not_found';
        _:_ -> 'unknown_error'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec commit_transactions(kz_term:ne_binary(),kz_transactions:kz_transactions()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions) ->
    _ = kz_transactions:save(Transactions),
    commit_transactions(BillingId, Transactions, 3).

-spec commit_transactions(kz_term:ne_binary(), kz_transactions:kz_transactions(), integer()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions, Try) when Try > 0 ->
    case kz_services:fetch_services_doc(BillingId, true) of
        {'error', _E} ->
            lager:error("could not open services for ~p: ~p retrying...", [BillingId, _E]),
            commit_transactions(BillingId, Transactions, Try - 1);
        {'ok', ServicesJObj} ->
            NewTransactions = kz_json:get_list_value(<<"transactions">>, ServicesJObj, [])
                ++ kz_transactions:to_json(Transactions),
            Values = [{?SERVICES_PVT_IS_DIRTY, 'true'}
                     ,{?SERVICES_PVT_MODIFIED, kz_time:now_s()}
                     ,{<<"transactions">>, NewTransactions}
                     ],
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Values, ServicesJObj)) of
                {'error', _E} ->
                    lager:error("could not save services for ~p: ~p retrying...", [BillingId, _E]),
                    commit_transactions(BillingId, Transactions, Try - 1);
                {'ok', _} -> 'ok'
            end
    end;
commit_transactions(BillingId, _Transactions, _Try) ->
    lager:error("too many attempts writing transaction to services in ~p", [BillingId]),
    'error'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec charge_transactions(kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
charge_transactions(BillingId, Transactions) ->
    charge_transactions(BillingId, Transactions, dict:new()).

-spec charge_transactions(kz_term:ne_binary(), kz_json:objects(), dict:dict()) -> kz_json:objects().
charge_transactions(BillingId, [], Dict) ->
    dict:fold(fun(Code, JObjs, Acc) when Code =:= ?CODE_TOPUP ->
                      case handle_topup(BillingId, JObjs) of
                          'true' -> Acc;
                          {'true', _} -> Acc;
                          {'false', ResponseText} ->
                              [kz_json:set_value(<<"failed_reason">>, ResponseText, J)
                               || J <- JObjs
                              ] ++ Acc
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
    Code = kz_json:get_value(<<"pvt_code">>, Transaction),
    NewDict = dict:append(Code, Transaction, Dict),
    charge_transactions(BillingId, Transactions, NewDict).

-spec handle_charged_transactions(kz_term:ne_binary(), pos_integer(), kz_json:objects()) -> boolean().
handle_charged_transactions(BillingId, Code, []) ->
    lager:debug("no transaction found for ~p", [{BillingId, Code}]),
    'true';
handle_charged_transactions(BillingId, Code, JObjs) ->
    Props = [{<<"purchase_order">>, Code}],
    Amount = calculate_amount(JObjs),
    {Success, _} = braintree_quick_sale(BillingId, Amount, Props),
    Success.

-spec handle_topup(kz_term:ne_binary(), kz_json:objects()) -> 'true' | {boolean(), kz_term:ne_binary()}.
handle_topup(BillingId, []) ->
    lager:debug("no top-up transaction found for ~s", [BillingId]),
    'true';
handle_topup(BillingId, JObjs) ->
    case already_charged(BillingId, ?CODE_TOPUP) of
        'true' -> 'true';
        'false' ->
            Amount = calculate_amount(JObjs),
            Props = [{<<"purchase_order">>, ?CODE_TOPUP}],
            {Success, BraintreeTransaction} =
                braintree_quick_sale(BillingId, Amount, Props),
            send_topup_notification(Success, BillingId, Amount, BraintreeTransaction)
    end.


-spec braintree_quick_sale(kz_term:ne_binary(), number() | kz_term:ne_binary(), kz_term:proplist()) ->
                                  {boolean(), api_bt_transaction()}.
braintree_quick_sale(BillingId, Amount, Props) ->
    try braintree_transaction:quick_sale(BillingId
                                        ,wht_util:units_to_dollars(Amount)
                                        ,Props
                                        )
    of
        BraintreeTransaction ->
            {handle_quick_sale_response(BraintreeTransaction), BraintreeTransaction}
    catch
        _E:_R ->
            lager:debug("braintree quick sale ~s: ~p", [_E, _R]),
            {'false', 'undefined'}
    end.

-spec handle_quick_sale_response(bt_transaction()) -> boolean().
handle_quick_sale_response(BtTransaction) ->
    Transaction = braintree_transaction:record_to_json(BtTransaction),
    RespCode = kz_json:get_value(<<"processor_response_code">>, Transaction, ?CODE_UNKNOWN),
    lager:debug("services charge for ~s resulted in braintree response: ~s ~s"
               ,[kz_json:get_value(<<"amount">>, Transaction)
                ,kz_json:get_value(<<"id">>, Transaction)
                ,kz_json:get_value(<<"processor_response_text">>, Transaction)
                ]
               ),
    %% https://www.braintreepayments.com/docs/ruby/reference/processor_responses
    kz_term:to_integer(RespCode) < 2000.


-spec send_topup_notification(boolean(), kz_term:ne_binary(), integer(), api_bt_transaction() | kz_term:ne_binary()) ->
                                     {boolean(), kz_term:ne_binary()}.
send_topup_notification(Success, BillingId, Amount, BraintreeTransaction) ->
    Props = notification_data(Success, BillingId, Amount, BraintreeTransaction),
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_topup/1),
    {Success, props:get_value(<<"Response">>, Props)}.

-spec notification_data(boolean(), kz_term:ne_binary(), integer(), api_bt_transaction()) -> kz_term:proplist().
notification_data(Success, BillingId, Amount, 'undefined') ->
    [{<<"Account-ID">>, BillingId}
    ,{<<"Amount">>, Amount}
    ,{<<"Success">>, Success}
    ,{<<"Response">>, <<"Unknown Error">>}
    ,{<<"Timestamp">>, kz_time:now_s()}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
notification_data(_Success, BillingId, _Amount, BraintreeTransaction) ->
    props:filter_empty(
      [{<<"Account-ID">>, BillingId}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
       ++ braintree_transaction:record_to_notification_props(BraintreeTransaction)
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec convert_transactions(kz_json:objects()) -> kz_transaction:transactions().
convert_transactions(BTTransactions) ->
    [convert_transaction(Tr) || Tr <- BTTransactions].

-spec convert_transaction(kz_json:object()) -> kz_transaction:transaction().
convert_transaction(BTTransaction) ->
    Transaction = kz_transaction:new(
                    #{account_id => account_id(BTTransaction)
                     ,account_db => account_db(BTTransaction)
                     ,amount => amount(BTTransaction)
                     ,type => type(BTTransaction)
                     ,description => ?TR_DESCRIPTION
                     ,bookkeeper_info => bookkeeper_info(BTTransaction)
                     ,metadata => BTTransaction
                     ,status => status(BTTransaction)
                     ,created => created(BTTransaction)
                     ,modified => modified(BTTransaction)
                     }),
    set_reason_and_code(BTTransaction, Transaction).

-spec set_reason_and_code(kz_json:object(), kz_transaction:transaction()) ->
                                 kz_transaction:transaction().
set_reason_and_code(BTTransaction, Transaction) ->
    Code = kz_json:get_integer_value(<<"purchase_order">>, BTTransaction),
    set_reason_and_code(BTTransaction, Transaction, Code).

-spec set_reason_and_code(kz_json:object(), kz_transaction:transaction(), kz_term:api_pos_integer()) ->
                                 kz_transaction:transaction().
set_reason_and_code(BTTransaction, Transaction, 'undefined') ->
    IsApi = kz_json:is_true(<<"is_api">>, BTTransaction),
    IsRecurring = kz_json:is_true(<<"is_recurring">>, BTTransaction),
    IsProrated = transaction_is_prorated(BTTransaction),
    case {IsProrated, IsRecurring, IsApi} of
        {'true', 'true', _} ->
            kz_transaction:set_reason(wht_util:recurring_prorate(), Transaction);
        {_, 'true', 'true'} ->
            kz_transaction:set_reason(wht_util:recurring_prorate(), Transaction);
        {_, 'true', _} ->
            kz_transaction:set_reason(wht_util:monthly_recurring(), Transaction);
        {_, _, 'true'} ->
            kz_transaction:set_reason(wht_util:manual_addition(), Transaction);
        _ ->
            kz_transaction:set_reason(wht_util:default_reason(), Transaction)
    end;
set_reason_and_code(_BTTransaction, Transaction, Code) ->
    kz_transaction:set_code(Code, Transaction).

-spec bookkeeper_info(kz_json:object()) -> kz_json:object().
bookkeeper_info(BTTransaction) ->
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(BTTransaction)}
      ,{<<"merchant_account_id">>, kz_json:get_value(<<"merchant_account_id">>, BTTransaction)}
      ]).

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(BTTransaction) ->
    case kz_json:get_ne_value(<<"type">>, BTTransaction) =:= ?BT_TRANS_SALE of
        'true' -> <<"debit">>;
        'false' -> <<"credit">>
    end.

-spec status(kz_json:object()) -> kz_term:ne_binary().
status(BTTransaction) ->
    kz_json:get_ne_binary_value(<<"status">>, BTTransaction).

-spec amount(kz_json:object()) -> units().
amount(BTTransaction) ->
    Amount = kz_json:get_integer_value(<<"amount">>, BTTransaction),
    wht_util:dollars_to_units(Amount).

-spec created(kz_json:object()) -> kz_time:gregorian_seconds().
created(BTTransaction) ->
    utc_to_gregorian_seconds(kz_json:get_value(<<"created_at">>, BTTransaction)).

-spec modified(kz_json:object()) -> kz_time:gregorian_seconds().
modified(BTTransaction) ->
    utc_to_gregorian_seconds(kz_json:get_value(<<"update_at">>, BTTransaction)).

-spec account_id(kz_json:object()) -> kz_term:ne_binary().
account_id(BTTransaction) ->
    CustomerId = kz_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    kz_util:format_account_id(CustomerId, 'raw').

-spec account_db(kz_json:object()) -> kz_term:ne_binary().
account_db(BTTransaction) ->
    CustormerId = kz_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    kz_util:format_account_db(CustormerId).

-spec transaction_is_prorated(kz_json:object()) -> boolean().
transaction_is_prorated(BTransaction) ->
    Addon = calculate_addon(BTransaction),
    Discount = calculate_discount(BTransaction),
    Amount = kz_json:get_number_value(<<"amount">>, BTransaction, 0),
    (Addon - Discount) =/= Amount.

-spec calculate_addon(kz_json:object()) -> number().
calculate_addon(BTransaction) ->
    Addons = kz_json:get_value(<<"add_ons">>, BTransaction, []),
    calculate(Addons, 0).

-spec calculate_discount(kz_json:object()) -> number().
calculate_discount(BTransaction) ->
    Addons = kz_json:get_value(<<"discounts">>, BTransaction, []),
    calculate(Addons, 0).

-spec calculate(kz_json:objects(), number()) -> number().
calculate([], Acc) ->
    Acc/100;
calculate([Addon|Addons], Acc) ->
    Amount = kz_json:get_number_value(<<"amount">>, Addon, 0)*100,
    Quantity = kz_json:get_number_value(<<"quantity">>, Addon, 0),
    calculate(Addons, (Amount*Quantity+Acc)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec timestamp_to_braintree(kz_time:api_seconds()) -> kz_term:ne_binary().
timestamp_to_braintree('undefined') ->
    lager:debug("timestamp undefined using now_s"),
    timestamp_to_braintree(kz_time:now_s());
timestamp_to_braintree(Timestamp) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([kz_date:pad_month(M), "/"
                   ,kz_date:pad_month(D), "/"
                   ,kz_term:to_binary(Y)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec utc_to_gregorian_seconds(kz_term:ne_binary()) -> kz_time:api_seconds().
utc_to_gregorian_seconds(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T"
                          ,H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary
                         >>
                        ) ->
    Date = {{kz_term:to_integer(Y), kz_term:to_integer(M), kz_term:to_integer(D)}
           ,{kz_term:to_integer(H), kz_term:to_integer(Mi), kz_term:to_integer(S)}
           },
    calendar:datetime_to_gregorian_seconds(Date);
utc_to_gregorian_seconds(UTC) ->
    lager:warning("unknown UTC date format ~s", [UTC]),
    'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_amount(kz_json:objects()) -> integer().
calculate_amount(JObjs) ->
    Adder = fun (JObj, Amount) ->
                    Amount + kz_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
            end,
    lists:foldl(Adder, 0, JObjs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec already_charged(kz_term:ne_binary() | integer() , integer() | kz_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
    lager:warning("checking if ~s has been charged for transaction of type ~p today", [BillingId, Code]),
    BtTransactions = braintree_transaction:find_by_customer(BillingId),
    Transactions = [braintree_transaction:record_to_json(BtTransaction)
                    || BtTransaction <- BtTransactions
                   ],
    already_charged(kz_term:to_binary(Code), Transactions);

already_charged(_, []) ->
    lager:warning("no transactions found matching code or made today"),
    'false';
already_charged(Code, [Transaction|Transactions]) ->
    case already_charged_transaction(Code
                                    ,kz_json:get_value(<<"status">>, Transaction)
                                    ,kz_json:get_value(<<"purchase_order">>, Transaction)
                                    ,Transaction
                                    )
    of
        'true' -> 'true';
        'false' -> already_charged(Code, Transactions)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec already_charged_transaction(integer(), kz_term:ne_binary(), integer(), kz_json:object()) -> boolean().
already_charged_transaction(_ , ?BT_TRANS_VOIDED, _, Transaction) ->
    _Id = kz_doc:id(Transaction),
    lager:warning("transaction was voided (~s)", [_Id]),
    'false';
already_charged_transaction(Code , _, Code, Transaction) ->
    <<Year:4/binary, _:1/binary
     ,Month:2/binary, _:1/binary
     ,Day:2/binary
     ,_/binary
    >> = kz_json:get_value(<<"created_at">>, Transaction),
    Id = kz_doc:id(Transaction),
    {YearNow, M, D} = erlang:date(),
    case {kz_term:to_binary(YearNow), kz_date:pad_month(M), kz_date:pad_month(D)} of
        {Year, Month, Day} ->
            lager:warning("found transaction matching code and date (~s)", [Id]),
            'true';
        _Now ->
            lager:warning("found transaction matching code but not date (~s)", [Id]),
            'false'
    end;
already_charged_transaction(_, _, _, _) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_single_discount(kz_service_item:item(), braintree_subscription:subscription()) ->
                                    braintree_subscription:subscription().
handle_single_discount(ServiceItem, Subscription) ->
    KeySingle = [<<"braintree">>, <<"discounts">>, <<"single">>],
    DiscountId = kz_service_item:bookkeeper(KeySingle, ServiceItem),
    SingleDiscount = kz_service_item:single_discount(ServiceItem),
    Rate = kz_service_item:single_discount_rate(ServiceItem),
    case
        kz_term:is_empty(SingleDiscount)
        orelse kz_term:is_empty(DiscountId)
        orelse (not erlang:is_number(Rate))
    of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, 1),
            braintree_subscription:update_discount_amount(S, DiscountId, Rate)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_cumulative_discounts(kz_service_item:item(), braintree_subscription:subscription()) ->
                                         braintree_subscription:subscription().
handle_cumulative_discounts(ServiceItem, Subscription) ->
    KeyCumulative = [<<"braintree">>, <<"discounts">>, <<"cumulative">>],
    DiscountId = kz_service_item:bookkeeper(KeyCumulative, ServiceItem),
    CumulativeDiscount = kz_service_item:cumulative_discount(ServiceItem),
    Rate = kz_service_item:cumulative_discount_rate(ServiceItem),
    case
        kz_term:is_empty(CumulativeDiscount)
        orelse kz_term:is_empty(DiscountId)
        orelse (not erlang:is_number(Rate))
    of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, CumulativeDiscount),
            braintree_subscription:update_discount_amount(S, DiscountId, Rate)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_subscriptions(kz_term:ne_binary(), braintree_subscription:subscription(), updates()) ->
                                  updates().
update_subscriptions(PlanId, Subscription, #kz_service_updates{bt_subscriptions=Subscriptions}=Updates) ->
    Update = #kz_service_update{bt_subscription = Subscription
                               ,plan_id = PlanId
                               },
    Replaced = lists:keystore(PlanId, #kz_service_update.plan_id, Subscriptions, Update),
    Updates#kz_service_updates{bt_subscriptions = Replaced}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_bt_customer(kz_term:ne_binary(), boolean()) ->
                               'undefined' | braintree_customer:customer().
fetch_bt_customer(AccountId, NewItems) ->
    lager:debug("requesting braintree customer ~s", [AccountId]),
    try braintree_customer:find(AccountId)
    catch
        'throw':{'not_found', Error} when NewItems ->
            throw({'no_payment_token', kz_json:from_list([{<<"no_payment_token">>, Error}])});
        'throw':{'not_found', _} -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_or_create_subscription(kz_term:ne_binary(), updates() | braintree_customer:customer()) ->
                                          braintree_subscription:subscription().
fetch_or_create_subscription(PlanId, #kz_service_updates{bt_subscriptions=[]
                                                        ,bt_customer=Customer
                                                        }) ->
    fetch_or_create_subscription(PlanId, Customer);
fetch_or_create_subscription(PlanId, #kz_service_updates{bt_subscriptions=Subscriptions
                                                        ,bt_customer=Customer
                                                        }) ->
    case find_subscription_by_plan_id(PlanId, Subscriptions) of
        'undefined' -> fetch_or_create_subscription(PlanId, Customer);
        Subscription -> Subscription
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

-spec find_subscription_by_plan_id(kz_term:ne_binary(), [update()]) ->
                                          'undefined' |
                                          braintree_subscription:subscription().
find_subscription_by_plan_id(PlanId, Subscriptions) ->
    case [Subscription || #kz_service_update{bt_subscription=Subscription
                                            ,plan_id=UpdatePlanId
                                            } <- Subscriptions,
                          UpdatePlanId =:= PlanId
         ]
    of
        [] -> 'undefined';
        [Subscription] -> Subscription
    end.

-spec prepare_subscription(kz_service_item:item(), kz_term:ne_binary(), kz_term:ne_binary(), updates()) ->
                                  braintree_subscription:subscription().
prepare_subscription(ServiceItem, AddOnId, PlanId, Updates) ->
    Routines = [fun(S) ->
                        Quantity = kz_service_item:quantity(ServiceItem),
                        braintree_subscription:update_addon_quantity(S, AddOnId, Quantity)
                end
               ,fun(S) ->
                        case kz_service_item:rate(ServiceItem) of
                            'undefined' ->
                                braintree_subscription:update_addon_amount(S, AddOnId, 0.0);
                            Rate ->
                                braintree_subscription:update_addon_amount(S, AddOnId, Rate)
                        end
                end
               ,fun(S) -> handle_single_discount(ServiceItem, S) end
               ,fun(S) -> handle_cumulative_discounts(ServiceItem, S) end
               ],
    Subscription = fetch_or_create_subscription(PlanId, Updates),
    lists:foldl(fun(F, S) -> F(S) end, Subscription, Routines).
