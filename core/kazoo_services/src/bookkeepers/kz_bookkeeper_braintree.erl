%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_bookkeeper_braintree).

-export([sync/2]).
-export([is_good_standing/1]).
-export([transactions/3]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).
-export([timestamp_to_braintree/1]).

-include_lib("braintree/include/braintree.hrl").
-include("kazoo_services.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-record(kz_service_update, {bt_subscription :: braintree_subscription:subscription()
			   ,plan_id :: ne_binary()
                           }).

-record(kz_service_updates, {bt_subscriptions = [] :: [update()]
			    ,account_id :: api_binary()
			    ,bt_customer :: braintree_customer:customer()
                            }).

-type update() :: #kz_service_update{}.
-type updates() :: #kz_service_updates{}.

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
-spec sync(kz_service_items:items(), ne_binary()) -> 'ok'.
-spec sync(kz_service_item:items(), ne_binary(), updates()) -> 'ok'.
sync(Items, AccountId) ->
    ItemList = kz_service_items:to_list(Items),
    case fetch_bt_customer(AccountId, ItemList =/= []) of
        'undefined' -> 'ok';
        Customer ->
            sync(ItemList, AccountId, #kz_service_updates{bt_customer=Customer})
    end.

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscriptions(ne_binary()) -> atom() | kz_json:objects().
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
-spec commit_transactions(ne_binary(),kz_transactions:kz_transactions()) -> 'ok' | 'error'.
-spec commit_transactions(ne_binary(), kz_transactions:kz_transactions(), integer()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions) ->
    commit_transactions(BillingId, Transactions, 3).

commit_transactions(BillingId, Transactions, Try) when Try > 0 ->
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, BillingId) of
        {'error', _E} ->
            lager:error("could not open services for ~p : ~p retrying...", [BillingId, _E]),
            commit_transactions(BillingId, Transactions, Try-1);
        {'ok', JObj} ->
            NewTransactions = kz_json:get_value(<<"transactions">>, JObj, [])
                ++ kz_transactions:to_json(Transactions),
            JObj1 = kz_json:set_values([{<<"pvt_dirty">>, 'true'}
				       ,{<<"pvt_modified">>, kz_util:current_tstamp()}
				       ,{<<"transactions">>, NewTransactions}
                                       ], JObj),
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, JObj1) of
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
-spec charge_transactions(ne_binary(), kz_json:objects()) -> kz_json:objects().
-spec charge_transactions(ne_binary(), kz_json:objects(), dict:dict()) -> kz_json:objects().
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
    Code = kz_json:get_value(<<"pvt_code">>, Transaction),
    charge_transactions(BillingId
		       ,Transactions
		       ,dict:append(Code, Transaction, Dict)
                       ).

-spec handle_charged_transactions(ne_binary(), pos_integer(), kz_json:objects()) -> boolean().
handle_charged_transactions(BillingId, Code, []) ->
    lager:debug("no transaction found for ~p", [{BillingId, Code}]),
    'true';
handle_charged_transactions(BillingId, Code, JObjs) ->
    Props = [{<<"purchase_order">>, Code}],
    Amount = calculate_amount(JObjs),
    {Success, _} = braintree_quick_sale(BillingId, Amount, Props),
    Success.

-spec handle_topup(ne_binary(), kz_json:objects()) -> boolean().
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


-spec braintree_quick_sale(ne_binary(), number() | ne_binary(), kz_proplist()) ->
                                  {boolean(), bt_transaction() | 'undefined'}.
braintree_quick_sale(BillingId, Amount, Props) ->
    try braintree_transaction:quick_sale(
          BillingId
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
    kz_util:to_integer(RespCode) < 2000.


-spec send_topup_notification(boolean(), ne_binary(), integer(), bt_transaction() | 'undefined' | ne_binary()) ->
                                     boolean().
send_topup_notification(Success, BillingId, Amount, 'undefined') ->
    send_topup_notification(Success, BillingId, Amount, <<"unknown error">>);
send_topup_notification(Success, BillingId, Amount, ResponseText) when is_binary(ResponseText) ->
    Props = [{<<"Account-ID">>, BillingId}
	    ,{<<"Amount">>, Amount}
	    ,{<<"Success">>, Success}
	    ,{<<"Response">>, ResponseText}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = case
            kapps_util:amqp_pool_send(
              Props
				     ,fun kapi_notifications:publish_topup/1
             )
        of
            'ok' -> lager:debug("topup notification sent for ~s", [BillingId]);
            {'error', _R} ->
                lager:error(
                  "failed to send topup notification for ~s : ~p"
                           ,[BillingId, _R]
                 )
        end,
    Success;
send_topup_notification(Success, BillingId, Amount, BraintreeTransaction) ->
    Transaction = braintree_transaction:record_to_json(BraintreeTransaction),
    ResponseText = kz_json:get_ne_value(<<"processor_response_text">>, Transaction, <<"missing response">>),
    send_topup_notification(Success, BillingId, Amount, ResponseText).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_transactions(kz_json:objects()) -> kz_transaction:transactions().
convert_transactions(BTTransactions) ->
    [convert_transaction(Tr) || Tr <- BTTransactions].

-spec convert_transaction(kz_json:object()) -> kz_transaction:transaction().
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
    lists:foldl(fun(F, T) -> F(BTTransaction, T) end, kz_transaction:new(), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_description(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_description(_BTTransaction, Transaction) ->
    kz_transaction:set_description(?TR_DESCRIPTION, Transaction).

-spec set_bookkeeper_info(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_bookkeeper_info(BTTransaction, Transaction) ->
    kz_transaction:set_bookkeeper_info(
      kz_json:from_list([
			 {<<"id">>, kz_doc:id(BTTransaction)}
			,{<<"merchant_account_id">>, kz_json:get_value(<<"merchant_account_id">>, BTTransaction)}
			])
				      ,Transaction
     ).

-spec set_metadata(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_metadata(BTTransaction, Transaction) ->
    kz_transaction:set_metadata(BTTransaction, Transaction).

-spec set_reason(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
-spec set_reason(kz_json:object(), kz_transaction:transaction(), pos_integer() | 'undefined') -> kz_transaction:transaction().
set_reason(BTTransaction, Transaction) ->
    Code = kz_json:get_integer_value(<<"purchase_order">>, BTTransaction),
    set_reason(BTTransaction, Transaction, Code).

set_reason(BTTransaction, Transaction, 'undefined') ->
    IsApi = kz_json:is_true(<<"is_api">>, BTTransaction),
    IsRecurring = kz_json:is_true(<<"is_recurring">>, BTTransaction),
    IsProrated = transaction_is_prorated(BTTransaction),
    if
        IsProrated, IsRecurring ->
            kz_transaction:set_reason(<<"recurring_prorate">>, Transaction);
        IsApi, IsRecurring ->
            kz_transaction:set_reason(<<"recurring_prorate">>, Transaction);
        IsRecurring ->
            kz_transaction:set_reason(<<"monthly_recurring">>, Transaction);
        IsApi ->
            kz_transaction:set_reason(<<"manual_addition">>, Transaction);
        'true' ->
            kz_transaction:set_reason(<<"unknown">>, Transaction)
    end;
set_reason(_BTTransaction, Transaction, Code) ->
    kz_transaction:set_code(Code, Transaction).

-spec set_type(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_type(BTTransaction, Transaction) ->
    case kz_json:get_ne_value(<<"type">>, BTTransaction) =:= ?BT_TRANS_SALE of
        'true'  -> kz_transaction:set_type(<<"debit">>, Transaction);
        'false' -> kz_transaction:set_type(<<"credit">>, Transaction)
    end.

-spec set_status(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_status(BTTransaction, Transaction) ->
    Status = kz_json:get_value(<<"status">>, BTTransaction),
    kz_transaction:set_status(Status, Transaction).

-spec set_amount(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_amount(BTTransaction, Transaction) ->
    Amount = kz_json:get_value(<<"amount">>, BTTransaction),
    kz_transaction:set_amount(wht_util:dollars_to_units(Amount), Transaction).

-spec set_created(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_created(BTTransaction, Transaction) ->
    Created = utc_to_gregorian_seconds(kz_json:get_value(<<"created_at">>, BTTransaction)),
    kz_transaction:set_created(Created, Transaction).

-spec set_modified(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_modified(BTTransaction, Transaction) ->
    Modified = utc_to_gregorian_seconds(kz_json:get_value(<<"update_at">>, BTTransaction)),
    kz_transaction:set_modified(Modified, Transaction).

-spec set_account_id(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_account_id(BTTransaction, Transaction) ->
    CustomerId = kz_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    AccountId = kz_util:format_account_id(CustomerId, 'raw'),
    kz_transaction:set_account_id(AccountId, Transaction).

-spec set_account_db(kz_json:object(), kz_transaction:transaction()) -> kz_transaction:transaction().
set_account_db(BTTransaction, Transaction) ->
    CustormerId = kz_json:get_value([<<"customer">>, <<"id">>], BTTransaction),
    AccountDb = kz_util:format_account_id(CustormerId, 'encoded'),
    kz_transaction:set_account_db(AccountDb, Transaction).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_braintree(api_seconds()) -> ne_binary().
timestamp_to_braintree('undefined') ->
    lager:debug("timestamp undefined using current_tstamp"),
    timestamp_to_braintree(kz_util:current_tstamp());
timestamp_to_braintree(Timestamp) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(kz_util:pad_month(M))/binary, "/"
      ,(kz_util:pad_month(D))/binary, "/"
      ,(kz_util:to_binary(Y))/binary
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
      {kz_util:to_integer(Y), kz_util:to_integer(M), kz_util:to_integer(D)}
	   ,{kz_util:to_integer(H), kz_util:to_integer(Mi), kz_util:to_integer(S)}
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
-spec calculate_amount(kz_json:objects()) -> integer().
calculate_amount(JObjs) ->
    Adder = fun (JObj, Amount) ->
                    Amount + kz_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
            end,
    lists:foldl(Adder, 0, JObjs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec already_charged(ne_binary() | integer() , integer() | kz_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
    lager:debug("checking if ~s has been charged for transaction of type ~p today", [BillingId, Code]),
    BtTransactions = braintree_transaction:find_by_customer(BillingId),
    Transactions = [braintree_transaction:record_to_json(BtTransaction)
                    || BtTransaction <- BtTransactions
                   ],
    already_charged(kz_util:to_binary(Code), Transactions);

already_charged(_, []) ->
    lager:debug("no transactions found matching code or made today"),
    'false';
already_charged(Code, [Transaction|Transactions]) ->
    case
        already_charged_transaction(
          Code
				   ,kz_json:get_value(<<"status">>, Transaction)
				   ,kz_json:get_value(<<"purchase_order">>, Transaction)
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
-spec already_charged_transaction(integer(), ne_binary(), integer(), kz_json:object()) -> boolean().
already_charged_transaction(_ , ?BT_TRANS_VOIDED, _, Transaction) ->
    _Id = kz_doc:id(Transaction),
    lager:debug("transaction was voided (~s)", [_Id]),
    'false';
already_charged_transaction(Code , _, Code, Transaction) ->
    <<Year:4/binary, _:1/binary
      ,Month:2/binary, _:1/binary
      ,Day:2/binary
      ,_/binary
    >> = kz_json:get_value(<<"created_at">>, Transaction),
    Id = kz_doc:id(Transaction),
    {YearNow, M, D} = erlang:date(),
    case {kz_util:to_binary(YearNow), kz_util:pad_month(M), kz_util:pad_month(D)} of
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
-spec handle_single_discount(kz_service_item:item(), braintree_subscription:subscription()) ->
                                    braintree_subscription:subscription().
handle_single_discount(ServiceItem, Subscription) ->
    KeySingle = [<<"braintree">>, <<"discounts">>, <<"single">>],
    DiscountId = kz_service_item:bookkeeper(KeySingle, ServiceItem),
    SingleDiscount = kz_service_item:single_discount(ServiceItem),
    Rate = kz_service_item:single_discount_rate(ServiceItem),
    case
        kz_util:is_empty(SingleDiscount)
        orelse kz_util:is_empty(DiscountId)
        orelse (not erlang:is_number(Rate))
    of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, 1),
            braintree_subscription:update_discount_amount(S, DiscountId, Rate)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cumulative_discounts(kz_service_item:item(), braintree_subscription:subscription()) ->
                                         braintree_subscription:subscription().
handle_cumulative_discounts(ServiceItem, Subscription) ->
    KeyCumulative = [<<"braintree">>, <<"discounts">>, <<"cumulative">>],
    DiscountId = kz_service_item:bookkeeper(KeyCumulative, ServiceItem),
    CumulativeDiscount = kz_service_item:cumulative_discount(ServiceItem),
    Rate = kz_service_item:cumulative_discount_rate(ServiceItem),
    case
        kz_util:is_empty(CumulativeDiscount)
        orelse kz_util:is_empty(DiscountId)
        orelse (not erlang:is_number(Rate))
    of
        'true' -> Subscription;
        'false' ->
            S = braintree_subscription:update_discount_quantity(Subscription, DiscountId, CumulativeDiscount),
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
update_subscriptions(PlanId, Subscription, #kz_service_updates{bt_subscriptions=Subscriptions}=Updates) ->
    Update = #kz_service_update{bt_subscription = Subscription
			       ,plan_id = PlanId
                               },
    Replaced = lists:keystore(PlanId, #kz_service_update.plan_id, Subscriptions, Update),
    Updates#kz_service_updates{bt_subscriptions = Replaced}.

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
    try braintree_customer:find(AccountId)
    catch
        'throw':{'not_found', Error} when NewItems ->
            throw({'no_payment_token', kz_json:from_list([{<<"no_payment_token">>, Error}])});
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
fetch_or_create_subscription(PlanId, #kz_service_updates{bt_subscriptions=[]
							,bt_customer=Customer
                                                        }) ->
    fetch_or_create_subscription(PlanId, Customer);
fetch_or_create_subscription(PlanId, #kz_service_updates{bt_subscriptions=Subscriptions
							,bt_customer=Customer
                                                        }) ->
    case lists:keyfind(PlanId, #kz_service_update.plan_id, Subscriptions) of
        'false' ->
            fetch_or_create_subscription(PlanId, Customer);
        #kz_service_update{bt_subscription=Subscription} -> Subscription
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
-spec prepare_subscription(kz_service_item:item(), ne_binary(), ne_binary(), updates()) ->
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
