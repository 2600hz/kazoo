%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(braintree_transaction).

-export([url/0, url/1, url/2]).
-export([find/1]).
-export([find_by_customer/1, find_by_customer/3]).
-export([create/1, create/2]).
-export([sale/1, sale/2]).
-export([quick_sale/2, quick_sale/3]).
-export([credit/1, credit/2]).
-export([quick_credit/2]).
-export([void/1]).
-export([refund/1, refund/2]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1]).
-export([record_to_json/1]).
-export([json_to_record/1]).

-define(MIN_AMOUNT, whapps_config:get_float(<<"braintree">>, <<"min_amount">>, 5.00)).

-import('braintree_util', [make_doc_xml/2]).
-import('wh_util', [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url() -> string().
-spec url(ne_binary()) -> string().
-spec url(ne_binary(), ne_binary()) -> string().

url() ->
    "/transactions/".

url(TransactionId) ->
    lists:append(["/transactions/", wh_util:to_list(TransactionId)]).

url(TransactionId, Options) ->
    lists:append(["/transactions/"
                  ,wh_util:to_list(TransactionId)
                  ,"/"
                  ,wh_util:to_list(Options)
                 ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a transaction by id
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary()) -> bt_transaction().
find(TransactionId) ->
    Url = url(TransactionId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find transactions by customer id
%% @end
%%--------------------------------------------------------------------
-spec find_by_customer(ne_binary()) -> bt_transactions().
find_by_customer(CustomerId) ->
    Url = url(<<"advanced_search">>),
    Props = [{'customer_id', [{'is', CustomerId}]}],
    Request = make_doc_xml(Props, 'search'),
    Xml = braintree_request:post(Url, Request),
    [xml_to_record(Transaction)
     || Transaction <- xmerl_xpath:string("/credit-card-transactions/transaction", Xml)
    ].

-spec find_by_customer(ne_binary(), ne_binary(), ne_binary()) -> bt_transactions().
find_by_customer(CustomerId, Min, Max) ->
    Url = url(<<"advanced_search">>),
    Props = [{'customer_id', [{'is', CustomerId}]}
             ,{'created_at', [{'checked', <<"created">>}
                              ,{'min', Min}
                              ,{'min_hour', 0}
                              ,{'min_minute', 0}
                              ,{'max', Max}
                              ,{'max_hour', 23}
                              ,{'max_minute', 59}
                             ]}
            ],
    Request = make_doc_xml(Props, 'search'),
    Xml = braintree_request:post(Url, Request),
    [xml_to_record(Transaction)
     || Transaction <- xmerl_xpath:string("/credit-card-transactions/transaction", Xml)
    ].



%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new transaction using the given record
%% @end
%%--------------------------------------------------------------------
-spec create(bt_transaction()) -> bt_transaction().
-spec create(ne_binary(), bt_transaction()) -> bt_transaction().

create(#bt_transaction{amount=Amount}=Transaction) ->
    MaxAmount = whapps_config:get_float(<<"braintree">>, <<"max_amount">>, 200.00),
    case wh_util:to_float(Amount) >  MaxAmount of
        'true' -> braintree_util:error_max_amount(MaxAmount);
        'false' -> 'ok'
    end,
    Url = url(),
    Request = record_to_xml(Transaction, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml).

create(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a sale transaction
%% @end
%%--------------------------------------------------------------------
-spec sale(bt_transaction()) -> bt_transaction().
-spec sale(ne_binary(), bt_transaction()) -> bt_transaction().
sale(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE}).

sale(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE
                                      ,customer_id=CustomerId
                                     }).

-spec quick_sale(ne_binary(), number() | ne_binary()) -> bt_transaction().
-spec quick_sale(ne_binary(), number() | ne_binary(), wh_proplist()) -> bt_transaction().
quick_sale(CustomerId, Amount) ->
    case wh_util:to_float(Amount) < ?MIN_AMOUNT of
        'true' -> braintree_util:error_min_amount(?MIN_AMOUNT);
        'false' -> sale(CustomerId, #bt_transaction{amount=wh_util:to_binary(Amount)})
    end.

quick_sale(CustomerId, Amount, Props) ->
    case wh_util:to_float(Amount) < ?MIN_AMOUNT of
        'true' -> braintree_util:error_min_amount(?MIN_AMOUNT);
        'false' ->
            Transaction = json_to_record(wh_json:from_list(Props)),
            sale(CustomerId, Transaction#bt_transaction{amount=wh_util:to_binary(Amount)})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a credit transaction
%% @end
%%--------------------------------------------------------------------
-spec credit(bt_transaction()) -> bt_transaction().
-spec credit(ne_binary(), bt_transaction()) -> bt_transaction().
credit(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT}).

credit(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT
                                      ,customer_id=CustomerId
                                     }).

-spec quick_credit(ne_binary(), ne_binary()) -> bt_transaction().
quick_credit(CustomerId, Amount) ->
    credit(CustomerId, #bt_transaction{amount=wh_util:to_binary(Amount)
                                       ,settle='false'
                                       ,tax_exempt='false'
                                      }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Void transactions that have a status:authorized or submitted_for_settlement
%% @end
%%--------------------------------------------------------------------
-spec void(bt_transaction() | ne_binary()) -> bt_transaction().
void(#bt_transaction{id=TransactionId}) ->
    void(TransactionId);
void(TransactionId) ->
    Url = url(TransactionId, <<"void">>),
    Xml = braintree_request:put(Url, <<>>),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Refund a transaction with status: settled or settling
%% @end
%%--------------------------------------------------------------------
-spec refund(bt_transaction() | ne_binary()) -> bt_transaction().
-spec refund(bt_transaction() | ne_binary(), api_binary()) -> bt_transaction().

refund(TransactionId) ->
    refund(TransactionId, 'undefined').

refund(#bt_transaction{id=TransactionId}, Amount) ->
    refund(TransactionId, Amount);
refund(TransactionId, Amount) ->
    Url = url(TransactionId, <<"refund">>),
    Request = record_to_xml(#bt_transaction{amount=Amount}, 'true'),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a transaction record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record(bt_xml()) -> bt_transaction().
-spec xml_to_record(bt_xml(), wh_deeplist()) -> bt_transaction().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/transaction").

xml_to_record(Xml, Base) ->
    AddOnsPath = lists:flatten([Base, "/add-ons/add-on"]),
    DiscountsPath = lists:flatten([Base, "/discounts/discount"]),
    BillingAddress = braintree_address:xml_to_record(Xml, [Base, "/billing"]),
    Card = braintree_card:xml_to_record(Xml, [Base, "/credit-card"]),
    StatusHistory = get_status_history(Xml, Base),
    #bt_transaction{id = get_xml_value([Base, "/id/text()"], Xml)
                    ,status = get_xml_value([Base, "/status/text()"], Xml)
                    ,type = get_xml_value([Base, "/type/text()"], Xml)
                    ,currency_code = get_xml_value([Base, "/currency-iso-code/text()"], Xml)
                    ,amount = get_xml_value([Base, "/amount/text()"], Xml)
                    ,merchant_account_id = get_xml_value([Base, "/merchant-account-id/text()"], Xml)
                    ,order_id = get_xml_value([Base, "/order-id/text()"], Xml)
                    ,purchase_order = get_xml_value([Base, "/purchase-order-number/text()"], Xml)
                    ,created_at = get_xml_value([Base, "/created-at/text()"], Xml)
                    ,update_at = get_xml_value([Base, "/updated-at/text()"], Xml)
                    ,refund_id = get_xml_value([Base, "/refund-id/text()"], Xml)
                    ,refunded_transaction = get_xml_value([Base, "/refunded-transaction-id /text()"], Xml)
                    ,settlement_batch = get_xml_value([Base, "/settlement-batch-id/text()"], Xml)
                    ,avs_error_code = get_xml_value([Base, "/avs-error-response-code/text()"], Xml)
                    ,avs_postal_response = get_xml_value([Base, "/avs-postal-code-response-code/text()"], Xml)
                    ,avs_street_response = get_xml_value([Base, "/avs-street-address-response-code/text()"], Xml)
                    ,ccv_response_code = get_xml_value([Base, "/cvv-response-code/text()"], Xml)
                    ,gateway_rejection = get_xml_value([Base, "/gateway-rejection-reason/text()"], Xml)
                    ,processor_authorization_code = get_xml_value([Base, "/processor-authorization-code/text()"], Xml)
                    ,processor_response_code = get_xml_value([Base, "/processor-response-code/text()"], Xml)
                    ,processor_response_text = get_xml_value([Base, "/processor-response-text/text()"], Xml)
                    ,tax_amount = get_xml_value([Base, "/tax-amount/text()"], Xml)
                    ,tax_exempt = wh_util:is_true(get_xml_value([Base, "/tax-exempt/text()"], Xml))
                    ,billing_address = BillingAddress
                    ,shipping_address = braintree_address:xml_to_record(Xml, [Base, "/shipping"])
                    ,customer = braintree_customer:xml_to_record(Xml, [Base, "/customer"])
                    ,card = Card#bt_card{billing_address=BillingAddress}
                    ,subscription_id = get_xml_value([Base, "/subscription-id/text()"], Xml)
                    ,add_ons = [braintree_addon:xml_to_record(Addon)
                                || Addon <- xmerl_xpath:string(AddOnsPath, Xml)
                               ]
                    ,discounts = [braintree_discount:xml_to_record(Discounts)
                                  || Discounts <- xmerl_xpath:string(DiscountsPath, Xml)
                                 ]
                   ,is_api = props:get_value('is_api', StatusHistory)
                   ,is_automatic = props:get_value('is_automatic', StatusHistory)
                   ,is_recurring = props:get_value('is_recurring', StatusHistory)
                   }.

-spec get_status_history(bt_xml(), wh_deeplist()) -> wh_proplist().
get_status_history(Xml, Base) ->
    HistoryPath = lists:flatten([Base, "/status-history/status-event"]),
    History = xmerl_xpath:string(HistoryPath, Xml),
    Sources = get_transaction_sources(History, []),
    Users = get_users(History, []),
    [{'is_automatic', lists:all(fun('undefined') -> 'true';
                                   (_) -> 'false'
                                end, Users)}
    ,{'is_api', lists:any(fun(<<"api">>) -> 'true';
                             (<<"2600hz-api">>) -> 'true';
                             (_) -> 'false'
                          end, Sources ++ Users)}
     ,{'is_recurring', lists:any(fun(<<"recurring">>) -> 'true';
                                    (_) -> 'false'
                                 end, Sources)}
    ].

-spec get_users(bt_xml(), api_binaries()) -> api_binaries().
get_users([], Users) -> Users;
get_users([Element|Elements], Users) ->
    User = get_xml_value("user/text()", Element),
    get_users(Elements, [User|Users]).

-spec get_transaction_sources(bt_xml(), api_binaries()) -> api_binaries().
get_transaction_sources([], Sources) -> Sources;
get_transaction_sources([Element|Elements], Sources) ->
    Source = get_xml_value("transaction-source/text()", Element),
    get_transaction_sources(Elements, [Source|Sources]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a transaction record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml(bt_transaction()) -> proplist() | bt_xml().
-spec record_to_xml(bt_transaction(), boolean()) -> proplist() | bt_xml().

record_to_xml(Transaction) ->
    record_to_xml(Transaction, 'false').

record_to_xml(#bt_transaction{}=Transaction, ToString) ->
    Props = [{'type', Transaction#bt_transaction.type}
             ,{'amount', Transaction#bt_transaction.amount}
             ,{'customer-id', Transaction#bt_transaction.customer_id}
             ,{'merchant-account-id', Transaction#bt_transaction.merchant_account_id}
             ,{'order-id', Transaction#bt_transaction.order_id}
             ,{'purchase-order-number', Transaction#bt_transaction.purchase_order}
             ,{'payment-method-token', Transaction#bt_transaction.payment_token}
             ,{'shipping-address-id', Transaction#bt_transaction.shipping_address_id}
             ,{'tax-amount', Transaction#bt_transaction.tax_amount}
             ,{'tax-exempt', Transaction#bt_transaction.tax_exempt}
            ],
    Conditionals = [fun(#bt_transaction{billing_address='undefined'}, P) -> P;
                       (#bt_transaction{billing_address=BA}, P) ->
                            [{'billing', braintree_address:record_to_xml(BA)}|P]
                    end,
                    fun(#bt_transaction{shipping_address='undefined'}, P) -> P;
                       (#bt_transaction{shipping_address=SA}, P) ->
                            [{'shipping', braintree_address:record_to_xml(SA)}|P]
                    end,
                    fun(#bt_transaction{card='undefined'}, P) -> P;
                       (#bt_transaction{card=CC}, P) ->
                            [{'credit-card', braintree_card:record_to_xml(CC)}|P]
                    end,
                    fun(#bt_transaction{customer='undefined'}, P) -> P;
                       (#bt_transaction{customer=Cust}, P) ->
                            [{'customer', braintree_customer:record_to_xml(Cust)}|P]
                    end,
                    fun(#bt_transaction{store_in_vault='true'}, P) ->
                            case props:get_value('options', P) of
                                'undefined' ->
                                    [{'options', [{'store-in-vault', 'true'}]}|P];
                                Options ->
                                    [{'options', [{'store-in-vault', 'true'}|Options]}
                                     |props:delete('options', P)
                                    ]
                            end;
                       (_, P) -> P
                    end,
                    fun(#bt_transaction{store_on_success='true'}, P) ->
                            case props:get_value('options', P) of
                                'undefined' ->
                                    [{'options', [{'store-in-vault-on-success', 'true'}]}|P];
                                Options ->
                                    [{'options', [{'store-in-vault-on-success', 'true'}|Options]}
                                     |props:delete('options', P)
                                    ]
                            end;
                       (_, P) -> P
                    end,
                    fun(#bt_transaction{store_on_success='true'}, P) ->
                            case props:get_value('options', P) of
                                'undefined' ->
                                    [{'options', [{'store-shipping-address-in-vault', 'true'}]}|P];
                                Options ->
                                    [{'options', [{'store-shipping-address-in-vault', 'true'}|Options]}
                                     |props:delete('options', P)
                                    ]
                            end;
                       (_, P) -> P
                    end,
                    fun(#bt_transaction{change_billing_address='true'}, P) ->
                            case props:get_value('options', P) of
                                'undefined' ->
                                    [{'options', [{'add-billing-address-to-payment-method', 'true'}]}|P];
                                Options ->
                                    [{'options', [{'add-billing-address-to-payment-method', 'true'}|Options]}
                                     |props:delete('options', P)
                                    ]
                            end;
                       (_, P) -> P
                    end,
                    fun(#bt_transaction{settle='true'}, P) ->
                            [{'options', [{'submit-for-settlement', 'true'}]}|P];
                       (_, P) -> P
                    end],
    Props1 = lists:foldr(fun(F, P) -> F(Transaction, P) end, Props, Conditionals),
    case ToString of
        'true' -> make_doc_xml(Props1, 'transaction');
        'false' -> Props1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(bt_transaction()) -> wh_json:object().
record_to_json(#bt_transaction{}=Transaction) ->
    Props = [{<<"id">>, Transaction#bt_transaction.id}
             ,{<<"status">>, Transaction#bt_transaction.status}
             ,{<<"type">>, Transaction#bt_transaction.type}
             ,{<<"currency_code">>, Transaction#bt_transaction.currency_code}
             ,{<<"amount">>, Transaction#bt_transaction.amount}
             ,{<<"merchant_account_id">>, Transaction#bt_transaction.merchant_account_id}
             ,{<<"order_id">>, Transaction#bt_transaction.order_id}
             ,{<<"purchase_order">>, Transaction#bt_transaction.purchase_order}
             ,{<<"created_at">>, Transaction#bt_transaction.created_at}
             ,{<<"update_at">>, Transaction#bt_transaction.update_at}
             ,{<<"refund_id">>, Transaction#bt_transaction.refund_id}
             ,{<<"refunded_transaction">>, Transaction#bt_transaction.refunded_transaction}
             ,{<<"settlement_batch">>, Transaction#bt_transaction.settlement_batch}
             ,{<<"avs_error_code">>, Transaction#bt_transaction.avs_error_code}
             ,{<<"avs_postal_response">>, Transaction#bt_transaction.avs_postal_response}
             ,{<<"avs_street_response">>, Transaction#bt_transaction.avs_street_response}
             ,{<<"ccv_response_code">>, Transaction#bt_transaction.ccv_response_code}
             ,{<<"gateway_rejection">>, Transaction#bt_transaction.gateway_rejection}
             ,{<<"processor_authorization_code">>, Transaction#bt_transaction.processor_authorization_code}
             ,{<<"processor_response_code">>, Transaction#bt_transaction.processor_response_code}
             ,{<<"processor_response_text">>, Transaction#bt_transaction.processor_response_text}
             ,{<<"tax_amount">>, Transaction#bt_transaction.tax_amount}
             ,{<<"tax_exempt">>, Transaction#bt_transaction.tax_exempt}
             ,{<<"billing_address">>, braintree_address:record_to_json(Transaction#bt_transaction.billing_address)}
             ,{<<"shipping_address_id">>, Transaction#bt_transaction.shipping_address_id}
             ,{<<"shipping_address">>, braintree_address:record_to_json(Transaction#bt_transaction.shipping_address)}
             ,{<<"customer_id">>, Transaction#bt_transaction.customer_id}
             ,{<<"customer">>, braintree_customer:record_to_json(Transaction#bt_transaction.customer)}
             ,{<<"payment_token">>, Transaction#bt_transaction.payment_token}
             ,{<<"card">>, braintree_card:record_to_json(Transaction#bt_transaction.card)}
             ,{<<"subscription_id">>, Transaction#bt_transaction.subscription_id}
             ,{<<"add_ons">>, [braintree_addon:record_to_json(Addon)
                               || Addon <- Transaction#bt_transaction.add_ons
                              ]}
             ,{<<"discounts">>, [braintree_discount:record_to_json(Discounts)
                                 || Discounts <- Transaction#bt_transaction.discounts
                                ]}
             ,{<<"is_api">>, Transaction#bt_transaction.is_api}
             ,{<<"is_automatic">>, Transaction#bt_transaction.is_automatic}
             ,{<<"is_recurring">>, Transaction#bt_transaction.is_recurring}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

-spec json_to_record(api_object()) -> bt_transaction() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_transaction{
       id = wh_doc:id(JObj)
       ,status = wh_json:get_binary_value(<<"status">>, JObj)
       ,type = wh_json:get_binary_value(<<"type">>, JObj)
       ,currency_code = wh_json:get_binary_value(<<"currency_code">>, JObj)
       ,amount = wh_json:get_binary_value(<<"amount">>, JObj)
       ,merchant_account_id = wh_json:get_binary_value(<<"merchant_account_id">>, JObj)
       ,order_id = wh_json:get_binary_value(<<"order_id">>, JObj)
       ,purchase_order = wh_json:get_binary_value(<<"purchase_order">>, JObj)
       ,created_at = wh_json:get_binary_value(<<"created_at">>, JObj)
       ,update_at = wh_json:get_binary_value(<<"update_at">>, JObj)
       ,refund_id = wh_json:get_binary_value(<<"refund_id">>, JObj)
       ,refund_ids = wh_json:get_binary_value(<<"refund_ids">>, JObj)
       ,refunded_transaction = wh_json:get_binary_value(<<"refunded_transaction">>, JObj)
       ,settlement_batch = wh_json:get_binary_value(<<"settlement_batch">>, JObj)
       ,avs_error_code = wh_json:get_binary_value(<<"avs_error_code">>, JObj)
       ,avs_postal_response = wh_json:get_binary_value(<<"avs_postal_response">>, JObj)
       ,avs_street_response = wh_json:get_binary_value(<<"avs_street_response">>, JObj)
       ,ccv_response_code = wh_json:get_binary_value(<<"ccv_response_code">>, JObj)
       ,gateway_rejection = wh_json:get_binary_value(<<"gateway_rejection">>, JObj)
       ,processor_authorization_code = wh_json:get_binary_value(<<"processor_authorization_code">>, JObj)
       ,processor_response_code = wh_json:get_binary_value(<<"processor_response_code">>, JObj)
       ,processor_response_text = wh_json:get_binary_value(<<"processor_response_text">>, JObj)
       ,tax_amount = wh_json:get_binary_value(<<"tax_amount">>, JObj)
       ,tax_exempt = wh_json:get_value(<<"tax_exempt">>, JObj, 'false')
       ,billing_address = braintree_address:json_to_record(wh_json:get_value(<<"billing_address">>, JObj))
       ,shipping_address_id  = wh_json:get_binary_value(<<"shipping_address_id">>, JObj)
       ,shipping_address = braintree_address:json_to_record(wh_json:get_value(<<"shipping_address">>, JObj))
       ,customer_id = wh_json:get_binary_value(<<"customer_id">>, JObj)
       ,customer = braintree_customer:json_to_record(wh_json:get_value(<<"customer">>, JObj))
       ,payment_token = wh_json:get_binary_value(<<"payment_token">>, JObj)
       ,card = braintree_card:json_to_record(wh_json:get_value(<<"customer">>, JObj))
       ,subscription_id = wh_json:get_binary_value(<<"subscription_id">>, JObj)
       ,add_ons = [braintree_addon:json_to_record(Addon)
                   || Addon <- wh_json:get_value(<<"add_ons">>, JObj, [])
                  ]
       ,discounts = [braintree_discount:json_to_record(Discount)
                     || Discount <- wh_json:get_value(<<"discounts">>, JObj, [])
                    ]
       ,descriptor = wh_json:get_binary_value(<<"descriptor">>, JObj)
       ,store_in_vault = wh_json:get_value(<<"store_in_vault">>, JObj, 'false')
       ,store_on_success = wh_json:get_value(<<"store_on_success">>, JObj, 'false')
       ,change_billing_address = wh_json:get_value(<<"change_billing_address">>, JObj, 'false')
       ,store_shipping_address = wh_json:get_value(<<"store_shipping_address">>, JObj, 'false')
       ,is_api = wh_json:is_true(<<"is_api">>, JObj)
       ,is_automatic = wh_json:is_true(<<"is_automatic">>, JObj)
       ,is_recurring = wh_json:is_true(<<"is_recurring">>, JObj)
      }.
