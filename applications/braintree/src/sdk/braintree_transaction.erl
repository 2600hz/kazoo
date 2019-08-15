%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
-export([record_to_notification_props/1]).
-export([json_to_record/1]).

-define(MIN_AMOUNT, kapps_config:get_float(?CONFIG_CAT, <<"min_amount">>, 5.00)).
-define(CODE_UNKNOWN, 9999).

-include("braintree.hrl").

%%------------------------------------------------------------------------------
%% @doc Create the partial URL for this module.
%% @end
%%------------------------------------------------------------------------------

-spec url() -> string().
url() ->
    "/transactions/".

-spec url(kz_term:ne_binary()) -> string().
url(TransactionId) ->
    lists:append(["/transactions/", kz_term:to_list(TransactionId)]).

-spec url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
url(TransactionId, Options) ->
    lists:append(["/transactions/"
                 ,kz_term:to_list(TransactionId)
                 ,"/"
                 ,kz_term:to_list(Options)
                 ]).

%%------------------------------------------------------------------------------
%% @doc Find a transaction by ID.
%% @end
%%------------------------------------------------------------------------------

-spec find(kz_term:ne_binary()) -> bt_transaction().
find(TransactionId) ->
    Url = url(TransactionId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%------------------------------------------------------------------------------
%% @doc Find transactions by customer ID.
%% @end
%%------------------------------------------------------------------------------

-spec find_by_customer(kz_term:ne_binary()) -> bt_transactions().
find_by_customer(CustomerId) ->
    Url = url(<<"advanced_search">>),
    Props = [{'customer_id', [{'is', CustomerId}]}],
    Request = braintree_util:make_doc_xml(Props, 'search'),
    Xml = braintree_request:post(Url, Request),
    [xml_to_record(Transaction)
     || Transaction <- xmerl_xpath:string("/credit-card-transactions/transaction", Xml)
    ].

-spec find_by_customer(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> bt_transactions().
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
    Request = braintree_util:make_doc_xml(Props, 'search'),
    Xml = braintree_request:post(Url, Request),
    [xml_to_record(Transaction)
     || Transaction <- xmerl_xpath:string("/credit-card-transactions/transaction", Xml)
    ].

%%------------------------------------------------------------------------------
%% @doc Creates a new transaction using the given record.
%% @end
%%------------------------------------------------------------------------------

-spec create(bt_transaction()) -> bt_transaction().
create(#bt_transaction{}=Transaction) ->
    Url = url(),
    Request = record_to_xml(Transaction, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml).

%%------------------------------------------------------------------------------
%% @doc Creates a new transaction using the given record. Sets `customer_id'
%% field to `CustomerId'.
%% @end
%%------------------------------------------------------------------------------

-spec create(kz_term:ne_binary(), bt_transaction()) -> bt_transaction().
create(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{customer_id=CustomerId}).

%% @equiv create(Transaction#bt_transaction{type = <<"sale">>})

-spec sale(bt_transaction()) -> bt_transaction().
sale(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE}).

%%------------------------------------------------------------------------------
%% @doc Set `customer_id' to `CustomerId' and create a sale transaction.
%% @see sale/1
%% @end
%%------------------------------------------------------------------------------

-spec sale(kz_term:ne_binary(), bt_transaction()) -> bt_transaction().
sale(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE
                                     ,customer_id=CustomerId
                                     }).

-spec quick_sale(kz_term:ne_binary(), number() | kz_term:ne_binary()) -> bt_transaction().
quick_sale(CustomerId, Amount) ->
    case kz_term:to_float(Amount) < ?MIN_AMOUNT of
        'true' -> braintree_util:error_min_amount(?MIN_AMOUNT);
        'false' -> sale(CustomerId, #bt_transaction{amount=kz_term:to_binary(Amount)})
    end.

-spec quick_sale(kz_term:ne_binary(), number() | kz_term:ne_binary(), kz_term:proplist()) -> bt_transaction().
quick_sale(CustomerId, Amount, Props) ->
    case kz_term:to_float(Amount) < ?MIN_AMOUNT of
        'true' -> braintree_util:error_min_amount(?MIN_AMOUNT);
        'false' ->
            Transaction = json_to_record(kz_json:from_list(Props)),
            sale(CustomerId, Transaction#bt_transaction{amount=kz_term:to_binary(Amount)})
    end.

%%------------------------------------------------------------------------------
%% @doc Create a credit transaction.
%% @end
%%------------------------------------------------------------------------------

-spec credit(bt_transaction()) -> bt_transaction().
credit(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT}).

-spec credit(kz_term:ne_binary(), bt_transaction()) -> bt_transaction().
credit(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT
                                     ,customer_id=CustomerId
                                     }).

-spec quick_credit(kz_term:ne_binary(), kz_currency:dollars()) -> bt_transaction().
quick_credit(CustomerId, Amount) ->
    credit(CustomerId, #bt_transaction{amount=kz_term:to_binary(Amount)
                                      ,settle='false'
                                      ,tax_exempt='false'
                                      }).

%%------------------------------------------------------------------------------
%% @doc Void transactions that have a status `authorized' or `submitted_for_settlement'.
%% @end
%%------------------------------------------------------------------------------

-spec void(bt_transaction() | kz_term:ne_binary()) -> bt_transaction().
void(#bt_transaction{id=TransactionId}) ->
    void(TransactionId);
void(TransactionId) ->
    Url = url(TransactionId, <<"void">>),
    Xml = braintree_request:put(Url, <<>>),
    xml_to_record(Xml).

%%------------------------------------------------------------------------------
%% @doc Refund a transaction with status `settled' or `settling'.
%% @end
%%------------------------------------------------------------------------------

-spec refund(bt_transaction() | kz_term:ne_binary()) -> bt_transaction().
refund(TransactionId) ->
    refund(TransactionId, 'undefined').

-spec refund(bt_transaction() | kz_term:ne_binary(), kz_term:api_binary()) -> bt_transaction().
refund(#bt_transaction{id=TransactionId}, Amount) ->
    refund(TransactionId, Amount);
refund(TransactionId, Amount) ->
    Url = url(TransactionId, <<"refund">>),
    Request = record_to_xml(#bt_transaction{amount=Amount}, 'true'),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%% @equiv xml_to_record(Xml, "/transaction")

-spec xml_to_record(bt_xml()) -> bt_transaction().
xml_to_record(Xml) ->
    xml_to_record(Xml, "/transaction").

%%------------------------------------------------------------------------------
%% @doc Convert the given XML to a transaction record. Uses `Base' as base path
%% to get values from XML.
%% @end
%%------------------------------------------------------------------------------

-spec xml_to_record(bt_xml(), kz_term:deeplist()) -> bt_transaction().
xml_to_record(Xml, Base) ->
    AddOnsPath = lists:flatten([Base, "/add-ons/add-on"]),
    DiscountsPath = lists:flatten([Base, "/discounts/discount"]),
    BillingAddress = braintree_address:xml_to_record(Xml, [Base, "/billing"]),
    Card = braintree_card:xml_to_record(Xml, [Base, "/credit-card"]),
    Descriptor = braintree_descriptor:xml_to_record(Xml),
    #bt_transaction{id = kz_xml:get_value([Base, "/id/text()"], Xml)
                   ,status = kz_xml:get_value([Base, "/status/text()"], Xml)
                   ,type = kz_xml:get_value([Base, "/type/text()"], Xml)
                   ,currency_code = kz_xml:get_value([Base, "/currency-iso-code/text()"], Xml)
                   ,amount = kz_xml:get_value([Base, "/amount/text()"], Xml)
                   ,merchant_account_id = kz_xml:get_value([Base, "/merchant-account-id/text()"], Xml)
                   ,order_id = kz_xml:get_value([Base, "/order-id/text()"], Xml)
                   ,purchase_order = kz_xml:get_value([Base, "/purchase-order-number/text()"], Xml)
                   ,created_at = kz_xml:get_value([Base, "/created-at/text()"], Xml)
                   ,update_at = kz_xml:get_value([Base, "/updated-at/text()"], Xml)
                   ,refund_ids = kz_xml:get_value([Base, "/refund-ids/text()"], Xml)
                   ,refunded_transaction = kz_xml:get_value([Base, "/refunded-transaction-id /text()"], Xml)
                   ,settlement_batch = kz_xml:get_value([Base, "/settlement-batch-id/text()"], Xml)
                   ,avs_error_code = kz_xml:get_value([Base, "/avs-error-response-code/text()"], Xml)
                   ,avs_postal_response = kz_xml:get_value([Base, "/avs-postal-code-response-code/text()"], Xml)
                   ,avs_street_response = kz_xml:get_value([Base, "/avs-street-address-response-code/text()"], Xml)
                   ,ccv_response_code = kz_xml:get_value([Base, "/cvv-response-code/text()"], Xml)
                   ,gateway_rejection = kz_xml:get_value([Base, "/gateway-rejection-reason/text()"], Xml)
                   ,processor_authorization_code = kz_xml:get_value([Base, "/processor-authorization-code/text()"], Xml)
                   ,processor_response_code = kz_xml:get_value([Base, "/processor-response-code/text()"], Xml)
                   ,processor_response_text = kz_xml:get_value([Base, "/processor-response-text/text()"], Xml)
                   ,tax_amount = kz_xml:get_value([Base, "/tax-amount/text()"], Xml)
                   ,tax_exempt = kz_term:is_true(kz_xml:get_value([Base, "/tax-exempt/text()"], Xml))
                   ,billing_address = BillingAddress
                   ,shipping_address = braintree_address:xml_to_record(Xml, [Base, "/shipping"])
                   ,customer = braintree_customer:xml_to_record(Xml, [Base, "/customer"])
                   ,card = Card#bt_card{billing_address=BillingAddress}
                   ,subscription_id = kz_xml:get_value([Base, "/subscription-id/text()"], Xml)
                   ,add_ons = [braintree_addon:xml_to_record(Addon)
                               || Addon <- xmerl_xpath:string(AddOnsPath, Xml)
                              ]
                   ,discounts = [braintree_discount:xml_to_record(Discounts)
                                 || Discounts <- xmerl_xpath:string(DiscountsPath, Xml)
                                ]
                   ,descriptor = Descriptor
                   }.

%% @equiv record_to_xml(Transaction, 'false')

-spec record_to_xml(bt_transaction()) -> kz_term:proplist() | bt_xml().
record_to_xml(Transaction) ->
    record_to_xml(Transaction, 'false').

%%------------------------------------------------------------------------------
%% @doc Convert the given XML to a transaction record. If `ToString' is
%% `true' returns exported XML as string binary.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_xml(bt_transaction(), boolean()) -> kz_term:proplist() | bt_xml().
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
        'true' -> braintree_util:make_doc_xml(Props1, 'transaction');
        'false' -> Props1
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a given record into a JSON object.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_json(bt_transaction()) -> kz_json:object().
record_to_json(#bt_transaction{}=Transaction) ->
    kz_json:from_list(
      [{<<"id">>, Transaction#bt_transaction.id}
      ,{<<"status">>, Transaction#bt_transaction.status}
      ,{<<"type">>, Transaction#bt_transaction.type}
      ,{<<"currency_code">>, Transaction#bt_transaction.currency_code}
      ,{<<"amount">>, Transaction#bt_transaction.amount}
      ,{<<"merchant_account_id">>, Transaction#bt_transaction.merchant_account_id}
      ,{<<"order_id">>, Transaction#bt_transaction.order_id}
      ,{<<"purchase_order">>, Transaction#bt_transaction.purchase_order}
      ,{<<"created_at">>, Transaction#bt_transaction.created_at}
      ,{<<"update_at">>, Transaction#bt_transaction.update_at}
      ,{<<"refund_ids">>, Transaction#bt_transaction.refund_ids}
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
      ]).

-spec json_to_record(kz_term:api_object()) -> bt_transaction() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_transaction{id = kz_doc:id(JObj)
                   ,status = kz_json:get_binary_value(<<"status">>, JObj)
                   ,type = kz_json:get_binary_value(<<"type">>, JObj)
                   ,currency_code = kz_json:get_binary_value(<<"currency_code">>, JObj)
                   ,amount = kz_json:get_binary_value(<<"amount">>, JObj)
                   ,merchant_account_id = kz_json:get_binary_value(<<"merchant_account_id">>, JObj)
                   ,order_id = kz_json:get_binary_value(<<"order_id">>, JObj)
                   ,purchase_order = kz_json:get_binary_value(<<"purchase_order">>, JObj)
                   ,created_at = kz_json:get_binary_value(<<"created_at">>, JObj)
                   ,update_at = kz_json:get_binary_value(<<"update_at">>, JObj)
                   ,refund_ids = kz_json:get_binary_value(<<"refund_ids">>, JObj)
                   ,refunded_transaction = kz_json:get_binary_value(<<"refunded_transaction">>, JObj)
                   ,settlement_batch = kz_json:get_binary_value(<<"settlement_batch">>, JObj)
                   ,avs_error_code = kz_json:get_binary_value(<<"avs_error_code">>, JObj)
                   ,avs_postal_response = kz_json:get_binary_value(<<"avs_postal_response">>, JObj)
                   ,avs_street_response = kz_json:get_binary_value(<<"avs_street_response">>, JObj)
                   ,ccv_response_code = kz_json:get_binary_value(<<"ccv_response_code">>, JObj)
                   ,gateway_rejection = kz_json:get_binary_value(<<"gateway_rejection">>, JObj)
                   ,processor_authorization_code = kz_json:get_binary_value(<<"processor_authorization_code">>, JObj)
                   ,processor_response_code = kz_json:get_binary_value(<<"processor_response_code">>, JObj)
                   ,processor_response_text = kz_json:get_binary_value(<<"processor_response_text">>, JObj)
                   ,tax_amount = kz_json:get_binary_value(<<"tax_amount">>, JObj)
                   ,tax_exempt = kz_json:get_value(<<"tax_exempt">>, JObj, 'false')
                   ,billing_address = braintree_address:json_to_record(kz_json:get_value(<<"billing_address">>, JObj))
                   ,shipping_address_id  = kz_json:get_binary_value(<<"shipping_address_id">>, JObj)
                   ,shipping_address = braintree_address:json_to_record(kz_json:get_value(<<"shipping_address">>, JObj))
                   ,customer_id = kz_json:get_binary_value(<<"customer_id">>, JObj)
                   ,customer = braintree_customer:json_to_record(kz_json:get_value(<<"customer">>, JObj))
                   ,payment_token = kz_json:get_binary_value(<<"payment_token">>, JObj)
                   ,card = braintree_card:json_to_record(kz_json:get_value(<<"customer">>, JObj))
                   ,subscription_id = kz_json:get_binary_value(<<"subscription_id">>, JObj)
                   ,add_ons = [braintree_addon:json_to_record(Addon)
                               || Addon <- kz_json:get_value(<<"add_ons">>, JObj, [])
                              ]
                   ,discounts = [braintree_discount:json_to_record(Discount)
                                 || Discount <- kz_json:get_value(<<"discounts">>, JObj, [])
                                ]
                   ,descriptor = kz_json:get_binary_value(<<"descriptor">>, JObj)
                   ,store_in_vault = kz_json:get_value(<<"store_in_vault">>, JObj, 'false')
                   ,store_on_success = kz_json:get_value(<<"store_on_success">>, JObj, 'false')
                   ,change_billing_address = kz_json:get_value(<<"change_billing_address">>, JObj, 'false')
                   ,store_shipping_address = kz_json:get_value(<<"store_shipping_address">>, JObj, 'false')
                   }.

-spec record_to_notification_props(bt_transaction()) -> kz_term:proplist().
record_to_notification_props(#bt_transaction{}=BraintreeTransaction) ->
    Transaction = record_to_json(BraintreeTransaction),
    RespCode = kz_json:get_value(<<"processor_response_code">>, Transaction, ?CODE_UNKNOWN),
    props:filter_empty(
      [{<<"Success">>, kz_term:to_integer(RespCode) < 2000}
      ,{<<"Amount">>, kz_json:get_value(<<"amount">>, Transaction)}
      ,{<<"Response">>, kz_json:get_ne_value(<<"processor_response_text">>, Transaction, <<"Missing Response">>)}
      ,{<<"ID">>, kz_json:get_value(<<"id">>, Transaction)}
      ,{<<"Add-Ons">>, kz_json:get_value(<<"add_ons">>, Transaction)}
      ,{<<"Discounts">>, kz_json:get_value(<<"discounts">>, Transaction)}
      ,{<<"Billing-Address">>, kz_json:get_value(<<"billing_address">>, Transaction)}
      ,{<<"Card-Last-Four">>, kz_json:get_value([<<"card">>, <<"last_four">>], Transaction)}
      ,{<<"Tax-Amount">>, kz_json:get_value(<<"tax_amount">>, Transaction)}
      ,{<<"Timestamp">>, kz_time:now_s()}
      ,{<<"Currency-Code">>, kz_json:get_value(<<"currency_code">>, Transaction)}
      ]).
