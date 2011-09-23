%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_transaction).

-include("braintree.hrl").

-export([create/1, create/2]).
-export([sale/1, sale/2, quick_sale/2, quick_sale/3]).
-export([credit/1, credit/2, quick_credit/2]).
-export([void/1, refund/1, refund/2]).
-export([find/1]).
-export([xml_to_record/1, record_to_xml/1]).

-import(braintree_utils, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Transaction) -> bt_result() when
      Transaction :: #bt_transaction{}.
-spec create/2 :: (CustomerId, Transaction) -> bt_result() when
      CustomerId :: string() | binary(),
      Transaction :: #bt_transaction{}.

create(Transaction) ->
    try
        true = validate_id(Transaction#bt_transaction.customer_id, true),
        Request = record_to_xml(Transaction, true),
        case braintree_request:post("/transactions", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

create(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a sale transaction
%% @end
%%--------------------------------------------------------------------
-spec sale/1 :: (Transaction) -> bt_result() when
      Transaction :: #bt_transaction{}.
-spec sale/2 :: (CustomerId, Transaction) -> bt_result() when
      CustomerId :: binary() | string(),
      Transaction :: #bt_transaction{}.
-spec quick_sale/2 :: (CustomerId, Amount) -> bt_result() when
      CustomerId :: binary() | string(),
      Amount :: binary() | string().

sale(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE}).

sale(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_SALE, customer_id=CustomerId}).

quick_sale(CustomerId, Amount) ->
    sale(CustomerId, #bt_transaction{amount=Amount}).

quick_sale(CustomerId, Token, Amount) ->
    sale(CustomerId, #bt_transaction{amount=Amount, payment_token=Token}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a credit transaction
%% @end
%%--------------------------------------------------------------------
-spec credit/1 :: (Transaction) -> bt_result() when
      Transaction :: #bt_transaction{}.
-spec credit/2 :: (CustomerId, Transaction) -> bt_result() when
      CustomerId :: binary() | string(),
      Transaction :: #bt_transaction{}.
-spec quick_credit/2 :: (CustomerId, Amount) -> bt_result() when
      CustomerId :: binary() | string(),
      Amount :: binary() | string().

credit(Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT}).

credit(CustomerId, Transaction) ->
    create(Transaction#bt_transaction{type=?BT_TRANS_CREDIT, customer_id=CustomerId}).

quick_credit(CustomerId, Amount) ->
    credit(CustomerId, #bt_transaction{amount=Amount, settle=false, tax_exempt=undefined}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Void transactions that have a status:authorized or submitted_for_settlement
%% @end
%%--------------------------------------------------------------------
-spec void/1 :: (Id) -> bt_result() when
      Id :: #bt_transaction{} | binary() | string().

void(#bt_transaction{id=Id}) ->
    void(Id);
void(Id) ->
    try
        true = validate_id(Id),
        case braintree_request:put("/transactions/" ++ Id ++ "/void", <<>>) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, transaction_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Refund a transaction with status: settled or settling
%% @end
%%--------------------------------------------------------------------
-spec refund/1 :: (Id) -> bt_result() when
      Id :: #bt_transaction{} | binary() | string().
-spec refund/2 :: (Id, Amount) -> bt_result() when
      Id :: #bt_transaction{} | binary() | string(),
      Amount :: undefined | binary() | string().

refund(Id) ->
    refund(Id, undefined).

refund(#bt_transaction{id=Id}, Amount) ->
    refund(Id, Amount);
refund(Id, Amount) ->
    try
        true = validate_id(Id),
        Request = record_to_xml(#bt_transaction{amount=Amount, tax_exempt=undefined, settle=undefined}, true),
        case braintree_request:put("/transactions/" ++ Id ++ "/refund", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, transaction_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (Id) -> bt_result() when
      Id :: binary() | string().
find(Id) ->
        try
            true = validate_id(Id),
            case braintree_request:get("/transactions/" ++ wh_util:to_list(Id)) of
                {ok, Xml} ->
                    {ok, xml_to_record(Xml)};
                {error, _}=E ->
                    E
            end
        catch
            error:{badmatch, _} ->
                {error, token_invalid}
        end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies that the id being used is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_id/1 :: (Id) -> boolean() when
      Id :: string() | binary().
-spec validate_id/2 :: (Id, AllowUndefined) -> boolean() when
      Id :: string() | binary(),
      AllowUndefined :: boolean().

validate_id(Id) ->
    validate_id(Id, false).

validate_id(undefined, false) ->
    false;
validate_id(Id, _) ->
    (Id =/= <<>> andalso Id =/= "")
        andalso (re:run(Id, "^[0-9A-Za-z_-]+$") =/= nomatch).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_transaction{} when
      Xml :: bt_xml().
xml_to_record(Xml) ->
    BillingAddress = braintree_address:xml_to_record(Xml, "/transaction/billing"),
    Card = braintree_card:xml_to_record(Xml, "/transaction/credit-card"),
    #bt_transaction{id = get_xml_value("/transaction/id/text()", Xml)
                    ,status = get_xml_value("/transaction/status/text()", Xml)
                    ,type = get_xml_value("/transaction/type/text()", Xml)
                    ,currency_code = get_xml_value("/transaction/currency-iso-code/text()", Xml)
                    ,amount = get_xml_value("/transaction/amount/text()", Xml)
                    ,merchant_account_id = get_xml_value("/transaction/merchant-account-id/text()", Xml)
                    ,order_id = get_xml_value("/transaction/order-id/text()", Xml)
                    ,purchase_order = get_xml_value("/transaction/purchase-order-number/text()", Xml)
                    ,created_at = get_xml_value("/transaction/created-at/text()", Xml)
                    ,update_at = get_xml_value("/transaction/updated-at/text()", Xml)
                    ,refund_id = get_xml_value("/transaction/refund-id/text()", Xml)
%%                    ,refund_ids = get_xml_value("/transaction/id/text()", Xml)
                    ,refunded_transaction = get_xml_value("/transaction/refunded-transaction-id /text()", Xml)
                    ,settlement_batch = get_xml_value("/transaction/settlement-batch-id/text()", Xml)
                    ,avs_error_code = get_xml_value("/transaction/avs-error-response-code/text()", Xml)
                    ,avs_postal_response = get_xml_value("/transaction/avs-postal-code-response-code/text()", Xml)
                    ,avs_street_response = get_xml_value("/transaction/avs-street-address-response-code/text()", Xml)
                    ,ccv_response_code = get_xml_value("/transaction/cvv-response-code/text()", Xml)
                    ,gateway_rejection = get_xml_value("/transaction/gateway-rejection-reason/text()", Xml)
                    ,processor_authorization_code = get_xml_value("/transaction/processor-authorization-code/text()", Xml)
                    ,processor_response_code = get_xml_value("/transaction/processor-response-code/text()", Xml)
                    ,processor_response_text = get_xml_value("/transaction/processor-response-text/text()", Xml)
                    ,tax_amount = get_xml_value("/transaction/tax-amount/text()", Xml)
                    ,tax_exempt = wh_util:is_true(get_xml_value("/transaction/tax-exempt/text()", Xml))
                    ,billing_address = BillingAddress
                    ,shipping_address = braintree_address:xml_to_record(Xml, "/transaction/shipping")
                    ,customer = braintree_customer:xml_to_record(Xml, "/transaction/customer")
                    ,card = Card#bt_card{billing_address=BillingAddress}
                    ,subscription_id = undefined = get_xml_value("/transaction/subscription-id/text()", Xml)}.
%%                    ,add_ons = undefined = get_xml_value("/transaction/id/text()", Xml)
%%                    ,discounts = undefined = get_xml_value("/transaction/id/text()", Xml)
%%                    ,descriptor = undefined = get_xml_value("/transaction/id/text()", Xml)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Transaction) -> bt_xml() when
      Transaction :: #bt_transaction{}.
-spec record_to_xml/2 :: (Transaction, ToString) -> bt_xml() when
      Transaction :: #bt_transaction{},
      ToString :: boolean().

record_to_xml(Transaction) ->
    record_to_xml(Transaction, false).

record_to_xml(Transaction, ToString) ->
    Props = [{'type', Transaction#bt_transaction.type}
             ,{'amount', Transaction#bt_transaction.amount}
             ,{'customer-id', Transaction#bt_transaction.customer_id}
             ,{'merchant-account-id', Transaction#bt_transaction.merchant_account_id}
             ,{'order-id', Transaction#bt_transaction.order_id}
             ,{'purchase-order-number', Transaction#bt_transaction.purchase_order}
             ,{'payment-method-token', Transaction#bt_transaction.payment_token}
             ,{'shipping-address-id', Transaction#bt_transaction.shipping_address_id}
             ,{'tax-amount', Transaction#bt_transaction.tax_amount}
             ,{'tax-exempt', Transaction#bt_transaction.tax_exempt}],
    Conditionals = [fun(#bt_transaction{billing_address=undefined}, P) ->
                            P;
                       (#bt_transaction{billing_address=BA}, P) ->
                            [{'billing', braintree_address:record_to_xml(BA)}|P]
                    end,
                    fun(#bt_transaction{shipping_address=undefined}, P) ->
                            P;
                       (#bt_transaction{shipping_address=SA}, P) ->
                            [{'shipping', braintree_address:record_to_xml(SA)}|P]
                    end,
                    fun(#bt_transaction{card=undefined}, P) ->
                            P;
                       (#bt_transaction{card=CC}, P) ->
                            [{'credit-card', braintree_card:record_to_xml(CC)}|P]
                    end,
                    fun(#bt_transaction{customer=undefined}, P) ->
                            P;
                       (#bt_transaction{customer=Cust}, P) ->
                            [{'customer', braintree_customer:record_to_xml(Cust)}|P]
                    end,
                    fun(#bt_transaction{store_in_vault=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'store-in-vault', true}]}|P];
                                Options ->
                                    Options1 = [{'store-in-vault', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_transaction{store_on_success=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'store-in-vault-on-success', true}]}|P];
                                Options ->
                                    Options1 = [{'store-in-vault-on-success', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_transaction{store_on_success=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'store-shipping-address-in-vault', true}]}|P];
                                Options ->
                                    Options1 = [{'store-shipping-address-in-vault', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_transaction{change_billing_address=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'add-billing-address-to-payment-method', true}]}|P];
                                Options ->
                                    Options1 = [{'add-billing-address-to-payment-method', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_transaction{settle=true}, P) ->
                            [{'options', [{'submit-for-settlement', true}]}|P];
                       (_, P) ->
                            P
                    end],
    Props1 = lists:foldr(fun(F, P) -> F(Transaction, P) end, Props, Conditionals),
    case ToString of
        true -> make_doc_xml(Props1, 'transaction');
        false -> Props1
    end.
