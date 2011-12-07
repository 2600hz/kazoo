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
-export([find/1, find_by_customer/1]).
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1]).
-export([record_to_json/1]).

-import(braintree_util, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new transaction using the given record
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
    credit(CustomerId, #bt_transaction{amount=wh_util:to_list(Amount), settle=false, tax_exempt=false}).

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
        Request = record_to_xml(#bt_transaction{amount=Amount}, true),
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
%% Find a transaction by id
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
%% @public
%% @doc
%% Find transactions by customer id
%% @end
%%--------------------------------------------------------------------
-spec find_by_customer/1 :: (CustomerId) -> bt_result() when
      CustomerId :: binary() | string().
find_by_customer(CustomerId) ->
    try
        true = validate_id(CustomerId),
        Props = [{'customer_id', [{'is', CustomerId}]}],
        Request = make_doc_xml(Props, 'search'),
        case braintree_request:post("/transactions/advanced_search", Request) of
            {ok, Xml} ->
                {ok, [xml_to_record(Transaction)
                     || Transaction <- xmerl_xpath:string("/credit-card-transactions/transaction", Xml)]};
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
%% @public
%% @doc
%% Contert the given XML to a transaction record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_transaction{} when
      Xml :: bt_xml().
-spec xml_to_record/2 :: (Xml, Base) -> #bt_transaction{} when
      Xml :: bt_xml(),
      Base :: string().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/transaction").

xml_to_record(Xml, Base) ->
    BillingAddress = braintree_address:xml_to_record(Xml, Base ++ "/billing"),
    Card = braintree_card:xml_to_record(Xml, Base ++ "/credit-card"),
    #bt_transaction{id = get_xml_value(Base ++ "/id/text()", Xml)
                    ,status = get_xml_value(Base ++ "/status/text()", Xml)
                    ,type = get_xml_value(Base ++ "/type/text()", Xml)
                    ,currency_code = get_xml_value(Base ++ "/currency-iso-code/text()", Xml)
                    ,amount = get_xml_value(Base ++ "/amount/text()", Xml)
                    ,merchant_account_id = get_xml_value(Base ++ "/merchant-account-id/text()", Xml)
                    ,order_id = get_xml_value(Base ++ "/order-id/text()", Xml)
                    ,purchase_order = get_xml_value(Base ++ "/purchase-order-number/text()", Xml)
                    ,created_at = get_xml_value(Base ++ "/created-at/text()", Xml)
                    ,update_at = get_xml_value(Base ++ "/updated-at/text()", Xml)
                    ,refund_id = get_xml_value(Base ++ "/refund-id/text()", Xml)
%%                    ,refund_ids = get_xml_value(Base ++ "/id/text()", Xml)
                    ,refunded_transaction = get_xml_value(Base ++ "/refunded-transaction-id /text()", Xml)
                    ,settlement_batch = get_xml_value(Base ++ "/settlement-batch-id/text()", Xml)
                    ,avs_error_code = get_xml_value(Base ++ "/avs-error-response-code/text()", Xml)
                    ,avs_postal_response = get_xml_value(Base ++ "/avs-postal-code-response-code/text()", Xml)
                    ,avs_street_response = get_xml_value(Base ++ "/avs-street-address-response-code/text()", Xml)
                    ,ccv_response_code = get_xml_value(Base ++ "/cvv-response-code/text()", Xml)
                    ,gateway_rejection = get_xml_value(Base ++ "/gateway-rejection-reason/text()", Xml)
                    ,processor_authorization_code = get_xml_value(Base ++ "/processor-authorization-code/text()", Xml)
                    ,processor_response_code = get_xml_value(Base ++ "/processor-response-code/text()", Xml)
                    ,processor_response_text = get_xml_value(Base ++ "/processor-response-text/text()", Xml)
                    ,tax_amount = get_xml_value(Base ++ "/tax-amount/text()", Xml)
                    ,tax_exempt = wh_util:is_true(get_xml_value(Base ++ "/tax-exempt/text()", Xml))
                    ,billing_address = BillingAddress
                    ,shipping_address = braintree_address:xml_to_record(Xml, Base ++ "/shipping")
                    ,customer = braintree_customer:xml_to_record(Xml, Base ++ "/customer")
                    ,card = Card#bt_card{billing_address=BillingAddress}
                    ,subscription_id = get_xml_value(Base ++ "/subscription-id/text()", Xml)
                    ,add_ons = [braintree_addon:xml_to_record(Addon)
                                || Addon <- xmerl_xpath:string(Base ++ "/add-ons/add-on", Xml)]}.
%%                    ,discounts = undefined = get_xml_value(Base ++ "/id/text()", Xml)
%%                    ,descriptor = undefined = get_xml_value(Base ++ "/id/text()", Xml)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a transaction record
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (Transaction) -> json_object() when
      Transaction :: #bt_transaction{}.
record_to_json(Transaction) ->
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
                               || Addon <- Transaction#bt_transaction.add_ons]}],
    braintree_util:props_to_json(Props).
