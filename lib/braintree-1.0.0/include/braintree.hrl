-type bt_result() :: tuple(ok|error, term()).
-type bt_xml() :: term().

-define(BT_XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(BT_API_VERSION, 2).
-define(BT_SERVER_URL, [{"prodcution", "www.braintreegateway.com"}
                         ,{"qa", "qa.braintreegateway.com"}
                         ,{"sandbox", "sandbox.braintreegateway.com"}]).
-define(BT_EMPTY_XML, fun() -> xmerl_scan:string("<?xml version=\"1.0\"?><empty />") end()).

-define(BT_CARD_AMEX, "American Express").
-define(BT_CARD_CARTE_BLANCHE, "Carte Blanche").
-define(BT_CARD_CHINA_UNION_PAY, "China UnionPay").
-define(BT_CARD_DINERS_CLUB_INTERNATIONAL, "Diners Club").
-define(BT_CARD_DISCOVER, "Discover").
-define(BT_CARD_JCB, "JCB").
-define(BT_CARD_LASER, "Laser").
-define(BT_CARD_MAESTRO, "Maestro").
-define(BT_CARD_MASTER_CARD, "MasterCard").
-define(BT_CARD_SOLO, "Solo").
-define(BT_CARD_SWITCH_TYPE, "Switch").
-define(BT_CARD_VISA, "Visa").
-define(BT_CARD_UNKNOWN, "Unknown").

-define(BT_INTERNATIONAL, "international").
-define(BT_US, "us").

-define(BT_TRANS_AUTHORIZATION_EXPIRED, "authorization_expired").
-define(BT_TRANS_AUTHORIZING, "authorizing").
-define(BT_TRANS_AUTHORIZED , "authorized").
-define(BT_TRANS_GATEWAY_REJECTED, "gateway_rejected").
-define(BT_TRANS_FAILED, "failed").
-define(BT_TRANS_PROCESSOR_DECLINED, "processor_declined").
-define(BT_TRANS_SETTLED, "settled").
-define(BT_TRANS_SETTLING, "settling").
-define(BT_TRANS_SUBMITTED_FOR_SETTLEMENT, "submitted_for_settlement").
-define(BT_TRANS_VOIDED, "voided").

-define(BT_TRANS_SALE, "sale").
-define(BT_TRANS_CREDIT, "credit").

-define(BT_TRANS_AVS, "avs").
-define(BT_TRANS_AVS_CVV, "avs_and_cvv").
-define(BT_TRANS_CVV, "cvv").
-define(BT_TRANS_DUP, "duplicate").

-record(bt_config, {environment = "sandbox"
                    ,merchant_id = "jbz9b3njpjfq3x63"
                    ,public_key = "rknwd52wsvns7hx4"
                    ,private_key = "5f7mdcn7j9jq32cq"
                   }).

-record(bt_address, {id = undefined :: undefined | string()
                     ,customer_id = undefined :: undefined | string()
                     ,first_name = undefined :: undefined | string()
                     ,last_name = undefined :: undefined | string()
                     ,company = undefined :: undefined | string()
                     ,street_address = undefined :: undefined | string()
                     ,extended_address = undefined :: undefined | string()
                     ,locality = undefined :: undefined | string()
                     ,region = undefined :: undefined | string()
                     ,postal_code = undefined :: undefined | string()
                     ,country_code_two = undefined :: undefined | string()
                     ,country_code_three = undefined :: undefined | string()
                     ,country_code = undefined :: undefined | string()
                     ,country_name = undefined :: undefined | string()
                     ,update_existing = false :: boolean()
                     ,created_at = undefined :: undefined | string()
                     ,updated_at = undefined :: undefined | string()
                    }).

-record(bt_card, {token = undefined :: undefined | string()
                  ,bin = undefined :: undefined | string()
                  ,cardholder_name = undefined :: undefined | string()
                  ,card_type = undefined :: undefined | string()
                  ,created_at = undefined :: undefined | string()
                  ,updated_at = undefined :: undefined | string()
                  ,default = false :: boolean()
                  ,expiration_date = undefined :: undefined | string()
                  ,expiration_month = undefined :: undefined | string()
                  ,expiration_year = undefined :: undefined | string()
                  ,expired = false :: boolean()
                  ,customer_location = undefined :: undefined | string()
                  ,last_four = undefined :: undefined | string()
                  ,number = undefined :: undefined | string()
                  ,cvv = undefined :: undefined | string()
                  ,customer_id = undefined :: undefined | string()
                  ,make_default = false :: boolean()
                  ,verify = true :: boolean()
                  ,update_existing = false :: boolean()
                  ,billing_address_id = undefined :: undefined | string()
                  ,billing_address = undefined :: undefined | #bt_address{}
                 }).

-record(bt_transaction, {id
                         ,status
                         ,type
                         ,currency_code
                         ,amount
                         ,merchant_account_id
                         ,order_id
                         ,purchase_order
                         ,created_at
                         ,update_at
                         ,refund_id
                         ,refund_ids
                         ,refunded_transaction
                         ,settlement_batch
                         ,avs_error_code
                         ,avs_postal_response
                         ,avs_street_response
                         ,ccv_response_code
                         ,gateway_rejection
                         ,processor_authorization_code
                         ,processor_response_code
                         ,processor_response_text
                         ,tax_amount
                         ,tax_exempt = false
                         ,billing_address = undefined
                         ,shipping_address_id = undefined
                         ,shipping_address = undefined
                         ,customer_id = undefined
                         ,customer = undefined
                         ,payment_token = undefined
                         ,card = undefined
                         ,subscription_id = undefined
                         ,add_ons = undefined
                         ,discounts = undefined
%%                         ,descriptor = undefined
                         ,store_in_vault = false
                         ,store_on_success = false
                         ,settle = true
                         ,change_billing_address = false
                         ,store_shipping_address = false
                        }).

-record(bt_customer, {id = undefined :: undefined | string()
                      ,first_name = undefined :: undefined | string()
                      ,last_name = undefined :: undefined | string()
                      ,company = undefined :: undefined | string()
                      ,email = undefined :: undefined | string()
                      ,phone = undefined :: undefined | string()
                      ,fax = undefined :: undefined | string()
                      ,website = undefined :: undefined | string()
                      ,created_at = undefined :: undefined | string()
                      ,updated_at = undefined :: undefined | string()
                      ,credit_cards = [] :: list(#bt_card{})
                      ,addresses = [] :: list(#bt_address{})
                     }).
