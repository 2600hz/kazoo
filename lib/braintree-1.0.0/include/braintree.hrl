-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-type bt_result() :: {'ok'|'error', term()}.
-type bt_xml() :: term().

-define(BT_DEBUG, 'false').

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


-define(BT_ACTIVE, "Active").
-define(BT_CANCELED, "Canceled").
-define(BT_EXPIRED, "Expired").
-define(BT_PAST_DUE, "Past Due").
-define(BT_PENDING, "Pending").

-record(bt_address, {id = 'undefined' :: 'undefined' | string()
                     ,customer_id = 'undefined' :: 'undefined' | string()
                     ,first_name = 'undefined' :: 'undefined' | string()
                     ,last_name = 'undefined' :: 'undefined' | string()
                     ,company = 'undefined' :: 'undefined' | string()
                     ,street_address = 'undefined' :: 'undefined' | string()
                     ,extended_address = 'undefined' :: 'undefined' | string()
                     ,locality = 'undefined' :: 'undefined' | string()
                     ,region = 'undefined' :: 'undefined' | string()
                     ,postal_code = 'undefined' :: 'undefined' | string()
                     ,country_code_two = 'undefined' :: 'undefined' | string()
                     ,country_code_three = 'undefined' :: 'undefined' | string()
                     ,country_code = 'undefined' :: 'undefined' | string()
                     ,country_name = 'undefined' :: 'undefined' | string()
                     ,update_existing = 'false' :: boolean()
                     ,created_at = 'undefined' :: 'undefined' | string()
                     ,updated_at = 'undefined' :: 'undefined' | string()
                    }).

-record(bt_card, {token = 'undefined' :: 'undefined' | string()
                  ,bin = 'undefined' :: 'undefined' | string()
                  ,cardholder_name = 'undefined' :: 'undefined' | string()
                  ,card_type = 'undefined' :: 'undefined' | string()
                  ,created_at = 'undefined' :: 'undefined' | string()
                  ,updated_at = 'undefined' :: 'undefined' | string()
                  ,default = 'false' :: boolean()
                  ,expiration_date = 'undefined' :: 'undefined' | string()
                  ,expiration_month = 'undefined' :: 'undefined' | string()
                  ,expiration_year = 'undefined' :: 'undefined' | string()
                  ,expired = 'false' :: boolean()
                  ,customer_location = 'undefined' :: 'undefined' | string()
                  ,last_four = 'undefined' :: 'undefined' | string()
                  ,number = 'undefined' :: 'undefined' | string()
                  ,cvv = 'undefined' :: 'undefined' | string()
                  ,customer_id = 'undefined' :: 'undefined' | string()
                  ,make_default = 'false' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | string()
                  ,billing_address_id = 'undefined' :: 'undefined' | string()
                  ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                 }).

-record(bt_customer, {id = 'undefined' :: 'undefined' | string()
                      ,first_name = 'undefined' :: 'undefined' | string()
                      ,last_name = 'undefined' :: 'undefined' | string()
                      ,company = 'undefined' :: 'undefined' | string()
                      ,email = 'undefined' :: 'undefined' | string()
                      ,phone = 'undefined' :: 'undefined' | string()
                      ,fax = 'undefined' :: 'undefined' | string()
                      ,website = 'undefined' :: 'undefined' | string()
                      ,created_at = 'undefined' :: 'undefined' | string()
                      ,updated_at = 'undefined' :: 'undefined' | string()
                      ,credit_cards = [] :: list(#bt_card{})
                      ,addresses = [] :: list(#bt_address{})
                     }).

-record(bt_addon, {id
                   ,amount
                   ,never_expires = 'true'
                   ,billing_cycle
                   ,number_of_cycles
                   ,quantity
                   ,inherited_from
                   ,existing_id
                  }).

-record(bt_transaction, {id = 'undefined' :: 'undefined' | string()
                         ,status = 'undefined' :: 'undefined' | string()
                         ,type = 'undefined' :: 'undefined' | string()
                         ,currency_code = 'undefined' :: 'undefined' | string()
                         ,amount = 'undefined' :: 'undefined' | string()
                         ,merchant_account_id = 'undefined' :: 'undefined' | string()
                         ,order_id = 'undefined' :: 'undefined' | string()
                         ,purchase_order = 'undefined' :: 'undefined' | string()
                         ,created_at = 'undefined' :: 'undefined' | string()
                         ,update_at = 'undefined' :: 'undefined' | string()
                         ,refund_id = 'undefined' :: 'undefined' | string()
                         ,refund_ids = 'undefined' :: 'undefined' | string()
                         ,refunded_transaction = 'undefined' :: 'undefined' | string()
                         ,settlement_batch = 'undefined' :: 'undefined' | string()
                         ,avs_error_code = 'undefined' :: 'undefined' | string()
                         ,avs_postal_response = 'undefined' :: 'undefined' | string()
                         ,avs_street_response = 'undefined' :: 'undefined' | string()
                         ,ccv_response_code = 'undefined' :: 'undefined' | string()
                         ,gateway_rejection = 'undefined' :: 'undefined' | string()
                         ,processor_authorization_code = 'undefined' :: 'undefined' | string()
                         ,processor_response_code = 'undefined' :: 'undefined' | string()
                         ,processor_response_text = 'undefined' :: 'undefined' | string()
                         ,tax_amount = 'undefined' :: 'undefined' | string()
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,shipping_address_id = 'undefined' :: 'undefined' | string()
                         ,shipping_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,customer_id = 'undefined' :: 'undefined' | string()
                         ,customer = 'undefined' :: 'undefined' | #bt_customer{}
                         ,payment_token = 'undefined' :: 'undefined' | string()
                         ,card = 'undefined' :: 'undefined' | #bt_card{}
                         ,subscription_id = 'undefined' :: 'undefined' | string()
                         ,add_ons = [] :: [#bt_addon{},...] | []
                         ,discounts = 'undefined' :: 'undefined' | string()
                         ,descriptor = 'undefined' :: 'undefined' | string()
                         ,store_in_vault = 'false' :: boolean()
                         ,store_on_success = 'false' :: boolean()
                         ,settle = 'true' :: boolean()
                         ,change_billing_address = 'false' :: boolean()
                         ,store_shipping_address = 'false' :: boolean()
                        }).

-record(bt_subscription, {id = 'undefined' :: 'undefined' | string()
                          ,balance = 'undefined' :: 'undefined' | string()
                          ,billing_dom = 'undefined' :: 'undefined' | string()
                          ,billing_first_date = 'undefined' :: 'undefined' | string()
                          ,billing_end_date = 'undefined' :: 'undefined' | string()
                          ,billing_start_date = 'undefined' :: 'undefined' | string()
                          ,billing_cycle = 'undefined' :: 'undefined' | string()
                          ,number_of_cycles = 'undefined' :: 'undefined' | string()
                          ,days_past_due = 'undefined' :: 'undefined' | string()
                          ,failure_count = 'undefined' :: 'undefined' | string()
                          ,merchant_account_id = 'undefined' :: 'undefined' | string()
                          ,never_expires = 'true' :: boolean()
                          ,next_bill_amount = 'undefined' :: 'undefined' | string()
                          ,next_cycle_amount = 'undefined' :: 'undefined' | string()
                          ,next_bill_date = 'undefined' :: 'undefined' | string()
                          ,paid_through_date = 'undefined' :: 'undefined' | string()
                          ,payment_token = 'undefined' :: 'undefined' | string()
                          ,plan_id = 'undefined' :: 'undefined' | string()
                          ,price = 'undefined' :: 'undefined' | string()
                          ,status = 'undefined' :: 'undefined' | string()
                          ,trial_duration = 'undefined' :: 'undefined' | string()
                          ,trial_duration_unit = 'undefined' :: 'undefined' | string()
                          ,trial_period = 'undefined' :: 'undefined' | string()
                          ,add_ons = [] :: [#bt_addon{},...] | []
                          ,discounts = 'undefined' :: 'undefined' | string()
                          ,descriptor = 'undefined' :: 'undefined' | string()
                          ,transactions = 'undefined' :: 'undefined' | string()
                          ,do_not_inherit = 'false' :: boolean()
                          ,start_immediately = 'true' :: boolean()
                          ,prorate_charges = 'true' :: boolean()
                          ,revert_on_prorate_fail = 'true' :: boolean()
                          ,replace_add_ons = 'false' :: boolean()
                         }).

-record(bt_error, {code = 'undefined' :: 'undefined' | string()
                   ,message = 'undefined' :: 'undefined' | string()
                   ,attribute = 'undefined' :: 'undefined' | string()
                  }).

-record(bt_verification, {verification_status = 'undefined' :: 'undefined' | string()
                          ,processor_response_code = 'undefined' :: 'undefined' | string()
                          ,processor_response_text = 'undefined' :: 'undefined' | string()
                          ,cvv_response_code = 'undefined' :: 'undefined' | string()
                          ,avs_response_code = 'undefined' :: 'undefined' | string()
                          ,postal_response_code = 'undefined' :: 'undefined' | string()
                          ,street_response_code = 'undefined' :: 'undefined' | string()
                          ,gateway_rejection_reason = 'undefined' :: 'undefined' | string()
                         }).

-record(bt_api_error, {errors = [] :: list(#bt_error{})
                       ,verification = 'undefined' :: 'undefined' | #bt_verification{}
                       ,message= 'undefined' :: 'undefined' | string()
                      }).
