-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-type bt_result() :: {'ok', term()}.
-type bt_xml() :: term(). %%  record_proplist() | braintree_util:char_to_bin_res().

-type braintree_failures() :: no_payment_token |
                              authentication |
                              authorization |
                              not_found |
                              upgrade_required |
                              server_error |
                              maintenance | 
                              io_fault |
                              api_error.

-define(BT_DEBUG, whapps_config:get_is_true(<<"braintree">>, <<"debug">>, false)).

-define(BT_XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(BT_API_VERSION, 2).
-define(BT_SERVER_URL, [{"prodcution", "www.braintreegateway.com"}
                         ,{"qa", "qa.braintreegateway.com"}
                         ,{"sandbox", "sandbox.braintreegateway.com"}]).
-define(BT_EMPTY_XML, fun() -> xmerl_scan:string("<?xml version=\"1.0\"?><empty />") end()).

-define(BT_CARD_AMEX, <<"American Express">>).
-define(BT_CARD_CARTE_BLANCHE, <<"Carte Blanche">>).
-define(BT_CARD_CHINA_UNION_PAY, <<"China UnionPay">>).
-define(BT_CARD_DINERS_CLUB_INTERNATIONAL, <<"Diners Club">>).
-define(BT_CARD_DISCOVER, <<"Discover">>).
-define(BT_CARD_JCB, <<"JCB">>).
-define(BT_CARD_LASER, <<"Laser">>).
-define(BT_CARD_MAESTRO, <<"Maestro">>).
-define(BT_CARD_MASTER_CARD, <<"MasterCard">>).
-define(BT_CARD_SOLO, <<"Solo">>).
-define(BT_CARD_SWITCH_TYPE, <<"Switch">>).
-define(BT_CARD_VISA, <<"Visa">>).
-define(BT_CARD_UNKNOWN, <<"Unknown">>).

-define(BT_INTERNATIONAL, <<"international">>).
-define(BT_US, <<"us">>).

-define(BT_TRANS_AUTHORIZATION_EXPIRED, <<"authorization_expired">>).
-define(BT_TRANS_AUTHORIZING, <<"authorizing">>).
-define(BT_TRANS_AUTHORIZED , <<"authorized">>).
-define(BT_TRANS_GATEWAY_REJECTED, <<"gateway_rejected">>).
-define(BT_TRANS_FAILED, <<"failed">>).
-define(BT_TRANS_PROCESSOR_DECLINED, <<"processor_declined">>).
-define(BT_TRANS_SETTLED, <<"settled">>).
-define(BT_TRANS_SETTLING, <<"settling">>).
-define(BT_TRANS_SUBMITTED_FOR_SETTLEMENT, <<"submitted_for_settlement">>).
-define(BT_TRANS_VOIDED, <<"voided">>).

-define(BT_TRANS_SALE, <<"sale">>).
-define(BT_TRANS_CREDIT, <<"credit">>).

-define(BT_TRANS_AVS, <<"avs">>).
-define(BT_TRANS_AVS_CVV, <<"avs_and_cvv">>).
-define(BT_TRANS_CVV, <<"cvv">>).
-define(BT_TRANS_DUP, <<"duplicate">>).

-define(BT_ACTIVE, <<"Active">>).
-define(BT_CANCELED, <<"Canceled">>).
-define(BT_EXPIRED, <<"Expired">>).
-define(BT_PAST_DUE, <<"Past Due">>).
-define(BT_PENDING, <<"Pending">>).

-define(BT_ACTIVE_STATUSES, [?BT_ACTIVE, ?BT_PENDING, ?BT_PAST_DUE]). 

-record(bt_address, {id = 'undefined' :: 'undefined' | ne_binary()
                     ,customer_id = 'undefined' :: 'undefined' | ne_binary()
                     ,first_name = 'undefined' :: 'undefined' | ne_binary()
                     ,last_name = 'undefined' :: 'undefined' | ne_binary()
                     ,company = 'undefined' :: 'undefined' | ne_binary()
                     ,street_address = 'undefined' :: 'undefined' | ne_binary()
                     ,extended_address = 'undefined' :: 'undefined' | ne_binary()
                     ,locality = 'undefined' :: 'undefined' | ne_binary()
                     ,region = 'undefined' :: 'undefined' | ne_binary()
                     ,postal_code = 'undefined' :: 'undefined' | ne_binary()
                     ,country_code_two = 'undefined' :: 'undefined' | ne_binary()
                     ,country_code_three = 'undefined' :: 'undefined' | ne_binary()
                     ,country_code = 'undefined' :: 'undefined' | ne_binary()
                     ,country_name = 'undefined' :: 'undefined' | ne_binary()
                     ,update_existing = 'false' :: boolean()
                     ,created_at = 'undefined' :: 'undefined' | ne_binary()
                     ,updated_at = 'undefined' :: 'undefined' | ne_binary()
                    }).

-record(bt_card, {token = 'undefined' :: 'undefined' | ne_binary()
                  ,bin = 'undefined' :: 'undefined' | ne_binary()
                  ,cardholder_name = 'undefined' :: 'undefined' | ne_binary()
                  ,card_type = 'undefined' :: 'undefined' | ne_binary()
                  ,created_at = 'undefined' :: 'undefined' | ne_binary()
                  ,updated_at = 'undefined' :: 'undefined' | ne_binary()
                  ,default = 'false' :: boolean()
                  ,expiration_date = 'undefined' :: 'undefined' | ne_binary()
                  ,expiration_month = 'undefined' :: 'undefined' | ne_binary()
                  ,expiration_year = 'undefined' :: 'undefined' | ne_binary()
                  ,expired = 'false' :: boolean()
                  ,customer_location = 'undefined' :: 'undefined' | ne_binary()
                  ,last_four = 'undefined' :: 'undefined' | ne_binary()
                  ,number = 'undefined' :: 'undefined' | ne_binary()
                  ,cvv = 'undefined' :: 'undefined' | ne_binary()
                  ,customer_id = 'undefined' :: 'undefined' | ne_binary()
                  ,make_default = 'false' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | ne_binary()
                  ,billing_address_id = 'undefined' :: 'undefined' | ne_binary()
                  ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                 }).

-record(bt_addon, {id = 'undefined' :: 'undefined' | ne_binary()
                   ,amount = 'undefined' :: 'undefined' | ne_binary()
                   ,never_expires = true :: boolean()
                   ,billing_cycle = 'undefined' :: 'undefined' | ne_binary()
                   ,number_of_cycles = 'undefined' :: 'undefined' | ne_binary()
                   ,quantity = 0 :: integer()
                   ,inherited_from = 'undefined' :: 'undefined' | ne_binary()
                   ,existing_id = 'undefined' :: 'undefined' | ne_binary()
                  }).

-record(bt_discount, {id = 'undefined' :: 'undefined' | ne_binary()
                      ,amount = 'undefined' :: 'undefined' | ne_binary()
                      ,never_expires = true :: boolean()
                      ,billing_cycle = 'undefined' :: 'undefined' | ne_binary()
                      ,number_of_cycles = 'undefined' :: 'undefined' | ne_binary()
                      ,quantity = 0 :: integer()
                      ,inherited_from = 'undefined' :: 'undefined' | ne_binary()
                      ,existing_id = 'undefined' :: 'undefined' | ne_binary()
                     }).

-record(bt_subscription, {id = 'undefined' :: 'undefined' | ne_binary()
                          ,balance = 'undefined' :: 'undefined' | ne_binary()
                          ,billing_dom = 'undefined' :: 'undefined' | ne_binary()
                          ,billing_first_date = 'undefined' :: 'undefined' | ne_binary()
                          ,billing_end_date = 'undefined' :: 'undefined' | ne_binary()
                          ,billing_start_date = 'undefined' :: 'undefined' | ne_binary()
                          ,billing_cycle = 'undefined' :: 'undefined' | ne_binary()
                          ,number_of_cycles = 'undefined' :: 'undefined' | ne_binary()
                          ,days_past_due = 'undefined' :: 'undefined' | ne_binary()
                          ,failure_count = 'undefined' :: 'undefined' | ne_binary()
                          ,merchant_account_id = 'undefined' :: 'undefined' | ne_binary()
                          ,never_expires = 'true' :: boolean()
                          ,next_bill_amount = 'undefined' :: 'undefined' | ne_binary()
                          ,next_cycle_amount = 'undefined' :: 'undefined' | ne_binary()
                          ,next_bill_date = 'undefined' :: 'undefined' | ne_binary()
                          ,paid_through_date = 'undefined' :: 'undefined' | ne_binary()
                          ,payment_token = 'undefined' :: 'undefined' | ne_binary()
                          ,plan_id = 'undefined' :: 'undefined' | ne_binary()
                          ,price = 'undefined' :: 'undefined' | ne_binary()
                          ,status = 'undefined' :: 'undefined' | ne_binary()
                          ,trial_duration = 'undefined' :: 'undefined' | ne_binary()
                          ,trial_duration_unit = 'undefined' :: 'undefined' | ne_binary()
                          ,trial_period = 'undefined' :: 'undefined' | ne_binary()
                          ,add_ons = [] :: [#bt_addon{},...] | []
                          ,discounts = [] :: [#bt_discount{},...] | []
                          ,descriptor = 'undefined' :: 'undefined' | ne_binary()
                          ,transactions = 'undefined' :: 'undefined' | ne_binary()
                          ,do_not_inherit = 'true' :: boolean()
                          ,start_immediately = 'true' :: boolean()
                          ,prorate_charges = 'true' :: boolean()
                          ,revert_on_prorate_fail = 'true' :: boolean()
                          ,replace_add_ons = 'false' :: boolean()
                          ,create = 'false' :: boolean() 
                         }).

-record(bt_customer, {id = 'undefined' :: 'undefined' | ne_binary()
                      ,first_name = 'undefined' :: 'undefined' | ne_binary()
                      ,last_name = 'undefined' :: 'undefined' | ne_binary()
                      ,company = 'undefined' :: 'undefined' | ne_binary()
                      ,email = 'undefined' :: 'undefined' | ne_binary()
                      ,phone = 'undefined' :: 'undefined' | ne_binary()
                      ,fax = 'undefined' :: 'undefined' | ne_binary()
                      ,website = 'undefined' :: 'undefined' | ne_binary()
                      ,created_at = 'undefined' :: 'undefined' | ne_binary()
                      ,updated_at = 'undefined' :: 'undefined' | ne_binary()
                      ,credit_cards = [] :: [] | [#bt_card{},...]
                      ,addresses = [] :: [] | [#bt_address{},...]
                      ,subscriptions = [] :: [] | [#bt_subscription{},...]
                     }).

-record(bt_transaction, {id = 'undefined' :: 'undefined' | ne_binary()
                         ,status = 'undefined' :: 'undefined' | ne_binary()
                         ,type = 'undefined' :: 'undefined' | ne_binary()
                         ,currency_code = 'undefined' :: 'undefined' | ne_binary()
                         ,amount = 'undefined' :: 'undefined' | ne_binary()
                         ,merchant_account_id = 'undefined' :: 'undefined' | ne_binary()
                         ,order_id = 'undefined' :: 'undefined' | ne_binary()
                         ,purchase_order = 'undefined' :: 'undefined' | ne_binary()
                         ,created_at = 'undefined' :: 'undefined' | ne_binary()
                         ,update_at = 'undefined' :: 'undefined' | ne_binary()
                         ,refund_id = 'undefined' :: 'undefined' | ne_binary()
                         ,refund_ids = 'undefined' :: 'undefined' | ne_binary()
                         ,refunded_transaction = 'undefined' :: 'undefined' | ne_binary()
                         ,settlement_batch = 'undefined' :: 'undefined' | ne_binary()
                         ,avs_error_code = 'undefined' :: 'undefined' | ne_binary()
                         ,avs_postal_response = 'undefined' :: 'undefined' | ne_binary()
                         ,avs_street_response = 'undefined' :: 'undefined' | ne_binary()
                         ,ccv_response_code = 'undefined' :: 'undefined' | ne_binary()
                         ,gateway_rejection = 'undefined' :: 'undefined' | ne_binary()
                         ,processor_authorization_code = 'undefined' :: 'undefined' | ne_binary()
                         ,processor_response_code = 'undefined' :: 'undefined' | ne_binary()
                         ,processor_response_text = 'undefined' :: 'undefined' | ne_binary()
                         ,tax_amount = 'undefined' :: 'undefined' | ne_binary()
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,shipping_address_id = 'undefined' :: 'undefined' | ne_binary()
                         ,shipping_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,customer_id = 'undefined' :: 'undefined' | ne_binary()
                         ,customer = 'undefined' :: 'undefined' | #bt_customer{}
                         ,payment_token = 'undefined' :: 'undefined' | ne_binary()
                         ,card = 'undefined' :: 'undefined' | #bt_card{}
                         ,subscription_id = 'undefined' :: 'undefined' | ne_binary()
                         ,add_ons = [] :: [#bt_addon{},...] | []
                         ,discounts = 'undefined' :: 'undefined' | ne_binary()
                         ,descriptor = 'undefined' :: 'undefined' | ne_binary()
                         ,store_in_vault = 'false' :: boolean()
                         ,store_on_success = 'false' :: boolean()
                         ,settle = 'true' :: boolean()
                         ,change_billing_address = 'false' :: boolean()
                         ,store_shipping_address = 'false' :: boolean()
                        }).

-record(bt_error, {code = 'undefined' :: 'undefined' | ne_binary()
                   ,message = 'undefined' :: 'undefined' | ne_binary()
                   ,attribute = 'undefined' :: 'undefined' | ne_binary()
                  }).

-record(bt_verification, {verification_status = 'undefined' :: 'undefined' | ne_binary()
                          ,processor_response_code = 'undefined' :: 'undefined' | ne_binary()
                          ,processor_response_text = 'undefined' :: 'undefined' | ne_binary()
                          ,cvv_response_code = 'undefined' :: 'undefined' | ne_binary()
                          ,avs_response_code = 'undefined' :: 'undefined' | ne_binary()
                          ,postal_response_code = 'undefined' :: 'undefined' | ne_binary()
                          ,street_response_code = 'undefined' :: 'undefined' | ne_binary()
                          ,gateway_rejection_reason = 'undefined' :: 'undefined' | ne_binary()
                         }).

-record(bt_api_error, {errors = [] :: [#bt_error{},...] | []
                       ,verification = 'undefined' :: 'undefined' | #bt_verification{}
                       ,message= 'undefined' :: 'undefined' | ne_binary()
                      }).
