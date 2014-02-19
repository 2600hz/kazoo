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
-define(BT_SERVER_URL, [{"production", "www.braintreegateway.com"}
                         ,{"prodcution", "www.braintreegateway.com"}
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

-record(bt_address, {id :: api_binary()
                     ,customer_id :: api_binary()
                     ,first_name :: api_binary()
                     ,last_name :: api_binary()
                     ,company :: api_binary()
                     ,street_address :: api_binary()
                     ,extended_address :: api_binary()
                     ,locality :: api_binary()
                     ,region :: api_binary()
                     ,postal_code :: api_binary()
                     ,country_code_two :: api_binary()
                     ,country_code_three :: api_binary()
                     ,country_code :: api_binary()
                     ,country_name :: api_binary()
                     ,update_existing = 'false' :: boolean()
                     ,created_at :: api_binary()
                     ,updated_at :: api_binary()
                    }).
-type bt_address() :: #bt_address{}.
-type bt_addresses() :: [bt_address(),...] | [].

-record(bt_card, {token :: api_binary()
                  ,bin :: api_binary()
                  ,cardholder_name :: api_binary()
                  ,card_type :: api_binary()
                  ,created_at :: api_binary()
                  ,updated_at :: api_binary()
                  ,default = 'false' :: boolean()
                  ,expiration_date :: api_binary()
                  ,expiration_month :: api_binary()
                  ,expiration_year :: api_binary()
                  ,expired = 'false' :: boolean()
                  ,customer_location :: api_binary()
                  ,last_four :: api_binary()
                  ,number :: api_binary()
                  ,cvv :: api_binary()
                  ,customer_id :: api_binary()
                  ,make_default = 'true' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | ne_binary()
                  ,billing_address_id :: api_binary()
                  ,billing_address :: bt_address()
                 }).
-type bt_card() :: #bt_card{}.
-type bt_cards() :: [bt_card(),...] | [].

-record(bt_addon, {id :: api_binary()
                   ,amount :: api_binary()
                   ,never_expires = true :: boolean()
                   ,billing_cycle :: api_binary()
                   ,number_of_cycles :: api_binary()
                   ,quantity = 0 :: integer()
                   ,inherited_from :: api_binary()
                   ,existing_id :: api_binary()
                  }).
-type bt_addon() :: #bt_addon{}.
-type bt_addons() :: [bt_addon(),...] | [].

-record(bt_discount, {id :: api_binary()
                      ,amount :: api_binary()
                      ,never_expires = true :: boolean()
                      ,billing_cycle :: api_binary()
                      ,number_of_cycles :: api_binary()
                      ,quantity = 0 :: integer()
                      ,inherited_from :: api_binary()
                      ,existing_id :: api_binary()
                     }).
-type bt_discount() :: #bt_discount{}.
-type bt_discounts() :: [bt_discount(),...] | [].

-record(bt_subscription, {id :: api_binary()
                          ,balance :: api_binary()
                          ,billing_dom :: api_binary()
                          ,billing_first_date :: api_binary()
                          ,billing_end_date :: api_binary()
                          ,billing_start_date :: api_binary()
                          ,billing_cycle :: api_binary()
                          ,number_of_cycles :: api_binary()
                          ,days_past_due :: api_binary()
                          ,failure_count :: api_binary()
                          ,merchant_account_id :: api_binary()
                          ,never_expires = 'true' :: boolean()
                          ,next_bill_amount :: api_binary()
                          ,next_cycle_amount :: api_binary()
                          ,next_bill_date :: api_binary()
                          ,paid_through_date :: api_binary()
                          ,payment_token :: api_binary()
                          ,plan_id :: api_binary()
                          ,price :: api_binary()
                          ,status :: api_binary()
                          ,trial_duration :: api_binary()
                          ,trial_duration_unit :: api_binary()
                          ,trial_period :: api_binary()
                          ,add_ons = [] :: bt_addons()
                          ,discounts = [] :: bt_discounts()
                          ,descriptor :: api_binary()
                          ,transactions :: api_binary()
                          ,do_not_inherit = 'true' :: boolean()
                          ,start_immediately = 'true' :: boolean()
                          ,prorate_charges = 'true' :: boolean()
                          ,revert_on_prorate_fail = 'true' :: boolean()
                          ,replace_add_ons = 'false' :: boolean()
                          ,create = 'false' :: boolean()
                         }).
-type bt_subscription() :: #bt_subscription{}.
-type bt_subscriptions() :: [bt_subscription(),...] | [].

-record(bt_customer, {id :: api_binary()
                      ,first_name :: api_binary()
                      ,last_name :: api_binary()
                      ,company :: api_binary()
                      ,email :: api_binary()
                      ,phone :: api_binary()
                      ,fax :: api_binary()
                      ,website :: api_binary()
                      ,created_at :: api_binary()
                      ,updated_at :: api_binary()
                      ,credit_cards = [] :: bt_cards()
                      ,addresses = [] :: bt_addresses()
                      ,subscriptions = [] :: bt_subscriptions()
                     }).
-type bt_customer() :: #bt_customer{}.
-type bt_customers() :: [bt_customer(),...] | [].

-record(bt_transaction, {id :: ne_binary()
                         ,status :: ne_binary()
                         ,type :: ne_binary()
                         ,currency_code :: ne_binary()
                         ,amount :: ne_binary()
                         ,merchant_account_id :: ne_binary()
                         ,order_id :: ne_binary()
                         ,purchase_order :: ne_binary()
                         ,created_at :: ne_binary()
                         ,update_at :: ne_binary()
                         ,refund_id :: ne_binary()
                         ,refund_ids :: ne_binary()
                         ,refunded_transaction :: ne_binary()
                         ,settlement_batch :: ne_binary()
                         ,avs_error_code :: ne_binary()
                         ,avs_postal_response :: ne_binary()
                         ,avs_street_response :: ne_binary()
                         ,ccv_response_code :: ne_binary()
                         ,gateway_rejection :: ne_binary()
                         ,processor_authorization_code :: ne_binary()
                         ,processor_response_code :: ne_binary()
                         ,processor_response_text :: ne_binary()
                         ,tax_amount :: api_binary()
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address :: bt_address()
                         ,shipping_address_id :: api_binary()
                         ,shipping_address :: bt_address()
                         ,customer_id :: api_binary()
                         ,customer :: bt_customer()
                         ,payment_token :: api_binary()
                         ,card :: bt_card()
                         ,subscription_id :: api_binary()
                         ,add_ons = [] :: bt_addons()
                         ,discounts = [] :: bt_discounts()
                         ,descriptor :: ne_binary()
                         ,store_in_vault = 'false' :: boolean()
                         ,store_on_success = 'false' :: boolean()
                         ,settle = 'true' :: boolean()
                         ,change_billing_address = 'false' :: boolean()
                         ,store_shipping_address = 'false' :: boolean()
                        }).
-type bt_transaction() :: #bt_transaction{}.
-type bt_transactions() :: [bt_transaction(),...] | [].

-record(bt_error, {code :: api_binary()
                   ,message :: api_binary()
                   ,attribute :: api_binary()
                  }).
-type bt_error() :: #bt_error{}.
-type bt_errors() :: [#bt_error{},...] | [].

-record(bt_verification, {verification_status :: api_binary()
                          ,processor_response_code :: api_binary()
                          ,processor_response_text :: api_binary()
                          ,cvv_response_code :: api_binary()
                          ,avs_response_code :: api_binary()
                          ,postal_response_code :: api_binary()
                          ,street_response_code :: api_binary()
                          ,gateway_rejection_reason :: api_binary()
                         }).
-type bt_verification() :: #bt_verification{}.

-record(bt_api_error, {errors = [] :: bt_errors()
                       ,verification :: bt_verification()
                       ,message:: api_binary()
                      }).
-type bt_api_error() :: #bt_api_error{}.
