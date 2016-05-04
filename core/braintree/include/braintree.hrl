-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-type bt_result() :: {'ok', any()}.
-type bt_xml() :: any(). %%  record_proplist() | braintree_util:char_to_bin_res().

-type braintree_failures() :: 'no_payment_token' |
                              'authentication' |
                              'authorization' |
                              'not_found' |
                              'upgrade_required' |
                              'server_error' |
                              'maintenance' |
                              'io_fault' |
                              'api_error'.

-define(BT_DEBUG, kapps_config:get_is_true(<<"braintree">>, <<"debug">>, 'false')).

-define(BT_XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(BT_API_VERSION, 2).
-define(BT_SERVER_URL, [{"production", "www.braintreegateway.com"}
                        ,{"prodcution", "www.braintreegateway.com"}
                        ,{"qa", "qa.braintreegateway.com"}
                        ,{"sandbox", "sandbox.braintreegateway.com"}
                       ]).
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

-record(bt_address, {id :: maybe(binary())
                     ,customer_id :: maybe(binary())
                     ,first_name :: maybe(binary())
                     ,last_name :: maybe(binary())
                     ,company :: maybe(binary())
                     ,street_address :: maybe(binary())
                     ,extended_address :: maybe(binary())
                     ,locality :: maybe(binary())
                     ,region :: maybe(binary())
                     ,postal_code :: maybe(binary())
                     ,country_code_two :: maybe(binary())
                     ,country_code_three :: maybe(binary())
                     ,country_code :: maybe(binary())
                     ,country_name :: maybe(binary())
                     ,update_existing = 'false' :: boolean()
                     ,created_at :: maybe(binary())
                     ,updated_at :: maybe(binary())
                    }).
-type bt_address() :: #bt_address{}.
-type bt_addresses() :: [bt_address()].

-record(bt_card, {token :: maybe(binary())
                  ,bin :: maybe(binary())
                  ,cardholder_name :: maybe(binary())
                  ,card_type :: maybe(binary())
                  ,created_at :: maybe(binary())
                  ,updated_at :: maybe(binary())
                  ,default = 'false' :: boolean()
                  ,expiration_date :: maybe(binary())
                  ,expiration_month :: maybe(binary())
                  ,expiration_year :: maybe(binary())
                  ,expired = 'false' :: boolean()
                  ,customer_location :: maybe(binary())
                  ,last_four :: maybe(binary())
                  ,number :: maybe(binary())
                  ,cvv :: maybe(binary())
                  ,customer_id :: maybe(binary())
                  ,make_default = 'true' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | ne_binary()
                  ,billing_address_id :: maybe(binary())
                  ,billing_address :: maybe(bt_address())
                 }).
-type bt_card() :: #bt_card{}.
-type bt_cards() :: [bt_card()].

-record(bt_addon, {id :: maybe(binary())
                   ,amount :: maybe(binary())
                   ,never_expires = true :: boolean()
                   ,billing_cycle :: maybe(binary())
                   ,number_of_cycles :: maybe(binary())
                   ,quantity = 0 :: integer()
                   ,inherited_from :: maybe(binary())
                   ,existing_id :: maybe(binary())
                  }).
-type bt_addon() :: #bt_addon{}.
-type bt_addons() :: [bt_addon()].

-record(bt_discount, {id :: maybe(binary())
                      ,amount :: maybe(binary())
                      ,never_expires = true :: boolean()
                      ,billing_cycle :: maybe(binary())
                      ,number_of_cycles :: maybe(binary())
                      ,quantity = 0 :: integer()
                      ,inherited_from :: maybe(binary())
                      ,existing_id :: maybe(binary())
                     }).
-type bt_discount() :: #bt_discount{}.
-type bt_discounts() :: [bt_discount()].

-record(bt_subscription, {id :: maybe(binary())
                          ,balance :: maybe(binary())
                          ,billing_dom :: maybe(binary())
                          ,billing_first_date :: maybe(binary())  %% Read only
                          ,billing_end_date :: maybe(binary())
                          ,billing_start_date :: maybe(binary())
                          ,billing_cycle :: maybe(binary())
                          ,number_of_cycles :: maybe(binary())
                          ,days_past_due :: maybe(binary())
                          ,failure_count :: maybe(binary())
                          ,merchant_account_id :: maybe(binary())
                          ,never_expires = 'true' :: maybe(boolean())
                          ,next_bill_amount :: maybe(binary())
                          ,next_cycle_amount :: maybe(binary())
                          ,next_bill_date :: maybe(binary())  %% Read only
                          ,paid_through_date :: maybe(binary())
                          ,payment_token :: maybe(binary())
                          ,plan_id :: maybe(binary())
                          ,price :: maybe(binary())
                          ,status :: maybe(binary())
                          ,trial_duration :: maybe(binary())
                          ,trial_duration_unit :: maybe(binary())
                          ,trial_period :: maybe(binary())
                          ,add_ons = [] :: bt_addons()
                          ,discounts = [] :: bt_discounts()
                          ,descriptor :: maybe(binary())
                          ,transactions :: maybe(binary())
                          ,do_not_inherit = 'true' :: maybe(boolean())
                          ,start_immediately = 'true' :: maybe(boolean())
                          ,prorate_charges = 'true' :: maybe(boolean())
                          ,revert_on_prorate_fail = 'true' :: maybe(boolean())
                          ,replace_add_ons = 'false' :: maybe(boolean())
                          ,create = 'false' :: boolean()
                         }).
-type bt_subscription() :: #bt_subscription{}.
-type bt_subscriptions() :: [bt_subscription()].

-record(bt_customer, {id :: maybe(binary())
                      ,first_name :: maybe(binary())
                      ,last_name :: maybe(binary())
                      ,company :: maybe(binary())
                      ,email :: maybe(binary())
                      ,phone :: maybe(binary())
                      ,fax :: maybe(binary())
                      ,website :: maybe(binary())
                      ,created_at :: maybe(binary())
                      ,updated_at :: maybe(binary())
                      ,credit_cards = [] :: bt_cards()
                      ,addresses = [] :: bt_addresses()
                      ,subscriptions = [] :: bt_subscriptions()
                     }).
-type bt_customer() :: #bt_customer{}.
-type bt_customers() :: [bt_customer()].

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
                         ,tax_amount :: maybe(binary())
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address :: bt_address()
                         ,shipping_address_id :: maybe(binary())
                         ,shipping_address :: bt_address()
                         ,customer_id :: maybe(binary())
                         ,customer :: bt_customer()
                         ,payment_token :: maybe(binary())
                         ,card :: bt_card()
                         ,subscription_id :: maybe(binary())
                         ,add_ons = [] :: bt_addons()
                         ,discounts = [] :: bt_discounts()
                         ,descriptor :: ne_binary()
                         ,store_in_vault = 'false' :: boolean()
                         ,store_on_success = 'false' :: boolean()
                         ,settle = 'true' :: boolean()
                         ,change_billing_address = 'false' :: boolean()
                         ,store_shipping_address = 'false' :: boolean()
                         ,is_api = 'false' :: boolean()
                         ,is_automatic = 'false' :: boolean()
                         ,is_recurring = 'false' :: boolean()
                        }).
-type bt_transaction() :: #bt_transaction{}.
-type bt_transactions() :: [bt_transaction()].

-record(bt_error, {code :: maybe(binary())
                   ,message :: maybe(binary())
                   ,attribute :: maybe(binary())
                  }).
-type bt_error() :: #bt_error{}.
-type bt_errors() :: [#bt_error{}].

-record(bt_verification, {verification_status :: maybe(binary())
                          ,processor_response_code :: maybe(binary())
                          ,processor_response_text :: maybe(binary())
                          ,cvv_response_code :: maybe(binary())
                          ,avs_response_code :: maybe(binary())
                          ,postal_response_code :: maybe(binary())
                          ,street_response_code :: maybe(binary())
                          ,gateway_rejection_reason :: maybe(binary())
                         }).
-type bt_verification() :: #bt_verification{}.

-record(bt_api_error, {errors = [] :: bt_errors()
                       ,verification :: bt_verification()
                       ,message:: maybe(binary())
                      }).
-type bt_api_error() :: #bt_api_error{}.
