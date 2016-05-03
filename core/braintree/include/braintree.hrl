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

-record(bt_address, {id :: api(binary())
                     ,customer_id :: api(binary())
                     ,first_name :: api(binary())
                     ,last_name :: api(binary())
                     ,company :: api(binary())
                     ,street_address :: api(binary())
                     ,extended_address :: api(binary())
                     ,locality :: api(binary())
                     ,region :: api(binary())
                     ,postal_code :: api(binary())
                     ,country_code_two :: api(binary())
                     ,country_code_three :: api(binary())
                     ,country_code :: api(binary())
                     ,country_name :: api(binary())
                     ,update_existing = 'false' :: boolean()
                     ,created_at :: api(binary())
                     ,updated_at :: api(binary())
                    }).
-type bt_address() :: #bt_address{}.
-type bt_addresses() :: [bt_address()].

-record(bt_card, {token :: api(binary())
                  ,bin :: api(binary())
                  ,cardholder_name :: api(binary())
                  ,card_type :: api(binary())
                  ,created_at :: api(binary())
                  ,updated_at :: api(binary())
                  ,default = 'false' :: boolean()
                  ,expiration_date :: api(binary())
                  ,expiration_month :: api(binary())
                  ,expiration_year :: api(binary())
                  ,expired = 'false' :: boolean()
                  ,customer_location :: api(binary())
                  ,last_four :: api(binary())
                  ,number :: api(binary())
                  ,cvv :: api(binary())
                  ,customer_id :: api(binary())
                  ,make_default = 'true' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | ne_binary()
                  ,billing_address_id :: api(binary())
                  ,billing_address :: api(bt_address())
                 }).
-type bt_card() :: #bt_card{}.
-type bt_cards() :: [bt_card()].

-record(bt_addon, {id :: api(binary())
                   ,amount :: api(binary())
                   ,never_expires = true :: boolean()
                   ,billing_cycle :: api(binary())
                   ,number_of_cycles :: api(binary())
                   ,quantity = 0 :: integer()
                   ,inherited_from :: api(binary())
                   ,existing_id :: api(binary())
                  }).
-type bt_addon() :: #bt_addon{}.
-type bt_addons() :: [bt_addon()].

-record(bt_discount, {id :: api(binary())
                      ,amount :: api(binary())
                      ,never_expires = true :: boolean()
                      ,billing_cycle :: api(binary())
                      ,number_of_cycles :: api(binary())
                      ,quantity = 0 :: integer()
                      ,inherited_from :: api(binary())
                      ,existing_id :: api(binary())
                     }).
-type bt_discount() :: #bt_discount{}.
-type bt_discounts() :: [bt_discount()].

-record(bt_subscription, {id :: api(binary())
                          ,balance :: api(binary())
                          ,billing_dom :: api(binary())
                          ,billing_first_date :: api(binary())  %% Read only
                          ,billing_end_date :: api(binary())
                          ,billing_start_date :: api(binary())
                          ,billing_cycle :: api(binary())
                          ,number_of_cycles :: api(binary())
                          ,days_past_due :: api(binary())
                          ,failure_count :: api(binary())
                          ,merchant_account_id :: api(binary())
                          ,never_expires = 'true' :: api(boolean())
                          ,next_bill_amount :: api(binary())
                          ,next_cycle_amount :: api(binary())
                          ,next_bill_date :: api(binary())  %% Read only
                          ,paid_through_date :: api(binary())
                          ,payment_token :: api(binary())
                          ,plan_id :: api(binary())
                          ,price :: api(binary())
                          ,status :: api(binary())
                          ,trial_duration :: api(binary())
                          ,trial_duration_unit :: api(binary())
                          ,trial_period :: api(binary())
                          ,add_ons = [] :: bt_addons()
                          ,discounts = [] :: bt_discounts()
                          ,descriptor :: api(binary())
                          ,transactions :: api(binary())
                          ,do_not_inherit = 'true' :: api(boolean())
                          ,start_immediately = 'true' :: api(boolean())
                          ,prorate_charges = 'true' :: api(boolean())
                          ,revert_on_prorate_fail = 'true' :: api(boolean())
                          ,replace_add_ons = 'false' :: api(boolean())
                          ,create = 'false' :: boolean()
                         }).
-type bt_subscription() :: #bt_subscription{}.
-type bt_subscriptions() :: [bt_subscription()].

-record(bt_customer, {id :: api(binary())
                      ,first_name :: api(binary())
                      ,last_name :: api(binary())
                      ,company :: api(binary())
                      ,email :: api(binary())
                      ,phone :: api(binary())
                      ,fax :: api(binary())
                      ,website :: api(binary())
                      ,created_at :: api(binary())
                      ,updated_at :: api(binary())
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
                         ,tax_amount :: api(binary())
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address :: bt_address()
                         ,shipping_address_id :: api(binary())
                         ,shipping_address :: bt_address()
                         ,customer_id :: api(binary())
                         ,customer :: bt_customer()
                         ,payment_token :: api(binary())
                         ,card :: bt_card()
                         ,subscription_id :: api(binary())
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

-record(bt_error, {code :: api(binary())
                   ,message :: api(binary())
                   ,attribute :: api(binary())
                  }).
-type bt_error() :: #bt_error{}.
-type bt_errors() :: [#bt_error{}].

-record(bt_verification, {verification_status :: api(binary())
                          ,processor_response_code :: api(binary())
                          ,processor_response_text :: api(binary())
                          ,cvv_response_code :: api(binary())
                          ,avs_response_code :: api(binary())
                          ,postal_response_code :: api(binary())
                          ,street_response_code :: api(binary())
                          ,gateway_rejection_reason :: api(binary())
                         }).
-type bt_verification() :: #bt_verification{}.

-record(bt_api_error, {errors = [] :: bt_errors()
                       ,verification :: bt_verification()
                       ,message:: api(binary())
                      }).
-type bt_api_error() :: #bt_api_error{}.
