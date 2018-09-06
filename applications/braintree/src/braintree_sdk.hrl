-ifndef(BRAINTREE_HRL).

-type bt_result() :: {'ok', any()}.
-type bt_xml() :: any().
%%  `record_proplist()' | {@link braintree_util:char_to_bin_res()}.

-type braintree_failures() :: 'no_payment_token' |
                              'authentication' |
                              'authorization' |
                              'not_found' |
                              'upgrade_required' |
                              'server_error' |
                              'maintenance' |
                              'io_fault' |
                              'api_error'.

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

-record(bt_address, {id :: kz_term:api_binary()
                    ,customer_id :: kz_term:api_binary()
                    ,first_name :: kz_term:api_binary()
                    ,last_name :: kz_term:api_binary()
                    ,company :: kz_term:api_binary()
                    ,street_address :: kz_term:api_binary()
                    ,extended_address :: kz_term:api_binary()
                    ,locality :: kz_term:api_binary()
                    ,region :: kz_term:api_binary()
                    ,postal_code :: kz_term:api_binary()
                    ,country_code_two :: kz_term:api_binary()
                    ,country_code_three :: kz_term:api_binary()
                    ,country_code :: kz_term:api_binary()
                    ,country_name :: kz_term:api_binary()
                    ,update_existing = 'false' :: boolean()
                    ,created_at :: kz_term:api_binary()
                    ,updated_at :: kz_term:api_binary()
                    }).
-type bt_address() :: #bt_address{}.
-type bt_addresses() :: [bt_address()].

-record(bt_card, {token :: kz_term:api_binary()
                 ,bin :: kz_term:api_binary()
                 ,cardholder_name :: kz_term:api_binary()
                 ,card_type :: kz_term:api_binary()
                 ,created_at :: kz_term:api_binary()
                 ,updated_at :: kz_term:api_binary()
                 ,default = 'false' :: boolean()
                 ,expiration_date :: kz_term:api_binary()
                 ,expiration_month :: kz_term:api_binary()
                 ,expiration_year :: kz_term:api_binary()
                 ,expired = 'false' :: boolean()
                 ,customer_location :: kz_term:api_binary()
                 ,last_four :: kz_term:api_binary()
                 ,number :: kz_term:api_binary()
                 ,cvv :: kz_term:api_binary()
                 ,customer_id :: kz_term:api_binary()
                 ,make_default = 'true' :: boolean()
                 ,verify = 'true' :: boolean()
                 ,payment_method_nonce :: kz_term:api_binary()
                 ,update_existing = 'false' :: boolean() | kz_term:ne_binary()
                 ,billing_address_id :: kz_term:api_binary()
                 ,billing_address :: bt_address() | 'undefined'
                 }).
-type bt_card() :: #bt_card{}.
-type bt_cards() :: [bt_card()].

-record(bt_addon, {id :: kz_term:api_binary()
                  ,amount :: kz_term:api_binary()
                  ,never_expires = true :: boolean()
                  ,billing_cycle :: kz_term:api_binary()
                  ,number_of_cycles :: kz_term:api_binary()
                  ,quantity = 0 :: integer()
                  ,inherited_from :: kz_term:api_binary()
                  ,existing_id :: kz_term:api_binary()
                  }).
-type bt_addon() :: #bt_addon{}.
-type bt_addons() :: [bt_addon()].

-record(bt_discount, {id :: kz_term:api_binary()
                     ,amount :: kz_term:api_binary()
                     ,never_expires = true :: boolean()
                     ,billing_cycle :: kz_term:api_binary()
                     ,number_of_cycles :: kz_term:api_binary()
                     ,quantity = 0 :: integer()
                     ,inherited_from :: kz_term:api_binary()
                     ,existing_id :: kz_term:api_binary()
                     }).
-type bt_discount() :: #bt_discount{}.
-type bt_discounts() :: [bt_discount()].

-record(bt_descriptor, {name :: kz_term:api_binary()
                       ,phone :: kz_term:api_binary()
                       ,url :: kz_term:api_binary()
                       }).

-type bt_descriptor() :: #bt_descriptor{}.

-record(bt_subscription, {id :: kz_term:api_ne_binary()
                         ,balance :: kz_term:api_binary()
                         ,billing_dom :: kz_term:api_binary()
                         ,billing_first_date :: kz_term:api_binary()  %% Read only
                         ,billing_end_date :: kz_term:api_binary()
                         ,billing_start_date :: kz_term:api_binary()
                         ,billing_cycle :: kz_term:api_binary()
                         ,number_of_cycles :: kz_term:api_binary()
                         ,days_past_due :: kz_term:api_binary()
                         ,failure_count :: kz_term:api_binary()
                         ,merchant_account_id :: kz_term:api_binary()
                         ,never_expires = 'true' :: kz_term:api_boolean()
                         ,next_bill_amount :: kz_term:api_binary()
                         ,next_cycle_amount :: kz_term:api_binary()
                         ,next_bill_date :: kz_term:api_binary()  %% Read only
                         ,paid_through_date :: kz_term:api_binary()
                         ,payment_token :: kz_term:api_ne_binary()
                         ,plan_id :: kz_term:api_binary()
                         ,price :: kz_term:api_binary()
                         ,status :: kz_term:api_binary()
                         ,trial_duration :: kz_term:api_binary()
                         ,trial_duration_unit :: kz_term:api_binary()
                         ,trial_period :: kz_term:api_ne_binary()
                         ,add_ons = [] :: bt_addons()
                         ,discounts = [] :: bt_discounts()
                         ,descriptor :: bt_descriptor() | 'undefined'
                         ,transactions :: kz_term:api_binary()
                         ,do_not_inherit = 'true' :: kz_term:api_boolean()
                         ,start_immediately = 'true' :: kz_term:api_boolean()
                         ,prorate_charges = 'true' :: kz_term:api_boolean()
                         ,revert_on_prorate_fail = 'true' :: kz_term:api_boolean()
                         ,replace_add_ons = 'false' :: kz_term:api_boolean()
                         ,create = 'false' :: boolean()
                         }).
-type bt_subscription() :: #bt_subscription{}.
-type bt_subscriptions() :: [bt_subscription()].

-record(bt_customer, {id :: kz_term:api_binary()
                     ,first_name :: kz_term:api_binary()
                     ,last_name :: kz_term:api_binary()
                     ,company :: kz_term:api_binary()
                     ,email :: kz_term:api_binary()
                     ,phone :: kz_term:api_binary()
                     ,fax :: kz_term:api_binary()
                     ,website :: kz_term:api_binary()
                     ,created_at :: kz_term:api_binary()
                     ,updated_at :: kz_term:api_binary()
                     ,payment_method_nonce :: kz_term:api_binary()
                     ,credit_cards = [] :: bt_cards()
                     ,addresses = [] :: bt_addresses()
                     ,subscriptions = [] :: bt_subscriptions()
                     }).
-type bt_customer() :: #bt_customer{}.
-type bt_customers() :: [bt_customer()].

-record(bt_transaction, {id :: kz_term:api_ne_binary()
                        ,status :: kz_term:api_ne_binary()
                        ,type :: kz_term:api_ne_binary()
                        ,currency_code :: kz_term:api_ne_binary()
                        ,amount :: kz_term:api_ne_binary()
                        ,merchant_account_id :: kz_term:api_ne_binary()
                        ,order_id :: kz_term:api_ne_binary()
                        ,purchase_order :: kz_term:api_ne_binary()
                        ,created_at :: kz_term:api_ne_binary()
                        ,update_at :: kz_term:api_ne_binary()
                        ,refund_ids :: kz_term:api_ne_binary()
                        ,refunded_transaction :: kz_term:api_ne_binary()
                        ,settlement_batch :: kz_term:api_ne_binary()
                        ,avs_error_code :: kz_term:api_ne_binary()
                        ,avs_postal_response :: kz_term:api_ne_binary()
                        ,avs_street_response :: kz_term:api_ne_binary()
                        ,ccv_response_code :: kz_term:api_ne_binary()
                        ,gateway_rejection :: kz_term:api_ne_binary()
                        ,processor_authorization_code :: kz_term:api_ne_binary()
                        ,processor_response_code :: kz_term:api_ne_binary()
                        ,processor_response_text :: kz_term:api_ne_binary()
                        ,tax_amount :: kz_term:api_binary()
                        ,tax_exempt = 'false' :: boolean()
                        ,billing_address :: bt_address() | 'undefined'
                        ,shipping_address_id :: kz_term:api_binary()
                        ,shipping_address :: bt_address() | 'undefined'
                        ,customer_id :: kz_term:api_binary()
                        ,customer :: bt_customer() | 'undefined'
                        ,payment_token :: kz_term:api_binary()
                        ,card :: bt_card() | 'undefined'
                        ,subscription_id :: kz_term:api_binary()
                        ,add_ons = [] :: bt_addons() | 'undefined'
                        ,discounts = [] :: bt_discounts() | 'undefined'
                        ,descriptor :: bt_descriptor() | 'undefined'
                        ,store_in_vault = 'false' :: boolean()
                        ,store_on_success = 'false' :: boolean()
                        ,settle = 'true' :: boolean()
                        ,change_billing_address = 'false' :: boolean()
                        ,store_shipping_address = 'false' :: boolean()
                        }).
-type bt_transaction() :: #bt_transaction{}.
-type bt_transactions() :: [bt_transaction()].
-type api_bt_transaction() :: undefined | bt_transaction().

-record(bt_error, {code :: kz_term:api_binary()
                  ,message :: kz_term:api_binary()
                  ,attribute :: kz_term:api_binary()
                  }).
-type bt_error() :: #bt_error{}.
-type bt_errors() :: [#bt_error{}].

-record(bt_verification, {verification_status :: kz_term:api_binary()
                         ,processor_response_code :: kz_term:api_binary()
                         ,processor_response_text :: kz_term:api_binary()
                         ,cvv_response_code :: kz_term:api_binary()
                         ,avs_response_code :: kz_term:api_binary()
                         ,postal_response_code :: kz_term:api_binary()
                         ,street_response_code :: kz_term:api_binary()
                         ,gateway_rejection_reason :: kz_term:api_binary()
                         }).
-type bt_verification() :: #bt_verification{}.

-record(bt_api_error, {errors = [] :: bt_errors()
                      ,verification :: bt_verification()
                      ,message:: kz_term:api_binary()
                      }).
-type bt_api_error() :: #bt_api_error{}.

-define(BRAINTREE_SDK_HRL, 'true').
-endif.
