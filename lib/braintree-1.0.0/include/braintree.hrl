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

-record(bt_address, {id :: ne_binary()
                     ,customer_id :: ne_binary() | nonempty_string()
                     ,first_name :: ne_binary()
                     ,last_name :: ne_binary()
                     ,company :: ne_binary()
                     ,street_address :: ne_binary()
                     ,extended_address :: ne_binary()
                     ,locality :: ne_binary()
                     ,region :: ne_binary()
                     ,postal_code :: ne_binary()
                     ,country_code_two :: ne_binary()
                     ,country_code_three :: ne_binary()
                     ,country_code :: ne_binary()
                     ,country_name :: ne_binary()
                     ,update_existing = 'false' :: boolean()
                     ,created_at :: ne_binary()
                     ,updated_at :: ne_binary()
                    }).

-record(bt_card, {token :: ne_binary()
                  ,bin :: ne_binary()
                  ,cardholder_name :: ne_binary()
                  ,card_type :: ne_binary()
                  ,created_at :: ne_binary()
                  ,updated_at :: ne_binary()
                  ,default = 'false' :: boolean()
                  ,expiration_date :: ne_binary()
                  ,expiration_month :: ne_binary()
                  ,expiration_year :: ne_binary()
                  ,expired = 'false' :: boolean()
                  ,customer_location :: ne_binary()
                  ,last_four :: ne_binary()
                  ,number :: ne_binary()
                  ,cvv :: ne_binary()
                  ,customer_id :: ne_binary() | nonempty_string()
                  ,make_default = 'false' :: boolean()
                  ,verify = 'true' :: boolean()
                  ,update_existing = 'false' :: boolean() | ne_binary()
                  ,billing_address_id :: ne_binary()
                  ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                 }).

-record(bt_addon, {id :: ne_binary()
                   ,amount :: ne_binary()
                   ,never_expires = true :: boolean()
                   ,billing_cycle :: ne_binary()
                   ,number_of_cycles :: ne_binary()
                   ,quantity = 0 :: integer()
                   ,inherited_from :: ne_binary()
                   ,existing_id :: ne_binary()
                  }).

-record(bt_discount, {id :: ne_binary()
                      ,amount :: ne_binary()
                      ,never_expires = true :: boolean()
                      ,billing_cycle :: ne_binary()
                      ,number_of_cycles :: ne_binary()
                      ,quantity = 0 :: integer()
                      ,inherited_from :: ne_binary()
                      ,existing_id :: ne_binary()
                     }).

-record(bt_subscription, {id :: ne_binary()
                          ,balance :: ne_binary()
                          ,billing_dom :: ne_binary()
                          ,billing_first_date :: ne_binary()
                          ,billing_end_date :: ne_binary()
                          ,billing_start_date :: ne_binary()
                          ,billing_cycle :: ne_binary()
                          ,number_of_cycles :: ne_binary()
                          ,days_past_due :: ne_binary()
                          ,failure_count :: ne_binary()
                          ,merchant_account_id :: ne_binary()
                          ,never_expires = 'true' :: boolean()
                          ,next_bill_amount :: ne_binary()
                          ,next_cycle_amount :: ne_binary()
                          ,next_bill_date :: ne_binary()
                          ,paid_through_date :: ne_binary()
                          ,payment_token :: ne_binary()
                          ,plan_id :: ne_binary()
                          ,price :: ne_binary()
                          ,status :: ne_binary()
                          ,trial_duration :: ne_binary()
                          ,trial_duration_unit :: ne_binary()
                          ,trial_period :: ne_binary()
                          ,add_ons = [] :: [#bt_addon{},...] | []
                          ,discounts = [] :: [#bt_discount{},...] | []
                          ,descriptor :: ne_binary()
                          ,transactions :: ne_binary()
                          ,do_not_inherit = 'true' :: boolean()
                          ,start_immediately = 'true' :: boolean()
                          ,prorate_charges = 'true' :: boolean()
                          ,revert_on_prorate_fail = 'true' :: boolean()
                          ,replace_add_ons = 'false' :: boolean()
                          ,create = 'false' :: boolean()
                         }).

-record(bt_customer, {id :: ne_binary()
                      ,first_name :: ne_binary()
                      ,last_name :: ne_binary()
                      ,company :: ne_binary()
                      ,email :: ne_binary()
                      ,phone :: ne_binary()
                      ,fax :: ne_binary()
                      ,website :: ne_binary()
                      ,created_at :: ne_binary()
                      ,updated_at :: ne_binary()
                      ,credit_cards = [] :: [] | [#bt_card{},...]
                      ,addresses = [] :: [] | [#bt_address{},...]
                      ,subscriptions = [] :: [] | [#bt_subscription{},...]
                     }).

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
                         ,tax_amount :: ne_binary()
                         ,tax_exempt = 'false' :: boolean()
                         ,billing_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,shipping_address_id :: ne_binary()
                         ,shipping_address = 'undefined' :: 'undefined' | #bt_address{}
                         ,customer_id :: ne_binary() | nonempty_string()
                         ,customer = 'undefined' :: 'undefined' | #bt_customer{}
                         ,payment_token :: ne_binary()
                         ,card = 'undefined' :: 'undefined' | #bt_card{}
                         ,subscription_id :: ne_binary()
                         ,add_ons = [] :: [#bt_addon{},...] | []
                         ,discounts :: ne_binary()
                         ,descriptor :: ne_binary()
                         ,store_in_vault = 'false' :: boolean()
                         ,store_on_success = 'false' :: boolean()
                         ,settle = 'true' :: boolean()
                         ,change_billing_address = 'false' :: boolean()
                         ,store_shipping_address = 'false' :: boolean()
                        }).

-record(bt_error, {code :: ne_binary()
                   ,message :: ne_binary()
                   ,attribute :: ne_binary()
                  }).

-record(bt_verification, {verification_status :: ne_binary()
                          ,processor_response_code :: ne_binary()
                          ,processor_response_text :: ne_binary()
                          ,cvv_response_code :: ne_binary()
                          ,avs_response_code :: ne_binary()
                          ,postal_response_code :: ne_binary()
                          ,street_response_code :: ne_binary()
                          ,gateway_rejection_reason :: ne_binary()
                         }).

-record(bt_api_error, {errors = [] :: [#bt_error{},...] | []
                       ,verification :: #bt_verification{}
                       ,message:: ne_binary()
                      }).
