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
                  ,billing_address = undefined :: undefined | #bt_address{}
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
