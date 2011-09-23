-type bt_result() :: tuple(ok|error, term()).
-type bt_xml() :: term().

-define(BT_XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(BT_API_VERSION, 2).
-define(BT_SERVER_URL, [{"prodcution", "www.braintreegateway.com"}
                         ,{"qa", "qa.braintreegateway.com"}
                         ,{"sandbox", "sandbox.braintreegateway.com"}]).
-define(BT_EMPTY_XML, fun() -> xmerl_scan:string("<?xml version=\"1.0\"?><empty />") end()).

-record(bt_config, {environment = "sandbox"
                    ,merchant_id = "jbz9b3njpjfq3x63"
                    ,public_key = "rknwd52wsvns7hx4"
                    ,private_key = "5f7mdcn7j9jq32cq"
                   }).

-record(bt_address, {first_name = undefined :: undefined | string()
                     ,last_name = undefined :: undefined | string()
                     ,company = undefined :: undefined | string()
                     ,street_address = undefined :: undefined | string()
                     ,extended_address = undefined :: undefined | string()
                     ,locality = undefined :: undefined | string()
                     ,region = undefined :: undefined | string()
                     ,postal_code = undefined :: undefined | string()
                     ,contry_code = undefined :: undefined | string()
                    }).

-record(bt_credit_card, {number = undefined :: undefined | string()
                         ,expiration_date = undefined :: undefined | string()
                         ,cvv = undefined :: undefined | string()
                         ,cardholderName = undefined :: undefined | string()
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
                      ,credit_cards = [] :: list(#bt_credit_card{})
                      ,addresses = [] :: list(#bt_address{})
                     }).
