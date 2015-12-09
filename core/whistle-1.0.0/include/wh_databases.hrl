-ifndef(WH_DATABASES_HRL).

-define(WH_CONFIG_DB, <<"system_config">>).
-define(WH_SCHEMA_DB, <<"system_schemas">>).
-define(WH_MEDIA_DB, <<"system_media">>).
-define(WH_SIP_DB, <<"sip_auth">>).
-define(WH_ACCOUNTS_DB, <<"accounts">>).
-define(WH_ALERTS_DB, <<"alerts">>).
-define(WH_RATES_DB, <<"ratedeck">>).
-define(WH_PROVISIONER_DB, <<"global_provisioner">>).
-define(WH_FAXES_DB, <<"faxes">>).
-define(WH_SERVICES_DB, <<"services">>).
-define(WH_OFFNET_DB, <<"offnet">>).
-define(WH_DEDICATED_IP_DB, <<"dedicated_ips">>).
-define(WH_ANONYMOUS_CDR_DB, <<"anonymous_cdrs">>).

-define(KZ_TOKEN_DB, <<"token_auth">>).

-define(WH_ACCOUNT_CONFIGS, <<"configs_">>).

-define(KZ_WEBHOOKS_DB, <<"webhooks">>).

-define(KZ_ACDC_DB, <<"acdc">>).

-define(KZ_CCCPS_DB, <<"cccps">>).

-define(KZ_PORT_REQUESTS_DB, <<"port_requests">>).

-define(KZ_OAUTH_DB,<<"oauth">>).

-define(KZ_SYSTEM_DBS, [?WH_CONFIG_DB
                        ,?WH_SCHEMA_DB
                        ,?WH_MEDIA_DB
                        ,?WH_SIP_DB
                        ,?WH_ACCOUNTS_DB
                        ,?WH_RATES_DB
                        ,?WH_PROVISIONER_DB
                        ,?WH_FAXES_DB
                        ,?WH_SERVICES_DB
                        ,?WH_OFFNET_DB
                        ,?WH_ANONYMOUS_CDR_DB
                        ,?KZ_PORT_REQUESTS_DB
                        ,?WH_DEDICATED_IP_DB
                        ,?KZ_ACDC_DB
                        ,?KZ_OAUTH_DB
                        ,?KZ_WEBHOOKS_DB
                        ,?KZ_TOKEN_DB
                       ]).

-define(WH_DATABASES_HRL, 'true').
-endif.
