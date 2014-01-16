-ifndef(WH_DATABASES_HRL).

-define(WH_CONFIG_DB, <<"system_config">>).
-define(WH_SCHEMA_DB, <<"system_schemas">>).
-define(WH_MEDIA_DB, <<"system_media">>).
-define(WH_SIP_DB, <<"sip_auth">>).
-define(WH_ACCOUNTS_DB, <<"accounts">>).
-define(WH_RATES_DB, <<"ratedeck">>).
-define(WH_PROVISIONER_DB, <<"global_provisioner">>).
-define(WH_FAXES, <<"faxes">>).
-define(WH_SERVICES_DB, <<"services">>).
-define(WH_OFFNET_DB, <<"offnet">>).

-define(WH_ANONYMOUS_CDR_DB, <<"anonymous_cdrs">>).

-define(WH_ACCOUNT_CONFIGS, <<"configs_">>).

-define(KZ_WEBHOOKS_DB, <<"webhooks">>).

-define(KZ_ACDC_DB, <<"acdc">>).

-define(KZ_PORT_REQUESTS_DB, <<"port_requests">>).

-define(KZ_SYSTEM_DBS, [?WH_CONFIG_DB
                        ,?WH_SCHEMA_DB
                        ,?WH_MEDIA_DB
                        ,?WH_SIP_DB
                        ,?WH_ACCOUNTS_DB
                        ,?WH_RATES_DB
                        ,?WH_PROVISIONER_DB
                        ,?WH_FAXES
                        ,?WH_SERVICES_DB
                        ,?WH_OFFNET_DB
                        ,?WH_ANONYMOUS_CDR_DB
                        ,?KZ_PORT_REQUESTS_DB
                        ,?KZ_ACDC_DB
                       ]).

-define(WH_DATABASES_HRL, 'true').
-endif.
