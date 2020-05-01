-ifndef(KZ_DATABASES_HRL).

-define(KZ_CONFIG_DB, <<"system_config">>).
-define(KZ_SCHEMA_DB, <<"system_schemas">>).
-define(KZ_MEDIA_DB, <<"system_media">>).
-define(KZ_SIP_DB, <<"sip_auth">>).
-define(KZ_ACCOUNTS_DB, <<"accounts">>).
-define(KZ_ALERTS_DB, <<"alerts">>).
-define(KZ_RATES_DB, <<"ratedeck">>).
-define(KZ_FAXES_DB, <<"faxes">>).
-define(KZ_SERVICES_DB, <<"services">>).
-define(KZ_OFFNET_DB, <<"offnet">>).
-define(KZ_DEDICATED_IP_DB, <<"dedicated_ips">>).
-define(KZ_ANONYMOUS_CDR_DB, <<"anonymous_cdrs">>).

-define(KZ_TOKEN_DB, <<"token_auth">>).

-define(KZ_ACCOUNT_CONFIGS, <<"configs_">>).

-define(KZ_WEBHOOKS_DB, <<"webhooks">>).
-define(KZ_FUNCTIONS_DB, <<"functions">>).

-define(KZ_ACDC_DB, <<"acdc">>).

-define(KZ_CCCPS_DB, <<"cccps">>).

-define(KZ_PORT_REQUESTS_DB, <<"port_requests">>).

-define(KZ_OAUTH_DB, <<"oauth">>).
-define(KZ_AUTH_DB, <<"system_auth">>).
-define(KZ_DATA_DB, <<"system_data">>).

-define(KZ_TASKS_DB, <<"tasks">>).

-define(KZ_PENDING_NOTIFY_DB, <<"pending_notifications">>).

-define(KZ_SYSTEM_DBS, [?KZ_DATA_DB
                       ,?KZ_CONFIG_DB
                       ,?KZ_SCHEMA_DB
                       ,?KZ_AUTH_DB
                       ,?KZ_MEDIA_DB
                       ,?KZ_SIP_DB
                       ,?KZ_ACCOUNTS_DB
                       ,?KZ_RATES_DB
                       ,?KZ_FAXES_DB
                       ,?KZ_SERVICES_DB
                       ,?KZ_OFFNET_DB
                       ,?KZ_ANONYMOUS_CDR_DB
                       ,?KZ_PORT_REQUESTS_DB
                       ,?KZ_DEDICATED_IP_DB
                       ,?KZ_ACDC_DB
                       ,?KZ_OAUTH_DB
                       ,?KZ_FUNCTIONS_DB
                       ,?KZ_WEBHOOKS_DB
                       ,?KZ_TOKEN_DB
                       ,?KZ_ALERTS_DB
                       ,?KZ_TASKS_DB
                       ,?KZ_PENDING_NOTIFY_DB
                       ]).

-define(KZ_DATABASES_HRL, 'true').
-endif.
