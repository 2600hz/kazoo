-ifndef(KAZOO_SERVICES_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

-define(APP, 'kazoo_services').
-define(APP_NAME, <<"kazoo_services">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(WHS_CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'kazoo_services_cache').

-ifndef(TEST).
-define(SUPPORT_BILLING_ID, kapps_config:get_is_true(?WHS_CONFIG_CAT, <<"support_billing_id">>, 'true')).
-else.
-define(SUPPORT_BILLING_ID, 'true').
-endif.

-define(SERVICES_BOM, <<"services_bom">>).
-define(SERVICES_EOM, <<"services_eom">>).

-define(KAZOO_SERVICES_HRL, 'true').
-endif.
