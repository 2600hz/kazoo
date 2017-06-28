-ifndef(KAZOO_SERVICES_HRL).

-include_lib("kazoo_types/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(APP, 'kazoo_services').
-define(APP_NAME, <<"kazoo_services">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(WHS_CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'kazoo_services_cache').

-type bookkeeper_sync_result() :: 'ok' | 'delinquent' | 'retry'.

-ifndef(TEST).
-define(SUPPORT_BILLING_ID, kapps_config:get_is_true(?WHS_CONFIG_CAT, <<"support_billing_id">>, 'true')).
-else.
-define(SUPPORT_BILLING_ID, 'true').
-endif.

-define(KAZOO_SERVICES_HRL, 'true').
-endif.
