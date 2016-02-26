-ifndef(WHISTLE_SERVICES_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_log.hrl").

-include_lib("whistle_transactions/include/whistle_transactions.hrl").

-define(APP_NAME, <<"whistle_services">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(WHS_CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'whistle_services_cache').

-ifndef(TEST).
-define(SUPPORT_BILLING_ID, whapps_config:get_is_true(?WHS_CONFIG_CAT, <<"support_billing_id">>, 'true')).
-else.
-define(SUPPORT_BILLING_ID, 'true').
-endif.

-define(WHISTLE_SERVICES_HRL, 'true').
-endif.
