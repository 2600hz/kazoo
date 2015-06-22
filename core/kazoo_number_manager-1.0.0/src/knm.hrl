-ifndef(KNM_HRL).
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"kazoo_number_manager">>).

-define(KNM_CACHE, 'knm_cache').
-define(KNM_CONFIG_CAT, <<"number_manager">>).
-define(KNM_DB_PREFIX, <<"numbers/">>).

-define(KNM_USER_AGENT, "Kazoo Number Manager 1.0.0").

-define(KNM_HRL, 'true').
-endif.