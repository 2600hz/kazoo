-ifndef(KNM_HRL).
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"kazoo_number_manager">>).

-define(KNM_CACHE, 'knm_cache').
-define(KNM_CONFIG_CAT, <<"number_manager">>).
-define(KNM_DB_PREFIX_L, "numbers/").
-define(KNM_DB_PREFIX, <<?KNM_DB_PREFIX_L>>).

-define(FEATURE_OUTBOUND_CNAM, <<"outbound_cnam">>).
-define(FEATURE_INBOUND_CNAM, <<"inbound_cnam">>).
-define(FEATURE_CNAM, <<"cnam">>).

-define(KEY_DISPLAY_NAME, <<"display_name">>).
-define(KEY_INBOUND_LOOKUP, <<"inbound_lookup">>).

-define(KNM_USER_AGENT, "Kazoo Number Manager 1.0.0").

-define(KNM_HRL, 'true').
-endif.
