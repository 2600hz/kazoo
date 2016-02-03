-ifndef(WNM_HRL).
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/kz_system_config.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"whistle_number_manager">>).

-define(WNM_NUMBER_CACHE, 'wnm_number_cache').

-define(WNM_HRL, 'true').
-endif.
