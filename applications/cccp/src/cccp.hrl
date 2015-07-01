-ifndef(CCCP_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle_apps/src/whapps_call_command_types.hrl").

-define(APP_NAME, <<"cccp">>).
-define(APP_VERSION, <<"0.0.1">> ).

-define(CCCP_CONFIG_CAT, <<"cccp">>).

-define(TIMEOUT, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 15).

-define(CCCP_HRL, 'true').
-endif.
