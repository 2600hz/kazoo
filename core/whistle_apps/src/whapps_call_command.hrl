-ifndef(WHAPPS_CALL_COMMAND_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").
-include("whapps_call_command_types.hrl").

-define(DEFAULT_TIMEOUT_S, 20).

-define(APP_NAME, <<"whapps_call_command">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(WHAPPS_CALL_COMMAND_HRL, 'true').
-endif.
