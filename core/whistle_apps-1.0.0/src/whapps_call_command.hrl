-ifndef(WHAPPS_CALL_COMMAND_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include("whapps_call_command_types.hrl").

-define(DEFAULT_TIMEOUT, <<"20">>).

-define(APP_NAME, <<"whapps_call_command">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(WHAPPS_CALL_CACHE, 'whapps_call_cache').

-define(WHAPPS_CALL_COMMAND_HRL, 'true').
-endif.
