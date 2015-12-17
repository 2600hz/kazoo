-ifndef(CAMPER_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle_apps/src/whapps_call_command_types.hrl").

-define(APP_NAME, <<"camper">>).
-define(APP_VERSION, <<"0.1.0">> ).

-define(CAMPER_CONFIG_CAT, <<"camper">>).

-define(TIMEOUT, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 15).

-define(CAMPER_HRL, 'true').
-endif.
