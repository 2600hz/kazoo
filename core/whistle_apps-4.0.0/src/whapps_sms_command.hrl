-ifndef(WHAPPS_SMS_COMMAND_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").
-include("whapps_call_command_types.hrl").

-define(DEFAULT_TIMEOUT_S, 20).

-define(APP_NAME, <<"whapps_sms_command">>).
-define(APP_VERSION, <<"1.0.0">>).


-define(WHAPPS_SMS_COMMAND_HRL, 'true').
-endif.
