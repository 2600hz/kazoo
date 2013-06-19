-ifndef(SYSCONF_HRL).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"sysconf">>).
-define(APP_VERSION, <<"0.2.0">>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).

-define(SYSCONF_HRL, 'true').
-endif.
