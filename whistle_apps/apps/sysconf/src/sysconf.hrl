-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"sysconf">>).
-define(APP_VERSION, <<"0.1.0">>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
