-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-define(APP_VERSION, <<"1.0.3">>).
-define(APP_NAME, <<"notify">>).
