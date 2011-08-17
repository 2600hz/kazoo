-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("hotornot/include/hon_amqp.hrl").

-define(APP_NAME, <<"hotornot">>).
-define(APP_VERSION, <<"0.1.0">>).
-define(RATES_DB, <<"ratedeck">>).
