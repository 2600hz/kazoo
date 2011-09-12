-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"jonny5">>).
-define(APP_VSN, <<"0.1.0">>).
-define(BLACKLIST_SERVER, blacklist_server).
