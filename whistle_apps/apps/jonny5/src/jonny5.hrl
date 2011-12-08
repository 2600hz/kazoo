-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"jonny5">>).
-define(APP_VERSION, <<"0.2.0">>).
-define(BLACKLIST_SERVER, blacklist_server).

-type call_types() :: 'per_min' | 'twoway' | 'inbound'.
