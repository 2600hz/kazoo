-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define(CARRIERS_DB, "carriers").
-define(VIEW_FILE, <<"views/carriers.json">>).
-define(LIST_BY_NUMBER, {?CARRIERS_DB, <<"listing_by_number">>}).
