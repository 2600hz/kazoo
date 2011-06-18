-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define(OFFNET_QUEUE_NAME, <<"carriers.offnet">>).

-define(CARRIERS_DB, "carriers").
-define(VIEW_FILE, <<"views/carriers.json">>).
-define(LIST_BY_NUMBER, {?CARRIERS_DB, <<"listing_by_number">>}).
-define(LIST_ACTIVE_RESOURCE, {?CARRIERS_DB, <<"listing_active_resource">>}).

-define(DEFAULT_PROGRESS_TIMEOUT, 8).
-define(DEFAULT_GRACE_PERIOD, 3).

-define(APP_NAME, <<"carriers">>).
-define(APP_VERSION, <<"0.0.3">>).
