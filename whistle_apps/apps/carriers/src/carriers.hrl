-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define(CARRIERS_DB, "carriers").
-define(VIEW_FILE, <<"views/carriers.json">>).
-define(LIST_BY_NUMBER, {?CARRIERS_DB, <<"listing_by_number">>}).
-define(LIST_ACTIVE_RESOURCE, {?CARRIERS_DB, <<"listing_active_resource">>}).

-define(DEFAULT_PROGRESS_TIMEOUT, 8).
-define(REFRESH_MSG, timeout).
-define(REFRESH_RATE, 43200000). % 1000ms * 60s * 60m * 12h = Every twelve hours

-define(APP_NAME, <<"carriers">>).
-define(APP_VERSION, <<"0.0.3">>).
