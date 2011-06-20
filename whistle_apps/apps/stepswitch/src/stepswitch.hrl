-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define(ROUTES_DB, <<"offnet">>).
-define(LIST_ROUTES_BY_NUMBER, {<<"routes">>, <<"listing_by_number">>}).

-define(RESOURCES_DB, <<"offnet">>).
-define(LIST_RESOURCES_BY_ID, {<<"resources">>, <<"listing_by_id">>}).

-define(DEFAULT_PROGRESS_TIMEOUT, 8).
-define(DEFAULT_GRACE_PERIOD, 3).

-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"0.1.0">>).
