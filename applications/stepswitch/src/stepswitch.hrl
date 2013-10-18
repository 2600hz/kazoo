-ifndef(STEPSWITCH_HRL).
-include_lib("rabbitmq_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(ROUTES_DB, <<"offnet">>).
-define(RESOURCES_DB, <<"offnet">>).
-define(LOCAL_RESOURCES_VIEW, <<"local_resources/crossbar_listing">>).

-define(LIST_ROUTES_BY_NUMBER, <<"routes/listing_by_number">>).
-define(LIST_ROUTE_DUPS, <<"routes/listing_by_assignment">>).
-define(LIST_ROUTE_ACCOUNTS, <<"routes/listing_by_account">>).
-define(LIST_RESOURCES_BY_ID, <<"resources/listing_by_id">>).

-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"0.5.0">>).

-define(STEPSWITCH_CACHE, 'stepswitch_cache').
-define(STEPSWITCH_CNAM_POOL, 'stepswitch_cnam_pool').

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-define(STEPSWITCH_HRL, true).
-endif.
