-ifndef(STEPSWITCH_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(ROUTES_DB, ?WH_OFFNET_DB).
-define(RESOURCES_DB, ?WH_OFFNET_DB).
-define(LOCAL_RESOURCES_VIEW, <<"resources/crossbar_listing">>).

-define(LIST_ROUTES_BY_NUMBER, <<"routes/listing_by_number">>).
-define(LIST_ROUTE_DUPS, <<"routes/listing_by_assignment">>).
-define(LIST_ROUTE_ACCOUNTS, <<"routes/listing_by_account">>).
-define(LIST_RESOURCES_BY_ID, <<"resources/listing_by_id">>).

-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(SS_CONFIG_CAT, <<"stepswitch">>).

-define(CACHE_NAME, 'stepswitch_cache').
-define(STEPSWITCH_CNAM_POOL, 'stepswitch_cnam_pool').

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-type direction() :: 'inbound' | 'outbound' | 'both'.

-define(DEFAULT_AMQP_EXCHANGE_OPTIONS
        ,wh_json:from_list([{'passive', 'true'}])
       ).

-define(RULES_HONOR_DIVERSION
        ,whapps_config:get_is_true(?SS_CONFIG_CAT, <<"cid_rules_honor_diversions">>, 'false')
       ).

-define(STEPSWITCH_HRL, 'true').
-endif.
