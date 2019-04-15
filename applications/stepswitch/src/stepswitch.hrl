-ifndef(STEPSWITCH_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_api_literals.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(LOCAL_RESOURCES_VIEW, <<"resources/crossbar_listing">>).

-define(LIST_ROUTES_BY_NUMBER, <<"routes/listing_by_number">>).
-define(LIST_ROUTE_DUPS, <<"routes/listing_by_assignment">>).
-define(LIST_ROUTE_ACCOUNTS, <<"routes/listing_by_account">>).
-define(LIST_RESOURCES_BY_ID, <<"resources/listing_by_id">>).

-define(APP, 'stepswitch').
-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(SS_CONFIG_CAT, <<"stepswitch">>).
-define(CNAM_CONFIG_CAT, <<"stepswitch.cnam">>).

-define(CACHE_NAME, 'stepswitch_cache').
-define(STEPSWITCH_CNAM_POOL, 'stepswitch_cnam_pool').

-define(DEFAULT_ROUTE_BY, <<"stepswitch_resources">>).

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-define(DEFAULT_AMQP_EXCHANGE_OPTIONS
       ,kz_json:from_list([{<<"passive">>, 'true'}])
       ).

-define(RULES_HONOR_DIVERSION
       ,kapps_config:get_is_true(?SS_CONFIG_CAT, <<"cid_rules_honor_diversions">>, 'false')
       ).

-define(DEFAULT_EMERGENCY_CID_NUMBER,
        kapps_config:get_ne_binary(?SS_CONFIG_CAT, <<"default_emergency_cid_number">>)
       ).

-define(DEFAULT_USER_AGENT_HDR, <<"Kazoo Stepswitch CNAM">>).

-define(HTTP_CONNECT_TIMEOUT_MS
       ,kapps_config:get_integer(?CNAM_CONFIG_CAT, <<"http_connect_timeout_ms">>, 500)
       ).
-define(HTTP_USER_AGENT
       ,kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_user_agent_header">>, ?DEFAULT_USER_AGENT_HDR)
       ).

-define(STEPSWITCH_HRL, 'true').
-endif.
