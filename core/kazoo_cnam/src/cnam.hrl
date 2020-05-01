-ifndef(KAZOO_CNAM_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(APP, 'cnam').
-define(APP_NAME, <<"cnam">>).
-define(APP_VERSION, <<"5.0.0">>).

-define(CNAM_CONFIG_CAT, <<"cnam">>).

-define(CACHE_NAME, 'cname_cache').
-define(CNAM_POOL, 'cnam_pool').

-define(DEFAULT_USER_AGENT_HDR, <<"Kazoo CNAM">>).

-define(HTTP_CONNECT_TIMEOUT_MS
       ,kapps_config:get_integer(?CNAM_CONFIG_CAT, <<"http_connect_timeout_ms">>, 500)
       ).
-define(HTTP_USER_AGENT
       ,kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_user_agent_header">>, ?DEFAULT_USER_AGENT_HDR)
       ).


-define(KAZOO_CNAM_HRL, 'true').
-endif.
