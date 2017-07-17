-ifndef(KZ_BUCKETS_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"token_buckets">>).
-define(DEFAULT_APP, <<"default">>).

-define(INACTIVITY_TIMEOUT_S
       ,kapps_config:get_integer(?APP_NAME, <<"inactivity_timeout_s">>, ?SECONDS_IN_MINUTE * 10)
       ).
-define(INACTIVITY_MSG, 'inactivity_timeout').

-define(KZ_BUCKETS_HRL, 'true').
-endif.
