-ifndef(KZ_BUCKETS_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(INACTIVITY_TIMEOUT_MS
        ,whapps_config:get_integer(<<"token_buckets">>, <<"inactivity_timeout_s">>, ?SECONDS_IN_MINUTE * 10)
        * ?MILLISECONDS_IN_SECOND
       ).
-define(INACTIVITY_MSG, 'inactivity_timeout').

-define(KZ_BUCKETS_HRL, 'true').
-endif.
