-ifndef(KZ_CACHES_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(APP_NAME, <<"kazoo_caches">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(EXPIRES, ?SECONDS_IN_HOUR).
-define(EXPIRE_PERIOD_MS, 10 * ?MILLISECONDS_IN_SECOND).
-define(EXPIRE_PERIOD_MSG, 'expire_cache_objects').

-define(DEFAULT_WAIT_TIMEOUT_MS, 5 * ?MILLISECONDS_IN_SECOND).

-define(MITIGATION, 'stampede_mitigation').

-record(cache_obj, {key :: any()| '_' | '$1'
                   ,value :: any() | '_' | '$1' | '$2'
                   ,expires_s :: timeout() | '_' | '$3'
                   ,timestamp_ms = kz_time:now_ms() :: pos_integer() | '_' | '$4'
                   ,callback :: callback_fun() | '_' | '$2' | '$3' | '$5' | 'undefined'
                   ,origin :: origin_tuple() | origin_tuples() | '$1' | '_' | 'undefined'
                   ,monitor_pids = [] :: [pid()] | '_'
                   }).
-type cache_obj() :: #cache_obj{}.
-type cache_objs() :: [cache_obj()].

-define(KZ_CACHES_HRL, 'true').
-endif.
