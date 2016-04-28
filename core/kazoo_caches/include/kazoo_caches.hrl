-ifndef(KAZOO_CACHES_HRL).

-define(WHAPPS_CONFIG_CACHE, 'whapps_config_cache').
-define(WHAPPS_CALL_CACHE, 'whapps_call_cache').

-include_lib("whistle/include/wh_types.hrl").

-type callback_fun() :: fun((any(), any(), 'flush' | 'erase' | 'expire') -> any()).
-type callback_funs() :: [callback_fun()].
-type origin_tuple() :: {'db', ne_binary(), ne_binary()} | %% {db, Database, PvtType or Id}
                        {'type', ne_binary(), ne_binary()} | %% {type, PvtType, Id}
                        {'db', ne_binary()} | %% {db, Database}
                        {'db', ne_binary(), ne_binary() | '_'} | %% {db, Database, Type}
                        {'database', ne_binary()} | %% {database, Database} added for notify db create/delete
                        {'type', ne_binary()}. %% {type, PvtType}
-type origin_tuples() :: [origin_tuple()].

-record(cache_obj, {key :: any()| '_' | '$1'
                    ,value :: any() | '_' | '$1' | '$2'
                    ,expires :: wh_timeout() | '_' | '$3'
                    ,timestamp = wh_util:current_tstamp() :: gregorian_seconds() | '_' | '$4'
                    ,callback :: callback_fun() | '_' | '$2' | '$3' | '$5'
                    ,origin :: origin_tuple() | origin_tuples() | '$1' | '_'
                   }).

-type cache_obj() :: #cache_obj{}.
-type cache_objs() :: [cache_obj()].

-define(KAZOO_CACHES_HRL, 'true').
-endif.
