-ifndef(KAZOO_CACHES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-type callback_msg() :: 'flush' | 'erase' | 'expire' | 'timeout' | 'store'.
-type callback_fun() :: fun((any(), any(), callback_msg()) -> any()).
-type callback_funs() :: [callback_fun()].
-type origin_tuple() :: {'db', kz_term:ne_binary(), kz_term:ne_binary()} | %% {db, Database, PvtType or Id}
                        {'type', kz_term:ne_binary(), kz_term:ne_binary()} | %% {type, PvtType, Id}
                        {'db', kz_term:ne_binary()} | %% {db, Database}
                        {'db', kz_term:ne_binary(), kz_term:ne_binary() | '_'} | %% {db, Database, Type}
                        {'database', kz_term:ne_binary()} | %% {database, Database} added for notify db create/delete
                        {'type', kz_term:ne_binary()}. %% {type, PvtType}
-type origin_tuples() :: [origin_tuple()].

-define(KAZOO_CACHES_HRL, 'true').
-endif.
