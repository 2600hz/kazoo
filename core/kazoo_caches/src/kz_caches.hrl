-ifndef(KZ_CACHES_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(APP_NAME, <<"kazoo_caches">>).
-define(APP_VERSION, <<"4.0.0">> ).


-define(DEFAULT_DATASTORE, 'kzc_ets_listener').

-define(EXPIRES, ?SECONDS_IN_HOUR). %% an hour
-define(EXPIRE_PERIOD, 10 * ?MILLISECONDS_IN_SECOND).
-define(EXPIRE_PERIOD_MSG, 'expire_cache_objects').
-define(DEFAULT_WAIT_TIMEOUT, 5).


-type callback_fun() :: fun((any(), any(), 'flush' | 'erase' | 'expire') -> any()).
-type callback_funs() :: [callback_fun()].

-type origin_tuple() :: {'db', Database::ne_binary(), PvtTypeOrId::ne_binary()} |
                        {'type', PvtType::ne_binary(), Id::ne_binary()} |
                        {'db', Database::ne_binary()} |
                        {'database', Database::ne_binary()} | %% Added for notify db create/delete
                        {'type', PvtType::ne_binary()}.
-type origin_tuples() :: [origin_tuple()].

-type store_options() :: [{'origin', origin_tuple() | origin_tuples()} |
                          {'expires', wh_timeout()} |
                          {'callback', 'undefined' | callback_fun()}
                         ].

-record(cache_obj, {key :: any()| '_' | '$1'
                    ,value :: any() | '_' | '$1' | '$2'
                    ,expires :: wh_timeout() | '_' | '$3'
                    ,timestamp = wh_util:current_tstamp() :: gregorian_seconds() | '_' | '$4'
                    ,callback :: callback_fun() | '_' | '$2' | '$3' | '$5'
                    ,origin :: origin_tuple() | origin_tuples() | '$1' | '_'
                   }).

-type cache_obj() :: #cache_obj{}.
-type cache_objs() :: [cache_obj()].


%%--------------------------------------------------------------------
-define(WHAPPS_CONFIG_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                        ,[{'db', ?WH_CONFIG_DB}]
                                       ]).

-define(WHAPPS_CONFIG_PROPS, [{'origin_bindings', ?WHAPPS_CONFIG_ORIGIN_BINDINGS}]).
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
-define(WHAPPS_GETBY_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                      ]).

-define(WHAPPS_GETBY_PROPS, [{'origin_bindings', ?WHAPPS_GETBY_ORIGIN_BINDINGS}]).
%%--------------------------------------------------------------------

-define(KZ_CACHES_HRL, 'true').
-endif.
