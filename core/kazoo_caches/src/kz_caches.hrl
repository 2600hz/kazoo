-ifndef(KZ_CACHES_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(APP_NAME, <<"kazoo_caches">>).
-define(APP_VERSION, <<"4.0.0">> ).

-ifdef(TEST).
-define(KAPPS_CONFIG_PROPS, []).
-define(KAPPS_GETBY_PROPS, []).
-else.
-define(KAPPS_CONFIG_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                      ,[{'type', <<"account_config">>}]
                                      ,[{'db', ?KZ_CONFIG_DB}]
                                      ]).

-define(KAPPS_CONFIG_PROPS, [{'origin_bindings', ?KAPPS_CONFIG_ORIGIN_BINDINGS}]).

-define(KAPPS_GETBY_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                     ]).

-define(KAPPS_GETBY_PROPS, [{'origin_bindings', ?KAPPS_GETBY_ORIGIN_BINDINGS}]).

-endif.
-define(KZ_CACHES_HRL, 'true').
-endif.
