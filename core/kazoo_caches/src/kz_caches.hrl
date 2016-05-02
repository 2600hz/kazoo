-ifndef(KZ_CACHES_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(APP_NAME, <<"kazoo_caches">>).
-define(APP_VERSION, <<"4.0.0">> ).


%%--------------------------------------------------------------------
-define(KAPPS_CONFIG_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                        ,[{'db', ?KZ_CONFIG_DB}]
                                       ]).

-define(KAPPS_CONFIG_PROPS, [{'origin_bindings', ?KAPPS_CONFIG_ORIGIN_BINDINGS}]).
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
-define(KAPPS_GETBY_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                      ]).

-define(KAPPS_GETBY_PROPS, [{'origin_bindings', ?KAPPS_GETBY_ORIGIN_BINDINGS}]).
%%--------------------------------------------------------------------

-define(KZ_CACHES_HRL, 'true').
-endif.
