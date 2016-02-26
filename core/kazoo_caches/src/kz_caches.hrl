-ifndef(KZ_CACHES_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(APP_NAME, <<"kazoo_caches">>).
-define(APP_VERSION, <<"4.0.0">> ).


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
