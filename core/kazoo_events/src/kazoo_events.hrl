-ifndef(KAZOO_EVENTS_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(APP_NAME, <<"kazoo_events">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(APP, 'kazoo_events').

-type bind_fun() :: {module(), atom(), list()} |
                    fun().

-define(KAZOO_EVENTS_HRL, 'true').
-endif.
