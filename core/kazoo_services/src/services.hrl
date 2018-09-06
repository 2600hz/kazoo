-ifndef(KAZOO_SERVICES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-define(APP, 'kazoo_services').
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'kazoo_services_cache').

-type bookkeeper_sync_result() :: 'ok' | 'delinquent' | 'retry'.

-define(KAZOO_SERVICES_HRL, 'true').
-endif.
