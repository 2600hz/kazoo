-ifndef(CALL_INSPECTOR_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP, call_inspector).
-define(APP_NAME, atom_to_binary(?APP, utf8)).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, ?APP_NAME).

-define(CALL_INSPECTOR_HRL, 'true').
-endif.
