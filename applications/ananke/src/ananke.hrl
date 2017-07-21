-ifndef(ANANKE_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"ananke">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-type pos_integers() :: list(pos_integer()).
-type check_fun() :: 'true' | fun(() -> boolean()) | {Module :: atom(), FunName :: atom(), Args :: list()}.

-define(ANANKE_HRL, 'true').
-endif.
