-ifndef(REG_HRL).
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"0.4.2">>).

-define(CONFIG_CAT, <<"registrar">>).

-define(REGISTRAR_CACHE, registrar_cache).

-define(REG_HRL, true).
-endif.
