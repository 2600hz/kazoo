-ifndef(CDR_HRL).

-include_lib("kazoo_types/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(APP_NAME, <<"cdr">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-type account_id() :: ne_binary().
-type account_db() :: ne_binary().

-define(CDR_HRL, 'true').
-endif.
