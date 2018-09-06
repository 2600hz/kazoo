-ifndef(KAZOO_TRANSACTIONS_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(APP, 'kazoo_transactions').
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(KAZOO_TRANSACTIONS_HRL, 'true').
-endif.
