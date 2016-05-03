-ifndef(KZL_HRL).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_ledgers/include/kazoo_ledger.hrl").

-define(APP_VERSION, <<"4.0.1">>).
-define(APP_NAME, <<"kazoo_ledgers">>).

-define(LIST_BY_SERVICE, <<"ledgers/listing_by_service">>).
-define(CONFIG_CAT, <<"ledgers">>).

-define(KZL_HRL, 'true').
-endif.
