-ifndef(KZL_HRL).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_ledgers/include/kazoo_ledger.hrl").

-define(APP_VERSION, <<"1.0.1">>).
-define(APP_NAME, <<"kazoo_ledgers">>).

-define(LIST_BY_SERVICE, <<"ledgers/listing_by_service">>).
-define(CONFIG_CAT, <<"ledgers">>).

-define(KZL_HRL, 'true').
-endif.
