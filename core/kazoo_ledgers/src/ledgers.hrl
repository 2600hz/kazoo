-ifndef(KZL_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_ledgers/include/kazoo_ledger.hrl").

-define(APP, kazoo_ledgers).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(LIST_BY_SERVICE, <<"ledgers/listing_by_service">>).
-define(LIST_BY_SERVICE_LEGACY, <<"ledgers/listing_by_service_legacy">>).
-define(TOTAL_BY_SERVICE, <<"ledgers/total_by_service">>).
-define(TOTAL_BY_SERVICE_LEGACY, <<"ledgers/total_by_service_legacy">>).

-define(CONFIG_CAT, <<"ledgers">>).

-define(KZL_HRL, 'true').
-endif.
