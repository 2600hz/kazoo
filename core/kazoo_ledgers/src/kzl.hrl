-ifndef(KZL_HRL).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_ledgers/include/kazoo_ledger.hrl").

-define(APP_VERSION, <<"4.0.1">>).
-define(APP_NAME, <<"kazoo_ledgers">>).

-define(LIST_BY_SERVICE, <<"ledgers/listing_by_service">>).
-define(LIST_BY_SERVICE_LEGACY, <<"ledgers/listing_by_service_legacy">>).
-define(TOTAL_BY_SERVICE, <<"ledgers/total_by_service">>).
-define(TOTAL_BY_SERVICE_LEGACY, <<"ledgers/total_by_service_legacy">>).

-define(CONFIG_CAT, <<"ledgers">>).

-define(KZL_HRL, 'true').
-endif.
