-ifndef(KAZOO_LEDGER_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-record(kz_ledger, {name :: ne_binary()
                    ,amount :: integer()
                    ,description :: api_binary()
                    ,account :: api_binary()
                    ,type = <<"debit">> :: ne_binary()
                   }).

-define(PVT_TYPE, <<"pvt_type">>).
-define(LEDGER_TYPE, <<"ledger">>).
-define(PVT_NAME, <<"pvt_name">>).
-define(PVT_AMOUNT, <<"pvt_amount">>).
-define(PVT_ACCOUNT, <<"pvt_account_id">>).
-define(PVT_DESC, <<"pvt_description">>).
-define(PVT_LEDGER_TYPE, <<"pvt_ledger_type">>).
-define(PVT_CREATED, <<"pvt_created">>).
-define(PVT_UPDATED, <<"pvt_updated">>).


-type ledger() :: #kz_ledger{}.


-define(KAZOO_LEDGER_HRL, 'true').
-endif.