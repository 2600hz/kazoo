-ifndef(KAZOO_LEDGER_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(AMOUNT, <<"amount">>).
-define(DESC, <<"description">>).
-define(SRC, <<"source">>).
-define(SRC_SERVICE, [?SRC, <<"service">>]).
-define(SRC_ID, [?SRC, <<"id">>]).
-define(USAGE, <<"usage">>).
-define(USAGE_TYPE, [?USAGE, <<"type">>]).
-define(USAGE_QUANTITY, [?USAGE, <<"quantity">>]).
-define(USAGE_UNIT, [?USAGE, <<"unit">>]).
-define(PERIOD, <<"period">>).
-define(PERIOD_START, [?PERIOD, <<"start">>]).
-define(PERIOD_END, [?PERIOD, <<"end">>]).
-define(ACCOUNT, <<"account">>).
-define(ACCOUNT_ID, [?ACCOUNT, <<"id">>]).
-define(ACCOUNT_NAME, [?ACCOUNT, <<"name">>]).

-define(PVT_LEDGER_TYPE, <<"pvt_ledger_type">>).
-define(PVT_TYPE, <<"ledger">>).

-type ledger() :: wh_json:object().

-define(KAZOO_LEDGER_HRL, 'true').
-endif.