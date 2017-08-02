-ifndef(KAZOO_TRANSACTIONS_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type units() :: non_neg_integer().
-type dollars() :: number().

-define(TOPUP_CONFIG, <<"topup">>).

-define(CODE_PER_MINUTE_CALL, 1001).
-define(CODE_PER_MINUTE_CALL_SUB_ACCOUNT, 1002).
-define(CODE_FEATURE_ACTIVATION, 2001).
-define(CODE_FEATURE_ACTIVATION_SUB_ACCOUNT, 2002).
-define(CODE_NUMBER_ACTIVATION, 2003).
-define(CODE_NUMBER_ACTIVATION_SUB_ACCOUNT, 2004).
-define(CODE_NUMBERS_ACTIVATION, 2005).
-define(CODE_NUMBERS_ACTIVATION_SUB_ACCOUNT, 2006).
-define(CODE_MANUAL_ADDITION, 3001).
-define(CODE_MANUAL_ADDITION_SUB_ACCOUNT, 3002).
-define(CODE_AUTO_ADDITION, 3003).
-define(CODE_SUB_ACCOUNT_AUTO_ADDITION, 3004).
-define(CODE_ADMIN_DISCRETION, 3005).
-define(CODE_TOPUP, 3006).
-define(CODE_DATABASE_ROLLUP, 4000).
-define(CODE_RECURRING, 5000).
-define(CODE_MONTHLY_RECURRING, 5001).
-define(CODE_RECURRING_PRORATE, 5002).
-define(CODE_MOBILE, 6000).
-define(CODE_UNKNOWN, 9999).

-define(KAZOO_TRANSACTIONS_HRL, 'true').
-endif.
