-ifndef(JONNY5_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CACHE_NAME, 'jonny5_cache').

-define(DEFAULT_RATE, 0.5).

-define(INBOUND_ACCOUNT_TYPES,
        [<<"account">>
         ,<<"device">>
         ,<<"sys_info">>
        ]).

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"jonny5">>).

-type tristate_integer() :: -1 | non_neg_integer().

-define(JONNY5_HRL, 'true').
-endif.
