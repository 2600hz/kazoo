-ifndef(JONNY5_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(JONNY5_CACHE, 'jonny5_cache').

-define(DEFAULT_RATE, 0.5).

-define(APP_VERSION, <<"2.0.0">>).
-define(APP_NAME, <<"jonny5">>).

-type tristate_integer() :: -1 | non_neg_integer().

-define(JONNY5_HRL, 'true').
-endif.
