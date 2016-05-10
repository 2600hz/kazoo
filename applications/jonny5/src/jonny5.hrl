-ifndef(JONNY5_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(CACHE_NAME, 'jonny5_cache').

-define(DEFAULT_RATE, 0.5).

-define(INBOUND_ACCOUNT_TYPES,
        [<<"account">>
         ,<<"device">>
         ,<<"sys_info">>
        ]).

-define(APP_VERSION, kz_util:application_version('jonny5')).
-define(APP_NAME, <<"jonny5">>).

-type tristate_integer() :: -1 | non_neg_integer().

-define(JONNY5_HRL, 'true').
-endif.
