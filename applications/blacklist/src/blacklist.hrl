-ifndef(BLACKLIST_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"blacklist">>).
-define(APP_VERSION, <<"0.0.1">> ).

-define(BLACKLIST_CACHE, 'blacklist_cache').
-define(CONFIG_CAT, <<"blacklist">>).

-define(BLACKLIST_HRL, 'true').
-endif.
