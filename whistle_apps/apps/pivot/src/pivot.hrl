-ifndef(PIVOT_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_api.hrl").

-define(PIVOT_CACHE, pivot_cache).

-define(APP_NAME, <<"pivot">>).
-define(APP_VERSION, <<"0.5.0">>).

-define(PIVOT_HRL, true).
-endif.
