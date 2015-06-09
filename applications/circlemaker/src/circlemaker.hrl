-ifndef(CIRCLEMAKER_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CIRCLEMAKER_CACHE, 'circlemaker_cache').

-define(CONFIG_CAT, <<"circlemaker">>).
-define(WORKER_POOL, 'circlemaker_worker_pool').

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"circlemaker">>).

-define(CIRCLEMAKER_HRL, 'true').
-endif.
