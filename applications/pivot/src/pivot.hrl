-ifndef(PIVOT_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_api.hrl").

-define(CACHE_NAME, 'pivot_cache').

-define(APP_NAME, <<"pivot">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(PIVOT_HRL, 'true').
-endif.
