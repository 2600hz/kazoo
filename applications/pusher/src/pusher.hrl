-ifndef(PUSHER_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"pusher">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"pusher">>).

-define(CACHE_NAME, 'pusher_cache').

-define(TOKEN_KEY, <<"Token-ID">>).
-define(TOKEN_PROXY_KEY, <<"Proxy-Path">>).

-define(PUSHER_HRL, 'true').
-endif.
