-ifndef(PUSHER_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"pusher">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'pusher_cache').

-define(TOKEN_KEY, <<"Token-ID">>).
-define(TOKEN_PROXY_KEY, <<"Proxy-Path">>).

-define(PUSHER_HRL, 'true').
-endif.
