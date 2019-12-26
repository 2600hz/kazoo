-ifndef(PUSHER_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"pusher">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'pusher_cache').

-define(DEFAULT_APNS_HOST, <<"api.push.apple.com">>).
-define(TOKEN_KEY, <<"Token-ID">>).
-define(TOKEN_PROXY_KEY, <<"Proxy-Path">>).

-define(MODULES, ['pm_apple', 'pm_firebase']).

-type push_app() :: {kz_term:api_pid(), map()} | 'undefined'.

-define(PUSHER_HRL, 'true').
-endif.
