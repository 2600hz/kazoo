-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(BLACKHOLE_CONFIG_CAT, <<"blackhole">>).
-define(TOKEN_DB, <<"token_auth">>).

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).



-define(BLACKHOLE_HRL, 'true').

-endif.
