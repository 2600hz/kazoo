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

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-record(bh_context, {
           auth_token = <<>> :: api_binary()
          ,auth_account_id :: api_binary()
          ,account_id :: api_binary()
          ,binding :: api_binary()
          ,websocket_session_id :: api_binary()
          ,websocket_pid :: api_pid()
          ,req_id = <<(wh_util:rand_hex_binary(16))/binary, "-bh">> :: ne_binary()
         }).

-define(BLACKHOLE_HRL, 'true').

-endif.
