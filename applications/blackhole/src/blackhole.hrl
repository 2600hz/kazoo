-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(BLACKHOLE_CONFIG_CAT, <<"blackhole">>).

-define(CACHE_NAME, 'blackhole_cache').

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-record(bh_context, {
           auth_token = <<>> :: api_binary() | '_'
          ,auth_account_id :: api_binary() | '_'
          ,account_id :: api_binary() | '_'
          ,bindings = [] :: ne_binaries() | '_'
          ,websocket_session_id :: api_binary() | '_'
          ,websocket_pid :: api_pid() | '_'
          ,req_id = <<(wh_util:rand_hex_binary(16))/binary, "-bh">> :: ne_binary() | '_'
          ,timestamp :: gregorian_seconds() | '_'
          ,name :: api_binary() | '_'
          ,metadata :: any() | '_'
          ,destination = wh_util:node_hostname() :: ne_binary() | '_'
          ,source :: api_binary() | '_'
         }).

-define(BLACKHOLE_HRL, 'true').

-endif.
