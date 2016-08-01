-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_amqp.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(BLACKHOLE_CONFIG_CAT, <<"blackhole">>).

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-record(bh_context, {
          auth_token = <<>> :: api_binary() | '_'
                    ,auth_account_id :: api_binary() | '_'
                    ,account_id :: api_binary() | '_'
                    ,bindings = [] :: ne_binaries() | '_'
                    ,websocket_session_id :: api_binary() | '_'
                    ,websocket_pid :: api_pid() | '_'
                    ,req_id = <<(kz_util:rand_hex_binary(16))/binary, "-bh">> :: ne_binary() | '_'
                    ,timestamp :: gregorian_seconds() | '_'
                    ,name :: api_binary() | '_'
                    ,metadata :: any() | '_'
                    ,destination = kz_util:node_hostname() :: ne_binary() | '_'
                    ,source :: api_binary() | '_'
                    ,binding :: api_binary() | '_'
         }).

-define(BLACKHOLE_HRL, 'true').

-endif.
