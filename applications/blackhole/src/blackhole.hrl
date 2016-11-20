-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, <<"blackhole">>).

-define(DEFAULT_MODULES, ['bh_token_auth'
                         ,'bh_call'
                         ,'bh_object'
                         ,'bh_fax'
                         ,'bh_conference'
                         ]).

-define(COMMAND_MODULES, ['bh_events'
                         ,'bh_authz_subscribe'
                         ]).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-type bh_subscribe_result() :: {'ok', bh_context:context()} | {'error', ne_binary()}.

-record(bh_context, {auth_token = <<>> :: api_binary() | '_'
                    ,auth_account_id :: api_binary() | '_'
                    ,bindings = [] :: ne_binaries() | '_'
                    ,websocket_session_id :: api_binary() | '_'
                    ,websocket_pid :: api_pid() | '_'
                    ,req_id = kz_util:rand_hex_binary(16) :: ne_binary() | '_'
                    ,timestamp :: gregorian_seconds() | '_'
                    ,name :: api_binary() | '_'
                    ,metadata :: any() | '_'
                    ,destination = kz_util:node_hostname() :: ne_binary() | '_'
                    ,source :: api_binary() | '_'
                    ,errors = [] :: ne_binaries() | '_'
                    ,result = 'ok' :: 'ok' | 'error' | 'shutdown' | '_'
                    ,listeners = [] :: list() | '_'
                    ,resp_status = <<"success">> :: ne_binary() | '_'
                    ,resp_data = kz_json:new() :: kz_json:object() | '_'
                    }).

-define(BLACKHOLE_HRL, 'true').

-endif.
