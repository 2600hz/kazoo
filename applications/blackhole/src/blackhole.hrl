-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_events/include/kz_hooks.hrl").

-define(APP, 'blackhole').
-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, <<"blackhole">>).

-define(DEFAULT_MODULES, ['bh_token_auth'
                         ,'bh_call'
                         ,'bh_object'
                         ,'bh_fax'
                         ,'bh_conference'
                         ,'bh_presence'
                         ]).

-define(COMMAND_MODULES, ['bh_events'
                         ,'bh_authz_subscribe'
                         ]).

-type bh_subscribe_result() :: {'ok', bh_context:context()} | {'error', kz_term:ne_binary()}.

-record(bh_context, {auth_token = <<>> :: kz_term:api_binary() | '_'
                    ,auth_account_id :: kz_term:api_binary() | '_'
                    ,bindings = [] :: kz_term:ne_binaries() | '_'
                    ,websocket_session_id :: kz_term:api_binary() | '_'
                    ,websocket_pid :: kz_term:api_pid() | '_'
                    ,req_id = kz_binary:rand_hex(16) :: kz_term:ne_binary() | '_'
                    ,timestamp = kz_time:now_s() :: kz_time:gregorian_seconds() | '_'
                    ,name :: kz_term:api_binary() | '_'
                    ,metadata :: any() | '_'
                    ,destination = kz_util:node_hostname() :: kz_term:ne_binary() | '_'
                    ,source :: kz_term:api_binary() | '_'
                    ,errors = [] :: kz_term:ne_binaries() | '_'
                    ,result = 'ok' :: 'ok' | 'error' | 'shutdown' | '_'
                    ,listeners = [] :: list() | '_'
                    ,resp_status = <<"success">> :: kz_term:ne_binary() | '_'
                    ,resp_data = kz_json:new() :: kz_json:object() | '_'
                    }).

-define(BLACKHOLE_HRL, 'true').

-endif.
