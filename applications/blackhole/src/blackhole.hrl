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

-define(DEFAULT_MODULES, ['bh_token_auth, bh_call, bh_conference']).

-define(VERSION_SUPPORTED, [<<"v1">>]).


-record(bh_context, {
          auth_token = <<>> :: ne_binary() | 'undefined'
          ,auth_account_id :: api_binary()
          ,auth_doc :: api_object()
          ,account_id :: api_binary()
          ,db_name :: api_binary()
          ,doc :: api_object() | wh_json:objects()
          ,req_id = ?LOG_SYSTEM_ID :: ne_binary()
          ,storage = [] :: wh_proplist()          
          ,event_name = <<>> :: ne_binary()
          ,event_data :: api_object()
          ,client_ip = <<"127.0.0.1">> :: ne_binary()
          ,api_version = <<"v1">> :: ne_binary()
          ,websocket_session_id :: ne_binary()
          ,websocket_pid :: pid()
         }).

-define(BLACKHOLE_HRL, 'true').

-endif.
