-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/freeswitch_xml.hrl").
-include_lib("whistle/src/wh_api.hrl").
-include_lib("whistle/include/wh_log.hrl").

-type fs_api_ret() :: {'ok', binary()} | {'error', binary()} | 'timeout'.
-type fs_sendmsg_ret() :: 'ok' | {'error', binary()} | 'timeout'.

-record(handler_stats, {lookups_success = 0 :: integer()
			,lookups_failed = 0 :: integer()
                        ,lookups_timeout = 0 :: integer()
                        ,lookups_requested = 0 :: integer()
			,started = {0,0,0} :: tuple(integer(), integer(), integer())
		       }).

-record(node_stats, {started = {0,0,0} :: tuple(integer(), integer(), integer())
		     ,last_heartbeat = {0,0,0} :: tuple(integer(), integer(), integer())
                     ,created_channels = 0 :: integer()
		     ,destroyed_channels = 0 :: integer()
		     ,fs_uptime = 0 :: integer() % in microseconds
		    }).

-define(DEFAULT_DOMAIN, <<"trunks.2600hz.org">>).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, 10000). % 10 seconds
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>]). %% list of dialplan Application-Names that can execute after a call has hung up

-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"0.8.0">>).

-define(WHISTLE_CONTEXT, <<"context_2">>).

-define(SIP_INTERFACE, "sofia/sipinterface_1/").
-define(DEFAULT_FS_PROFILE, "sipinterface_1").

-define(STARTUP_FILE, [code:lib_dir(ecallmgr, priv), "/startup.config"]).
-define(SETTINGS_FILE, [code:lib_dir(ecallmgr, priv), "/settings.config"]).

-define(STARTUP_FILE_CONTENTS, <<"{'fs_nodes', []}.
{'fs_cmds', [{'load', \"mod_sofia\"}
             ,{'load', \"mod_shout\"}
             ,{'load', \"mod_shell_stream\"}
]}.">>).

%% Call and Channel Vars that have a special prefix instead of the standard CHANNEL_VAR_PREFIX prefix
%% [{AMQP-Header, FS-var-name}]
%% so FS-var-name of "foo_var" would become "foo_var=foo_val" in the channel/call string
-define(SPECIAL_CHANNEL_VARS, [
			       {<<"Auto-Answer">>, <<"sip_auto_answer">>}
			       ,{<<"Eavesdrop-Group">>, <<"eavesdrop_group">>}
			       ,{<<"Fax-Enabled">>, <<"t38_passthrough">>}
			      ]).

-define(DEFAULT_RESPONSE_CODE, <<"488">>).
