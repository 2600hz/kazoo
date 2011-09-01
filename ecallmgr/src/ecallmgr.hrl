-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/freeswitch_xml.hrl").
-include_lib("whistle/src/wh_api.hrl").
-include_lib("whistle/include/wh_log.hrl").

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
-define(APP_VERSION, <<"0.7.2">>).

-define(WHISTLE_CONTEXT, <<"context_2">>).

-define(SIP_INTERFACE, "sofia/sipinterface_1/").

-define(STARTUP_FILE, [code:lib_dir(ecallmgr, priv), "/startup.config"]).
-define(SETTINGS_FILE, [code:lib_dir(ecallmgr, priv), "/settings.config"]).
