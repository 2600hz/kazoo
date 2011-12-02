-include_lib("amqp_client/include/amqp_client.hrl").
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
			,started = {0,0,0} :: wh_now()
		       }).

-record(node_stats, {started = {0,0,0} :: wh_now()
		     ,last_heartbeat = {0,0,0} :: wh_now()
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
			       ,{<<"Outgoing-Caller-ID-Name">>, <<"origination_caller_id_name">>}
			       ,{<<"Outgoing-Caller-ID-Number">>,<<"origination_caller_id_number">>}
			       ,{<<"Outgoing-Callee-ID-Name">>, <<"origination_callee_id_name">>}
			       ,{<<"Outgoing-Callee-ID-Number">>, <<"origination_callee_id_number">>}
			       ,{<<"Auth-User">>, <<"sip_auth_username">>}
			       ,{<<"Auth-Password">>, <<"sip_auth_password">>}
			       ,{<<"Caller-ID-Name">>, <<"effective_caller_id_name">>}
			       ,{<<"Caller-ID-Number">>, <<"effective_caller_id_number">>}
			       ,{<<"Callee-ID-Name">>, <<"effective_callee_id_name">>}
			       ,{<<"Callee-ID-Number">>, <<"effective_callee_id_number">>}
			       ,{<<"Progress-Timeout">>, <<"progress_timeout">>}
			       ,{<<"Rate">>, <<"rate">>}
			       ,{<<"Rate-Increment">>, <<"rate_increment">>}
			       ,{<<"Rate-Minimum">>, <<"rate_minimum">>}
			       ,{<<"Surcharge">>, <<"surcharge">>}
			       ,{<<"Ignore-Early-Media">>, <<"ignore_early_media">>}
			       ,{<<"Continue-On-Fail">>, <<"continue_on_fail">>}
			       ,{<<"Endpoint-Timeout">>, <<"leg_timeout">>}
			       ,{<<"Endpoint-Progress-Timeout">>, <<"leg_progress_timeout">>}
			       ,{<<"Endpoint-Delay">>, <<"leg_delay_start">>}
			       ,{<<"Endpoint-Ignore-Forward">>, <<"outbound_redirect_fatal">>}
			       ,{<<"Overwrite-Channel-Vars">>, <<"local_var_clobber">>}
			       ,{<<"Confirm-File">>, <<"group_confirm_file">>}
			       ,{<<"Confirm-Key">>, <<"group_confirm_key">>}
			       ,{<<"Confirm-Cancel-Timeout">>, <<"group_confirm_cancel_timeout">>}
			       ,{<<"Fax-Enabled">>, <<"t38_passthrough">>}
			       ,{<<"Presence-ID">>, <<"presence_id">>}
			       ,{<<"Hold-Media">>, <<"hold_music">>}
			      ]).

-define(DEFAULT_RESPONSE_CODE, <<"488">>).
