-ifndef(ECALLMGR_HRL).
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/freeswitch_xml.hrl").
-include_lib("whistle/src/wh_api.hrl").

-define(ECALLMGR_AMQP_POOL, ecallmgr_amqp_pool).

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

-define(DEFAULT_DOMAIN, <<"whistle.2600hz.org">>).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, 10000). % 10 seconds

%% list of dialplan Application-Names that can execute after a call has hung up
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>, <<"presence">>, <<"record">>]). 

-define(SANITY_CHECK_PERIOD, 300000).

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

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%% Call and Channel Vars that have a special prefix instead of the standard CHANNEL_VAR_PREFIX prefix
%% [{AMQP-Header, FS-var-name}]
%% so FS-var-name of "foo_var" would become "foo_var=foo_val" in the channel/call string
-define(SPECIAL_CHANNEL_VARS, [{<<"Auto-Answer">>, <<"sip_auto_answer">>}
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
                               ,{<<"Inherit-Codec">>, <<"inherit_codec">>}
                               ,{<<"From-URI">>, <<"sip_from_uri">>}
                               ,{<<"Bypass-Media">>, <<"bypass_media">>}
                               ,{<<"Origination-UUID">>, <<"origination_uuid">>}
                               ,{<<"Ignore-Display-Updates">>, <<"ignore_display_updates">>}
                               %% ,{<<"Hold-Media">>, <<"hold_music">>}
                              ]).

%% [{FreeSWITCH-App-Name, Whistle-App-Name}]
%% Dialplan-related applications
%% convert from FS-named applications to Whistle-named Dialplan applications
-define(FS_APPLICATION_NAMES, [{<<"playback">>, <<"play">>}
                               ,{<<"hangup">>, <<"hangup">>}
                               ,{<<"record">>, <<"record">>}
                               ,{<<"playback">>, <<"tones">>}
                               ,{<<"park">>, <<"park">>}
                               ,{<<"set">>, <<"set">>}
                               ,{<<"export">>, <<"set">>}
                               ,{<<"say">>, <<"say">>}
                               ,{<<"sleep">>, <<"sleep">>}
                               ,{<<"respond">>, <<"respond">>}
                               ,{<<"bridge">>, <<"bridge">>}
                               ,{<<"signal_bridge">>, <<"bridge">>}
                               ,{<<"answer">>, <<"answer">>}
                               ,{<<"pre_answer">>, <<"progress">>}
                               ,{<<"ring_ready">>, <<"ring">>}
                               ,{<<"tone_detect">>, <<"tone_detect">>}
                               ,{<<"play_and_get_digits">>, <<"play_and_collect_digits">>}
                               ,{<<"respond">>, <<"respond">>}
                               ,{<<"redirect">>, <<"redirect">>}
                               ,{<<"conference">>, <<"conference">>}
                               ,{<<"noop">>, <<"noop">>}
                               ,{<<"execute_extension">>, <<"execute_extension">>}
                               ,{<<"endless_playback">>, <<"hold">>}
                               ,{<<"uuid_record">>, <<"record_call">>}
                               ,{<<"presence">>, <<"presence">>}
                              ]).

-define(FS_EVENTS, [<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"CHANNEL_HANGUP">>
                        ,<<"CHANNEL_HANGUP_COMPLETE">>, <<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
                        ,<<"DETECTED_TONE">>, <<"DTMF">>, <<"CALL_UPDATE">>, <<"RECORD_STOP">>, <<"CHANNEL_CREATE">>
                        %%                      ,<<"CUSTOM">>
                        ,<<"CHANNEL_DESTROY">>, <<"CHANNEL_EXECUTE_ERROR">>, <<"CHANNEL_PROGRESS_MEDIA">> %% custom error
                   ]).

-define(FS_DEFAULT_HDRS, [<<"Event-Name">>, <<"Core-UUID">>, <<"FreeSWITCH-Hostname">>, <<"FreeSWITCH-Switchname">>
                              ,<<"FreeSWITCH-IPv4">>, <<"FreeSWITCH-IPv6">>, <<"Event-Date-Local">>
                              ,<<"Event-Date-GMT">>, <<"Event-Date-Timestamp">>, <<"Event-Calling-File">>
                              ,<<"Event-Calling-Function">>, <<"Event-Calling-Line-Number">>]).

-define(FS_CHANNEL_STATES, [{<<"CS_NEW">>, <<"new">>}
                            ,{<<"CS_INIT">>, <<"initialize">>}
                            ,{<<"CS_ROUTING">>, <<"routing">>}
                            ,{<<"CS_SOFT_EXECUTE">>, <<"soft_execute">>}
                            ,{<<"CS_EXECUTE">>, <<"execute">>}
                            ,{<<"CS_EXCHANGE_MEDIA">>, <<"exchange_media">>}
                            ,{<<"CS_PARK">>, <<"park">>}
                            ,{<<"CS_CONSUME_MEDIA">>, <<"consume_media">>}
                            ,{<<"CS_HIBERNATE">>, <<"hibernate">>}
                            ,{<<"CS_RESET">>, <<"reset">>}
                            ,{<<"CS_HANGUP">>, <<"hangup">>}
                            ,{<<"CS_REPORTING">>, <<"reporting">>}
                            ,{<<"CS_DESTROY">>, <<"destroy">>}]).

-define(DEFAULT_RESPONSE_CODE, <<"488">>).

-define(FS_CMD_SAFELIST, [<<"load">>, <<"set">>, <<"uuid_dump">>, <<"uuid_record">>
                              ,<<"uuid_kill">>, <<"uuid_getvar">>, <<"show">>
                              ,<<"uuid_exists">>
                         ]).

-define(FS_CONFERNCE_ATTRS, [{'name', <<"Conference-ID">>}
                             ,{'member-count', <<"Participant-Count">>}
                             ,{'rate', <<"Rate">>}
                             ,{'uuid', <<"UUID">>}
                             ,{'locked', <<"Locked">>}
                             ,{'run_time', <<"Run-Time">>}
                             ,{'running', <<"Running">>}
                             ,{'answered', <<"Answered">>}
                             ,{'dynamic', <<"Dynamic">>}
                            ]).

-define(FS_CONFERENCE_PARTICIPANT, [{'id', <<"Participant-ID">>}
                                    ,{'uuid', <<"Call-ID">>}
                                    ,{'caller_id_name', <<"Caller-ID-Name">>}
                                    ,{'caller_id_number', <<"Caller-ID-Number">>}
                                    ,{'join_time', <<"Join-Time">>}
                                    ,{'last_talking', <<"Last-Talking-Time">>}
                                    ,{'energy', <<"Energy-Level">>}
                                    ,{'volume_in', <<"Volume-In-Level">>}
                                    ,{'volume_out', <<"Volume-Out-Level">>}
                                    ,{'output-volume', <<"Output-Volume-Level">>}
                                    ,{'input-volume', <<"Input-Volume-Level">>}
                                    ,{'auto-adjusted-input-volume', <<"Adjusted-Input-Volume-Level">>}
                                   ]).

-define(FS_CONFERENCE_FLAGS, [{'can_hear', <<"Can-Hear">>}
                              ,{'can_speak', <<"Can-Speak">>}
                              ,{'mute_detect', <<"Mute-Detect">>}
                              ,{'talking', <<"Talking">>}
                              ,{'has_video', <<"Has-Video">>}
                              ,{'had_floor', <<"Had-Floor">>}
                              ,{'is_moderator', <<"Moderator">>}
                              ,{'end_conference', <<"End-Conference">>}
                             ]).

-define(ECALLMGR_HRL, true).
-endif.
