-ifndef(ECALLMGR_HRL).

-compile([{parse_transform, switchblade_transform}]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_api.hrl").

-define(ECALLMGR_AMQP_POOL, ecallmgr_amqp_pool).

-define(ECALLMGR_UTIL_CACHE, ecallmgr_util_cache).
-define(ECALLMGR_REG_CACHE, ecallmgr_reg_cache).
-define(ECALLMGR_CALL_CACHE, ecallmgr_call_cache).

-define(ECALLMGR_RECORDED_MEDIA_KEY(M), {recorded_media, M}).

-define(WHISTLE_CONTEXT, <<"context_2">>).

-define(SIP_INTERFACE, "sofia/sipinterface_1/").
-define(DEFAULT_FS_PROFILE, "sipinterface_1").
-define(DEFAULT_FS_DIALPLAN, "XML").

-type fs_api_ret()       :: {'ok', binary()} |
                            {'error', 'badarg'} |
                            'timeout'.
-type fs_sendmsg_ret()   :: 'ok' |
                            {'error', 'badarg' | 'badmem' | 'nosession'} |
                            'timeout'.
-type fs_sendevent_ret() :: 'ok' |
                            {'error', 'badarg' | 'badmem'} |
                            'timeout'.
-type fs_bind_ret()      :: 'ok' |
                            {'error', 'badarg' | 'badmem'} |
                            'timeout'.
-type fs_handlecall_ret() :: 'ok' |
                             {'error', 'badarg' | 'session_attach_failed' | 'badsession' | 'baduuid'} |
                             'timeout'.

-record(sip_subscription, {key=undefined
                           ,to=undefined
                           ,from=undefined
                           ,node=undefined
                           ,expires=300
                           ,timestamp=wh_util:current_tstamp()
                          }).

-record(channel, {uuid = '_'
                  ,destination = '_'
                  ,direction = '_'
                  ,account_id = '_'
                  ,account_billing = '_'
                  ,authorizing_id = '_'
                  ,authorizing_type = '_'
                  ,owner_id = '_'
                  ,resource_id = '_'
                  ,presence_id = '_'
                  ,billing_id = '_'
                  ,bridge_id = '_'
                  ,reseller_id = '_'
                  ,reseller_billing = '_'
                  ,realm = '_'
                  ,username = '_'
                  ,import_moh = '_'
                  ,node = '_'
                  ,former_node = '_'
                  ,timestamp = '_'
                  ,profile = '_'
                  ,context = '_'
                  ,dialplan = '_'
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel(),...] | [].

-define(DEFAULT_DOMAIN, <<"whistle.2600hz.org">>).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, 10000). % 10 seconds
-define(MAX_NODE_RESTART_FAILURES, 3).

%% list of dialplan Application-Names that can execute after a call has hung up
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>, <<"presence">>, <<"record">>, <<"store_fax">>]). 

-define(SANITY_CHECK_PERIOD, 300000).

-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"0.8.0">>).

-define(STARTUP_FILE, [code:lib_dir(ecallmgr, priv), "/startup.config"]).
-define(SETTINGS_FILE, [code:lib_dir(ecallmgr, priv), "/settings.config"]).

-define(AUTHZ_RESPONSE_KEY(CallId), {authz_response, CallId}).

-define(STARTUP_FILE_CONTENTS, <<"{'fs_nodes', []}.
{'fs_cmds', [{'load', \"mod_sofia\"}
             ,{'reloadacl', \"\"}
]}.">>).

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

-define(GET_CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(SET_CCV(Key, Value), <<?CHANNEL_VAR_PREFIX, Key/binary, "=", Value/binary>>).

%% Call and Channel Vars that have a special prefix instead of the standard CHANNEL_VAR_PREFIX prefix
%% [{AMQP-Header, FS-var-name}]
%% so FS-var-name of "foo_var" would become "foo_var=foo_val" in the channel/call string
-define(SPECIAL_CHANNEL_VARS, [{<<"Auto-Answer">>, <<"sip_auto_answer">>}
                               ,{<<"Auto-Answer-Notify">>, <<"sip_auto_answer_notify">>}
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
                               ,{<<"Eavesdrop-Group-ID">>, <<"eavesdrop_group">>}
                               %% ,{<<"Hold-Media">>, <<"hold_music">>}
                               ,{<<"Loopback-Bowout">>, <<"loopback_bowout_on_execute">>}
                               ,{<<"tts_engine">>, <<"tts_engine">>}
                               ,{<<"tts_voice">>, <<"tts_voice">>}
                               ,{<<"playback_terminators">>, <<"playback_terminators">>}
                               ,{<<"record_waste_resources">>, <<"record_waste_resources">>}
                               ,{<<"enable_file_write_buffering">>, <<"enable_file_write_buffering">>}
                               ,{<<"RECORD_APPEND">>, <<"RECORD_APPEND">>}
                               ,{<<"fax_enable_t38_request">>, <<"fax_enable_t38_request">>}
                               ,{<<"fax_enable_t38">>, <<"fax_enable_t38">>}
                               ,{<<"sip_rh_X-Redirect-Server">>, <<"sip_rh_X-Redirect-Server">>}
                               ,{<<"park_after_bridge">>, <<"park_after_bridge">>}
                               ,{<<"continue_on_fail">>, <<"continue_on_fail">>}
                               ,{<<"continue_on_cancel">>, <<"continue_on_cancel">>}
                              ]).

%% [{FreeSWITCH-App-Name, Kazoo-App-Name}]
%% Dialplan-related applications
%% convert from FS-named applications to Kazoo-named Dialplan applications
-define(FS_APPLICATION_NAMES, [{<<"playback">>, <<"play">>}
                               ,{<<"speak">>, <<"tts">>}
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
                               ,{<<"rxfax">>, <<"receive_fax">>}
                               ,{<<"tone_detect">>, <<"tone_detect">>}
                               ,{<<"play_and_get_digits">>, <<"play_and_collect_digits">>}
                               ,{<<"respond">>, <<"respond">>}
                               ,{<<"redirect">>, <<"redirect">>}
                               ,{<<"conference">>, <<"conference">>}
                               ,{<<"noop">>, <<"noop">>}
                               ,{<<"execute_extension">>, <<"execute_extension">>}
                               ,{<<"endless_playback">>, <<"hold">>}
                               ,{<<"uuid_record">>, <<"record_call">>}
                               ,{<<"record">>, <<"record_call">>}
                               ,{<<"presence">>, <<"presence">>}
                               ,{<<"privacy">>, <<"privacy">>}
                              ]).

-define(FS_EVENTS, [<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"CHANNEL_HANGUP">>
                        ,<<"CHANNEL_HANGUP_COMPLETE">>, <<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
                        ,<<"DETECTED_TONE">>, <<"DTMF">>, <<"CALL_UPDATE">>, <<"CHANNEL_CREATE">>
                        ,<<"RECORD_START">>, <<"RECORD_STOP">>
                        ,<<"CHANNEL_DESTROY">>, <<"CHANNEL_EXECUTE_ERROR">>, <<"CHANNEL_PROGRESS_MEDIA">>
                        ,<<"CHANNEL_ANSWER">>, <<"CHANNEL_PARK">>
                   ]).

-define(FS_DEFAULT_HDRS, [<<"Event-Name">>, <<"Core-UUID">>, <<"FreeSWITCH-Hostname">>, <<"FreeSWITCH-Switchname">>
                              ,<<"FreeSWITCH-IPv4">>, <<"FreeSWITCH-IPv6">>, <<"Event-Date-Local">>
                              ,<<"Event-Date-GMT">>, <<"Event-Date-Timestamp">>, <<"Event-Calling-File">>
                              ,<<"Event-Calling-Function">>, <<"Event-Calling-Line-Number">>, <<"Event-Sequence">>
                         ]).

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
                            ,{<<"CS_DESTROY">>, <<"destroy">>}
                           ]).

-define(DEFAULT_RESPONSE_CODE, <<"488">>).

-define(FS_CMD_SAFELIST, ["load", "set", "uuid_dump", "uuid_record"
                              ,"uuid_kill", "uuid_getvar", "show"
                              ,"uuid_exists", "reload", "reloadxml"
                              ,"reloadacl"
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
