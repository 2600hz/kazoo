-ifndef(ECALLMGR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_api.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(ECALLMGR_UTIL_CACHE, 'ecallmgr_util_cache').
-define(ECALLMGR_AUTH_CACHE, 'ecallmgr_auth_cache').
-define(ECALLMGR_CALL_CACHE, 'ecallmgr_call_cache').

-define(CHANNELS_TBL, 'ecallmgr_channels').

-define(DEFAULT_FETCH_TIMEOUT, 2600).

-define(ECALLMGR_PLAYBACK_MEDIA_KEY(M), {'playback_media', M}).

-define(DEFAULT_FREESWITCH_CONTEXT, ecallmgr_config:get(<<"freeswitch_context">>, <<"context_2">>)).

-define(SIP_INTERFACE, "sipinterface_1").
-define(DEFAULT_FS_PROFILE, "sipinterface_1").
-define(DEFAULT_FS_TECHNOLOGY, "sofia").
-define(DEFAULT_FS_DIALPLAN, "XML").

-define(LOCAL_MEDIA_PATH, "/tmp/").

-define(DEFAULT_SAMPLE_RATE, ecallmgr_config:get_integer(<<"record_sample_rate">>, 8000)).

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

-record(sip_subscription, {key :: api_binary() | '_'
                           ,to :: api_binary() | '$1' | '_'
                           ,from :: api_binary() | '$2' | '_'
                           ,node :: atom() | '$1' | '_'
                           ,expires = 300 :: pos_integer() | '$1' | '_'
                           ,timestamp = wh_util:current_tstamp() :: pos_integer() | '$2' | '_'
                          }).

-record(channel, {uuid :: api_binary() | '$1' | '_'
                  ,destination :: api_binary() | '_'
                  ,direction :: api_binary() | '$1' | '_'
                  ,account_id :: api_binary() | '$1' | '$2' | '_'
                  ,account_billing :: api_binary() | '$7' | '_'
                  ,authorizing_id :: api_binary() | '$1' | '$3' | '_'
                  ,authorizing_type :: api_binary() | '_'
                  ,owner_id :: api_binary() | '$1' | '_'
                  ,resource_id :: api_binary() | '$4' | '_'
                  ,presence_id :: api_binary() | '$2' | '_'
                  ,fetch_id :: api_binary() | '$5' | '_'
                  ,bridge_id :: api_binary() | '$5' | '_'
                  ,reseller_id :: api_binary() | '_'
                  ,reseller_billing :: api_binary() | '_'
                  ,realm :: api_binary() | '_' | '$2'
                  ,username :: api_binary() | '_' | '$1'
                  ,import_moh = 'false' :: boolean() | '_'
                  ,answered = 'true' :: boolean() | '_'
                  ,other_leg :: api_binary() | '$2' | '_'
                  ,node :: atom() | '$1' | '$2' | '$3' | '_'
                  ,former_node :: atom() | '$2' | '_'
                  ,timestamp :: gregorian_seconds() | '$3' | '_'
                  ,profile :: api_binary() | '_'
                  ,context :: api_binary() | '_'
                  ,dialplan :: api_binary() | '_'
                  ,precedence = 5 :: pos_integer() | '$2' | '_'
                  ,handling_locally = 'false' :: boolean() | '_' %% is this ecallmgr handling the call control?
                  ,to_tag :: api_binary() | '_'
                  ,from_tag :: api_binary() | '_'
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].

-record(conference, {name :: api_binary() | '$1' | '_'
                     ,uuid :: api_binary() | '$1' | '_'
                     ,node :: atom() | '$1' | '$2' | '_'
                     ,participants = 0 :: non_neg_integer() | '_'
                     ,profile_name = <<"default">> :: ne_binary() | '_'
                     ,with_floor :: 'undefined' | non_neg_integer() | '_' % which participant has the floor
                     ,lost_floor :: 'undefined' | non_neg_integer() | '_' % which participant has lost the floor
                     ,running = 'true' :: boolean() | '_'
                     ,answered = 'true' :: boolean() | '_'
                     ,enforce_min = 'true' :: boolean() | '_'
                     ,dynamic = 'true' :: boolean() | '_'
                     ,exit_sound = 'true' :: boolean() | '_'
                     ,enter_sound = 'true' :: boolean() | '_'
                     ,start_time = wh_util:current_tstamp() :: non_neg_integer() | '_'
                     ,switch_hostname :: api_binary() | '_'
                     ,switch_url :: api_binary() | '_'
                     ,switch_external_ip :: api_binary() | '_'
                     ,account_id :: api_binary() | '_'
                    }).

-type conference() :: #conference{}.
-type conferences() :: [conference()].

-record(participant, {uuid :: api_binary() | '$1' | '_'
                      ,node :: atom() | '$2' | '_'
                      ,conference_uuid :: api_binary() | '$1'| '_'
                      ,conference_name :: api_binary() | '$1'| '_'
                      ,floor = 'false' :: boolean() | '_'
                      ,hear = 'true' :: boolean() | '_'
                      ,speak = 'true' :: boolean() | '_'
                      ,talking = 'false' :: boolean() | '_'
                      ,mute_detect = 'false' :: boolean() | '_'
                      ,member_id = 0 :: non_neg_integer() | '_'
                      ,member_type :: api_binary() | '_'
                      ,energy_level = 0 :: non_neg_integer() | '_'
                      ,volume_level = 0 :: non_neg_integer() | '_'
                      ,gain_level = 0 :: non_neg_integer() | '_'
                      ,current_energy = 0 :: non_neg_integer() | '_'
                      ,video = 'false' :: boolean() | '_'
                      ,is_moderator = 'false' :: boolean() | '_'
                     }).
-type participant() :: #participant{}.
-type participants() :: [participant()].

-define(DEFAULT_REALM, ecallmgr_config:get(<<"default_realm">>, <<"nodomain.com">>)).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, ecallmgr_config:get_integer(<<"max_timeout_for_node_restart">>, 10 * ?MILLISECONDS_IN_SECOND)). % 10 seconds
-define(MAX_NODE_RESTART_FAILURES, 3).

%% list of dialplan Application-Names that can execute after a call has hung up
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>, <<"presence">>
                               ,<<"record">>, <<"store_fax">>, <<"receive_fax">>
                              ]).

-define(SANITY_CHECK_PERIOD, 300 * ?MILLISECONDS_IN_SECOND).

-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"0.8.0">>).

-define(STARTUP_FILE, [code:lib_dir('ecallmgr', 'priv'), "/startup.config"]).
-define(SETTINGS_FILE, [code:lib_dir('ecallmgr', 'priv'), "/settings.config"]).

-define(AUTHZ_RESPONSE_KEY(CallId), {'authz_response', CallId}).

-define(STARTUP_FILE_CONTENTS, <<"{'fs_nodes', []}.
{'fs_cmds', [{'load', \"mod_sofia\"}
             ,{'reloadacl', \"\"}
]}.">>).

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

-define(GET_CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(SET_CCV(Key, Value), <<?CHANNEL_VAR_PREFIX, Key/binary, "=", Value/binary>>).

-define(CREDS_KEY(Realm, Username), {'authn', Username, Realm}).

%% Call and Channel Vars that have a special prefix instead of the standard CHANNEL_VAR_PREFIX prefix
%% [{AMQP-Header, FS-var-name}]
%% so FS-var-name of "foo_var" would become "foo_var=foo_val" in the channel/call string
-define(SPECIAL_CHANNEL_VARS, [{<<"Auto-Answer">>, <<"sip_auto_answer">>}
                               ,{<<"Auto-Answer-Notify">>, <<"sip_auto_answer_notify">>}
                               ,{<<"Eavesdrop-Group">>, <<"eavesdrop_group">>}
                               ,{<<"Outbound-Caller-ID-Name">>, <<"origination_caller_id_name">>}
                               ,{<<"Outbound-Caller-ID-Number">>,<<"origination_caller_id_number">>}
                               ,{<<"Outbound-Callee-ID-Name">>, <<"origination_callee_id_name">>}
                               ,{<<"Outbound-Callee-ID-Number">>, <<"origination_callee_id_number">>}
                               ,{<<"Auth-User">>, <<"sip_auth_username">>}
                               ,{<<"Auth-Password">>, <<"sip_auth_password">>}
                               ,{<<"Auth-Realm">>, <<"sip_auth_realm">>}
                               ,{<<"Caller-ID-Name">>, <<"effective_caller_id_name">>}
                               ,{<<"Caller-ID-Number">>, <<"effective_caller_id_number">>}
                               ,{<<"Callee-ID-Name">>, <<"effective_callee_id_name">>}
                               ,{<<"Callee-ID-Number">>, <<"effective_callee_id_number">>}
                               ,{<<"effective_callee_id_number">>, <<"effective_callee_id_number">>}
                               ,{<<"effective_callee_id_name">>, <<"effective_callee_id_name">>}
                               ,{<<"effective_caller_id_number">>, <<"effective_caller_id_number">>}
                               ,{<<"effective_caller_id_name">>, <<"effective_caller_id_name">>}

                               ,{<<"Progress-Timeout">>, <<"progress_timeout">>}
                               ,{<<"Ignore-Early-Media">>, <<"ignore_early_media">>}
                               ,{<<"Continue-On-Fail">>, <<"continue_on_fail">>}
                               ,{<<"failure_causes">>, <<"failure_causes">>}
                               ,{<<"Endpoint-Timeout">>, <<"leg_timeout">>}
                               ,{<<"Endpoint-Progress-Timeout">>, <<"leg_progress_timeout">>}
                               ,{<<"Endpoint-Delay">>, <<"leg_delay_start">>}
                               ,{<<"Ignore-Forward">>, <<"outbound_redirect_fatal">>}
                               ,{<<"Overwrite-Channel-Vars">>, <<"local_var_clobber">>}
                               ,{<<"Confirm-File">>, <<"group_confirm_file">>}
                               ,{<<"Confirm-Key">>, <<"group_confirm_key">>}
                               ,{<<"Confirm-Cancel-Timeout">>, <<"group_confirm_cancel_timeout">>}
                               ,{<<"Alert-Info">>, <<"alert_info">>}
                               ,{<<"Fax-Enabled">>, <<"t38_passthrough">>}
                               ,{<<"Presence-ID">>, <<"presence_id">>}
                               ,{<<"Inherit-Codec">>, <<"inherit_codec">>}
                               ,{<<"From-URI">>, <<"sip_from_uri">>}
                               ,{<<"Bypass-Media">>, <<"bypass_media_after_bridge">>}
                               ,{<<"Bridge-Generate-Comfort-Noise">>,<<"bridge_generate_comfort_noise">>}
                               ,{<<"Origination-UUID">>, <<"origination_uuid">>}
                               ,{<<"Ignore-Display-Updates">>, <<"ignore_display_updates">>}
                               ,{<<"Eavesdrop-Group-ID">>, <<"eavesdrop_group">>}

                               ,{<<"Loopback-Bowout">>, <<"loopback_bowout_on_execute">>}
                               ,{<<"Simplify-Loopback">>, <<"loopback_bowout">>}

                               ,{<<"SIP-Invite-Domain">>, <<"sip_invite_domain">>}
                               ,{<<"Media-Encryption-Enforce-Security">>,<<"sdp_secure_savp_only">>}

                               ,{<<"Secure-RTP">>, <<"rtp_secure_media">>}
                               ,{<<"RTP-Secure-Media">>, <<"rtp_secure_media">>}
                               ,{<<"RTP-Secure-Media-Confirmed">>, <<"rtp_secure_media_confirmed">>}
                               ,{<<"RTP-Secure-Audio-Confirmed">>, <<"rtp_secure_media_confirmed_audio">>}
                               ,{<<"RTP-Secure-Video-Confirmed">>, <<"rtp_secure_media_confirmed_video">>}

                               ,{<<"Secure-ZRTP">>, <<"zrtp_secure_media">>}
                               ,{<<"ZRTP-Secure-Media">>, <<"zrtp_secure_media">>}
                               ,{<<"ZRTP-Secure-Media-Confirmed">>, <<"zrtp_secure_media_confirmed">>}
                               ,{<<"ZRTP-Secure-Audio-Confirmed">>, <<"zrtp_secure_media_confirmed_audio">>}
                               ,{<<"ZRTP-Secure-Video-Confirmed">>, <<"zrtp_secure_media_confirmed_video">>}
                               ,{<<"ZRTP-Enrollment">>, <<"zrtp_enrollment">>}

                               ,{<<"Ignore-Completed-Elsewhere">>, <<"ignore_completed_elsewhere">>}
                               ,{<<"tts_engine">>, <<"tts_engine">>}
                               ,{<<"tts_voice">>, <<"tts_voice">>}
                               ,{<<"playback_terminators">>, <<"playback_terminators">>}
                               ,{<<"record_waste_resources">>, <<"record_waste_resources">>}
                               ,{<<"record_sample_rate">>, <<"record_sample_rate">>}
                               ,{<<"Record-Sample-Rate">>, <<"record_sample_rate">>}
                               ,{<<"recording_follow_transfer">>, <<"recording_follow_transfer">>}
                               ,{<<"recording_follow_attxfer">>, <<"recording_follow_attxfer">>}
                               ,{<<"Record-Min-Sec">>, <<"record_min_sec">>}
                               ,{<<"record_min_sec">>, <<"record_min_sec">>}

                               ,{<<"enable_file_write_buffering">>, <<"enable_file_write_buffering">>}
                               ,{<<"RECORD_APPEND">>, <<"RECORD_APPEND">>}
                               ,{<<"fax_enable_t38_request">>, <<"fax_enable_t38_request">>}
                               ,{<<"fax_enable_t38">>, <<"fax_enable_t38">>}
                               ,{<<"Enable-T38-Fax">>, <<"fax_enable_t38">>}
                               ,{<<"Enable-T38-Fax-Request">>, <<"fax_enable_t38_request">>}
                               ,{<<"Enable-T38-Passthrough">>, <<"t38_passthru">>}
                               ,{<<"execute_on_answer">>, <<"execute_on_answer">>}
                               ,{<<"Fax-Identity-Number">>, <<"fax_ident">>}
                               ,{<<"Fax-Identity-Name">>, <<"fax_header">>}
                               ,{<<"Fax-Timezone">>, <<"fax_timezone">>}
                               ,{<<"sip_rh_X-Redirect-Server">>, <<"sip_rh_X-Redirect-Server">>}
                               ,{<<"park_after_bridge">>, <<"park_after_bridge">>}
                               ,{<<"Park-After-Pickup">>, <<"park_after_bridge">>}
                               ,{<<"park_after_pickup">>, <<"park_after_bridge">>}
                               ,{<<"Transfer-After-Pickup">>, <<"transfer_after_bridge">>}
                               ,{<<"Hangup-After-Pickup">>, <<"hangup_after_bridge">>}
                               ,{<<"hangup_after_pickup">>, <<"hangup_after_bridge">>}
                               ,{<<"continue_on_fail">>, <<"continue_on_fail">>}
                               ,{<<"continue_on_cancel">>, <<"continue_on_cancel">>}
                               ,{<<"Unbridged-Only">>, <<"intercept_unbridged_only">>}
                               ,{<<"intercept_unbridged_only">>, <<"intercept_unbridged_only">>}
                               ,{<<"Unanswered-Only">>, <<"intercept_unanswered_only">>}
                               ,{<<"intercept_unanswered_only">>, <<"intercept_unanswered_only">>}
                               ,{<<"conference_member_nospeak_relational">>, <<"conference_member_nospeak_relational">>}
                               ,{<<"conference_member_nospeak_check">>, <<"conference_member_nospeak_check">>}
                               ,{<<"Fax-Doc-ID">>, <<"fax_doc_id">>}
                               ,{<<"Fax-Doc-DB">>, <<"fax_doc_database">>}
                               ,{<<"default_language">>, <<"default_language">>}
                               ,{<<"Default-Language">>, <<"default_language">>}
                               ,{<<"RECORD_STEREO">>, <<"RECORD_STEREO">>}
                               ,{<<"RECORD_SOFTWARE">>, <<"RECORD_SOFTWARE">>}
                              ]).

%% [{FreeSWITCH-App-Name, Kazoo-App-Name}]
%% Dialplan-related applications
%% convert from FS-named applications to Kazoo-named Dialplan applications
-define(FS_APPLICATION_NAMES, [{<<"playback">>, <<"play">>}
                               ,{<<"playback">>, <<"tts">>}
                               ,{<<"play-file">>, <<"play">>}
                               ,{<<"play-file-done">>, <<"play">>}
                               ,{<<"play-file-member">>, <<"play">>}
                               ,{<<"play-file-member-done">>, <<"play">>}
                               ,{<<"speak">>, <<"tts">>}
                               ,{<<"hangup">>, <<"hangup">>}
                               ,{<<"record">>, <<"record">>}
                               ,{<<"start-recording">>, <<"record">>}
                               ,{<<"stop-recording">>, <<"record">>}
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
                               ,{<<"soft_hold">>, <<"soft_hold">>}
                               ,{<<"uuid_record">>, <<"record_call">>}
                               ,{<<"record">>, <<"record_call">>}
                               ,{<<"presence">>, <<"presence">>}
                               ,{<<"privacy">>, <<"privacy">>}
                               ,{<<"conference">>, <<"page">>}
                              ]).

-define(FS_EVENTS, [['CHANNEL_CREATE', 'CHANNEL_ANSWER', 'CHANNEL_DESTROY']
                    ,'CALL_UPDATE', 'DETECTED_TONE', 'CHANNEL_PROGRESS_MEDIA'
                    ,'DTMF', 'RECORD_START', 'RECORD_STOP', 'CHANNEL_BRIDGE'
                    ,'CHANNEL_UNBRIDGE', 'CHANNEL_EXECUTE', 'CHANNEL_EXECUTE_COMPLETE'
                    ,'CHANNEL_DATA', 'CALL_SECURE'
                   ]).

-define(FS_CUSTOM_EVENTS, ['whistle::noop', 'whistle::masquerade'
                           ,'sofia::transferor', 'sofia::transferee'
                           ,'sofia::replaced', 'sofia::register'
                           ,'sofia::intercepted'
                           ,'conference::maintenance'
                           ,'spandsp::txfaxresult'
                           ,'spandsp::rxfaxresult'
                           ,'spandsp::txfaxpageresult'
                           ,'spandsp::rxfaxpageresult'
                           ,'spandsp::txfaxnegociateresult'
                           ,'spandsp::rxfaxnegociateresult'
                           ,?CHANNEL_MOVE_RELEASED_EVENT
                           ,?CHANNEL_MOVE_COMPLETE_EVENT
                           ,'KZ::DELIVERY_REPORT'
                           ,'SMS::DELIVERY_REPORT'
                           ,'KZ::MESSAGE'
                           ,'loopback::bowout'
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

-define(CHANNEL_MOVE_REQUEST_EVENT, 'channel_move::move_request').
-define(CHANNEL_MOVE_RELEASED_EVENT, 'channel_move::move_released').
-define(CHANNEL_MOVE_COMPLETE_EVENT, 'channel_move::move_complete').

-define(CHANNEL_MOVE_RELEASED_EVENT_BIN, <<"channel_move::move_released">>).
-define(CHANNEL_MOVE_COMPLETE_EVENT_BIN, <<"channel_move::move_complete">>).

-define(CHANNEL_MOVE_REG(Node, UUID), {'channel_move', Node, UUID}).
-define(CHANNEL_MOVE_RELEASED_MSG(Node, UUID, Evt), {'channel_move_released', Node, UUID, Evt}).
-define(CHANNEL_MOVE_COMPLETE_MSG(Node, UUID, Evt), {'channel_move_complete', Node, UUID, Evt}).

-define(REGISTER_SUCCESS_REG, 'register_success').
-define(REGISTER_SUCCESS_MSG(Node, Props), {Node, Props}).

-define(LOOPBACK_BOWOUT_REG(CallId), {'loopback_bowout', CallId}).
-define(LOOPBACK_BOWOUT_MSG(Node, Props), {Node, Props}).

-define(FS_EVENT_REG_MSG(Node, EvtName), {'event', Node, EvtName}).
-define(FS_CALL_EVENT_REG_MSG(Node, EvtName), {'call_event', Node, EvtName}).
-define(FS_CALL_EVENTS_PROCESS_REG(Node, CallId)
        ,{'n', 'l', {'call_events_process', Node, CallId}}
       ).

-define(FS_CARRIER_ACL_LIST, <<"trusted">>).
-define(FS_SBC_ACL_LIST, <<"authoritative">>).

-define(SEPARATOR_SIMULTANEOUS, <<",">>).
-define(SEPARATOR_SINGLE, <<"|">>).

-define(CHANNEL_VARS_EXT, "Execute-Extension-Original-").
-define(CHANNEL_LOOPBACK_HEADER_PREFIX, "FSLoopBack-").

-define(ECALLMGR_HRL, 'true').
-endif.
