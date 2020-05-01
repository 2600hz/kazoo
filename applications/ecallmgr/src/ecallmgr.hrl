-ifndef(ECALLMGR_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

-define(APP, 'ecallmgr').
-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"5.0.0">>).

-define(MIN_FS_VERSION, <<"v1.5">>).

-define(ECALLMGR_UTIL_CACHE, 'ecallmgr_util_cache').
-define(ECALLMGR_AUTH_CACHE, 'ecallmgr_auth_cache').
-define(ECALLMGR_CALL_CACHE, 'ecallmgr_call_cache').
-define(ECALLMGR_INTERACTION_CACHE, 'ecallmgr_interaction_cache').

-define(CHANNELS_TBL, 'ecallmgr_channels').

-define(DEFAULT_FETCH_TIMEOUT, 2600).

-define(FS_NODES, kapps_config:get_ne_binaries(?APP_NAME, <<"fs_nodes">>, [])).
-define(FS_NODES(Node), kapps_config:get_ne_binaries(?APP_NAME, <<"fs_nodes">>, [], Node)).

-define(ECALLMGR_PLAYBACK_MEDIA_KEY(M), {'playback_media', M}).

-define(DEFAULT_FREESWITCH_CONTEXT
       ,kapps_config:get_ne_binary(?APP_NAME, <<"freeswitch_context">>, <<"context_2">>)
       ).

-define(SIP_INTERFACE, "sipinterface_1").
-define(DEFAULT_FS_PROFILE, "sipinterface_1").
-define(DEFAULT_FS_TECHNOLOGY, "sofia").
-define(DEFAULT_FS_DIALPLAN, "XML").

-define(LOCAL_MEDIA_PATH, "/tmp/").

-define(DEFAULT_SAMPLE_RATE, kapps_config:get_integer(?APP_NAME, <<"record_sample_rate">>, 8000)).
-define(DEFAULT_STEREO_SAMPLE_RATE, kapps_config:get_integer(?APP_NAME, <<"record_stereo_sample_rate">>, 16000)).

-define(RESTRICTED_PUBLISHING, kapps_config:is_true(?APP_NAME, <<"restrict_channel_event_publisher">>, 'true')).

-type fs_app() :: {kz_term:ne_binary(), binary() | 'noop'} |
                  {kz_term:ne_binary(), kz_term:ne_binary(), atom()} |
                  {kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:proplist()}.
-type fs_apps() :: [fs_app()].

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

-record(channel, {uuid :: kz_term:api_ne_binary() | '$1' | '$2' | '_'
                 ,destination :: kz_term:api_ne_binary() | '_'
                 ,direction :: kz_term:api_ne_binary() | '$1' | '_'
                 ,account_id :: kz_term:api_ne_binary() | '$1' | '$2' | '_'
                 ,account_billing :: kz_term:api_ne_binary() | '$7' | '_'
                 ,authorizing_id :: kz_term:api_ne_binary() | '$1' | '$3' | '_'
                 ,authorizing_type :: kz_term:api_binary() | '_'
                 ,is_authorized :: kz_term:api_boolean() | '_'
                 ,owner_id :: kz_term:api_ne_binary() | '$1' | '_'
                 ,resource_id :: kz_term:api_ne_binary() | '$4' | '_'
                 ,presence_id :: kz_term:api_ne_binary() | '$2' | '_'
                 ,fetch_id :: kz_term:api_ne_binary() | '$5' | '_'
                 ,bridge_id :: kz_term:api_ne_binary() | '$5' | '_'
                 ,reseller_id :: kz_term:api_ne_binary() | '$1' | '$2' | '_'
                 ,reseller_billing :: kz_term:api_ne_binary() | '_'
                 ,realm :: kz_term:api_ne_binary() | '_' | '$2'
                 ,username :: kz_term:api_ne_binary() | '_' | '$1'
                 ,import_moh = 'false' :: boolean() | '_'
                 ,answered = 'true' :: boolean() | '_'
                 ,other_leg :: kz_term:api_binary() | '$2' | '_'
                 ,node :: atom() | '$1' | '$2' | '$3' | '_'
                 ,former_node :: atom() | '$2' | '_'
                 ,timestamp :: kz_time:gregorian_seconds() | 'undefined' | '$3' | '_'
                 ,profile :: kz_term:api_ne_binary() | '_'
                 ,context :: kz_term:api_ne_binary() | '_'
                 ,dialplan :: kz_term:api_ne_binary() | '_'
                 ,precedence = 5 :: pos_integer() | '$2' | '_'
                 ,handling_locally = 'false' :: boolean() | '_' %% is this ecallmgr handling the call control?
                 ,to_tag :: kz_term:api_ne_binary() | '_'
                 ,from_tag :: kz_term:api_ne_binary() | '_'
                 ,interaction_id :: kz_term:api_ne_binary() | '$5' | '_'
                 ,callee_number :: kz_term:api_ne_binary() | '$5' | '_'
                 ,callee_name :: kz_term:api_ne_binary() | '$5' | '_'
                 ,caller_number :: kz_term:api_ne_binary() | '_'
                 ,caller_name :: kz_term:api_ne_binary() | '_'
                 ,is_loopback = 'false' :: boolean() | '_'
                 ,loopback_leg_name :: kz_term:api_ne_binary() | '_'
                 ,loopback_other_leg :: kz_term:api_ne_binary() | '_'
                 ,callflow_id :: kz_term:api_ne_binary() | '_'
                 ,is_onhold = 'false' :: boolean() | '_'
                 ,cavs :: kz_term:api_object() | '_'
                 ,ccvs :: kz_term:api_object() | '_'
                 ,from :: kz_term:api_binary() | '_'
                 ,to :: kz_term:api_binary() | '_'
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].
-type channel_updates() :: [{pos_integer(), any()}].

-record(conference, {name :: kz_term:api_ne_binary() | '$1' | '_'
                    ,uuid :: kz_term:api_ne_binary() | '$1' | '_'
                    ,node :: atom() | '$1' | '$2' | '_'
                    ,participants = 0 :: non_neg_integer() | '_'
                    ,profile_name = <<"default">> :: kz_term:ne_binary() | '_'
                    ,with_floor :: 'undefined' | non_neg_integer() | '_' % which participant has the floor
                    ,lost_floor :: 'undefined' | non_neg_integer() | '_' % which participant has lost the floor
                    ,running = 'true' :: boolean() | '_'
                    ,answered = 'true' :: boolean() | '_'
                    ,locked = 'false' :: boolean() | '_'
                    ,enforce_min = 'true' :: boolean() | '_'
                    ,dynamic = 'true' :: boolean() | '_'
                    ,exit_sound = 'true' :: boolean() | '_'
                    ,enter_sound = 'true' :: boolean() | '_'
                    ,start_time = kz_time:now_s() :: non_neg_integer() | '_'
                    ,switch_hostname :: kz_term:api_binary() | '_'
                    ,switch_url :: kz_term:api_binary() | '_'
                    ,switch_external_ip :: kz_term:api_binary() | '_'
                    ,account_id :: kz_term:api_binary() | '_'
                    ,handling_locally = 'false' :: boolean() | '_' %% was this ecallmgr handling the call control?
                    ,origin_node :: atom() | '_'
                    ,control_node :: atom() | '_'
                    }).

-type conference() :: #conference{}.
-type conferences() :: [conference()].

-record(participant, {uuid :: kz_term:api_ne_binary() | '$1' | '_'
                     ,node :: atom() | '$2' | '_'
                     ,conference_uuid :: kz_term:api_ne_binary() | '$1'| '_'
                     ,conference_name :: kz_term:api_ne_binary() | '$1'| '_'
                     ,join_time = kz_time:now_s() :: kz_time:gregorian_seconds() | '_'
                     ,caller_id_name :: kz_term:api_ne_binary() | '_'
                     ,caller_id_number :: kz_term:api_ne_binary() | '_'
                     ,conference_channel_vars :: kz_term:api_object() | '_'
                     ,custom_channel_vars :: kz_term:api_object() | '_'
                     ,custom_application_vars :: kz_term:api_object() | '_'
                     }).
-type participant() :: #participant{}.
-type participants() :: [participant()].

-define(DEFAULT_REALM, kapps_config:get_ne_binary(?APP_NAME, <<"default_realm">>, <<"nodomain.com">>)).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, kapps_config:get_integer(?APP_NAME, <<"max_timeout_for_node_restart">>, 10 * ?MILLISECONDS_IN_SECOND)).
-define(MAX_NODE_RESTART_FAILURES, 3).

-define(EXPIRES_DEVIATION_TIME
       ,kapps_config:get_integer(?APP_NAME, <<"expires_deviation_time">>, 180)
       ).

%% list of dialplan Application-Names that can execute after a call has hung up
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>, <<"presence">>
                              ,<<"record">>, <<"store_fax">>, <<"receive_fax">>
                              ]).

-define(SANITY_CHECK_PERIOD, 300 * ?MILLISECONDS_IN_SECOND).

-define(STARTUP_FILE, [code:priv_dir(?APP), "/startup.config"]).
-define(SETTINGS_FILE, [code:priv_dir(?APP), "/settings.config"]).

-define(STARTUP_FILE_CONTENTS, <<"{'fs_nodes', []}.\n"
                                 "{'fs_cmds', [{'load', \"mod_sofia\"}\n"
                                 "            ,{'reloadacl', \"\"}\n"
                                 "            ]}.\n"
                               >>).

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API
%% message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(APPLICATION_VAR_PREFIX, "cav_").
-define(JSON_APPLICATION_VAR_PREFIX, "json_cav_").
-define(RECORD_VARS_PREFIX, "Recording-Variable-").

-define(CCV(Key), <<?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(GET_CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(SET_CCV(Key, Value), <<?CHANNEL_VAR_PREFIX, Key/binary, "=", Value/binary>>).
-define(GET_CCV_HEADER(Key), <<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(GET_CUSTOM_HEADER(Key), <<"variable_sip_h_X-", Key/binary>>).
-define(CUSTOM_HEADER(Key), <<"sip_h_X-", Key/binary>>).
-define(GET_VAR(Key), <<"variable_", Key/binary>>).

-define(CAV(Key), <<?APPLICATION_VAR_PREFIX, Key/binary>>).
-define(GET_CAV(Key), <<"variable_", ?APPLICATION_VAR_PREFIX, Key/binary>>).
-define(SET_CAV(Key, Value), <<?APPLICATION_VAR_PREFIX, Key/binary, "=", Value/binary>>).
-define(GET_CAV_HEADER(Key), <<"variable_sip_h_X-", ?APPLICATION_VAR_PREFIX, Key/binary>>).

-define(JSON_CAV(Key), <<?JSON_APPLICATION_VAR_PREFIX, Key/binary>>).
-define(GET_JSON_CAV(Key), <<"variable_", ?JSON_APPLICATION_VAR_PREFIX, Key/binary>>).

-define(CREDS_KEY(Realm, Username), {'authn', Username, Realm}).

-define(DP_EVENT_VARS, [{<<"Execute-On-Answer">>, <<"execute_on_answer">>}
                       ,{<<"Execute-On-Bridge">>, <<"execute_on_pre_bridge">>}
                       ,{<<"Execute-On-Before-Bridge">>, <<"execute_on_pre_bridge">>}
                       ,{<<"Execute-On-After-Bridge">>, <<"execute_on_after_bridge">>}
                       ,{<<"Execute-On-Tone-Detect">>, <<"execute_on_tone_detect">>}
                       ,{<<"Execute-On-Record-Post-Process">>, <<"record_post_process_exec_app">>}
                       ]).
-define(BRIDGE_CHANNEL_VAR_SEPARATOR, "!").
-define(RECORD_CALL_PARAM_SEPARATOR, "#").

%% Call and Channel Vars that have a special prefix instead of the
%% standard CHANNEL_VAR_PREFIX prefix [{AMQP-Header, FS-var-name}] so
%% FS-var-name of "foo_var" would become "foo_var=foo_val" in the
%% channel/call string
-define(SPECIAL_CHANNEL_VARS, [{<<"Alert-Info">>, <<"alert_info">>}
                              ,{<<"Attended-Transfer-Cancel-Key">>, <<"attxfer_cancel_key">>}
                              ,{<<"Attended-Transfer-Hangup-Key">>, <<"attxfer_hangup_key">>}
                              ,{<<"Attended-Transfer-Conference-Key">>, <<"attxfer_conf_key">>}
                              ,{<<"Attended-Transfer-Cancel-Dial-Key">>, <<"origination_cancel_key">>}
                              ,{<<"Auth-Password">>, <<"sip_auth_password">>}
                              ,{<<"Auth-Realm">>, <<"sip_auth_realm">>}
                              ,{<<"Auth-User">>, <<"sip_auth_username">>}
                              ,{<<"Auto-Answer">>, <<"sip_auto_answer">>}
                              ,{<<"Auto-Answer-Suppress-Notify">>, <<"sip_auto_answer_suppress_notify">>}
                              ,{<<"Bridge-Execute-On-Answer">>, <<"execute_on_answer">>}
                              ,{<<"Bridge-Generate-Comfort-Noise">>,<<"bridge_generate_comfort_noise">>}
                              ,{<<"Bypass-Media">>, <<"bypass_media_after_bridge">>}

                              ,{<<"Callee-ID-Name">>, <<"callee_id_name">>}
                              ,{<<"Callee-ID-Number">>, <<"callee_id_number">>}
                              ,{<<"Caller-Callee-ID-Name">>, <<"caller_callee_id_name">>}
                              ,{<<"Caller-Callee-ID-Number">>, <<"caller_callee_id_number">>}
                              ,{<<"Caller-Caller-ID-Name">>, <<"caller_caller_id_name">>}
                              ,{<<"Caller-Caller-ID-Number">>, <<"caller_caller_id_number">>}
                              ,{<<"Caller-ID-Name">>, <<"caller_id_name">>}
                              ,{<<"Caller-ID-Number">>, <<"caller_id_number">>}
                              ,{<<"Outbound-Call-ID">>, <<"origination_uuid">>}
                              ,{<<"Outbound-Callee-ID-Name">>, <<"origination_callee_id_name">>}
                              ,{<<"Outbound-Callee-ID-Number">>, <<"origination_callee_id_number">>}
                              ,{<<"Outbound-Caller-ID-Name">>, <<"origination_caller_id_name">>}
                              ,{<<"Outbound-Caller-ID-Number">>,<<"origination_caller_id_number">>}

                              ,{<<"Caller-ID-Type">>, <<"sip_cid_type">>}

                              ,{<<"Conference-Entry-Sound">>, <<"conference_enter_sound">>}
                              ,{<<"Conference-Exit-Sound">>, <<"conference_exit_sound">>}

                              ,{<<"Confirm-Cancel-Timeout">>, <<"group_confirm_cancel_timeout">>}
                              ,{<<"Confirm-File">>, <<"group_confirm_file">>}
                              ,{<<"Confirm-Key">>, <<"group_confirm_key">>}
                              ,{<<"Confirm-Read-Timeout">>, <<"group_confirm_read_timeout">>}

                              ,{<<"Continue-On-Fail">>, <<"continue_on_fail">>}
                              ,{<<"Default-Language">>, <<"default_language">>}
                              ,{<<"Diversions">>, <<"sip_h_Diversion">>}
                              ,{<<"Eavesdrop-Group">>, <<"eavesdrop_group">>}
                              ,{<<"Eavesdrop-Group-ID">>, <<"eavesdrop_group">>}
                              ,{<<"Enable-T38-Fax">>, <<"fax_enable_t38">>}
                              ,{<<"Enable-T38-Fax-Request">>, <<"fax_enable_t38_request">>}
                              ,{<<"Enable-T38-Passthrough">>, <<"t38_passthru">>}
                              ,{<<"Endpoint-Delay">>, <<"leg_delay_start">>}
                              ,{<<"Endpoint-Progress-Timeout">>, <<"leg_progress_timeout">>}
                              ,{<<"Endpoint-Timeout">>, <<"leg_timeout">>}
                              ,{<<"Export-Variables">>, <<"export_vars">>}
                              ,{<<"Export-Bridge-Variables">>, <<"bridge_export_vars">>}
                              ,{<<"Fail-On-Single-Reject">>, <<"fail_on_single_reject">>}
                              ,{<<"Fax-Doc-DB">>, <<"fax_doc_database">>}
                              ,{<<"Fax-Doc-ID">>, <<"fax_doc_id">>}
                              ,{<<"Fax-Enabled">>, <<"fax_enable_t38">>}
                              ,{<<"Fax-Identity-Name">>, <<"fax_header">>}
                              ,{<<"Fax-Identity-Number">>, <<"fax_ident">>}
                              ,{<<"Fax-Timezone">>, <<"fax_timezone">>}
                              ,{<<"From-URI">>, <<"sip_from_uri">>}
                              ,{<<"From-User">>, <<"sip_from_user">>}
                              ,{<<"Hangup-After-Pickup">>, <<"hangup_after_bridge">>}
                              ,{<<"Hold-Media">>, <<"hold_music">>}
                              ,{<<"Ignore-Completed-Elsewhere">>, <<"ignore_completed_elsewhere">>}
                              ,{<<"Ignore-Display-Updates">>, <<"ignore_display_updates">>}
                              ,{<<"Ignore-Early-Media">>, <<"ignore_early_media">>}
                              ,{<<"Ignore-Ring-Ready">>, <<"ignore_ring_ready">>}
                              ,{<<"Ignore-Forward">>, <<"outbound_redirect_fatal">>}
                              ,{<<"Inherit-Codec">>, <<"inherit_codec">>}
                              ,{<<"Loopback-Bowout">>, <<"loopback_bowout">>}
                              ,{<<"Loopback-Request-Variables">>, <<"loopback_bleg_variables">>}
                              ,{<<"Loopback-Export">>, <<"loopback_export">>}
                              ,{<<"Loopback-Request-URI">>, <<"sip_loopback_req_uri">>}
                              ,{<<"Loopback-From-URI">>, <<"sip_loopback_from_uri">>}
                              ,{<<"Call-Forward-For-UUID">>, <<"loopback_from_uuid">>}
                              ,{<<"Call-Forward-Request-URI">>, <<"sip_loopback_req_uri">>}
                              ,{<<"Media-Encryption">>, <<"rtp_secure_media">>}
                              ,{<<"Media-Encryption-Enforce-Security">>,<<"sdp_secure_savp_only">>}
                              ,{<<"Media-Files-Separator">>, <<"playback_delimiter">>}
                              ,{<<"Media-Group-ID">>, <<"media_group_id">>}
                              ,{<<"Media-Webrtc">>, <<"media_webrtc">>}
                              ,{<<"Application-Other-Leg-UUID">>, <<"Application-Other-Leg-UUID">>}
                              ,{<<"Origination-Call-ID">>, <<"sip_origination_call_uuid">>}
                              ,{<<"Origination-UUID">>, <<"origination_uuid">>}
                              ,{<<"Outbound-Context">>,<<"origination_context">>}
                              ,{<<"Overwrite-Channel-Vars">>, <<"local_var_clobber">>}
                              ,{<<"Park-After-Pickup">>, <<"park_after_bridge">>}
                              ,{<<"Presence-ID">>, <<"presence_id">>}
                              ,{<<"Progress-Timeout">>, <<"progress_timeout">>}
                              ,{<<"RECORD_APPEND">>, <<"RECORD_APPEND">>}
                              ,{<<"RECORD_SOFTWARE">>, <<"RECORD_SOFTWARE">>}
                              ,{<<"RECORD_STEREO">>, <<"RECORD_STEREO">>}
                              ,{<<"RTCP-MUX">>, <<"rtcp_mux">>}
                              ,{<<"RTP-Secure-Audio-Confirmed">>, <<"rtp_secure_media_confirmed_audio">>}
                              ,{<<"RTP-Secure-Media">>, <<"rtp_secure_media">>}
                              ,{<<"RTP-Secure-Media-Confirmed">>, <<"rtp_secure_media_confirmed">>}
                              ,{<<"RTP-Secure-Video-Confirmed">>, <<"rtp_secure_media_confirmed_video">>}
                              ,{<<"Record-Min-Sec">>, <<"record_min_sec">>}
                              ,{<<"Record-Sample-Rate">>, <<"record_sample_rate">>}
                              ,{<<"Request-URI">>, <<"sip_req_uri">>}
                              ,{<<"Signal-Bridge-To">>, <<"signal_bridge_to">>}
                              ,{<<"SIP-Invite-Domain">>, <<"sip_invite_domain">>}
                              ,{<<"SIP-Invite-To-URI">>, <<"sip_invite_to_uri">>}
                              ,{<<"SIP-Invite-Request-URI">>, <<"sip_invite_req_uri">>}
                              ,{<<"SIP-Refer-To">>, <<"sip_refer_to">>}
                              ,{<<"SIP-Referred-By">>, <<"sip_h_Referred-By">>}
                              ,{<<"Secure-RTP">>, <<"rtp_secure_media">>}
                              ,{<<"Secure-ZRTP">>, <<"zrtp_secure_media">>}
                              ,{<<"Simplify-Loopback">>, <<"loopback_bowout_on_execute">>}
                              ,{<<"Proxy-Path">>, <<"sip_route_uri">>}
                              ,{<<"To-URI">>, <<"sip_to_uri">>}
                              ,{<<"To-User">>, <<"sip_to_user">>}
                              ,{<<"To-Realm">>, <<"sip_to_realm">>}
                              ,{<<"From-URI">>, <<"sip_from_uri">>}
                              ,{<<"From-User">>, <<"sip_from_user">>}
                              ,{<<"From-Realm">>, <<"sip_from_realm">>}
                              ,{<<"Transfer-After-Pickup">>, <<"transfer_after_bridge">>}
                              ,{<<"Unanswered-Only">>, <<"intercept_unanswered_only">>}
                              ,{<<"Unbridged-Only">>, <<"intercept_unbridged_only">>}
                              ,{<<"ZRTP-Enrollment">>, <<"zrtp_enrollment">>}
                              ,{<<"ZRTP-Secure-Audio-Confirmed">>, <<"zrtp_secure_media_confirmed_audio">>}
                              ,{<<"ZRTP-Secure-Media">>, <<"zrtp_secure_media">>}
                              ,{<<"ZRTP-Secure-Media-Confirmed">>, <<"zrtp_secure_media_confirmed">>}
                              ,{<<"ZRTP-Secure-Video-Confirmed">>, <<"zrtp_secure_media_confirmed_video">>}
                              ,{<<"conference_member_nospeak_check">>, <<"conference_member_nospeak_check">>}
                              ,{<<"conference_member_nospeak_relational">>, <<"conference_member_nospeak_relational">>}
                              ,{<<"continue_on_cancel">>, <<"continue_on_cancel">>}
                              ,{<<"continue_on_fail">>, <<"continue_on_fail">>}
                              ,{<<"default_language">>, <<"default_language">>}
                              ,{<<"enable_file_write_buffering">>, <<"enable_file_write_buffering">>}
                              ,{<<"execute_on_answer">>, <<"execute_on_answer">>}
                              ,{<<"failure_causes">>, <<"failure_causes">>}
                              ,{<<"fax_enable_t38">>, <<"fax_enable_t38">>}
                              ,{<<"fax_enable_t38_request">>, <<"fax_enable_t38_request">>}
                              ,{<<"hangup_after_pickup">>, <<"hangup_after_bridge">>}
                              ,{<<"intercept_unanswered_only">>, <<"intercept_unanswered_only">>}
                              ,{<<"intercept_unbridged_only">>, <<"intercept_unbridged_only">>}
                              ,{<<"park_after_bridge">>, <<"park_after_bridge">>}
                              ,{<<"park_after_pickup">>, <<"park_after_bridge">>}
                              ,{<<"playback_terminators">>, <<"playback_terminators">>}
                              ,{<<"record_min_sec">>, <<"record_min_sec">>}
                              ,{<<"record_sample_rate">>, <<"record_sample_rate">>}
                              ,{<<"record_waste_resources">>, <<"record_waste_resources">>}
                              ,{<<"recording_follow_attxfer">>, <<"recording_follow_attxfer">>}
                              ,{<<"recording_follow_transfer">>, <<"recording_follow_transfer">>}
                              ,{<<"sip_rh_X-Redirect-Server">>, <<"sip_rh_X-Redirect-Server">>}
                              ,{<<"tts_engine">>, <<"tts_engine">>}
                              ,{<<"tts_voice">>, <<"tts_voice">>}
                              ]).

-define(CALLER_PROFILE_VARS, [{<<"Caller-ID-Name">>, <<"caller_id_name">>}
                             ,{<<"Caller-ID-Number">>, <<"caller_id_number">>}
                             ,{<<"Callee-ID-Name">>, <<"callee_id_name">>}
                             ,{<<"Callee-ID-Number">>, <<"callee_id_number">>}
                             ,{<<"Outbound-Callee-ID-Name">>, <<"origination_callee_id_name">>}
                             ,{<<"Outbound-Callee-ID-Number">>, <<"origination_callee_id_number">>}
                             ,{<<"Outbound-Caller-ID-Name">>, <<"origination_caller_id_name">>}
                             ,{<<"Outbound-Caller-ID-Number">>, <<"origination_caller_id_number">>}
                             ,{<<"Caller-Callee-ID-Name">>, <<"callee_id_name">>}
                             ,{<<"Caller-Callee-ID-Number">>, <<"callee_id_number">>}
                             ,{<<"Caller-Caller-ID-Name">>, <<"caller_id_name">>}
                             ,{<<"Caller-Caller-ID-Number">>, <<"caller_id_number">>}
                             ,{<<"Context">>, <<"context">>}
                             ,{<<"Device-ID">>, <<"device_id">>}
                             ]).

%% [{FreeSWITCH-App-Name, Kazoo-App-Name}] Dialplan-related
%% applications convert from FS-named applications to Kazoo-named
%% Dialplan applications
-define(FS_APPLICATION_NAMES, [{<<"playback">>, <<"play">>}
                              ,{<<"broadcast">>, <<"play">>}
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
                              ,{<<"kz_endless_playback">>, <<"hold">>}
                              ,{<<"soft_hold">>, <<"soft_hold">>}
                              ,{<<"uuid_record">>, <<"record_call">>}
                              ,{<<"record_session">>, <<"record_call">>}
                              ,{<<"record">>, <<"record_call">>}
                              ,{<<"presence">>, <<"presence">>}
                              ,{<<"privacy">>, <<"privacy">>}
                              ,{<<"conference">>, <<"page">>}
                              ,{<<"playback">>, <<"play_macro">>}
                              ,{<<"intercept">>, <<"call_pickup">>}
                              ]).

-define(FAX_EVENTS, ['spandsp::txfaxresult'
                    ,'spandsp::rxfaxresult'
                    ,'spandsp::txfaxpageresult'
                    ,'spandsp::rxfaxpageresult'
                    ,'spandsp::txfaxnegociateresult'
                    ,'spandsp::rxfaxnegociateresult'
                    ]).

-define(FS_SOFIA_TRANSFER_EVENTS, ['sofia::transferor'
                                  ,'sofia::transferee'
                                  ,'sofia::replaced'
                                  ,'sofia::intercepted'
                                  ]).
-define(IS_SOFIA_TRANSFER(N), lists:member(kz_term:to_atom(N, 'true'), ?FS_SOFIA_TRANSFER_EVENTS)).

-define(FS_EVENTS, [{'channel', ['CHANNEL_CREATE', 'CHANNEL_ANSWER', 'CHANNEL_DESTROY']}
                   ,{'bridge', ['CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE']}
                   ,{'media', ['DETECTED_TONE', 'DTMF','CHANNEL_PROGRESS','CHANNEL_PROGRESS_MEDIA']}
                   ,{'record', ['RECORD_START', 'RECORD_STOP']}
                   ,{'callflow', ['ROUTE_WINNER', 'CHANNEL_EXECUTE_COMPLETE', 'CHANNEL_METAFLOW']}
                   ,{'presence', ['PRESENCE_IN']}
                   ,{'channel_full_update', ['CHANNEL_DATA','CHANNEL_SYNC','CALL_UPDATE']}
                   ,{'channel_update', ['CHANNEL_HOLD','CHANNEL_UNHOLD']}
                   ,{'conference', ['conference::maintenance']}
                   ,{'fax', ?FAX_EVENTS}
                   ,{'kazoo', ['kazoo::noop', 'kazoo::masquerade']}
                   ,{'transfer', ?FS_SOFIA_TRANSFER_EVENTS}
                   ,{'loopback', ['loopback::bowout', 'loopback::direct']}
                   ]).

-define(FS_FETCH_SECTIONS, ['configuration'
                           ,'directory'
                           ,'dialplan'
                           ,'channels'
                           ,'languages'
                           ]).

-define(FETCH_HANDLERS_MODS, ['ecallmgr_fs_fetch_configuration_acl'
                             ,'ecallmgr_fs_fetch_configuration_conference'
                             ,'ecallmgr_fs_fetch_configuration_kazoo'
                             ,'ecallmgr_fs_fetch_configuration_sofia'
                             ,'ecallmgr_fs_fetch_dialplan'
                             ,'ecallmgr_fs_fetch_channels'
                             ,'ecallmgr_fs_fetch_directory'
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


-define(REGISTER_SUCCESS_REG, 'register_success').
-define(REGISTER_SUCCESS_MSG(Node, Props), {Node, Props}).

-define(LOOPBACK_BOWOUT_REG(CallId), {'loopback_bowout', CallId}).
-define(LOOPBACK_BOWOUT_MSG(Node, Props), {Node, Props}).

-define(FS_EVENT_REG_MSG(Node, EvtName), {'event', Node, EvtName}).
-define(FS_CALL_EVENT_REG_MSG(Node, EvtName), {'call_event', Node, EvtName}).
-define(FS_CALL_EVENT_MSG(Node, EvtName, CallId), {'call_event', Node, EvtName, CallId}).
-define(FS_CALL_EVENTS_PROCESS_REG(Node, CallId), {'n', 'l', {'call_events_process', Node, CallId}}).

-define(FS_CONFERENCE_ALL_REG_MSG(Node), {'conference', Node}).
-define(FS_CONFERENCE_ALL_EVENT_REG_MSG(Node, EvtName), {'conference', Node, 'all', EvtName}).
-define(FS_CONFERENCE_EVENT_ALL_REG_MSG(Node, ConferenceId), {'conference', Node, ConferenceId, 'all'}).
-define(FS_CONFERENCE_EVENT_REG_MSG(Node, ConferenceId, EvtName), {'conference', Node, ConferenceId, EvtName}).
-define(FS_CONFERENCE_EVENT_MSG(ConferenceId, EvtName, JObj), {'conference', ConferenceId, EvtName, JObj}).

-define(FS_ROUTE_MSG(Node, Section, Context), {'route', Node, Section, Context}).

-define(FS_OPTION_MSG(Node), {'option', Node}).

-define(FS_NODE_GRACE_PERIOD_REG, 'fs_node_grace_period').
-define(FS_NODE_GRACE_PERIOD_MSG(Node), {'fs_node_grace_period', Node}).
-define(FS_NODEDOWN_REG, 'fs_node_down').
-define(FS_NODEDOWN_MSG(Node, Options), {'fs_node_down', Node, Options}).
-define(FS_NODEDOWN(Node), {'fs_node_down', Node}).
-define(FS_NODEUP_REG, 'fs_node_up').
-define(FS_NODEUP_MSG(Node, Options), {'fs_node_up', Node, Options}).

-define(ROUTE_WINNER_EVENT, <<"ROUTE_WINNER">>).

-define(FS_CARRIER_ACL_LIST, <<"trusted">>).
-define(FS_SBC_ACL_LIST, <<"authoritative">>).

-define(SEPARATOR_ENTERPRISE, <<":_:">>).
-define(SEPARATOR_SIMULTANEOUS, <<",">>).
-define(SEPARATOR_SINGLE, <<"|">>).

-define(CHANNEL_VARS_EXT, "Execute-Extension-Original-").

-define(CONFERENCE_VARS, [<<"variable_conference_moderator">>
                         ,<<"Floor">>
                         ,<<"Video">>
                         ,<<"See">>
                         ,<<"Speak">>
                         ,<<"Hear">>
                         ,<<"Talking">>
                         ,<<"Mute-Detect">>
                         ,<<"Energy-Level">>
                         ,<<"Member-ID">>
                         ,<<"Member-Type">>
                         ,<<"Member-Ghost">>
                         ]).

-define(CONFERENCE_VAR_MAP, [{<<"variable_conference_moderator">>, {<<"Is-Moderator">>, fun kz_term:to_boolean/1}}
                            ,{<<"Floor">>, fun kz_term:to_boolean/1}
                            ,{<<"Video">>, fun kz_term:to_boolean/1}
                            ,{<<"See">>, fun kz_term:to_boolean/1}
                            ,{<<"Speak">>, fun kz_term:to_boolean/1}
                            ,{<<"Hear">>, fun kz_term:to_boolean/1}
                            ,{<<"Talking">>, fun kz_term:to_boolean/1}
                            ,{<<"Mute-Detect">>, fun kz_term:to_boolean/1}
                            ,{<<"Energy-Level">>, fun kz_term:to_integer/1}
                            ,{<<"Current-Energy">>, fun kz_term:to_integer/1}
                            ,{<<"Member-ID">>, fun kz_term:to_integer/1}
                            ,{<<"Member-Ghost">>, fun kz_term:to_boolean/1}
                            ]).

-define(EXTRA_VARS, [<<"Routing-Queue">>
                    ,<<"Request-From-PID">>
                    ,<<"Reply-To-PID">>
                    ,<<"Controller-Queue">>
                    ,<<"Controller-PID">>
                    ,<<"Fetch-UUID">>
                    ,<<"Fetch-Winning-PID">>
                    ,<<"Event-Category">>
                    ,<<"Call-Control-Queue">>
                    ,<<"Call-Control-PID">>
                    ,<<"Call-Control-Node">>
                    ,<<"Application-UUID">>
                    ,<<"app_uuid">>
                    ,<<"variable_app_uuid">>
                    ,<<"caller-unique-id">>
                    ]).

-define(FS_EVENT_FILTERS,
        lists:usort(
          ?FS_GENERATED_EVENT_FILTERS
          ++ ?CONFERENCE_VARS
          ++ ?FS_MOD_KAZOO_EVENT_FILTERS
          ++ ?FS_PRESERVED_EVENT_FILTERS
          ++ ?EXTRA_VARS
         )
       ).

-define(NODE_MODULES_KEY(R), [<<"configuration">>
                             ,R
                             ,<<"modules">>
                             ]).

-define(NODE_MODULES,
        [<<"node">>
        ,<<"monitor">>
        ,<<"event_stream_sup">>
        ,<<"fetch_sup">>
        ,<<"notify">>
        ,<<"resource">>
        ]).

-define(EVENTSTREAM_MODS, ['ecallmgr_fs_channel_stream'
                          ,'ecallmgr_fs_conference_stream'
                          ,'ecallmgr_fs_event_stream_registered'
                          ,'ecallmgr_call_event_publisher'
                          ,'ecallmgr_conference_event_publisher'
                          ,'ecallmgr_presence_event_publisher'
                          ,'ecallmgr_fs_recordings'
                          ]).

-define(HTTP_GET_PREFIX, "http_cache://").


-type dialplan_callback() :: fun((dialplan_context()) -> 'ok' | {'ok', dialplan_context()}).
-type dialplan_exit_fun() :: fun((dialplan_context()) -> 'ok').
-type dialplan_init_fun() :: fun((dialplan_context()) -> 'ok').

-type dialplan_timers() :: #{atom() => pos_integer()}. %% #{Name => TimestampUs
-type dialplan_reply() ::  #{payload => kzd_fetch:data() | kz_json:object()}.
-type dialplan_winner() :: #{payload => kzd_fetch:data()}.
-type dialplan_xml_fun() :: fun((kz_term:ne_binary(), kz_json:objects(), kz_json:object(), dialplan_context()) -> {'ok', iolist()}).

-type dialplan_context() :: #{amqp_worker => pid() %% AMQP Worker
                             ,authz_timeout => timeout()
                             ,authz_worker => kz_term:pid_ref()
                             ,call_id => kz_term:ne_binary()
                             ,callback => dialplan_callback()
                             ,channel => pid() %% AMQP Channel
                             ,controller_q => kz_term:ne_binary() %% AMQP Queue of controller
                             ,control_p => pid() %% Ecallmgr control PID
                             ,control_q => kz_term:ne_binary() %% AMQP Control Queue
                             ,core_uuid => kz_term:ne_binary() %% FS UUID
                             ,exit_fun => dialplan_exit_fun()
                             ,fetch_id => kz_term:ne_binary()
                             ,initial_ccvs => kz_json:object()
                             ,init_fun => dialplan_init_fun()
                             ,node => atom() %% FS Node
                             ,options => kz_term:proplist()
                             ,payload => kzd_fetch:data()
                             ,start_result => {'ok', pid()} | {'error', any()}
                             ,timer => dialplan_timers()
                             ,timeout => non_neg_integer()
                             ,reply => dialplan_reply()
                             ,request => kapi_route:req()
                             ,route_resp_xml_fun => dialplan_xml_fun()
                             ,winner => dialplan_winner()
                             ,blocked => boolean()
                             }.

-define(ECALLMGR_HRL, 'true').
-endif.
