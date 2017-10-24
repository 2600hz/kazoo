-ifndef(ECALLMGR_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-include("fs_event_filters.hrl").
-include("fs_mod_kazoo_event_filters.hrl").

-define(ECALLMGR_UTIL_CACHE, 'ecallmgr_util_cache').
-define(ECALLMGR_AUTH_CACHE, 'ecallmgr_auth_cache').
-define(ECALLMGR_CALL_CACHE, 'ecallmgr_call_cache').
-define(ECALLMGR_INTERACTION_CACHE, 'ecallmgr_interaction_cache').

-define(CHANNELS_TBL, 'ecallmgr_channels').

-define(DEFAULT_FETCH_TIMEOUT, 2600).

-define(FS_NODES, ecallmgr_config:get_ne_binaries(<<"fs_nodes">>, [])).
-define(FS_NODES(Node), ecallmgr_config:get_ne_binaries(<<"fs_nodes">>, [], Node)).

-define(ECALLMGR_PLAYBACK_MEDIA_KEY(M), {'playback_media', M}).

-define(DEFAULT_FREESWITCH_CONTEXT, ecallmgr_config:get_ne_binary(<<"freeswitch_context">>, <<"context_2">>)).

-define(SIP_INTERFACE, "sipinterface_1").
-define(DEFAULT_FS_PROFILE, "sipinterface_1").
-define(DEFAULT_FS_TECHNOLOGY, "sofia").
-define(DEFAULT_FS_DIALPLAN, "XML").

-define(LOCAL_MEDIA_PATH, "/tmp/").

-define(DEFAULT_SAMPLE_RATE, ecallmgr_config:get_integer(<<"record_sample_rate">>, 8000)).
-define(DEFAULT_STEREO_SAMPLE_RATE, ecallmgr_config:get_integer(<<"record_stereo_sample_rate">>, 16000)).

-type fs_app() :: {ne_binary(), binary() | 'noop'} |
                  {ne_binary(), ne_binary(), atom()}.
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

-record(sip_subscription, {key :: api_binary() | '_'
                          ,to :: api_binary() | '$1' | '_'
                          ,from :: api_binary() | '$2' | '_'
                          ,node :: atom() | '$1' | '_'
                          ,expires = 300 :: pos_integer() | '$1' | '_'
                          ,timestamp = kz_time:now_s() :: pos_integer() | '$2' | '_'
                          }).

-record(channel, {uuid :: api_binary() | '$1' | '$2' | '_'
                 ,destination :: api_binary() | '_'
                 ,direction :: api_binary() | '$1' | '_'
                 ,account_id :: api_binary() | '$1' | '$2' | '_'
                 ,account_billing :: api_binary() | '$7' | '_'
                 ,authorizing_id :: api_binary() | '$1' | '$3' | '_'
                 ,authorizing_type :: api_binary() | '_'
                 ,is_authorized :: api_boolean() | '_'
                 ,owner_id :: api_binary() | '$1' | '_'
                 ,resource_id :: api_binary() | '$4' | '_'
                 ,presence_id :: api_binary() | '$2' | '_'
                 ,fetch_id :: api_binary() | '$5' | '_'
                 ,bridge_id :: api_binary() | '$5' | '_'
                 ,reseller_id :: api_binary() | '$1' | '$2' | '_'
                 ,reseller_billing :: api_binary() | '_'
                 ,realm :: api_binary() | '_' | '$2'
                 ,username :: api_binary() | '_' | '$1'
                 ,import_moh = 'false' :: boolean() | '_'
                 ,answered = 'true' :: boolean() | '_'
                 ,other_leg :: api_binary() | '$2' | '_'
                 ,node :: atom() | '$1' | '$2' | '$3' | '_'
                 ,former_node :: atom() | '$2' | '_'
                 ,timestamp = kz_time:now_s() :: gregorian_seconds() | '$3' | '_'
                 ,profile :: api_binary() | '_'
                 ,context :: api_binary() | '_'
                 ,dialplan :: api_binary() | '_'
                 ,precedence = 5 :: pos_integer() | '$2' | '_'
                 ,handling_locally = 'false' :: boolean() | '_' %% is this ecallmgr handling the call control?
                 ,to_tag :: api_binary() | '_'
                 ,from_tag :: api_binary() | '_'
                 ,interaction_id :: api_binary() | '$5' | '_'
                 ,callee_number :: api_binary() | '$5' | '_'
                 ,callee_name :: api_binary() | '$5' | '_'
                 ,is_loopback = 'false' :: boolean() | '_'
                 ,loopback_leg_name :: api_binary() | '_'
                 ,loopback_other_leg :: api_binary() | '_'
                 ,callflow_id :: api_binary() | '_'
                 ,is_onhold = 'false' :: boolean() | '_'
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].
-type channel_updates() :: [{pos_integer(), any()}].

-record(conference, {name :: api_binary() | '$1' | '_'
                    ,uuid :: api_binary() | '$1' | '_'
                    ,node :: atom() | '$1' | '$2' | '_'
                    ,participants = 0 :: non_neg_integer() | '_'
                    ,profile_name = <<"default">> :: ne_binary() | '_'
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
                    ,switch_hostname :: api_binary() | '_'
                    ,switch_url :: api_binary() | '_'
                    ,switch_external_ip :: api_binary() | '_'
                    ,account_id :: api_binary() | '_'
                    ,handling_locally = 'false' :: boolean() | '_' %% was this ecallmgr handling the call control?
                    ,origin_node :: atom() | '_'
                    ,control_node :: atom() | '_'
                    }).

-type conference() :: #conference{}.
-type conferences() :: [conference()].

-record(participant, {uuid :: api_ne_binary() | '$1' | '_'
                     ,node :: atom() | '$2' | '_'
                     ,conference_uuid :: api_ne_binary() | '$1'| '_'
                     ,conference_name :: api_ne_binary() | '$1'| '_'
                     ,join_time = kz_time:now_s() :: gregorian_seconds() | '_'
                     ,caller_id_name :: api_ne_binary() | '_'
                     ,caller_id_number :: api_ne_binary() | '_'
                     ,conference_channel_vars :: api_object() | '_'
                     ,custom_channel_vars :: api_object() | '_'
                     }).
-type participant() :: #participant{}.
-type participants() :: [participant()].

-define(DEFAULT_REALM, ecallmgr_config:get_ne_binary(<<"default_realm">>, <<"nodomain.com">>)).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, ecallmgr_config:get_integer(<<"max_timeout_for_node_restart">>, 10 * ?MILLISECONDS_IN_SECOND)).
-define(MAX_NODE_RESTART_FAILURES, 3).

-define(EXPIRES_DEVIATION_TIME,
        ecallmgr_config:get_integer(<<"expires_deviation_time">>, 180)).

%% list of dialplan Application-Names that can execute after a call has hung up
-define(POST_HANGUP_COMMANDS, [<<"store">>, <<"set">>, <<"presence">>
                              ,<<"record">>, <<"store_fax">>, <<"receive_fax">>
                              ]).

-define(SANITY_CHECK_PERIOD, 300 * ?MILLISECONDS_IN_SECOND).

-define(APP, 'ecallmgr').
-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"4.0.0">>).

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

-define(CCV(Key), <<?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(GET_CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(SET_CCV(Key, Value), <<?CHANNEL_VAR_PREFIX, Key/binary, "=", Value/binary>>).
-define(GET_CCV_HEADER(Key), <<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(GET_CUSTOM_HEADER(Key), <<"variable_sip_h_X-", Key/binary>>).
-define(CUSTOM_HEADER(Key), <<"sip_h_X-", Key/binary>>).
-define(GET_VAR(Key), <<"variable_", Key/binary>>).

-define(CREDS_KEY(Realm, Username), {'authn', Username, Realm}).

-define(DP_EVENT_VARS, [{<<"Execute-On-Answer">>, <<"execute_on_answer">>}]).
-define(BRIDGE_CHANNEL_VAR_SEPARATOR, "!").

%% Call and Channel Vars that have a special prefix instead of the
%% standard CHANNEL_VAR_PREFIX prefix [{AMQP-Header, FS-var-name}] so
%% FS-var-name of "foo_var" would become "foo_var=foo_val" in the
%% channel/call string
-define(SPECIAL_CHANNEL_VARS, [{<<"Auto-Answer">>, <<"sip_auto_answer">>}
                              ,{<<"Auto-Answer-Suppress-Notify">>, <<"sip_auto_answer_suppress_notify">>}
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
                              ,{<<"Caller-Callee-ID-Name">>, <<"caller_callee_id_name">>}
                              ,{<<"Caller-Callee-ID-Number">>, <<"caller_callee_id_number">>}
                              ,{<<"Caller-Caller-ID-Name">>, <<"caller_caller_id_name">>}
                              ,{<<"Caller-Caller-ID-Number">>, <<"caller_caller_id_number">>}

                              ,{<<"Progress-Timeout">>, <<"progress_timeout">>}
                              ,{<<"Ignore-Early-Media">>, <<"ignore_early_media">>}
                              ,{<<"Continue-On-Fail">>, <<"continue_on_fail">>}
                              ,{<<"Fail-On-Single-Reject">>, <<"fail_on_single_reject">>}
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
                              ,{<<"Bypass-Media">>, <<"bypass_media_after_bridge">>}
                              ,{<<"Bridge-Generate-Comfort-Noise">>,<<"bridge_generate_comfort_noise">>}
                              ,{<<"Origination-UUID">>, <<"origination_uuid">>}
                              ,{<<"Ignore-Display-Updates">>, <<"ignore_display_updates">>}
                              ,{<<"Eavesdrop-Group-ID">>, <<"eavesdrop_group">>}
                              ,{<<"Media-Webrtc">>, <<"media_webrtc">>}

                              ,{<<"Loopback-Bowout">>, <<"loopback_bowout">>}
                              ,{<<"Simplify-Loopback">>, <<"loopback_bowout_on_execute">>}

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
                              ,{<<"From-User">>, <<"sip_from_user">>}
                              ,{<<"From-URI">>, <<"sip_from_uri">>}
                              ,{<<"To-URI">>, <<"sip_to_uri">>}
                              ,{<<"Request-URI">>, <<"sip_req_uri">>}
                              ,{<<"Loopback-Request-URI">>, <<"sip_loopback_req_uri">>}
                              ,{<<"Loopback-Export">>, <<"loopback_export">>}
                              ,{<<"Hold-Media">>, <<"hold_music">>}
                              ,{<<"Diversions">>, <<"sip_h_Diversion">>}
                              ,{<<"Bridge-Execute-On-Answer">>, <<"execute_on_answer">>}
                              ,{<<"Media-Files-Separator">>, <<"playback_delimiter">>}
                              ,{<<"Conference-Entry-Sound">>, <<"conference_enter_sound">>}
                              ,{<<"Conference-Exit-Sound">>, <<"conference_exit_sound">>}
                              ,{<<"SIP-Refer-To">>, <<"sip_refer_to">>}
                              ,{<<"SIP-Referred-By">>, <<"sip_h_Referred-By">>}
                              ,{<<"Origination-Call-ID">>, <<"sip_origination_call_id">>}
                              ,{<<"RTCP-MUX">>, <<"rtcp_mux">>}
                              ]).

-define(CALLER_PROFILE_VARS, [{<<"Caller-ID-Name">>, <<"caller_id_name">>}
                             ,{<<"Caller-ID-Number">>, <<"caller_id_number">>}
                             ,{<<"Callee-ID-Name">>, <<"callee_id_name">>}
                             ,{<<"Callee-ID-Number">>, <<"callee_id_number">>}
                             ,{<<"Caller-Callee-ID-Name">>, <<"callee_id_name">>}
                             ,{<<"Caller-Callee-ID-Number">>, <<"callee_id_number">>}
                             ,{<<"Caller-Caller-ID-Name">>, <<"caller_id_name">>}
                             ,{<<"Caller-Caller-ID-Number">>, <<"caller_id_number">>}
                             ]).

%% [{FreeSWITCH-App-Name, Kazoo-App-Name}] Dialplan-related
%% applications convert from FS-named applications to Kazoo-named
%% Dialplan applications
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

-define(FS_EVENTS, [{channel, ['CHANNEL_CREATE', 'CHANNEL_ANSWER', 'CHANNEL_DESTROY']}
                   ,{bridge, ['CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE']}
                   ,{media, ['DETECTED_TONE', 'DTMF','CHANNEL_PROGRESS_MEDIA']}
                   ,{record, ['RECORD_START', 'RECORD_STOP']}
                   ,{callflow, ['ROUTE_WINNER', 'CHANNEL_EXECUTE_COMPLETE']}
                   ,{presence, ['PRESENCE_IN']}
                   ,{channel_full_update, ['CHANNEL_DATA','CALL_UPDATE', 'CALL_SECURE']}
                   ,{channel_update, ['CHANNEL_HOLD','CHANNEL_UNHOLD']}
                   ,{conference, ['conference::maintenance']}
                   ,{fax, ?FAX_EVENTS}
                   ,{kazoo, ['kazoo::noop', 'kazoo::masquerade']}
                   ,{transfer, ?FS_SOFIA_TRANSFER_EVENTS}
                   ,{loopback, ['loopback::bowout']}
                   ]).

-define(FS_FETCH_SECTIONS, ['configuration'
                           ,'directory'
                           ,'dialplan'
                           ,'channels'
                           ,'languages'
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
-define(FS_CALL_EVENTS_PROCESS_REG(Node, CallId)
       ,{'n', 'l', {'call_events_process', Node, CallId}}
       ).

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

-define(FS_EVENT_FILTERS,
        lists:usort(
          ?FS_GENERATED_EVENT_FILTERS
          ++ ?CONFERENCE_VARS
          ++ ?FS_MOD_KAZOO_EVENT_FILTERS
          ++ ?FS_PRESERVED_EVENT_FILTERS
         )
       ).

-define(NODE_MODULES,
        [<<"node">>
        ,<<"monitor">>
        ,<<"event_stream_sup">>
        ,<<"fetch_sup">>
        ,<<"call_control_sup">>
        ,<<"notify">>
        ,<<"resource">>
        ]).

-define(CALL_CTL_NAME(C), kz_term:to_atom(<<"callctl_", C/binary>>, 'true')).
-define(CALLCTL_SUPERVISOR_NAME(Node), kz_term:to_atom(<<"call_ctl_sup_", (kz_term:to_binary(Node))/binary>>, true)).

-define(ECALLMGR_HRL, 'true').
-endif.
