-ifndef(FS_EVENT_FILTERS_HRL).
-include("fs_manual_event_filters.hrl").
-define(FS_EVENT_FITLERS
       ,[<<"Acquired-UUID">>
        ,<<"Action">>
        ,<<"Answer-State">>
        ,<<"Application">>
        ,<<"Application-Data">>
        ,<<"Application-Logical-Direction">>
        ,<<"Application-Response">>
        ,<<"Auth-Realm">>
        ,<<"Bridge-A-Unique-ID">>
        ,<<"Bridge-B-Unique-ID">>
        ,<<"Call-Direction">>
        ,<<"Call-ID">>
        ,<<"Call-Setup">>
        ,<<"Caller-Callee-ID-Name">>
        ,<<"Caller-Callee-ID-Number">>
        ,<<"Caller-Caller-ID-Name">>
        ,<<"Caller-Caller-ID-Number">>
        ,<<"Caller-Channel-Created-Time">>
        ,<<"Caller-Context">>
        ,<<"Caller-Destination-Number">>
        ,<<"Caller-Dialplan">>
        ,<<"Caller-Logical-Direction">>
        ,<<"Caller-Unique-ID">>
        ,<<"Channel-Call-State">>
        ,<<"Channel-Call-UUID">>
        ,<<"Channel-Name">>
        ,<<"Channel-Presence-ID">>
        ,<<"Channel-State">>
        ,<<"Conf-Name">>
        ,<<"Conference-Name">>
        ,<<"Conference-Profile-Name">>
        ,<<"Conference-Unique-ID">>
        ,<<"Controls">>
        ,<<"Core-UUID">>
        ,<<"DTMF-Digit">>
        ,<<"DTMF-Duration">>
        ,<<"Detected-Fax-Tone">>
        ,<<"Detected-Tone">>
        ,<<"Event-Date-Timestamp">>
        ,<<"Event-Name">>
        ,<<"Event-Subclass">>
        ,<<"FreeSWITCH-Hostname">>
        ,<<"Hangup-Cause">>
        ,<<"Hunt-Callee-ID-Number">>
        ,<<"Hunt-Context">>
        ,<<"Hunt-Destination-Number">>
        ,<<"Join-Time">>
        ,<<"Other-Leg-Call-ID">>
        ,<<"Other-Leg-Callee-ID-Number">>
        ,<<"Other-Leg-Caller-ID-Name">>
        ,<<"Other-Leg-Caller-ID-Number">>
        ,<<"Other-Leg-Channel-Name">>
        ,<<"Other-Leg-Destination-Number">>
        ,<<"Other-Leg-Direction">>
        ,<<"Other-Leg-Unique-ID">>
        ,<<"Publish-Channel-State">>
        ,<<"Record-File-Path">>
        ,<<"Resigning-UUID">>
        ,<<"Route-Resp-Fun">>
        ,<<"Route-Resp-Xml-Fun">>
        ,<<"Switch-Nodename">>
        ,<<"Switch-URI">>
        ,<<"Switch-URL">>
        ,<<"Unique-ID">>
        ,<<"X-AUTH-IP">>
        ,<<"X-AUTH-PORT">>
        ,<<"action">>
        ,<<"att_xfer_replaced_by">>
        ,<<"context">>
        ,<<"domain">>
        ,<<"expires">>
        ,<<"from_user">>
        ,<<"intercepted_by">>
        ,<<"ip">>
        ,<<"kazoo_application_name">>
        ,<<"kazoo_application_response">>
        ,<<"kazoo_event_name">>
        ,<<"metadata">>
        ,<<"old_node_channel_uuid">>
        ,<<"port">>
        ,<<"profile_name">>
        ,<<"sip_auth_method">>
        ,<<"sip_auth_nonce">>
        ,<<"sip_auth_realm">>
        ,<<"sip_auth_response">>
        ,<<"sip_auth_uri">>
        ,<<"sip_call_id">>
        ,<<"sip_loopback_req_uri">>
        ,<<"sip_req_host">>
        ,<<"sip_req_uri">>
        ,<<"sip_request_host">>
        ,<<"sip_to_host">>
        ,<<"sip_to_user">>
        ,<<"sip_user_agent">>
        ,<<"technology">>
        ,<<"to_user">>
        ,<<"user">>
        ,<<"variable_billmsec">>
        ,<<"variable_billsec">>
        ,<<"variable_bridge_hangup_cause">>
        ,<<"variable_bridge_uuid">>
        ,<<"variable_channel_is_moving">>
        ,<<"variable_collected_digits">>
        ,<<"variable_current_application">>
        ,<<"variable_current_application_data">>
        ,<<"variable_domain_name">>
        ,<<"variable_duration">>
        ,<<"variable_effective_callee_id_name">>
        ,<<"variable_effective_callee_id_number">>
        ,<<"variable_effective_caller_id_name">>
        ,<<"variable_effective_caller_id_number">>
        ,<<"variable_endpoint_disposition">>
        ,<<"variable_fax_bad_rows">>
        ,<<"variable_fax_doc_database">>
        ,<<"variable_fax_doc_id">>
        ,<<"variable_fax_document_total_pages">>
        ,<<"variable_fax_document_transferred_pages">>
        ,<<"variable_fax_ecm_used">>
        ,<<"variable_fax_encoding">>
        ,<<"variable_fax_encoding_name">>
        ,<<"variable_fax_file_image_pixel_size">>
        ,<<"variable_fax_file_image_resolution">>
        ,<<"variable_fax_header">>
        ,<<"variable_fax_ident">>
        ,<<"variable_fax_image_pixel_size">>
        ,<<"variable_fax_image_resolution">>
        ,<<"variable_fax_image_size">>
        ,<<"variable_fax_local_station_id">>
        ,<<"variable_fax_longest_bad_row_run">>
        ,<<"variable_fax_remote_country">>
        ,<<"variable_fax_remote_model">>
        ,<<"variable_fax_remote_station_id">>
        ,<<"variable_fax_remote_vendor">>
        ,<<"variable_fax_result_text">>
        ,<<"variable_fax_result_code">>
        ,<<"variable_fax_success">>
        ,<<"variable_fax_timezone">>
        ,<<"variable_fax_transfer_rate">>
        ,<<"variable_hangup_cause">>
        ,<<"variable_has_t38">>
        ,<<"variable_hold_music">>
        ,<<"variable_is_loopback">>
        ,<<"variable_last_bridge_proto_specific_hangup_cause">>
        ,<<"variable_loopback_bowout">>
        ,<<"variable_loopback_bowout_on_execute">>
        ,<<"variable_loopback_leg">>
        ,<<"variable_media_group_id">>
        ,<<"variable_originate_disposition">>
        ,<<"variable_origination_uuid">>
        ,<<"variable_other_loopback_leg_uuid">>
        ,<<"variable_playback_terminator_used">>
        ,<<"variable_presence_id">>
        ,<<"variable_progresssec">>
        ,<<"variable_proto_specific_hangup_cause">>
        ,<<"variable_record_ms">>
        ,<<"variable_record_silence_hits">>
        ,<<"variable_recovered">>
        ,<<"variable_refer_uuid">>
        ,<<"variable_rtp_local_sdp_str">>
        ,<<"variable_silence_hits_exhausted">>
        ,<<"variable_sip_auth_realm">>
        ,<<"variable_sip_call_id">>
        ,<<"variable_sip_contact_user">>
        ,<<"variable_sip_from_tag">>
        ,<<"variable_sip_from_uri">>
        ,<<"variable_sip_from_user">>
        ,<<"variable_sip_invite_domain">>
        ,<<"variable_sip_loopback_req_uri">>
        ,<<"variable_sip_received_ip">>
        ,<<"variable_sip_received_port">>
        ,<<"variable_sip_refer_to">>
        ,<<"variable_sip_req_host">>
        ,<<"variable_sip_req_uri">>
        ,<<"variable_sip_to_host">>
        ,<<"variable_sip_to_tag">>
        ,<<"variable_sip_to_uri">>
        ,<<"variable_sip_to_user">>
        ,<<"variable_sip_user_agent">>
        ,<<"variable_sofia_profile_name">>
        ,<<"variable_switch_r_sdp">>
        ,<<"variable_transfer_history">>
        ,<<"variable_transfer_to">>
        ,<<"variable_user_name">>
        ,<<"variable_uuid">>
             | ?FS_MANUAL_HEADERS
        ]).
-define(FS_EVENT_FILTERS_HRL, 'true').
-endif.
