%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz
%%% @doc
%%% Receive call events from freeSWITCH, publish to the call's event queue
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_event_stream).

-include("ecallmgr.hrl").

-define(DEFAULT_DEBUG_CHANNEL, 'false' ).
-define(DEBUG_CHANNEL, ecallmgr_config:get_boolean(<<"debug_channel">>, ?DEFAULT_DEBUG_CHANNEL) ).

-export([to_json/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec to_json(kz_proplist()) -> kz_json:object().
to_json(Props) ->
    kz_json:from_list(create_event(Props)).


-spec create_event(kz_proplist()) -> kz_proplist().
create_event(Props) ->
    create_event(get_event_name(Props), Props).

-spec create_event(ne_binary(), kz_proplist()) -> kz_proplist().
create_event(EventName, Props) ->
    create_event(EventName, get_application_name(Props), Props).

-spec create_event(ne_binary(), api_binary(), kz_proplist()) -> kz_proplist().
create_event(EventName, ApplicationName, Props) ->
    props:filter_undefined(
      [{<<"Event-Name">>, EventName}
       |specific_call_event_props(EventName, ApplicationName, Props)
       ++ generic_call_event_props(Props)
       ++ specific_call_channel_vars_props(EventName, Props)
      ]).

-spec specific_call_channel_vars_props(ne_binary(), kz_proplist()) ->
                                              kz_proplist().
specific_call_channel_vars_props(_EventName, Props) ->
    [{<<"Custom-Channel-Vars">>, kz_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
    ,{<<"Custom-Application-Vars">>, kz_json:from_list(ecallmgr_util:custom_application_vars(Props))}    
    ].

-spec generic_call_event_props(kz_proplist()) -> kz_proplist().
generic_call_event_props(Props) ->
    Timestamp = kz_time:now_us(),
    FSTimestamp = props:get_integer_value(<<"Event-Date-Timestamp">>, Props, Timestamp),
    NormalizedFSTimestamp = kz_time:unix_seconds_to_gregorian_seconds(FSTimestamp div 1000000),

    [{<<"Timestamp">>, NormalizedFSTimestamp}
    ,{<<"Msg-ID">>, kz_term:to_binary(FSTimestamp)}
    ,{<<"Origination-Call-ID">>, kz_evt_freeswitch:origination_call_id(Props)}
    ,{<<"Call-ID">>, get_call_id(Props)}
    ,{<<"Transfer-History">>, get_transfer_history(Props)}
    ,{<<"Hangup-Cause">>, get_hangup_cause(Props)}
    ,{<<"Hangup-Code">>, get_hangup_code(Props)}
    ,{<<"Disposition">>, get_disposition(Props)}
    ,{<<"Raw-Application-Name">>, get_raw_application_name(Props)}
    ,{<<"Channel-Moving">>, get_channel_moving(Props)}
    ,{<<"Call-Direction">>, kz_evt_freeswitch:call_direction(Props)}
    ,{<<"Caller-ID-Number">>, kz_evt_freeswitch:caller_id_number(Props)}
    ,{<<"Caller-ID-Name">>, kz_evt_freeswitch:caller_id_name(Props)}
    ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Props)}
    ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)}
    ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)}
    ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
    ,{<<"Other-Leg-Call-ID">>, get_other_leg(Props)}

    ,{<<"Presence-ID">>, props:get_value(<<"variable_presence_id">>, Props)}
    ,{<<"Raw-Application-Data">>, props:get_value(<<"Application-Data">>, Props)}
    ,{<<"Media-Server">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
    ,{<<"Replaced-By">>, props:get_first_defined([<<"att_xfer_replaced_by">>, ?ACQUIRED_UUID], Props)}
    ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
    ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
    ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
    ,{<<"Switch-URL">>, props:get_value(<<"Switch-URL">>, Props)}
    ,{<<"Switch-URI">>, props:get_value(<<"Switch-URI">>, Props)}
    ,{<<"Switch-Nodename">>, props:get_value(<<"Switch-Nodename">>, Props)}
    ,{<<"Channel-State">>, get_channel_state(Props)}
    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
    ,{<<"Channel-Name">>, props:get_value(<<"Channel-Name">>, Props)}
    ,{<<"Channel-Is-Loopback">>, get_is_loopback(props:get_value(<<"variable_is_loopback">>, Props))}
    ,{<<"Channel-Loopback-Leg">>, props:get_value(<<"variable_loopback_leg">>, Props)}
    ,{<<"Channel-Loopback-Other-Leg-ID">>, props:get_value(<<"variable_other_loopback_leg_uuid">>, Props)}
    ,{<<"Channel-Loopback-Bowout">>, props:get_is_true(<<"variable_loopback_bowout">>, Props)}
    ,{<<"Channel-Loopback-Bowout-Execute">>, props:get_is_true(<<"variable_loopback_bowout_on_execute">>, Props)}
    ,{<<"Channel-Created-Time">>, props:get_integer_value(<<"Caller-Channel-Created-Time">>, Props)}
     | callee_call_event_props(Props)
     ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

%% return a proplist of k/v pairs specific to the event
-spec specific_call_event_props(binary(), api_binary(), kz_proplist()) -> kz_proplist().
specific_call_event_props(<<"CHANNEL_EXECUTE">>, <<"conference">>, Props) ->
    conference_specific(Props);
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"conference">>, Props) ->
    case props:get_value(<<"variable_current_application_data">>, Props) of
        <<"page_", _/binary>> -> page_specific(Props);
        _Else -> conference_specific(Props)
    end;
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"playback">> = Application, Props) ->
    %% if the playback was terminated as a result of DTMF, include it
    [{<<"DTMF-Digit">>, props:get_value(<<"variable_playback_terminator_used">>, Props)}
    ,{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ,{<<"Group-ID">>, props:get_value(<<"variable_media_group_id">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>, Props) ->
    [{<<"Application-Name">>, <<"noop">>}
    ,{<<"Application-Response">>, props:get_value(<<"kazoo_application_response">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, Props) ->
    [{<<"Application-Name">>, <<"bridge">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Props, <<"FAIL">>)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"record">>, Props) ->
    [{<<"Application-Name">>, <<"bridge">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Props, <<"FAIL">>)}
    ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"set">>, Props) ->
    [{<<"Application-Name">>, props:get_value(<<"set">>, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_CREATE">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_ANSWER">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_DESTROY">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ,{<<"Remote-SDP">>, props:get_value(<<"variable_switch_r_sdp">>, Props)}
    ,{<<"Local-SDP">>, props:get_value(<<"variable_rtp_local_sdp_str">>, Props)}
    ,{<<"Duration-Seconds">>, props:get_value(<<"variable_duration">>, Props)}
    ,{<<"Billing-Seconds">>, get_billing_seconds(Props)}
    ,{<<"Ringing-Seconds">>, props:get_value(<<"variable_progresssec">>, Props)}
    ,{<<"User-Agent">>, props:get_value(<<"variable_sip_user_agent">>, Props)}
    ,{<<"Fax-Info">>, maybe_fax_specific(Props)}
     | debug_channel_props(Props)
    ];
specific_call_event_props(<<"RECORD_START">>, _, Props) ->
    [{<<"Application-Name">>, <<"record">>}
    ,{<<"Application-Response">>, props:get_first_defined([<<"Record-File-Path">>
                                                          ,<<"kazoo_application_response">>
                                                          ], Props)
     }
    ];
specific_call_event_props(<<"RECORD_STOP">>, _, Props) ->
    [{<<"Application-Name">>, <<"record">>}
    ,{<<"Application-Response">>, props:get_first_defined([<<"Record-File-Path">>
                                                          ,<<"kazoo_application_response">>
                                                          ], Props)
     }
    ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Props)}
    ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Props, 0)}
    ,{<<"Silence-Terminated">>, silence_terminated(Props)}
    ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ];
specific_call_event_props(<<"DETECTED_TONE">>, _, Props) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Props)}];
specific_call_event_props(<<"DTMF">>, _, Props) ->
    [{<<"DTMF-Digit">>, props:get_value(<<"DTMF-Digit">>, Props)}
    ,{<<"DTMF-Duration">>, props:get_value(<<"DTMF-Duration">>, Props)}
    ];
specific_call_event_props(_, <<"play_and_get_digits">>, Props) ->
    [{<<"Application-Name">>, <<"play_and_collect_digits">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Props, <<"">>)}
    ];
specific_call_event_props(<<"FAX_DETECTED">>, _, _Props) ->
    [{<<"Application-Name">>, <<"fax_detection">>}];
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"rxfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"receive_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, kz_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"txfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"send_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, kz_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(<<"CHANNEL_INTERCEPTED">>, _, Props) ->
    [{<<"Intercepted-By">>, props:get_value(<<"intercepted_by">>, Props)}];
specific_call_event_props(<<"CHANNEL_TRANSFEROR">>, _, Props) ->
    {Type, To} = transfer_to(Props),
    [{<<"Transfer-Type">>, Type}
    ,{<<"Transfer-To">>, To}
    ];
specific_call_event_props(_Evt, Application, Props) ->
    [{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec transfer_to(kz_proplist() | api_binary()) -> {api_binary(), api_binary()}.
transfer_to(Props)
  when is_list(Props) ->
    transfer_to(props:get_value(<<"variable_transfer_to">>, Props));
transfer_to(<<"att:", TransferTo/binary>>) -> {<<"attended">>, TransferTo};
transfer_to(<<"blind:", TransferTo/binary>>) -> {<<"blind">>, TransferTo};
transfer_to(_) -> {'undefined', 'undefined'}.

-spec page_specific(kz_proplist()) -> kz_proplist().
page_specific(Props) ->
    [{<<"Application-Name">>, <<"page">>}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec conference_specific(kz_proplist()) -> kz_proplist().
conference_specific(Props) ->
    Default = [{<<"Application-Name">>, <<"conference">>}
              ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
              ],
    case props:get_value(<<"Application-Data">>, Props) of
        'undefined' -> Default;
        ConfData ->
            case binary:split(ConfData, <<"@">>) of
                [ConfName, ConfConfig] ->
                    [{<<"Conference-Name">>, ConfName}
                    ,{<<"Conference-Config">>, ConfConfig}
                     | Default
                    ];
                _ -> Default
            end
    end.

-spec maybe_fax_specific(kz_proplist()) -> api_object().
maybe_fax_specific(Props) ->
    case fax_specific(Props) of
        [] -> 'undefined';
        FaxProps -> kz_json:from_list(FaxProps)
    end.

-spec fax_specific(kz_proplist()) -> kz_proplist().
fax_specific(Props) ->
    props:filter_undefined(
      [{<<"Fax-Success">>, get_fax_success(Props)}
      ,{<<"Fax-ECM-Used">>, get_fax_ecm_used(Props)}
      ,{<<"Fax-T38-Used">>, get_fax_t38_used(Props)}
      ,{<<"Fax-Result-Text">>, props:get_value(<<"variable_fax_result_text">>, Props)}
      ,{<<"Fax-Result-Code">>, props:get_value(<<"variable_fax_result_code">>, Props)}
      ,{<<"Fax-Transferred-Pages">>, props:get_value(<<"variable_fax_document_transferred_pages">>, Props)}
      ,{<<"Fax-Total-Pages">>, props:get_value(<<"variable_fax_document_total_pages">>, Props)}
      ,{<<"Fax-Bad-Rows">>, props:get_value(<<"variable_fax_bad_rows">>, Props)}
      ,{<<"Fax-Transfer-Rate">>, props:get_value(<<"variable_fax_transfer_rate">>, Props)}
      ,{<<"Fax-Local-Station-ID">>, props:get_value(<<"variable_fax_local_station_id">>, Props)}
      ,{<<"Fax-Remote-Station-ID">>, props:get_value(<<"variable_fax_remote_station_id">>, Props)}
      ,{<<"Fax-Remote-Country">>, props:get_value(<<"variable_fax_remote_country">>, Props)}
      ,{<<"Fax-Remote-Vendor">>, props:get_value(<<"variable_fax_remote_vendor">>, Props)}
      ,{<<"Fax-Remote-Model">>, props:get_value(<<"variable_fax_remote_model">>, Props)}
      ,{<<"Fax-Image-Resolution">>, props:get_value(<<"variable_fax_image_resolution">>, Props)}
      ,{<<"Fax-File-Image-Resolution">>, props:get_value(<<"variable_fax_file_image_resolution">>, Props)}
      ,{<<"Fax-Image-Size">>, props:get_value(<<"variable_fax_image_size">>, Props)}
      ,{<<"Fax-Image-Pixel-Size">>, props:get_value(<<"variable_fax_image_pixel_size">>, Props)}
      ,{<<"Fax-File-Image-Pixel-Size">>, props:get_value(<<"variable_fax_file_image_pixel_size">>, Props)}
      ,{<<"Fax-Longest-Bad-Row-Run">>, props:get_value(<<"variable_fax_longest_bad_row_run">>, Props)}
      ,{<<"Fax-Encoding">>, props:get_value(<<"variable_fax_encoding">>, Props)}
      ,{<<"Fax-Encoding-Name">>, props:get_value(<<"variable_fax_encoding_name">>, Props)}
      ,{<<"Fax-Timezone">>, props:get_value(<<"variable_fax_timezone">>, Props)}
      ,{<<"Fax-Identity-Number">>, props:get_value(<<"variable_fax_ident">>, Props)}
      ,{<<"Fax-Identity-Name">>, props:get_value(<<"variable_fax_header">>, Props)}
      ,{<<"Fax-Doc-ID">>, props:get_value(<<"variable_fax_doc_id">>, Props)}
      ,{<<"Fax-Doc-DB">>, props:get_value(<<"variable_fax_doc_database">>, Props)}
      ]).

-spec silence_terminated(api_integer() | kz_proplist()) -> api_boolean().
silence_terminated('undefined') -> 'undefined';
silence_terminated(Hits) when is_integer(Hits) -> Hits =:= 0;
silence_terminated(Prop) when is_list(Prop) ->
    case props:get_value(<<"variable_silence_hits_exhausted">>, Prop) of
        'undefined' -> silence_terminated(props:get_integer_value(<<"variable_record_silence_hits">>, Prop));
        Ex -> kz_term:is_true(Ex)
    end.

-spec is_channel_moving(kz_proplist()) -> boolean().
is_channel_moving(Props) ->
    props:get_is_true(<<"variable_channel_is_moving">>, Props, 'false').

-spec get_channel_moving(kz_proplist()) -> api_boolean().
get_channel_moving(Props) ->
    case is_channel_moving(Props) of
        'false' -> 'undefined';
        'true' -> 'true'
    end.

-spec get_channel_state(kz_proplist()) -> api_binary().
get_channel_state(Props) ->
    case props:get_value(<<"Channel-State">>, Props) of
        'undefined' -> 'undefined';
        <<"CS_", ChannelState/binary>> -> ChannelState;
        Other -> Other
    end.

-spec get_call_id(kz_proplist()) -> api_binary().
get_call_id(Props) ->
    kz_evt_freeswitch:call_id(Props).

-spec get_other_leg(kz_proplist()) -> api_binary().
get_other_leg(Props) ->
    ecallmgr_fs_channel:get_other_leg(get_call_id(Props), Props).

-spec get_event_name(kz_proplist()) -> api_binary().
get_event_name(Props) ->
    case kz_evt_freeswitch:application_name(Props) of
        <<"sofia::transferee">> -> <<"CHANNEL_TRANSFEREE">>;
        <<"sofia::transferor">> -> <<"CHANNEL_TRANSFEROR">>;
        <<"sofia::replaced">> -> <<"CHANNEL_REPLACED">>;
        <<"sofia::intercepted">> -> <<"CHANNEL_INTERCEPTED">>;
        <<"spandsp::txfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        <<"spandsp::rxfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        <<"loopback::bowout">> -> <<"CHANNEL_REPLACED">>;
        _AppName -> get_fs_event_name(Props)
    end.

-spec get_fs_event_name(kz_proplist()) -> api_binary().
get_fs_event_name(Props) ->
    case kz_evt_freeswitch:event_name(Props) of
        <<"DETECTED_TONE">> ->
            case props:get_value(<<"Detected-Fax-Tone">>, Props) of
                'undefined' -> <<"DETECTED_TONE">>;
                _FaxDetected -> <<"FAX_DETECTED">>
            end;
        Event -> Event
    end.

-spec get_application_name(kz_proplist()) -> api_binary().
get_application_name(Props) ->
    case kz_evt_freeswitch:application_name(Props) of
        <<"sofia::transferee">> -> <<"transfer">>;
        <<"sofia::transferor">> -> <<"transfer">>;
        <<"sofia::replaced">> -> <<"transfer">>;
        <<"spandsp::rxfax", Event/binary >> -> <<"rxfax",Event/binary>>;
        <<"spandsp::txfax", Event/binary >> -> <<"txfax", Event/binary>>;
        AppName -> AppName
    end.

-spec get_raw_application_name(kz_proplist()) -> api_binary().
get_raw_application_name(Props) ->
    kz_evt_freeswitch:raw_application_name(Props).

-spec get_fax_success(kz_proplist()) -> api_boolean().
get_fax_success(Props) ->
    case props:get_value(<<"variable_fax_success">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"0">>
    end.

-spec get_fax_t38_used(kz_proplist()) -> api_boolean().
get_fax_t38_used(Props) ->
    case props:get_value(<<"variable_has_t38">>, Props) of
        'undefined' -> 'undefined';
        Else -> kz_term:is_true(Else)
    end.

-spec get_fax_ecm_used(kz_proplist()) -> api_boolean().
get_fax_ecm_used(Props) ->
    case props:get_value(<<"variable_fax_ecm_used">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"off">>
    end.

-spec get_serialized_history(kz_proplist()) -> binaries().
get_serialized_history(Props) ->
    case kz_evt_freeswitch:transfer_history(Props) of
        'undefined' -> [];
        History when is_binary(History) ->
            ecallmgr_util:unserialize_fs_array(History);
        History when is_list(History) ->
            History
    end.

-spec get_transfer_history(kz_proplist()) -> api_object().
get_transfer_history(Props) ->
    SerializedHistory = get_serialized_history(Props),
    case [HistJObj
          || Trnsf <- SerializedHistory,
             (HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, ['global']))) =/= 'undefined'
         ]
    of
        [] -> 'undefined';
        History -> kz_json:from_list(History)
    end.

-spec create_trnsf_history_object(list()) -> {ne_binary(), kz_json:object()} | 'undefined'.
create_trnsf_history_object([Epoch, CallId, <<"att_xfer">>, Props]) ->
    [Transferee, Transferer] = binary:split(Props, <<"/">>),
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"attended">>}
            ,{<<"Transferee">>, Transferee}
            ,{<<"Transferer">>, Transferer}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object([Epoch, CallId, <<"bl_xfer">> | Props]) ->
    %% This looks confusing but FS uses the same delimiter to in the array
    %% as it does for inline dialplan actions (like those created during partial attended)
    %% so we have to put it together to take it apart... I KNOW! ARRRG
    Dialplan = lists:last(binary:split(kz_binary:join(Props, <<":">>), <<",">>)),
    [Exten | _] = binary:split(Dialplan, <<"/">>, ['global']),
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"blind">>}
            ,{<<"Extension">>, Exten}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object([Epoch, CallId, <<"uuid_br">> , OtherLeg]) ->
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"bridge">>}
            ,{<<"Other-Leg">>, OtherLeg}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object(_Params) ->
    lager:debug("unhandled transfer type : ~p", [_Params]),
    'undefined'.

-spec get_hangup_cause(kz_proplist()) -> api_binary().
get_hangup_cause(Props) ->
    kz_evt_freeswitch:hangup_cause(Props).

-spec get_disposition(kz_proplist()) -> api_binary().
get_disposition(Props) ->
    kz_evt_freeswitch:disposition(Props).

-spec get_hangup_code(kz_proplist()) -> api_binary().
get_hangup_code(Props) ->
    kz_evt_freeswitch:hangup_code(Props).

-spec get_billing_seconds(kz_proplist()) -> api_binary().
get_billing_seconds(Props) ->
    case props:get_integer_value(<<"variable_billmsec">>, Props) of
        'undefined' -> props:get_value(<<"variable_billsec">>, Props);
        Billmsec -> kz_term:to_binary(kz_term:ceiling(Billmsec / 1000))
    end.

-spec get_is_loopback(api_binary()) -> atom().
get_is_loopback('undefined') -> 'undefined';
get_is_loopback(_) -> 'true'.

-spec callee_call_event_props(kz_proplist()) -> kz_proplist().
callee_call_event_props(Props) ->
    UUID = get_call_id(Props),
    case kz_cache:peek_local(?ECALLMGR_INTERACTION_CACHE, {'channel', UUID}) of
        {'ok', #channel{callee_name = Name,
                        callee_number = Num}} when Num =/= 'undefined' ->
            [{<<"Callee-ID-Number">>, Num}
            ,{<<"Callee-ID-Name">>, Name}
            ];
        _ ->
            [{<<"Callee-ID-Number">>, kz_evt_freeswitch:callee_id_number(Props)}
            ,{<<"Callee-ID-Name">>, kz_evt_freeswitch:callee_id_name(Props)}
            ]
    end.

-spec debug_channel_props(kz_proplist()) -> kz_proplist().
-spec debug_channel_props(kz_proplist(), boolean()) -> kz_proplist().
debug_channel_props(Props) ->
    debug_channel_props(Props, ?DEBUG_CHANNEL).

debug_channel_props(_Props, 'false') -> [];
debug_channel_props(Props, 'true') ->
    [{<<"Channel-Debug">>
     ,kz_json:from_list(lists:sort(fun sort_debug/2, Props))
     }
    ].

-spec sort_debug({any(), any()}, {any(), any()}) -> boolean().
sort_debug({A,_}, {B,_}) -> A =< B.
