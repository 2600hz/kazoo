%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle the emulation of the Dial verb
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_twiml_dial).

-export([exec/3]).

-ifdef(TEST).
-export([cleanup_dial_me/1]).
-endif.

-include("kzt.hrl").

-spec exec(kapps_call:call(), kz_types:xml_els() | kz_types:xml_texts(), kz_types:xml_attribs()) ->
                  {'ok' | 'stop', kapps_call:call()}.
exec(Call, [#xmlText{type='text'}|_]=DialMeTxts, Attrs) ->
    kapps_call_command:answer(Call),

    case cleanup_dial_me(kz_xml:texts_to_binary(DialMeTxts)) of
        <<>> ->
            lager:info("no text to dial, using only xml elements"),
            exec(Call, kz_xml:elements(DialMeTxts), Attrs);
        DialMe -> dial_me(Call, Attrs, knm_converters:normalize(DialMe))
    end;
exec(Call
    ,[#xmlElement{name='Number'
                 ,content=Number
                 ,attributes=NumberAttrs
                 }=_El
     ]
    ,DialAttrs) ->
    lager:info("single <Number>"),
    case cleanup_dial_me(kz_xml:texts_to_binary(Number)) of
        <<>> ->
            lager:debug("no dial-able Number in tag, continuing"),
            {'ok', Call};
        NumberText ->
            DialMe = knm_converters:normalize(NumberText),
            NumberProps = kz_xml:attributes_to_proplist(NumberAttrs),

            SendDigits = props:get_value('sendDigits', NumberProps),
            _Url = props:get_value('url', NumberProps),
            _Method = props:get_value('method', NumberProps),

            lager:info("maybe sending number ~s: send ~s", [DialMe, SendDigits]),

            dial_me(Call, DialAttrs, DialMe)
    end;
exec(Call, [#xmlElement{name='Conference'
                       ,content=ConfIdTxts
                       ,attributes=ConfAttrs
                       }], DialAttrs) ->
    kapps_call_command:answer(Call),

    ConfId = conference_id(ConfIdTxts),
    lager:info("dialing into conference '~s'", [ConfId]),

    ConfProps = kz_xml:attributes_to_proplist(ConfAttrs),
    DialProps = kz_xml:attributes_to_proplist(DialAttrs),

    gen_listener:add_binding(kzt_util:get_amqp_listener(Call)
                            ,'conference'
                            ,[{'restrict_to'
                              ,[{'event', {ConfId, kapps_call:call_id_direct(Call)}}]
                              }
                             ]),

    ConfDoc = build_conference_doc(Call, ConfId, ConfProps),

    ConfReq = [{<<"Call">>, kapps_call:to_json(Call)}
              ,{<<"Conference-ID">>, ConfId}
              ,{<<"Conference-Doc">>, ConfDoc}
              ,{<<"Conference-Name">>, kzd_conferences:name(ConfDoc)}
              ,{<<"Moderator">>, props:get_is_true('startConferenceOnEnter', ConfProps, 'true')}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kapi_conference:publish_discovery_req(ConfReq),

    lager:debug("published conference request"),

    %% Will need to support fetching media OR TwiML
    _WaitUrl = props:get_value('waitUrl', ConfProps),
    _WaitMethod = kzt_util:http_method(ConfProps),

    CallWithConf = add_conference_profile(Call, ConfProps),
    SetupCall = setup_call_for_dial(CallWithConf, DialProps),
    AnsweredCall = kzt_util:update_call_status(?STATUS_ANSWERED, SetupCall),

    {'ok', Call1} = kzt_receiver:wait_for_conference(AnsweredCall),

    _ = maybe_end_dial(Call1, DialProps),
    {'stop', Call1};

exec(Call, [#xmlElement{name='Queue'
                       ,content=QueueIdTxts
                       ,attributes=QueueAttrs
                       }], DialAttrs) ->
    DialProps = kz_xml:attributes_to_proplist(DialAttrs),

    QueueId = kz_xml:texts_to_binary(QueueIdTxts),
    QueueProps = kz_xml:attributes_to_proplist(QueueAttrs),

    %% Fetch TwiML to play to caller before connecting agent
    _Url = props:get_value('url', QueueProps),
    _Method = kzt_util:http_method(QueueProps),

    Call1 = setup_call_for_dial(kzt_util:set_queue_sid(QueueId, Call)
                               ,DialProps
                               ),

    lager:info("dialing into queue ~s, unsupported", [QueueId]),
    {'stop', Call1};

exec(Call, [#xmlElement{}|_]=Endpoints, Attrs) ->
    lager:info("dialing endpoints: ~p", [Endpoints]),

    Props = kz_xml:attributes_to_proplist(Attrs),
    Call1 = setup_call_for_dial(Call, Props),

    case xml_elements_to_endpoints(Call1, Endpoints) of
        [] ->
            lager:info("no endpoints were found to dial"),
            {'stop', Call1};
        EPs ->
            lager:debug("endpoints created, sending dial"),
            Timeout = dial_timeout(Props),
            IgnoreEarlyMedia = kz_endpoints:ignore_early_media(EPs),
            Strategy = dial_strategy(Props),

            send_bridge_command(EPs, Timeout, Strategy, IgnoreEarlyMedia, Call1),

            {'ok', Call2} = kzt_receiver:wait_for_offnet(kzt_util:update_call_status(?STATUS_RINGING, Call1)
                                                        ,Props
                                                        ),
            maybe_end_dial(Call2, Props)
    end.

dial_me(Call, Attrs, DialMe) ->
    lager:info("dial text DID '~s'", [DialMe]),

    Props = kz_xml:attributes_to_proplist(Attrs),

    Call1 = setup_call_for_dial(kapps_call:set_request(request_id(DialMe, Call), Call)
                               ,Props
                               ),

    OffnetProps = [{<<"Timeout">>, kzt_util:get_call_timeout(Call1)}
                  ,{<<"Media">>, media_processing(Call1)}
                  ,{<<"Force-Outbound">>, force_outbound(Props)}
                  ,{<<"Server-ID">>, kapps_call:controller_queue(Call1)}
                  ],
    'ok' = kzt_util:offnet_req(OffnetProps, Call1),

    {'ok', Call2} = kzt_receiver:wait_for_offnet(kzt_util:update_call_status(?STATUS_RINGING, Call1)
                                                ,Props
                                                ),
    maybe_end_dial(Call2, Props).

send_bridge_command(EPs, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    B = [{<<"Application-Name">>, <<"bridge">>}
        ,{<<"Endpoints">>, EPs}
        ,{<<"Timeout">>, Timeout}
        ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
        ,{<<"Dial-Endpoint-Method">>, Strategy}
         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    kapps_call_command:send_command(B, Call).

-spec setup_call_for_dial(kapps_call:call(), kz_term:proplist()) -> kapps_call:call().
setup_call_for_dial(Call, Props) ->
    Setters = [{fun kapps_call:set_caller_id_number/2, caller_id(Props, Call)}
              ,{fun kzt_util:set_hangup_dtmf/2, hangup_dtmf(Props)}
              ,{fun kzt_util:set_record_call/2, should_record_call(Props)}
              ,{fun kzt_util:set_call_timeout/2, kzt_twiml_util:timeout_s(Props)}
              ,{fun kzt_util:set_call_time_limit/2, timelimit_s(Props)}
              ],
    kapps_call:exec(Setters, Call).

-spec maybe_end_dial(kapps_call:call(), kz_term:proplist()) ->
                            {'ok' | 'stop' | 'request', kapps_call:call()}.
maybe_end_dial(Call, Props) ->
    maybe_end_dial(Call, Props, kzt_twiml_util:action_url(Props)).

maybe_end_dial(Call, _Props, 'undefined') ->
    lager:debug("a-leg status after bridge: ~s", [kzt_util:get_call_status(Call)]),
    {'ok', Call}; % will progress to next TwiML element
maybe_end_dial(Call, Props, ActionUrl) ->
    CurrentUri = kzt_util:get_voice_uri(Call),
    NewUri = kzt_util:resolve_uri(CurrentUri, ActionUrl),
    lager:debug("sending req to ~s: ~s", [ActionUrl, NewUri]),
    Method = kzt_util:http_method(Props),

    Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
              ,{fun kzt_util:set_voice_uri/2, NewUri}
              ],
    {'request', kapps_call:exec(Setters, Call)}.

-spec cleanup_dial_me(binary()) -> binary().
cleanup_dial_me(<<_/binary>> = Txt) ->
    << <<C>> || <<C>> <= Txt, is_numeric_or_plus(C)>>.

-spec is_numeric_or_plus(pos_integer()) -> boolean().
is_numeric_or_plus(Num) when Num >= $0, Num =< $9 -> 'true';
is_numeric_or_plus($+) -> 'true';
is_numeric_or_plus(_) -> 'false'.

%% To maintain compatibility with Twilo, we force the call offnet (otherwise
%% the redirect onnet steals our callid, and callflow/trunkstore/other could
%% potentially hangup our A-leg. If the B-leg is forced offnet, we can still
%% capture the failed B-leg and continue processing the TwiML (if any).
force_outbound(Props) -> props:get_is_true('continueOnFail', Props, 'true').

-spec xml_elements_to_endpoints(kapps_call:call(), kz_types:xml_els()) ->
                                       kz_json:objects().
xml_elements_to_endpoints(Call, EPs) ->
    xml_elements_to_endpoints(Call, EPs, []).

-spec xml_elements_to_endpoints(kapps_call:call(), kz_types:xml_els(), kz_json:objects()) ->
                                       kz_json:objects().
xml_elements_to_endpoints(_, [], Acc) -> Acc;
xml_elements_to_endpoints(Call
                         ,[#xmlElement{name='Device'
                                      ,content=DeviceIdTxt
                                      ,attributes=_DeviceAttrs
                                      }
                           | EPs
                          ]
                         ,Acc
                         ) ->
    DeviceId = kz_xml:texts_to_binary(DeviceIdTxt),
    lager:debug("maybe adding device ~s to ring group", [DeviceId]),
    case kz_endpoint:build(DeviceId, Call) of
        {'ok', DeviceEPs} -> xml_elements_to_endpoints(Call, EPs, DeviceEPs ++ Acc);
        {'error', _E} ->
            lager:debug("failed to add device ~s: ~p", [DeviceId, _E]),
            xml_elements_to_endpoints(Call, EPs, Acc)
    end;
xml_elements_to_endpoints(Call
                         ,[#xmlElement{name='User'
                                      ,content=UserIdTxt
                                      ,attributes=_UserAttrs
                                      }
                           | EPs
                          ]
                         ,Acc
                         ) ->
    UserId = kz_xml:texts_to_binary(UserIdTxt),
    lager:debug("maybe adding user ~s to ring group", [UserId]),

    case get_endpoints(UserId, kz_json:new(), Call) of
        [] ->
            lager:debug("no user endpoints built for ~s, skipping", [UserId]),
            xml_elements_to_endpoints(Call, EPs, Acc);
        UserEPs -> xml_elements_to_endpoints(Call, EPs, UserEPs ++ Acc)
    end;
xml_elements_to_endpoints(Call
                         ,[#xmlElement{name='Number'
                                      ,content=Number
                                      ,attributes=Attrs
                                      }
                           | EPs
                          ]
                         ,Acc
                         ) ->
    Props = kz_xml:attributes_to_proplist(Attrs),

    SendDigits = props:get_value('sendDigits', Props),
    _Url = props:get_value('url', Props),
    _Method = props:get_value('method', Props),

    DialMe = knm_converters:normalize(kz_xml:texts_to_binary(Number)),

    lager:debug("maybe add number ~s: send ~s", [DialMe, SendDigits]),

    CallFwd = kz_json:from_list([{<<"number">>, DialMe}
                                ,{<<"require_keypress">>, 'false'}
                                ,{<<"substitute">>, 'true'}
                                ]),
    Endpoint = kz_json:from_list([{<<"call_forward">>, CallFwd}]),
    EP = kz_endpoint:create_call_fwd_endpoint(Endpoint, kz_json:new(), Call),

    xml_elements_to_endpoints(Call, EPs, [EP|Acc]);

xml_elements_to_endpoints(Call
                         ,[#xmlElement{name='Sip'
                                      ,content=Number
                                      ,attributes=Attrs
                                      }
                           | EPs
                          ]
                         ,Acc
                         ) ->
    _Props = kz_xml:attributes_to_proplist(Attrs),

    try kzsip_uri:parse(kz_xml:texts_to_binary(Number)) of
        URI ->
            xml_elements_to_endpoints(Call, EPs, [sip_uri(Call, URI)|Acc])
    catch
        'throw':_E ->
            lager:debug("failed to parse SIP uri: ~p", [_E]),
            xml_elements_to_endpoints(Call, EPs, Acc)
    end;

xml_elements_to_endpoints(Call, [_Xml|EPs], Acc) ->
    lager:debug("unknown endpoint, skipping: ~p", [_Xml]),
    xml_elements_to_endpoints(Call, EPs, Acc).

-spec sip_uri(kapps_call:call(), kzsip_uri:sip_uri()) -> kz_json:object().
sip_uri(Call, URI) ->
    lager:debug("maybe adding SIP endpoint: ~s", [kzsip_uri:encode(URI)]),
    SIPDevice = sip_device(URI),
    kz_endpoint:create_sip_endpoint(SIPDevice, kz_json:new(), Call).

-spec sip_device(kzsip_uri:sip_uri()) -> kzd_devices:doc().
sip_device(URI) ->
    lists:foldl(fun({F, V}, D) -> F(D, V) end
               ,kzd_devices:new()
               ,[{fun kzd_devices:set_sip_invite_format/2, <<"route">>}
                ,{fun kzd_devices:set_sip_route/2, kzsip_uri:encode(URI)}
                ]).

request_id(N, Call) -> iolist_to_binary([N, <<"@">>, kapps_call:from_realm(Call)]).

-spec media_processing(kapps_call:call()) -> kz_term:ne_binary().
media_processing(Call) ->
    media_processing(kzt_util:get_record_call(Call), kzt_util:get_hangup_dtmf(Call)).

-spec media_processing(boolean(), kz_term:api_binary()) -> kz_term:ne_binary().
media_processing('false', 'undefined') -> <<"bypass">>;
media_processing('true', _HangupDTMF) -> <<"process">>.

get_max_participants(Props) when is_list(Props) ->
    get_max_participants(props:get_integer_value('maxParticipants', Props, 40));
get_max_participants(N) when is_integer(N), N =< 40, N > 2 -> N.

-spec dial_timeout(kz_term:proplist() | pos_integer()) -> pos_integer().
dial_timeout(Props) when is_list(Props) ->
    dial_timeout(props:get_integer_value('timeout', Props, 20));
dial_timeout(T) when is_integer(T), T > 0 -> T.

dial_strategy(Props) ->
    case props:get_value('strategy', Props) of
        'undefined' -> <<"simultaneous">>;
        <<"simultaneous">> -> <<"simultaneous">>;
        <<"single">> -> <<"single">>
    end.

-spec caller_id(kz_term:proplist(), kapps_call:call()) -> kz_term:ne_binary().
caller_id(Props, Call) ->
    kz_term:to_binary(
      props:get_value('callerId', Props, kapps_call:caller_id_number(Call))
     ).

-spec hangup_dtmf(kz_term:proplist() | kz_term:api_binary()) -> kz_term:api_binary().
hangup_dtmf(Props) when is_list(Props) ->
    case props:get_value('hangupOnStar', Props) of
        'true' -> <<"*">>;
        _ -> hangup_dtmf(props:get_binary_value('hangupOn', Props))
    end;
hangup_dtmf(DTMF) ->
    case lists:member(DTMF, ?ANY_DIGIT) of
        'true' -> DTMF;
        'false' -> 'undefined'
    end.

should_record_call(Props) -> kz_term:is_true(props:get_value('record', Props, 'false')).

timelimit_s(Props) -> props:get_integer_value('timeLimit', Props, 14400).

-spec build_conference_doc(kapps_call:call(), kz_term:ne_binary(), kz_term:proplist()) ->
                                  kz_json:object().
build_conference_doc(Call, ConfId, ConfProps) ->
    StartOnEnter = props:is_true('startConferenceOnEnter', ConfProps),
    AccountId = kapps_call:account_id(Call),

    kz_json:from_list([{<<"name">>, ConfId}
                      ,{<<"id">>, ConfId}
                      ,{<<"play_welcome">>, 'false'}
                      ,{<<"play_entry_tone">>, props:is_true('beep', ConfProps, 'true')}
                      ,{<<"member">>, member_flags(ConfProps, StartOnEnter)}
                      ,{<<"moderator">>, moderator_flags(ConfProps, StartOnEnter)}
                      ,{<<"require_moderator">>, require_moderator(StartOnEnter)}
                      ,{<<"wait_for_moderator">>, 'true'}
                      ,{<<"max_participants">>, get_max_participants(ConfProps)}
                      ,{<<"profile_name">>, <<ConfId/binary, "_", AccountId/binary>>}
                      ,{<<"profile">>, conference_profile(kapps_call:account_id(Call), ConfProps)}
                      ]).

require_moderator('undefined') -> 'false';
require_moderator('true') -> 'false';
require_moderator('false') -> 'true'.

member_flags(_, 'true') -> kz_json:new();
member_flags(ConfProps, _) ->
    kz_json:from_list([{<<"join_muted">>, props:is_true('muted', ConfProps, 'false')}
                      ,{<<"join_deaf">>, props:is_true('deaf', ConfProps, 'false')}
                      ,{<<"play_name">>, props:is_true('play_name', ConfProps, 'false')}
                      ,{<<"play_entry_prompt">>, props:is_true('play_entry_prompt', ConfProps, 'true')}
                      ]).

moderator_flags(ConfProps, 'true') ->
    kz_json:from_list([{<<"join_muted">>, props:is_true('muted', ConfProps, 'false')}
                      ,{<<"join_deaf">>, props:is_true('deaf', ConfProps, 'false')}
                      ,{<<"play_name">>, props:is_true('play_name', ConfProps, 'false')}
                      ,{<<"play_entry_prompt">>, props:is_true('play_entry_prompt', ConfProps, 'true')}
                      ]);
moderator_flags(_, _) -> kz_json:new().

conference_id(Txts) ->
    Id = kz_xml:texts_to_binary(Txts),
    MD5 = kz_term:to_hex_binary(erlang:md5(Id)),
    lager:debug("conf name: ~s (~s)", [Id, MD5]),
    MD5.

-spec add_conference_profile(kapps_call:call(), kz_term:proplist()) -> kapps_call:call().
add_conference_profile(Call, ConfProps) ->
    AccountId = kapps_call:account_id(Call),
    Profile = conference_profile(AccountId, ConfProps),
    kzt_util:set_conference_profile(Profile, Call).

-spec conference_profile(kz_term:ne_binary(), kz_term:proplist()) -> kz_json:object().
conference_profile(AccountId, ConfProps) ->
    kz_json:from_list(
      [{<<"announce-count">>, props:get_integer_value('announceCount', ConfProps)}
      ,{<<"caller-controls">>, props:get_binary_value('callerControls', ConfProps, <<"default">>)}
      ,{<<"caller-id-name">>, props:get_binary_value('callerIdName', ConfProps, kapps_call:unknown_caller_id_name(AccountId))}
      ,{<<"caller-id-number">>, props:get_binary_value('callerIdNumber', ConfProps, kz_privacy:anonymous_caller_id_number(AccountId))}
      ,{<<"comfort-noise">>, props:get_integer_value('comfortNoise', ConfProps, 1000)}
      ,{<<"conference-flags">>, conference_flags(ConfProps)}
      ,{<<"energy-level">>, props:get_integer_value('energyLevel', ConfProps, 20)}
      ,{<<"interval">>, props:get_integer_value('interval', ConfProps, 20)}
      ,{<<"max-members">>, get_max_participants(ConfProps)}
      ,{<<"member-flags">>, conference_member_flags(ConfProps)}
      ,{<<"moderator-controls">>, props:get_binary_value('moderatorControls', ConfProps, <<"default">>)}
      ,{<<"moh-sound">>, get_moh(ConfProps)}
      ,{<<"rate">>, props:get_integer_value('rate', ConfProps, 16000)}
      ,{<<"tts-engine">>, kzt_twiml_util:get_engine(ConfProps)}
      ,{<<"tts-voice">>, kzt_twiml_util:get_voice(ConfProps)}
      ]).

-spec get_moh(kz_term:proplist()) -> kz_term:ne_binary_value().
get_moh(ConfProps) ->
    Default = kapps_config:get_ne_binary(<<"pivot.twiml">>, <<"conference_moh">>, <<"$${hold_music}">>),
    props:get_binary_value('waitUrl', ConfProps, Default).

-spec conference_flags(kz_term:proplist()) -> kz_term:api_ne_binary_value().
conference_flags(ConfProps) ->
    case props:get_is_true('startConferenceOnEnter', ConfProps, 'true') of
        'true' -> 'undefined';
        'false' -> <<"wait-mod">>
    end.

-spec conference_member_flags(kz_term:proplist()) -> kz_term:api_ne_binary_value().
conference_member_flags(ConfProps) ->
    case props:get_is_true('endConferenceOnExit', ConfProps, 'false') of
        'true' -> <<"endconf">>;
        'false' -> 'undefined'
    end.

-spec get_endpoints(binary(), kz_json:object(), kapps_call:call()) ->
                           kz_json:objects().
get_endpoints(UserId, Data, Call) ->
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),
    kz_endpoints:by_owner_id(UserId, Params, Call).
