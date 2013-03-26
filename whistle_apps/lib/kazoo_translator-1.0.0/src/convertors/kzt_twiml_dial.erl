%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handle the emulation of the Dial verb
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml_dial).

-export([exec/3]).

-include("./src/kzt.hrl").

-spec exec(whapps_call:call(), xml_els(), xml_els()) -> {'ok' | 'stop', whapps_call:call()}.
exec(Call, [#xmlText{type='text'}|_]=DialMeTxts, Attrs) ->
    whapps_call_command:answer(Call),

    case wnm_util:to_e164(cleanup_dial_me(kzt_util:xml_text_to_binary(DialMeTxts))) of
        <<>> ->
            lager:debug("no text to dial, using only xml elements"),
            exec(Call, kzt_util:xml_elements(DialMeTxts), Attrs);
        DialMe -> dial_me(Call, Attrs, DialMe)
    end;

exec(Call, [#xmlElement{name='Conference'
                        ,content=ConfIdTxts
                        ,attributes=ConfAttrs
                       }], DialAttrs) ->
    whapps_call_command:answer(Call),

    ConfId = conference_id(ConfIdTxts),
    lager:debug("dial into conference '~s'", [ConfId]),

    ConfProps = kzt_util:xml_attributes_to_proplist(ConfAttrs),
    DialProps = kzt_util:xml_attributes_to_proplist(DialAttrs),

    ConfDoc = build_conference_doc(ConfId, ConfProps),
    ConfReq = [{<<"Call">>, whapps_call:to_json(Call)}
               ,{<<"Conference-Doc">>, ConfDoc}
               ,{<<"Moderator">>, props:get_is_true('startConferenceOnEnter', ConfProps, 'true')}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_discovery_req(ConfReq),

    lager:debug("published conference request"),

    %% Will need to support fetching media OR TwiML
    _WaitUrl = props:get_value('waitUrl', ConfProps),
    _WaitMethod = kzt_util:http_method(ConfProps),

    {'ok', Call1} = kzt_receiver:wait_for_conference(
                      kzt_util:update_call_status(?STATUS_ANSWERED, setup_call_for_dial(Call, DialProps))
                     ),

    lager:debug("waited for offnet, maybe ending dial"),

    maybe_end_dial(Call1),
    {'stop', Call1};

exec(Call, [#xmlElement{name='Queue'
                        ,content=QueueIdTxts
                        ,attributes=QueueAttrs
                       }], DialAttrs) ->
    DialProps = kzt_util:xml_attributes_to_proplist(DialAttrs),

    QueueId = kzt_util:xml_text_to_binary(QueueIdTxts),
    QueueProps = kzt_util:xml_attributes_to_proplist(QueueAttrs),

    %% Fetch TwiML to play to caller before connecting agent
    _Url = props:get_value('url', QueueProps),
    _Method = kzt_util:http_method(QueueProps),

    Call1 = setup_call_for_dial(
              kzt_util:set_queue_sid(QueueId, Call)
              ,DialProps
             ),

    lager:debug("dial into queue ~s, unsupported", [QueueId]),
    {'stop', Call1};

exec(Call, [#xmlElement{}|_]=Endpoints, Attrs) ->
    lager:debug("dialing endpoints"),

    Props = kzt_util:xml_attributes_to_proplist(Attrs),
    Call1 = setup_call_for_dial(Call, Props),

    case xml_elements_to_endpoints(Call1, Endpoints) of
        [] ->
            lager:debug("no endpoints were available"),
            {'stop', Call1};
        EPs ->
            lager:debug("endpoints created, sending dial"),
            Timeout = dial_timeout(Props),
            IgnoreEarlyMedia = cf_util:ignore_early_media(EPs),
            Strategy = dial_strategy(Props),

            send_bridge_command(EPs, Timeout, Strategy, IgnoreEarlyMedia, Call1),

            {'ok', Call2} = kzt_receiver:wait_for_offnet(
                              kzt_util:update_call_status(?STATUS_RINGING, Call1)
                             ),
            maybe_end_dial(Call2)
    end.

dial_me(Call, Attrs, DialMe) ->
    lager:debug("dial text DID '~s'", [DialMe]),

    Props = kzt_util:xml_attributes_to_proplist(Attrs),

    Call1 = setup_call_for_dial(whapps_call:set_request(request_id(DialMe, Call), Call)
                                ,Props
                               ),

    OffnetProps = [{<<"Timeout">>, kzt_util:get_call_timeout(Call1)}
                   ,{<<"Media">>, media_processing(Call1)}
                   ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"park_after_bridge">>, 'true'}])}
                   ,{<<"Force-Outbound">>, force_outbound(Props)}
                   ,{<<"Server-ID">>, whapps_call:controller_queue(Call1)}
                  ],
    'ok' = kzt_util:offnet_req(OffnetProps, Call1),

    {'ok', Call2} = kzt_receiver:wait_for_offnet(
                      kzt_util:update_call_status(?STATUS_RINGING, Call1)
                     ),
    maybe_end_dial(Call2).

send_bridge_command(EPs, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    B = [{<<"Application-Name">>, <<"bridge">>}
         ,{<<"Endpoints">>, EPs}
         ,{<<"Timeout">>, Timeout}
         ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
         ,{<<"Dial-Endpoint-Method">>, Strategy}
         ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"park_after_bridge">>, 'true'}])}
        ],
    lager:debug("kzt_cmd: ~p", [B]),
    {'ok', Bin} = wapi_dialplan:bridge(B),
    lager:debug("kzt_json: ~s", [iolist_to_binary(Bin)]),
    whapps_call_command:send_command(B, Call).
        
setup_call_for_dial(Call, Props) ->
    Setters = [{fun whapps_call:set_caller_id_number/2, caller_id(Props, Call)}
               ,{fun kzt_util:set_hangup_dtmf/2, hangup_dtmf(Props)}
               ,{fun kzt_util:set_record_call/2, should_record_call(Props)}
               ,{fun kzt_util:set_call_timeout/2, kzt_twiml:timeout_s(Props)}
               ,{fun kzt_util:set_call_time_limit/2, timelimit_s(Props)}
              ],

    lists:foldl(fun({F, V}, C) when is_function(F, 2) -> F(V, C) end
                ,Call
                ,Setters
               ).

-spec maybe_end_dial(whapps_call:call()) -> {'ok' | 'stop', whapps_call:call()}.
maybe_end_dial(Call) ->
    case kzt_util:get_call_status(Call) of
        ?STATUS_COMPLETED -> {'stop', Call};
        _Status ->
            lager:debug("a-leg status after bridge: ~s", [_Status]),
            {'ok', Call} % will progress to next TwiML element
    end.

-spec cleanup_dial_me(ne_binary()) -> ne_binary().
cleanup_dial_me(Txt) -> << <<C>> || <<C>> <= Txt, is_numeric_or_plus(C)>>.

-spec is_numeric_or_plus(pos_integer()) -> boolean().
is_numeric_or_plus(Num) when Num >= $0, Num =< $9 -> 'true';
is_numeric_or_plus($+) -> 'true';
is_numeric_or_plus(_) -> 'false'.

%% To maintain compatibility with Twilo, we force the call offnet (otherwise
%% the redirect onnet steals our callid, and callflow/trunkstore/other could
%% potentially hangup our A-leg. If the B-leg is forced offnet, we can still
%% capture the failed B-leg and continue processing the TwiML (if any).
force_outbound(Props) -> props:get_is_true('continueOnFail', Props, 'true').

-spec xml_elements_to_endpoints(whapps_call:call(), xml_els()) -> wh_json:objects().
-spec xml_elements_to_endpoints(whapps_call:call(), xml_els(), wh_json:objects()) ->
                                             wh_json:objects().
xml_elements_to_endpoints(Call, EPs) ->
    xml_elements_to_endpoints(Call, EPs, []).

xml_elements_to_endpoints(_, [], Acc) -> Acc;
xml_elements_to_endpoints(Call, [#xmlElement{name='Device'
                                             ,content=DeviceIdTxt
                                             ,attributes=_DeviceAttrs
                                            }
                                 | EPs], Acc
                         ) ->
    DeviceId = kzt_util:xml_text_to_binary(DeviceIdTxt),
    lager:debug("maybe adding device ~s to ring group", [DeviceId]),
    case cf_endpoint:build(DeviceId, Call) of
        {'ok', [EP]} -> xml_elements_to_endpoints(Call, EPs, [EP|Acc]);
        {'ok', DeviceEPs} -> xml_elements_to_endpoints(Call, EPs, DeviceEPs ++ Acc);
        {'error', _E} ->
            lager:debug("failed to add device ~s: ~p", [DeviceId, _E]),
            xml_elements_to_endpoints(Call, EPs, Acc)
    end;
xml_elements_to_endpoints(Call, [#xmlElement{name='User'
                                            ,content=UserIdTxt
                                            ,attributes=_UserAttrs
                                            }
                                | EPs], Acc) ->
    UserId = kzt_util:xml_text_to_binary(UserIdTxt),
    lager:debug("maybe adding user ~s to ring group", [UserId]),

    case cf_endpoint:build(UserId, Call) of
        {'ok', [EP]} -> xml_elements_to_endpoints(Call, EPs, [EP|Acc]);
        {'ok', UserEPs} -> xml_elements_to_endpoints(Call, EPs, UserEPs ++ Acc);
        {'error', _E} ->
            lager:debug("failed to add user ~s: ~p", [UserId, _E]),
            xml_elements_to_endpoints(Call, EPs, Acc)
    end;
xml_elements_to_endpoints(Call, [#xmlElement{name='Number'
                                             ,content=Number
                                             ,attributes=Attrs
                                            }
                                 | EPs], Acc) ->
    Props = kzt_util:xml_attributes_to_proplist(Attrs),

    SendDigits = props:get_value('sendDigis', Props),
    Url = props:get_value('url', Props),
    Method = props:get_value('method', Props),

    DialMe = wmn_util:to_e164(kzt_util:xml_text_to_binary(Number)),

    lager:debug("maybe add number ~s: send ~s, skipping", [DialMe, SendDigits]),
    xml_elements_to_endpoints(Call, EPs, Acc);

xml_elements_to_endpoints(Call, [_Xml|EPs], Acc) ->
    lager:debug("unknown endpoint: ~p", [_Xml]),
    xml_elements_to_endpoints(Call, EPs, Acc).

request_id(N, Call) -> iolist_to_binary([N, $@, whapps_call:from_realm(Call)]).

media_processing(Call) ->
    media_processing(kzt_util:get_record_call(Call), kzt_util:get_hangup_dtmf(Call)).

media_processing('false', 'undefined') -> <<"bypass">>;
media_processing('true', _HangupDTMF) -> <<"process">>.

get_max_participants(Props) when is_list(Props) ->
    get_max_participants(props:get_integer_value('maxParticipants', Props, 40));
get_max_participants(N) when is_integer(N), N =< 40, N > 0 -> N.

-spec dial_timeout(wh_proplist() | pos_integer()) -> pos_integer().
dial_timeout(Props) when is_list(Props) ->
    dial_timeout(props:get_integer_value('timeout', Props, 20));
dial_timeout(T) when is_integer(T), T > 0 -> T.

dial_strategy(Props) ->
    case props:get_value('strategy', Props) of
        'undefined' -> <<"simultaneous">>;
        <<"simultaneous">> -> <<"simultaneous">>;
        <<"single">> -> <<"single">>
    end.

-spec caller_id(wh_proplist(), whapps_call:call()) -> ne_binary().
caller_id(Props, Call) ->
    wh_util:to_binary(
      props:get_value('callerId', Props, whapps_call:caller_id_number(Call))
     ).

-spec hangup_dtmf(wh_proplist() | api_binary()) -> api_binary().
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

should_record_call(Props) -> wh_util:is_true(props:get_value('record', Props, 'false')).
timelimit_s(Props) -> props:get_integer_value('timeLimit', Props, 14400).


-spec build_conference_doc(ne_binary(), wh_proplist()) -> wh_json:object().
build_conference_doc(ConfId, ConfProps) ->
    StartOnEnter = props:is_true('startConferenceOnEnter', ConfProps),

    wh_json:from_list([{<<"name">>, ConfId}
                       ,{<<"play_welcome">>, 'false'}
                       ,{<<"play_entry_tone">>, props:is_true('beep', ConfProps, 'true')}
                       ,{<<"member">>, member_flags(ConfProps, StartOnEnter)}
                       ,{<<"moderator">>, moderator_flags(ConfProps, StartOnEnter)}
                       ,{<<"require_moderator">>, require_moderator(StartOnEnter)}
                       ,{<<"wait_for_moderator">>, 'true'}
                       ,{<<"max_members">>, get_max_participants(ConfProps)}
                       ,{<<"profile">>, <<"pivot">>}
                      ]).

require_moderator('undefined') -> 'false';
require_moderator('true') -> 'false';
require_moderator('false') -> 'true'.

member_flags(_, 'true') -> wh_json:new();
member_flags(ConfProps, _) ->
    wh_json:from_list([{<<"join_muted">>, props:is_true('muted', ConfProps, 'false')}
                       ,{<<"join_deaf">>, props:is_true('deaf', ConfProps, 'false')}
                       ,{<<"play_name">>, props:is_true('play_name', ConfProps, 'false')}
                       ,{<<"play_entry_prompt">>, props:is_true('play_entry_prompt', ConfProps, 'true')}
                      ]).

moderator_flags(ConfProps, 'true') ->
    wh_json:from_list([{<<"join_muted">>, props:is_true('muted', ConfProps, 'false')}
                       ,{<<"join_deaf">>, props:is_true('deaf', ConfProps, 'false')}
                       ,{<<"play_name">>, props:is_true('play_name', ConfProps, 'false')}
                       ,{<<"play_entry_prompt">>, props:is_true('play_entry_prompt', ConfProps, 'true')}
                      ]);
moderator_flags(_, _) -> wh_json:new().

conference_id(Txts) ->
    Id = kzt_util:xml_text_to_binary(Txts),
    MD5 = wh_util:to_hex_binary(erlang:md5(Id)),
    lager:debug("conf name: ~s (~s)", [Id, MD5]),
    MD5.
