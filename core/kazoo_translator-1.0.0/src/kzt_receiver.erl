%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Receive call events for various scenarios
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_receiver).

-include("kzt.hrl").

-export([wait_for_offnet/1
         ,wait_for_noop/2
         ,wait_for_hangup/1
         ,wait_for_conference/1
         ,say_loop/6, say_loop/7
         ,play_loop/3, play_loop/4
         ,record_loop/2
         ,collect_dtmfs/4, collect_dtmfs/5

         ,recording_meta/2
        ]).

-record(dial_req, {call :: whapps_call:call()
                   ,hangup_dtmf :: api_binary()
                   ,record_call :: boolean()
                   ,call_timeout :: integer()
                   ,call_time_limit :: integer()
                   ,start :: wh_now()
                   ,call_b_leg :: api_binary()
                  }).
-type dial_req() :: #dial_req{}.

-define(DEFAULT_EVENT_WAIT, 10000). % 10s or 10000ms

-type collect_dtmfs_return() :: {'ok', whapps_call:call()} |
                                {'ok', 'timeout', whapps_call:call()} |
                                {'ok', 'dtmf_finish', whapps_call:call()} |
                                {'stop', whapps_call:call()}.
-export_types([collect_dtmfs_return/0]).

-spec default_on_first_fun(any()) -> 'ok'.
default_on_first_fun(_) -> 'ok'.

-spec collect_dtmfs(whapps_call:call(), api_binary(), wh_timeout(), pos_integer()) ->
                           collect_dtmfs_return().
-spec collect_dtmfs(whapps_call:call(), api_binary(), wh_timeout(), pos_integer(), function()) ->
                           collect_dtmfs_return().
-spec collect_dtmfs(whapps_call:call(), api_binary(), wh_timeout(), pos_integer(), function(), binary()) ->
                           collect_dtmfs_return().
-spec collect_dtmfs(whapps_call:call(), api_binary(), wh_timeout(), pos_integer(), function(), binary(), wh_json:object()) ->
                           collect_dtmfs_return().

collect_dtmfs(Call, FinishKey, Timeout, N) ->
    collect_dtmfs(Call, FinishKey, Timeout, N, fun default_on_first_fun/1, kzt_util:get_digits_collected(Call)).

collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun) when is_function(OnFirstFun, 1) ->
    collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, kzt_util:get_digits_collected(Call)).

collect_dtmfs(Call, _FinishKey, _Timeout, N, _OnFirstFun, Collected) when byte_size(Collected) =:= N ->
    {'ok', Call};
collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, Collected) ->
    lager:debug("collect_dtmfs: n: ~p collected: ~s", [N, Collected]),
    Start = erlang:now(),

    case whapps_call_command:receive_event(collect_timeout(Call, Timeout), 'false') of
        {'ok', JObj} ->
            collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start)
                          ,N, OnFirstFun, Collected, JObj
                         );
        {'error', 'timeout'} -> {'ok', 'timeout', Call};
        {'other', {'DOWN', Ref, 'process', Pid, _Reason}} ->
            case kzt_util:get_gather_pidref(Call) of
                {Pid, Ref} when is_pid(Pid), is_reference(Ref) ->
                    lager:debug("subactions are done, timer can start"),
                    collect_dtmfs(kzt_util:set_gather_pidref('undefined', Call)
                                  ,FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected
                                 );
                _ ->
                    collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected)
            end;
        {'other', OtherJObj} ->
            lager:debug("other message: ~p", [OtherJObj]),
            collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected);
        _Msg ->
            lager:debug("unhandled message: ~p", [_Msg]),
            collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected)
    end.

collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, Collected, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            {'stop', Call};
        {<<"call_event">>, <<"DTMF">>} ->
            handle_dtmf(Call, FinishKey, Timeout, N, OnFirstFun, Collected, wh_json:get_value(<<"DTMF-Digit">>, JObj, <<>>));
        _Evt ->
            lager:debug("ignore event ~p", [_Evt]),
            collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, Collected)
    end.

-spec handle_dtmf(whapps_call:call(), api_binary(), wh_timeout(), pos_integer(), function(), binary(), binary()) ->
                         collect_dtmfs_return().
handle_dtmf(Call, FinishKey, _Timeout, _N, _OnFirstFun, _Collected, FinishKey) ->
    lager:info("finish key '~s' pressed", [FinishKey]),
    {'ok', 'dtmf_finish', Call};
handle_dtmf(Call, _FinishKey, _Timeout, _N, _OnFirstFun, _Collected, <<>>) ->
    lager:info("no dtmf pressed in time"),
    {'ok', 'timeout', Call};
handle_dtmf(Call, FinishKey, Timeout, N, OnFirstFun, <<>>, DTMF) ->
    lager:info("first dtmf pressed: ~s", [DTMF]),
    Call1 = kzt_util:add_digit_collected(DTMF, Call),
    _ = try OnFirstFun(Call1) of _ -> 'ok' catch _:_ -> 'ok' end,
    collect_dtmfs(kzt_util:set_gather_pidref('undefined', Call1)
                  ,FinishKey, Timeout, N, fun default_on_first_fun/1, DTMF
                 );
handle_dtmf(Call, FinishKey, Timeout, N, OnFirstFun, Collected, DTMF) ->
    lager:debug("dtmf pressed: '~s'", [DTMF]),
    collect_dtmfs(kzt_util:add_digit_collected(DTMF, Call)
                  ,FinishKey, Timeout, N, OnFirstFun, <<DTMF/binary, Collected/binary>>
                 ).

-spec collect_decr_timeout(whapps_call:call(), wh_timeout(), wh_now()) -> wh_timeout().
collect_decr_timeout(Call, Timeout, Start) ->
    case kzt_util:get_gather_pidref(Call) of
        {_Pid, _Ref} when is_pid(_Pid) andalso is_reference(_Ref) -> Timeout;
        _ -> whapps_util:decr_timeout(Timeout, Start)
    end.

-spec collect_timeout(whapps_call:call(), wh_timeout()) -> wh_timeout().
collect_timeout(Call, Timeout) ->
    case kzt_util:get_gather_pidref(Call) of
        {_Pid, _Ref} when is_pid(_Pid) andalso is_reference(_Ref) -> 'infinity';
        _ -> Timeout
    end.

-spec say_loop(whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                      {'ok', whapps_call:call()} |
                      {'error', _, whapps_call:call()}.
-spec say_loop(whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), list() | 'undefined', ne_binary(), wh_timeout()) ->
                      {'ok', whapps_call:call()} |
                      {'error', _, whapps_call:call()}.
say_loop(Call, SayMe, Voice, Lang, Engine, N) ->
    say_loop(Call, SayMe, Voice, Lang, 'undefined', Engine, N).

say_loop(Call, _SayMe, _Voice, _Lang, _Terminators, _Engine, N) when N =< 0 -> {'ok', Call};
say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N) ->
    NoopId = whapps_call_command:tts(SayMe, Voice, Lang, Terminators, Engine, Call),
    case wait_for_noop(Call, NoopId) of
        {'ok', C} -> say_loop(C, SayMe, Voice, Lang, Terminators, Engine, decr_loop_counter(N));
        {'error', _, _}=ERR -> ERR
    end.

-spec play_loop(whapps_call:call(), ne_binary(), wh_timeout()) ->
                       {'ok', whapps_call:call()} |
                       {'error', _, whapps_call:call()}.
-spec play_loop(whapps_call:call(), ne_binary(), list() | 'undefined', wh_timeout()) ->
                       {'ok', whapps_call:call()} |
                       {'error', _, whapps_call:call()}.
play_loop(Call, PlayMe, N) -> play_loop(Call, PlayMe, 'undefined', N).
play_loop(Call, _, _, 0) -> {'ok', Call};
play_loop(Call, PlayMe, Terminators, N) ->
    NoopId = whapps_call_command:play(PlayMe, Terminators, Call),
    lager:debug("terminators: ~p loop: ~p noop: ~s", [Terminators, N, NoopId]),
    case wait_for_noop(Call, NoopId) of
        {'ok', C} ->
            lager:debug("noop recv"),
            play_loop(C, PlayMe, Terminators, decr_loop_counter(N));
        {'error', _, _}=ERR ->
            lager:debug("err: ~p", [ERR]),
            ERR
    end.

-spec record_loop(whapps_call:call(), pos_integer()) ->
                         {'ok', whapps_call:call()} |
                         {'empty', whapps_call:call()} |
                         {'error', whapps_call:call()}.
record_loop(Call, SilenceTimeout) ->
    case wait_for_call_event(Call, <<"RECORD_STOP">>) of
        {'ok', EvtJObj} ->
            Len = wh_util:milliseconds_to_seconds(wh_json:get_value(<<"Length">>, EvtJObj, 0)),
            DTMF = wh_json:get_value(<<"Terminator">>, EvtJObj, <<"hangup">>),

            case {wh_json:is_true(<<"Silence-Terminated">>, EvtJObj, 'false')
                  ,SilenceTimeout >= Len
                 }
            of
                {'true', 'true'} ->
                    lager:debug("recording stopped by silence, and was ~b s long (timeout was ~b, considering this empty", [Len, SilenceTimeout]),
                    {'empty', Call};
                _ ->
                    Fs = [{fun kzt_util:set_digit_pressed/2, DTMF}
                          ,{fun kzt_util:set_recording_duration/2, Len}
                         ],
                    {'ok', lists:foldl(fun({F, V}, C) -> F(V, C) end, Call, Fs)}
            end;
        {'error', 'channel_destroy', EvtJObj} ->
            Len = wh_util:milliseconds_to_seconds(wh_json:get_value(<<"Length">>, EvtJObj, 0)),

            lager:debug("recording ended (hangup): len: ~p", [Len]),

            Fs = [{fun kzt_util:set_digit_pressed/2, <<"hangup">>}
                  ,{fun kzt_util:set_recording_duration/2, Len}
                 ],
            {'ok', lists:foldl(fun({F, V}, C) -> F(V, C) end, Call, Fs)};
        {'error', E, _}=ERR ->
            lager:debug("error: ~p", [E]),
            ERR
    end.

wait_for_call_event(Call, EvtName) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} -> process_call_event(Call, EvtName, JObj);
        {'error', 'timeout'} ->
            case whapps_call_command:b_channel_status(Call) of
                {'ok', _} -> wait_for_call_event(Call, EvtName);
                {'error', 'timeout'} -> wait_for_call_event(Call, EvtName);
                {'error', E} -> {'error', E, Call}
            end
    end.

process_call_event(Call, EvtName, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, EvtName} -> {'ok', JObj};
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'error', 'channel_destroy', JObj};
        {<<"call_event">>, _Evt} -> wait_for_call_event(Call, EvtName);
        {_, _} -> wait_for_call_event(Call, EvtName)
    end.

-spec decr_loop_counter(wh_timeout()) -> wh_timeout().
decr_loop_counter('infinity') -> 'infinity';
decr_loop_counter(N) when is_integer(N), N > 0 -> N-1;
decr_loop_counter(_) -> 0.

-spec wait_for_noop(whapps_call:call(), ne_binary()) ->
                           {'ok', whapps_call:call()} |
                           {'error', noop_error(), whapps_call:call()}.
wait_for_noop(Call, NoopId) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} -> process_noop_event(Call, NoopId, JObj);
        {'error', 'timeout'} ->
            case whapps_call_command:b_channel_status(Call) of
                {'ok', _} -> wait_for_noop(Call, NoopId);
                {'error', E} -> {'error', E, Call}
            end
    end.

-type noop_error() :: 'channel_destroy' | 'channel_hungup'.
-spec process_noop_event(whapps_call:call(), ne_binary(), wh_json:object()) ->
                                {'ok', whapps_call:call()} |
                                {'error', noop_error(), whapps_call:call()}.
process_noop_event(Call, NoopId, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'error', 'channel_destroy', Call};
        {<<"call_event">>, <<"CHANNEL_HANGUP_COMPLETE">>} -> {'error', 'channel_hungup', Call};
        {<<"call_event">>, <<"DTMF">>} ->
            DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
            lager:info("adding dtmf tone '~s' to collection", [DTMF]),
            wait_for_noop(kzt_util:add_digit_collected(DTMF, Call), NoopId);
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case wh_json:get_value(<<"Application-Response">>, JObj) of
                NoopId -> {'ok', Call};
                _ -> wait_for_noop(Call, NoopId)
            end;
        _ -> wait_for_noop(Call, NoopId)
    end.

-spec wait_for_offnet(whapps_call:call()) -> {'ok', whapps_call:call()}.
wait_for_offnet(Call) ->
    HangupDTMF = kzt_util:get_hangup_dtmf(Call),
    RecordCall = kzt_util:get_record_call(Call),

    CallTimeLimit = kzt_util:get_call_time_limit(Call) * 1000,
    CallTimeout = kzt_util:get_call_timeout(Call) * 1000,

    lager:debug("tl: ~p t: ~p", [kzt_util:get_call_time_limit(Call), kzt_util:get_call_timeout(Call)]),

    wait_for_offnet_events(#dial_req{call=Call
                                     ,hangup_dtmf=HangupDTMF
                                     ,record_call=RecordCall
                                     ,call_timeout=CallTimeout
                                     ,call_time_limit=CallTimeLimit
                                     ,start=erlang:now()
                                    }).

-spec wait_for_hangup(whapps_call:call()) -> {'ok', whapps_call:call()}.
wait_for_hangup(Call) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
                    RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
                    lager:info("offnet resp finished: ~s(~s)", [RespCode, RespMsg]),
                    {'ok', kzt_util:update_call_status(call_status(RespMsg), Call)};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
                    lager:debug("channel was destroyed"),
                    {'ok', kzt_util:update_call_status(call_status(wh_json:get_value(<<"Hangup-Cause">>, JObj)), Call)};
                _Type -> wait_for_hangup(Call)
            end;
        {'error', 'timeout'} ->
            lager:debug("timeout waiting for hangup...seems peculiar"),
            {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)}
    end.

-spec wait_for_conference(whapps_call:call()) -> {'ok', whapps_call:call()}.
wait_for_conference(Call) ->
    HangupDTMF = kzt_util:get_hangup_dtmf(Call),
    RecordCall = kzt_util:get_record_call(Call),

    CallTimeLimit = kzt_util:get_call_time_limit(Call) * 1000,
    CallTimeout = kzt_util:get_call_timeout(Call) * 1000,

    wait_for_conference_events(#dial_req{call=Call
                                         ,hangup_dtmf=HangupDTMF
                                         ,record_call=RecordCall
                                         ,call_timeout=CallTimeout
                                         ,call_time_limit=CallTimeLimit
                                         ,start=erlang:now()
                                        }).

call_status(<<"ORIGINATOR_CANCEL">>) -> ?STATUS_COMPLETED;
call_status(<<"NORMAL_CLEARING">>) -> ?STATUS_COMPLETED;
call_status(<<"SUCCESS">>) -> ?STATUS_COMPLETED;
call_status(<<"NO_ANSWER">>) -> ?STATUS_NOANSWER;
call_status(<<"USER_BUSY">>) -> ?STATUS_BUSY;
call_status(<<"CALL_REJECTED">>) -> ?STATUS_BUSY;
call_status(_Status) ->
    lager:debug("unhandled call status: ~p", [_Status]),
    ?STATUS_FAILED.

-spec wait_for_offnet_events(dial_req()) -> {'ok', whapps_call:call()}.
wait_for_offnet_events(#dial_req{call_timeout=CallTimeout
                                 ,call_time_limit=CallTimeLimit
                                }=OffnetReq) ->
    RecvTimeout = which_time(CallTimeout, CallTimeLimit),

    case whapps_call_command:receive_event(RecvTimeout) of
        {'ok', JObj} -> process_offnet_event(OffnetReq, JObj);
        {'error', 'timeout'} -> handle_offnet_timeout(OffnetReq);
        _O ->
            lager:debug("recv offnet other: ~p", [_O]),
            wait_for_offnet_events(OffnetReq)
    end.

process_offnet_event(#dial_req{call=Call
                               ,hangup_dtmf=HangupDTMF
                               ,call_b_leg=CallBLeg
                              }=OffnetReq
                     ,JObj) ->
    CallId = whapps_call:call_id(Call),

    case {wh_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"resource">>, <<"offnet_resp">>}, _} ->
            RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
            RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
            lager:info("offnet resp: ~s(~s)", [RespMsg, RespCode]),
            {'ok', kzt_util:update_call_status(call_status(RespMsg), Call)};
        {{<<"call_event">>, <<"DTMF">>}, CallId} ->
            case (DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj)) =:= HangupDTMF of
                'false' ->
                    lager:info("caller pressed dtmf tone '~s', adding to collection", [DTMF]),

                    wait_for_offnet_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{
                          call=kzt_util:add_digit_collected(DTMF, Call)
                         }));
                'true' ->
                    lager:info("recv'd hangup DTMF '~s'", [HangupDTMF]),
                    whapps_call_command:hangup(Call)
            end;
        {{<<"call_event">>, <<"LEG_CREATED">>}, CallId} ->
            BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
            lager:info("b-leg created: ~s", [BLeg]),
            Srv = kzt_util:get_amqp_listener(Call),
            lager:debug("adding binding to ~p", [Srv]),

            _ = gen_listener:add_binding(Srv, 'call', [{'callid', BLeg}
                                                       ,{'restrict_to', ['events']}
                                                      ]),

            Updates = [{BLeg, fun kzt_util:set_dial_call_sid/2}
                       ,{?STATUS_RINGING, fun kzt_util:update_call_status/2}
                       ,{?STATUS_RINGING, fun kzt_util:set_dial_call_status/2}
                      ],

            wait_for_offnet_events(
              update_offnet_timers(
                OffnetReq#dial_req{call_b_leg=BLeg
                                     ,call=lists:foldl(fun({V, F}, CallAcc) ->
                                                               F(V, CallAcc)
                                                       end
                                                       ,Call, Updates)
                                    }));
        {{<<"call_event">>, <<"CHANNEL_BRIDGE">>}, CallId} ->
            MediaJObj = maybe_start_recording(OffnetReq),

            lager:info("b-leg bridged: ~s", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]),

            Updates = [{MediaJObj, fun kzt_util:set_media_meta/2}
                       ,{?STATUS_ANSWERED, fun kzt_util:update_call_status/2}
                       ,{?STATUS_ANSWERED, fun kzt_util:set_dial_call_status/2}
                      ],

            wait_for_offnet_events(
              update_offnet_timers(
                OffnetReq#dial_req{call_timeout='undefined'
                                     ,call=lists:foldl(fun({V, F}, CallAcc) ->
                                                               F(V, CallAcc)
                                                       end
                                                       ,Call, Updates)
                                    }));

        {{<<"call_event">>, <<"CHANNEL_UNBRIDGE">>}, CallId} ->
            case wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj) of
                CallBLeg ->
                    HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj),
                    lager:info("a-leg (~s) has unbridged from ~s(~s), continuing the call", [CallId, CallBLeg, HangupCause]);
                _O ->
                    lager:info("unknown b-leg (~s) unbridged (waiting on ~s)", [_O, CallBLeg])
            end,
            wait_for_offnet_events(update_offnet_timers(OffnetReq));

        {{<<"call_event">>, <<"CHANNEL_EXECUTE">>}, CallId} ->
            wait_for_offnet_events(update_offnet_timers(OffnetReq));

        {{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}, CallId} ->
            case wh_json:get_value(<<"Application-Name">>, JObj) of
                <<"bridge">> ->
                    HangupCause = wh_json:get_value(<<"Application-Response">>, JObj),
                    lager:debug("bridge completed: ~s", [HangupCause]),

                    Updates = [{call_status(HangupCause), fun kzt_util:set_dial_call_status/2}
                               ,{dial_status(Call), fun kzt_util:set_dial_call_status/2}
                              ],
                    wait_for_offnet_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{call=lists:foldl(fun({V, F}, Acc) -> F(V, Acc) end
                                                            ,Call, Updates)
                                          }
                       ));
                _ -> wait_for_offnet_events(update_offnet_timers(OffnetReq))
            end;

        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj),
            lager:debug("caller channel finished: ~s", [HangupCause]),

            Updates = [{call_status(HangupCause), fun kzt_util:update_call_status/2}
                       ,{dial_status(Call), fun kzt_util:set_dial_call_status/2}
                      ],

            {'ok', lists:foldl(fun({V, F}, CallAcc) -> F(V, CallAcc) end
                               ,Call, Updates)
            };
        {{<<"call_event">>, <<"CHANNEL_PARK">>}, CallId} ->
            lager:debug("channel ~s has parked, probably means none of the bridge strings succeeded", [CallId]),
            {'ok', Call};
        {{_Cat, _Name}, _CallId} ->
            lager:debug("unhandled event for ~s: ~s: ~s: ~s"
                        ,[_CallId, _Cat, _Name, wh_json:get_value(<<"Application-Name">>, JObj)]
                       ),
            wait_for_offnet_events(update_offnet_timers(OffnetReq))
    end.

dial_status(?STATUS_ANSWERED) -> ?STATUS_COMPLETED;
dial_status(?STATUS_RINGING) -> ?STATUS_NOANSWER;
dial_status(Status) when is_binary(Status) -> ?STATUS_FAILED;
dial_status(Call) -> dial_status(kzt_util:get_dial_call_status(Call)).

-spec wait_for_conference_events(dial_req()) -> {'ok', whapps_call:call()}.
wait_for_conference_events(#dial_req{call_timeout=CallTimeout
                                     ,call_time_limit=CallTimeLimit
                                    }=OffnetReq) ->
    RecvTimeout = which_time(CallTimeout, CallTimeLimit),

    case whapps_call_command:receive_event(RecvTimeout) of
        {'ok', JObj} -> process_conference_event(OffnetReq, JObj);
        {'error', 'timeout'} -> handle_conference_timeout(OffnetReq);
        _O ->
            lager:debug("recv offnet other: ~p", [_O]),
            wait_for_conference_events(OffnetReq)
    end.

process_conference_event(#dial_req{call=Call
                                   ,hangup_dtmf=HangupDTMF
                                  }=OffnetReq, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"DTMF">>} ->
            case (DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj)) =:= HangupDTMF of
                'false' ->
                    lager:debug("caller pressed dtmf tone '~s', adding to collection", [DTMF]),

                    wait_for_conference_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{
                          call=kzt_util:add_digit_collected(DTMF, Call)
                         }));
                'true' ->
                    lager:debug("recv'd hangup DTMF '~s'", [HangupDTMF]),
                    whapps_call_command:park(Call),
                    {'ok', Call}
            end;

        {<<"call_event">>, <<"CHANNEL_EXECUTE">>} ->
            case wh_json:get_value(<<"Application-Name">>, JObj) of
                <<"conference">> ->
                    lager:debug("conferencing has started to execute"),
                    wait_for_conference_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{call_timeout='undefined'}
                       ));
                _App ->
                    lager:debug("ignoring the start of app ~s", [_App]),
                    wait_for_conference_events(update_offnet_timers(OffnetReq))
            end;

        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case wh_json:get_value(<<"Application-Name">>, JObj) of
                <<"conference">> ->
                    lager:debug("conferencing has ended"),
                    whapps_call_command:park(Call),
                    {'ok', Call};
                _App ->
                    lager:debug("ignoring the end of app ~s", [_App]),
                    wait_for_conference_events(update_offnet_timers(OffnetReq))
            end;

        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("call has ended"),
            {'ok', Call};

        {<<"conference">>, <<"config_req">>} ->
            ConfigName = wh_json:get_value(<<"Profile">>, JObj),
            lager:debug("conference profile ~s requested", [ConfigName]),

            Profile = kzt_util:get_conference_profile(Call),
            Resp = [{<<"Profiles">>, wh_json:from_list([{ConfigName, Profile}])}
                    ,{<<"Caller-Controls">>, kzt_util:get_caller_controls(Call)}
                    ,{<<"Advertise">>, kzt_util:get_advertise(Call)}
                    ,{<<"Chat-Permissions">>, kzt_util:get_chat_permissions(Call)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_conference:publish_config_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                                ,Resp
                                               ),
            wait_for_conference_events(update_offnet_timers(OffnetReq));
        {_Cat, _Name} ->
            lager:debug("unhandled event for ~s: ~s: ~s"
                        ,[_Cat, _Name, wh_json:get_value(<<"Application-Name">>, JObj)]
                       ),
            wait_for_conference_events(update_offnet_timers(OffnetReq))
    end.

maybe_start_recording(#dial_req{record_call='false'}) -> 'undefined';
maybe_start_recording(#dial_req{record_call='true'
                                  ,call=Call
                                  ,call_time_limit=CallTimeLimit
                                 }) ->
    OtherLegId = kzt_util:get_dial_call_sid(Call),
    RecordingName = recording_name(whapps_call:call_id(Call), OtherLegId),

    lager:info("starting recording '~s'", [RecordingName]),
    {'ok', MediaJObj} = recording_meta(Call, RecordingName),
    whapps_call_command:record_call(RecordingName, <<"start">>
                                    ,CallTimeLimit, Call
                                   ),
    MediaJObj.

recording_meta(Call, MediaName) ->
    AcctDb = whapps_call:account_db(Call),
    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list(
                   [{<<"name">>, MediaName}
                    ,{<<"description">>, <<"recording ", MediaName/binary>>}
                    ,{<<"content_type">>, <<"audio/mp3">>}
                    ,{<<"media_source">>, <<"recorded">>}
                    ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
                    ,{<<"pvt_type">>, <<"private_media">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, whapps_call:call_id(Call)}
                   ])
                 ,AcctDb
                ),
    couch_mgr:save_doc(AcctDb, MediaDoc).

recording_name(ALeg) ->
    DateTime = wh_util:pretty_print_datetime(calendar:universal_time()),
    iolist_to_binary([DateTime, "_", ALeg, ".mp3"]).
recording_name(ALeg, BLeg) ->
    DateTime = wh_util:pretty_print_datetime(calendar:universal_time()),
    iolist_to_binary([DateTime, "_", ALeg, "_to_", BLeg, ".mp3"]).

-spec handle_offnet_timeout(dial_req()) -> {'ok', whapps_call:call()}.
handle_offnet_timeout(#dial_req{
                         call=Call
                         ,call_timeout='undefined'
                        }) ->
    lager:info("time limit for call exceeded"),
    whapps_call_command:hangup(Call),
    {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)};
handle_offnet_timeout(#dial_req{call=Call}) ->
    lager:debug("timed out waiting for call to be answered by endpoint(s)"),
    {'ok', kzt_util:update_call_status(?STATUS_NOANSWER, Call)}.

-spec handle_conference_timeout(dial_req()) -> {'ok', whapps_call:call()}.
handle_conference_timeout(#dial_req{
                         call=Call
                         ,call_timeout='undefined'
                        }) ->
    lager:debug("time limit for conference exceeded"),
    whapps_call_command:park(Call),
    {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)};
handle_conference_timeout(#dial_req{call=Call}) ->
    lager:debug("timed out waiting for call to be answered by the conference"),
    {'ok', kzt_util:update_call_status(?STATUS_NOANSWER, Call)}.

-spec which_time(api_integer(), pos_integer()) -> non_neg_integer().
which_time('undefined', Timelimit) when is_integer(Timelimit), Timelimit > 0 -> Timelimit;
which_time(Timeout, _) when is_integer(Timeout), Timeout > 0 -> Timeout;
which_time(_, _) -> 0.

-spec update_offnet_timers(dial_req()) -> dial_req().
update_offnet_timers(#dial_req{call_timeout='undefined'
                               ,call_time_limit=CallTimeLimit
                               ,start=Start
                                }=OffnetReq) ->
    OffnetReq#dial_req{call_time_limit=whapps_util:decr_timeout(CallTimeLimit, Start)
                       ,start=erlang:now()
                      };
update_offnet_timers(#dial_req{call_timeout=CallTimeout
                               ,start=Start
                              }=OffnetReq) ->
    OffnetReq#dial_req{call_timeout=whapps_util:decr_timeout(CallTimeout, Start)
                       ,start=erlang:now()
                      }.
