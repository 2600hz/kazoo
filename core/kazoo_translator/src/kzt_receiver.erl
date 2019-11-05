%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Receive call events for various scenarios
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_receiver).

-include("kzt.hrl").

-export([wait_for_offnet/1, wait_for_offnet/2
        ,wait_for_noop/2
        ,wait_for_hangup/1
        ,wait_for_conference/1
        ,say_loop/6, say_loop/7
        ,play_loop/3, play_loop/4
        ,record_loop/2
        ,collect_dtmfs/4, collect_dtmfs/5

        ,recording_meta/2
        ]).

-record(dial_req, {call :: kapps_call:call()
                  ,hangup_dtmf :: kz_term:api_binary()
                  ,collect_dtmf = 'false' :: boolean()
                  ,record_call :: kz_term:api_boolean()
                  ,call_timeout :: timeout() | 'undefined'
                  ,call_time_limit :: timeout() | 'undefined'
                  ,start = kz_time:start_time():: kz_time:start_time()
                  ,call_b_leg :: kz_term:api_ne_binary()
                  }).
-type dial_req() :: #dial_req{}.

-define(DEFAULT_EVENT_WAIT, 10 * ?MILLISECONDS_IN_SECOND). % 10s or 10000ms

-type collect_dtmfs_return() :: {'ok', kapps_call:call()} |
                                {'ok', 'timeout', kapps_call:call()} |
                                {'ok', 'dtmf_finish', kapps_call:call()} |
                                {'stop', kapps_call:call()}.
-export_type([collect_dtmfs_return/0]).

-spec default_on_first_fun(any()) -> 'ok'.
default_on_first_fun(_) -> 'ok'.

-spec collect_dtmfs(kapps_call:call(), kz_term:api_ne_binary(), timeout(), pos_integer()) ->
                           collect_dtmfs_return().
collect_dtmfs(Call, FinishKey, Timeout, N) ->
    collect_dtmfs(Call
                 ,FinishKey
                 ,Timeout
                 ,N
                 ,fun default_on_first_fun/1
                 ,kzt_util:get_digits_collected(Call)
                 ).

-spec collect_dtmfs(kapps_call:call(), kz_term:api_ne_binary(), timeout(), pos_integer(), function()) ->
                           collect_dtmfs_return().
collect_dtmfs(Call
             ,FinishKey
             ,Timeout
             ,N
             ,OnFirstFun
             ) when is_function(OnFirstFun, 1) ->
    collect_dtmfs(Call
                 ,FinishKey
                 ,Timeout
                 ,N
                 ,OnFirstFun
                 ,kzt_util:get_digits_collected(Call)
                 ).

-spec collect_dtmfs(kapps_call:call(), kz_term:api_ne_binary(), timeout(), pos_integer(), function(), binary()) ->
                           collect_dtmfs_return().
collect_dtmfs(Call
             ,_FinishKey
             ,_Timeout
             ,N
             ,_OnFirstFun
             ,Collected
             ) when byte_size(Collected) =:= N ->
    {'ok', Call};
collect_dtmfs(Call
             ,FinishKey
             ,Timeout
             ,N
             ,OnFirstFun
             ,Collected
             ) ->
    lager:debug("collect_dtmfs: n: ~p collected: ~s", [N, Collected]),
    Start = kz_time:start_time(),

    case kapps_call_command:receive_event(collect_timeout(Call, Timeout), 'false') of
        {'ok', JObj} ->
            collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start)
                         ,N, OnFirstFun, Collected, JObj
                         );
        {'error', 'timeout'} -> {'ok', 'timeout', Call};
        {'other', {'DOWN', Ref, 'process', Pid, _Reason}} ->
            case kzt_util:get_gather_pidref(Call) of
                {Pid, Ref} when is_pid(Pid), is_reference(Ref) ->
                    lager:info("subactions are done, timer can start"),
                    collect_dtmfs(kzt_util:set_gather_pidref('undefined', Call)
                                 ,FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected
                                 );
                _ ->
                    collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected)
            end;
        {'other', OtherJObj} ->
            lager:debug("other message: ~p", [OtherJObj]),
            collect_dtmfs(Call, FinishKey, collect_decr_timeout(Call, Timeout, Start), N, OnFirstFun, Collected)
    end.

-spec collect_dtmfs(kapps_call:call(), kz_term:api_ne_binary(), timeout(), pos_integer(), function(), binary(), kz_json:object()) ->
                           collect_dtmfs_return().
collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, Collected, JObj) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            {'stop', Call};
        {<<"call_event">>, <<"DTMF">>} ->
            handle_dtmf(Call
                       ,FinishKey
                       ,Timeout
                       ,N
                       ,OnFirstFun
                       ,Collected
                       ,kz_call_event:dtmf_digit(JObj)
                       );
        _Evt ->
            collect_dtmfs(Call, FinishKey, Timeout, N, OnFirstFun, Collected)
    end.

-spec handle_dtmf(kapps_call:call(), kz_term:api_ne_binary(), timeout(), pos_integer(), function(), binary(), kz_term:api_ne_binary()) ->
                         collect_dtmfs_return().
handle_dtmf(Call, FinishKey, _Timeout, _N, _OnFirstFun, _Collected, FinishKey) when is_binary(FinishKey) ->
    lager:info("finish key '~s' pressed", [FinishKey]),
    {'ok', 'dtmf_finish', Call};
handle_dtmf(Call, _FinishKey, _Timeout, _N, _OnFirstFun, _Collected, <<>>) ->
    lager:info("no dtmf pressed in time"),
    {'ok', 'timeout', Call};
handle_dtmf(Call, _FinishKey, _Timeout, _N, _OnFirstFun, _Collected, 'undefined') ->
    lager:info("no dtmf pressed in time"),
    {'ok', 'timeout', Call};
handle_dtmf(Call, FinishKey, Timeout, N, OnFirstFun, <<>>, DTMF) ->
    lager:info("first dtmf pressed: ~s", [DTMF]),
    Call1 = kzt_util:add_digit_collected(DTMF, Call),
    _ = try OnFirstFun(Call1) of _ -> 'ok' catch _:_ -> 'ok' end,
    collect_dtmfs(kzt_util:set_gather_pidref('undefined', Call1)
                 ,FinishKey
                 ,Timeout
                 ,N
                 ,fun default_on_first_fun/1
                 ,DTMF
                 );
handle_dtmf(Call, FinishKey, Timeout, N, OnFirstFun, Collected, DTMF) ->
    lager:debug("dtmf pressed: '~s'", [DTMF]),
    collect_dtmfs(kzt_util:add_digit_collected(DTMF, Call)
                 ,FinishKey
                 ,Timeout
                 ,N
                 ,OnFirstFun
                 ,<<DTMF/binary, Collected/binary>>
                 ).

-spec collect_decr_timeout(kapps_call:call(), timeout(), kz_time:start_time()) ->
                                  timeout().
collect_decr_timeout(Call, Timeout, Start) ->
    case kzt_util:get_gather_pidref(Call) of
        {_Pid, _Ref} when is_pid(_Pid)
                          andalso is_reference(_Ref) ->
            Timeout;
        _ -> kz_time:decr_timeout(Timeout, Start)
    end.

-spec collect_timeout(kapps_call:call(), timeout()) -> timeout().
collect_timeout(Call, Timeout) ->
    case kzt_util:get_gather_pidref(Call) of
        {_Pid, _Ref} when is_pid(_Pid)
                          andalso is_reference(_Ref) ->
            'infinity';
        _ -> Timeout
    end.

-spec say_loop(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), timeout()) ->
                      {'ok', kapps_call:call()} |
                      {'error', _, kapps_call:call()}.
say_loop(Call, SayMe, Voice, Lang, Engine, N) ->
    say_loop(Call, SayMe, Voice, Lang, 'undefined', Engine, N).

-spec say_loop(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), list() | 'undefined', kz_term:ne_binary(), timeout()) ->
                      {'ok', kapps_call:call()} |
                      {'error', _, kapps_call:call()}.
say_loop(Call, _SayMe, _Voice, _Lang, _Terminators, _Engine, N) when N =< 0 ->
    {'ok', Call};
say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N) ->
    NoopId = kapps_call_command:tts(SayMe, Voice, Lang, Terminators, Engine, Call),
    case wait_for_noop(Call, NoopId) of
        {'ok', Call1} ->
            say_loop(Call1
                    ,SayMe
                    ,Voice
                    ,Lang
                    ,Terminators
                    ,Engine
                    ,decr_loop_counter(N)
                    );
        {'error', _, _}=ERR -> ERR
    end.

-spec play_loop(kapps_call:call(), binary(), timeout()) ->
                       {'ok', kapps_call:call()} |
                       {'error', _, kapps_call:call()}.
play_loop(Call, PlayMe, N) ->
    play_loop(Call, PlayMe, 'undefined', N).

-spec play_loop(kapps_call:call(), binary(), list() | 'undefined', timeout()) ->
                       {'ok', kapps_call:call()} |
                       {'error', _, kapps_call:call()}.
play_loop(Call, <<>>, _Terminators, _N) ->
    {'error', 'no_media', Call};
play_loop(Call, _, _, 0) ->
    {'ok', Call};
play_loop(Call, PlayMe, Terminators, N) ->
    NoopId = kapps_call_command:play(PlayMe, Terminators, Call),
    lager:debug("terminators: ~p loop: ~p noop: ~s", [Terminators, N, NoopId]),
    case wait_for_noop(Call, NoopId) of
        {'ok', C} ->
            lager:debug("noop recv"),
            play_loop(C, PlayMe, Terminators, decr_loop_counter(N));
        {'error', _, _}=ERR ->
            lager:debug("err: ~p", [ERR]),
            ERR
    end.

-spec record_loop(kapps_call:call(), pos_integer()) ->
                         {'ok', kapps_call:call()} |
                         {'empty', kapps_call:call()} |
                         {'error', atom(), kapps_call:call()}.
record_loop(Call, SilenceTimeout) ->
    case wait_for_call_event(Call, <<"RECORD_STOP">>) of
        {'ok', EvtJObj} ->
            Len = kz_time:milliseconds_to_seconds(kz_json:get_value(<<"Length">>, EvtJObj, 0)),
            DTMF = kz_json:get_value(<<"Terminator">>, EvtJObj, <<"hangup">>),

            case {kz_json:is_true(<<"Silence-Terminated">>, EvtJObj, 'false')
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
                    {'ok', kapps_call:exec(Fs, Call)}
            end;
        {'error', 'channel_destroy', EvtJObj} ->
            Len = kz_time:milliseconds_to_seconds(kz_json:get_value(<<"Length">>, EvtJObj, 0)),

            lager:debug("recording ended (hangup): len: ~p", [Len]),

            Fs = [{fun kzt_util:set_digit_pressed/2, <<"hangup">>}
                 ,{fun kzt_util:set_recording_duration/2, Len}
                 ],
            {'ok', kapps_call:exec(Fs, Call)};
        {'error', E, _}=ERR ->
            lager:debug("error: ~p", [E]),
            ERR
    end.

wait_for_call_event(Call, EvtName) ->
    case kapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} ->
            process_call_event(Call, EvtName, JObj);
        {'error', 'timeout'} ->
            case kapps_call_command:b_channel_status(Call) of
                {'ok', _} -> wait_for_call_event(Call, EvtName);
                {'error', 'timeout'} -> wait_for_call_event(Call, EvtName);
                {'error', E} -> {'error', E, Call}
            end
    end.

process_call_event(Call, EvtName, JObj) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, EvtName} ->
            {'ok', JObj};
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            {'error', 'channel_destroy', JObj};
        {_, _} ->
            wait_for_call_event(Call, EvtName)
    end.

-spec decr_loop_counter(timeout()) -> timeout().
decr_loop_counter('infinity') -> 'infinity';
decr_loop_counter(N) when is_integer(N), N > 0 -> N-1;
decr_loop_counter(_) -> 0.

-spec wait_for_noop(kapps_call:call(), kz_term:ne_binary()) ->
                           {'ok', kapps_call:call()} |
                           {'error', noop_error(), kapps_call:call()}.
wait_for_noop(Call, NoopId) ->
    case kapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} ->
            process_noop_event(Call, NoopId, JObj);
        {'error', 'timeout'} ->
            case kapps_call_command:b_channel_status(Call) of
                {'ok', _} -> wait_for_noop(Call, NoopId);
                {'error', E} -> {'error', E, Call}
            end
    end.

-type noop_error() :: 'channel_destroy' | 'channel_hungup'.
-spec process_noop_event(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
                                {'ok', kapps_call:call()} |
                                {'error', noop_error(), kapps_call:call()}.
process_noop_event(Call, NoopId, JObj) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            {'error', 'channel_destroy', Call};
        {<<"call_event">>, <<"DTMF">>} ->
            DTMF = kz_call_event:dtmf_digit(JObj),
            lager:info("adding dtmf tone '~s' to collection", [DTMF]),
            wait_for_noop(kzt_util:add_digit_collected(DTMF, Call), NoopId);
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case kz_call_event:application_response(JObj) of
                NoopId -> {'ok', Call};
                _ -> wait_for_noop(Call, NoopId)
            end;
        {_, _} ->
            wait_for_noop(Call, NoopId)
    end.

-spec wait_for_offnet(kapps_call:call()) ->
                             {'ok', kapps_call:call()}.
wait_for_offnet(Call) ->
    wait_for_offnet(Call, []).

-spec wait_for_offnet(kapps_call:call(), kz_term:proplist()) ->
                             {'ok', kapps_call:call()}.
wait_for_offnet(Call, DialProps) ->
    HangupDTMF = kzt_util:get_hangup_dtmf(Call),
    RecordCall = kzt_util:get_record_call(Call),

    CollectDTMF = props:get_is_true('collect_dtmf', DialProps, 'false'),

    CallTimeLimit = kzt_util:get_call_time_limit(Call) * ?MILLISECONDS_IN_SECOND,
    CallTimeout = kzt_util:get_call_timeout(Call) * ?MILLISECONDS_IN_SECOND,

    lager:debug("limit: ~p s timeout: ~p s"
               ,[kzt_util:get_call_time_limit(Call)
                ,kzt_util:get_call_timeout(Call)
                ]),

    wait_for_offnet_events(#dial_req{call=kzt_util:clear_digits_collected(Call)
                                    ,hangup_dtmf=HangupDTMF
                                    ,collect_dtmf=CollectDTMF
                                    ,record_call=RecordCall
                                    ,call_timeout=CallTimeout
                                    ,call_time_limit=CallTimeLimit
                                    ,start=kz_time:start_time()
                                    }).

-spec wait_for_hangup(kapps_call:call()) -> {'ok', kapps_call:call()}.
wait_for_hangup(Call) ->
    case kapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {'ok', JObj} ->
            case kz_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    RespMsg = kz_call_event:response_message(JObj),
                    RespCode = kz_call_event:response_code(JObj),
                    lager:info("offnet resp finished: ~s(~s)", [RespCode, RespMsg]),
                    {'ok', kzt_util:update_call_status(call_status(RespMsg), Call)};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
                    lager:debug("channel was destroyed"),
                    {'ok', kzt_util:update_call_status(call_status(kz_json:get_value(<<"Hangup-Cause">>, JObj)), Call)};
                _Type -> wait_for_hangup(Call)
            end;
        {'error', 'timeout'} ->
            lager:debug("timeout waiting for hangup...seems peculiar"),
            {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)}
    end.

-spec wait_for_conference(kapps_call:call()) -> {'ok', kapps_call:call()}.
wait_for_conference(Call) ->
    HangupDTMF = kzt_util:get_hangup_dtmf(Call),
    RecordCall = kzt_util:get_record_call(Call),

    CallTimeLimit = kzt_util:get_call_time_limit(Call) * ?MILLISECONDS_IN_SECOND,
    CallTimeout = kzt_util:get_call_timeout(Call) * ?MILLISECONDS_IN_SECOND,

    wait_for_conference_events(#dial_req{call=Call
                                        ,hangup_dtmf=HangupDTMF
                                        ,collect_dtmf='true'
                                        ,record_call=RecordCall
                                        ,call_timeout=CallTimeout
                                        ,call_time_limit=CallTimeLimit
                                        ,start=kz_time:start_time()
                                        }).

-spec call_status(kz_term:ne_binary()) -> kz_term:ne_binary().
call_status(<<"ORIGINATOR_CANCEL">>) -> ?STATUS_COMPLETED;
call_status(<<"NORMAL_CLEARING">>) -> ?STATUS_COMPLETED;
call_status(<<"SUCCESS">>) -> ?STATUS_COMPLETED;
call_status(<<"NO_ANSWER">>) -> ?STATUS_NOANSWER;
call_status(<<"USER_BUSY">>) -> ?STATUS_BUSY;
call_status(<<"CALL_REJECTED">>) -> ?STATUS_BUSY;
call_status(_Status) ->
    lager:debug("unhandled call status: ~p", [_Status]),
    ?STATUS_FAILED.

-spec wait_for_offnet_events(dial_req()) -> {'ok', kapps_call:call()}.
wait_for_offnet_events(#dial_req{call_timeout=CallTimeout
                                ,call_time_limit=CallTimeLimit
                                }=OffnetReq) ->
    RecvTimeout = which_time(CallTimeout, CallTimeLimit),
    case kapps_call_command:receive_event(RecvTimeout) of
        {'ok', JObj} -> process_offnet_event(OffnetReq, JObj);
        {'error', 'timeout'} -> handle_offnet_timeout(OffnetReq)
    end.

-spec process_offnet_event(dial_req(), kz_json:object()) ->
                                  {'ok', kapps_call:call()}.
process_offnet_event(#dial_req{call=Call}=OffnetReq
                    ,JObj) ->
    CallId = kapps_call:call_id(Call),

    case {kz_util:get_event_type(JObj)
         ,kz_call_event:call_id(JObj)
         }
    of
        {{<<"resource">>, <<"offnet_resp">>}, _} ->
            RespMsg = kz_call_event:response_message(JObj),
            RespCode = kz_call_event:response_code(JObj),
            lager:info("offnet resp: ~s(~s)", [RespMsg, RespCode]),
            {'ok', kzt_util:update_call_status(call_status(RespMsg), Call)};
        {{<<"call_event">>, <<"DTMF">>}, CallId} ->
            handle_offnet_dtmf(OffnetReq, JObj);
        {{<<"call_event">>, <<"LEG_CREATED">>}, CallId} ->
            handle_offnet_b_leg(OffnetReq, JObj);
        {{<<"call_event">>, <<"CHANNEL_BRIDGE">>}, CallId} ->
            MediaJObj = maybe_start_recording(OffnetReq),
            lager:debug("b-leg bridged: ~s"
                       ,[kz_call_event:other_leg_call_id(JObj)]
                       ),

            Updates = [{fun kzt_util:set_media_meta/2, MediaJObj}
                      ,{fun kzt_util:update_call_status/2, ?STATUS_ANSWERED}
                      ,{fun kzt_util:set_dial_call_status/2, ?STATUS_ANSWERED}
                      ],

            wait_for_offnet_events(
              update_offnet_timers(
                OffnetReq#dial_req{call_timeout='undefined'
                                  ,call=kapps_call:exec(Updates, Call)
                                  }));

        {{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}, CallId} ->
            case kz_call_event:application_name(JObj) of
                <<"bridge">> ->
                    handle_hangup(Call, JObj, kz_call_event:application_response(JObj));
                _App ->
                    wait_for_offnet_events(update_offnet_timers(OffnetReq))
            end;

        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            handle_hangup(Call, JObj);
        {{<<"call_event">>, <<"CHANNEL_PARK">>}, CallId} ->
            lager:debug("channel ~s has parked, probably means none of the bridge strings succeeded", [CallId]),
            {'ok', Call};
        {{_Cat, _Name}, _CallId} ->
            wait_for_offnet_events(update_offnet_timers(OffnetReq))
    end.

-spec handle_offnet_b_leg(dial_req(), kz_json:object()) ->
                                 {'ok', kapps_call:call()}.
handle_offnet_b_leg(#dial_req{call=Call}=OffnetReq, JObj) ->
    BLeg = kz_call_event:other_leg_call_id(JObj),
    lager:debug("b-leg created: ~s", [BLeg]),

    Srv = kzt_util:get_amqp_listener(Call),
    lager:debug("adding binding to ~p", [Srv]),

    _ = gen_listener:add_binding(Srv, 'call', [{'callid', BLeg}
                                              ,{'restrict_to', ['events']}
                                              ]),

    Updates = [{fun kzt_util:set_dial_call_sid/2, BLeg}
              ,{fun kapps_call:set_other_leg_call_id/2, BLeg}
              ,{fun kzt_util:update_call_status/2, ?STATUS_RINGING}
              ,{fun kzt_util:set_dial_call_status/2, ?STATUS_RINGING}
              ],

    wait_for_offnet_events(
      update_offnet_timers(
        OffnetReq#dial_req{call_b_leg=BLeg
                          ,call=kapps_call:exec(Updates, Call)
                          }
       )
     ).

-spec handle_offnet_dtmf(dial_req(), kz_json:object()) ->
                                {'ok', kapps_call:call()}.
handle_offnet_dtmf(OffnetReq, JObj) ->
    handle_dtmf_event(OffnetReq, JObj, fun wait_for_offnet_events/1).

-spec handle_dtmf_event(dial_req(), kz_json:object(), fun((dial_req()) -> {'ok', kapps_call:call()})) ->
                               {'ok', kapps_call:call()}.
handle_dtmf_event(#dial_req{call=Call
                           ,hangup_dtmf=HangupDTMF
                           ,collect_dtmf=CollectDTMF
                           }=DialReq
                 ,JObj
                 ,LoopFun
                 ) ->
    case kz_call_event:dtmf_digit(JObj) of
        HangupDTMF ->
            lager:info("recv'd hangup DTMF '~s'", [HangupDTMF]),
            kapps_call_command:hangup(Call),
            {'ok', Call};
        DTMF when CollectDTMF ->
            lager:info("collecting dtmf ~s", [DTMF]),
            LoopFun(
              update_offnet_timers(
                DialReq#dial_req{call=kzt_util:add_digit_collected(DTMF, Call)}
               ));
        _DTMF ->
            lager:info("caller pressed dtmf tone but we're not collecting it"),
            wait_for_offnet_events(update_offnet_timers(DialReq))
    end.

-spec handle_hangup(kapps_call:call(), kz_json:object()) ->
                           {'ok', kapps_call:call()}.
handle_hangup(Call, JObj) ->
    handle_hangup(Call, JObj, kz_call_event:hangup_cause(JObj)).

-spec handle_hangup(kapps_call:call(), kz_json:object(), kz_term:ne_binary()) ->
                           {'ok', kapps_call:call()}.
handle_hangup(Call, _JObj, HangupCause) ->
    lager:debug("caller channel finished: ~s", [HangupCause]),

    Updates = [{fun kzt_util:update_call_status/2, call_status(HangupCause)}
              ,{fun kzt_util:set_dial_call_status/2, dial_status(Call)}
              ],

    kapps_call_command:hangup(Call),
    maybe_hangup_other_leg(Call),

    {'ok', kapps_call:exec(Updates, Call)}.

-spec maybe_hangup_other_leg(kapps_call:call()) -> 'ok'.
maybe_hangup_other_leg(Call) ->
    maybe_hangup_other_leg(Call, kapps_call:other_leg_call_id(Call)).

-spec maybe_hangup_other_leg(kapps_call:call(), kz_term:api_binary()) -> 'ok'.
maybe_hangup_other_leg(_Call, 'undefined') -> 'ok';
maybe_hangup_other_leg(Call, OtherLeg) ->
    Req = [{<<"Application-Name">>, <<"hangup">>}
          ,{<<"Insert-At">>, <<"now">>}
          ,{<<"Call-ID">>, OtherLeg}
          ],
    kapps_call_command:send_command(Req, Call).

-spec dial_status(kapps_call:call() | kz_term:api_binary()) -> kz_term:ne_binary().
dial_status(?STATUS_ANSWERED) -> ?STATUS_COMPLETED;
dial_status(?STATUS_RINGING) -> ?STATUS_NOANSWER;
dial_status(<<_/binary>>) -> ?STATUS_FAILED;
dial_status('undefined') -> ?STATUS_FAILED;
dial_status(Call) -> dial_status(kzt_util:get_dial_call_status(Call)).

-spec wait_for_conference_events(dial_req()) -> {'ok', kapps_call:call()}.
wait_for_conference_events(#dial_req{call_timeout=CallTimeout
                                    ,call_time_limit=CallTimeLimit
                                    }=OffnetReq) ->
    RecvTimeout = which_time(CallTimeout, CallTimeLimit),

    case kapps_call_command:receive_event(RecvTimeout) of
        {'ok', JObj} -> process_conference_event(OffnetReq, JObj);
        {'error', 'timeout'} -> handle_conference_timeout(OffnetReq)
    end.

process_conference_event(#dial_req{call=Call}=OffnetReq, JObj) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"DTMF">>} ->
            handle_dtmf_event(OffnetReq, JObj, fun wait_for_conference_events/1);

        {<<"call_event">>, <<"CHANNEL_EXECUTE">>} ->
            case kz_call_event:application_name(JObj) of
                <<"conference">> ->
                    lager:info("conferencing has started to execute"),
                    wait_for_conference_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{call_timeout='undefined'}
                       )
                     );
                _App ->
                    lager:debug("ignoring app ~s", [_App]),
                    wait_for_conference_events(update_offnet_timers(OffnetReq))
            end;

        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case kz_call_event:application_name(JObj) of
                <<"conference">> ->
                    lager:info("conferencing has ended"),
                    kapps_call_command:park(Call),
                    {'ok', Call};
                _App ->
                    lager:debug("app ~s has ended", [_App]),
                    wait_for_conference_events(update_offnet_timers(OffnetReq))
            end;

        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:info("our call has ended"),
            {'ok', Call};

        {<<"conference">>, <<"event">>} ->
            case kz_json:get_ne_binary_value(<<"Event">>, JObj) of
                <<"conference-create">> ->
                    lager:info("conference has been created"),
                    wait_for_conference_events(
                      update_offnet_timers(
                        OffnetReq#dial_req{call_timeout='undefined'}
                       )
                     );
                <<"conference-destroy">> ->
                    lager:info("conference has been destroyed"),
                    kapps_call_command:park(Call),
                    {'ok', Call};
                <<"add-member">> ->
                    case kz_api:call_id(JObj) =:= kapps_call:call_id_direct(Call) of
                        'true' ->
                            lager:info("call added as member of the conference"),
                            wait_for_conference_events(
                              update_offnet_timers(
                                OffnetReq#dial_req{call_timeout='undefined'}
                               )
                             );
                        'false' ->
                            lager:debug("member event not for us, skipping"),
                            wait_for_conference_events(update_offnet_timers(OffnetReq))
                    end;
                _Event ->
                    lager:debug("ignoring conference event ~s", [_Event]),
                    wait_for_conference_events(update_offnet_timers(OffnetReq))
            end;
        {_Cat, _Name} ->
            lager:debug("unhandled event for ~s: ~s: ~s"
                       ,[_Cat, _Name, kz_call_event:application_name(JObj)]
                       ),
            wait_for_conference_events(update_offnet_timers(OffnetReq))
    end.

maybe_start_recording(#dial_req{record_call='false'}) -> 'undefined';
maybe_start_recording(#dial_req{record_call='true'
                               ,call=Call
                               ,call_time_limit=CallTimeLimit
                               }) ->
    OtherLegId = kzt_util:get_dial_call_sid(Call),
    RecordingName = recording_name(kapps_call:call_id(Call), OtherLegId),

    lager:info("starting recording '~s'", [RecordingName]),
    {'ok', MediaJObj} = recording_meta(Call, RecordingName),
    kapps_call_command:record_call([{<<"Media-Name">>, RecordingName}]
                                  ,<<"start">>
                                  ,CallTimeLimit
                                  ,Call
                                  ),
    MediaJObj.

-spec recording_meta(kapps_call:call(), kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
recording_meta(Call, MediaName) ->
    AccountDb = kapps_call:account_db(Call),
    BaseDoc = kz_json:from_list([{<<"name">>, MediaName}
                                ,{<<"description">>, <<"recording ", MediaName/binary>>}
                                ,{<<"content_type">>, <<"audio/mp3">>}
                                ,{<<"media_source">>, <<"recorded">>}
                                ,{<<"source_type">>, kz_term:to_binary(?MODULE)}
                                ,{<<"pvt_type">>, <<"private_media">>}
                                ,{<<"from">>, kapps_call:from(Call)}
                                ,{<<"to">>, kapps_call:to(Call)}
                                ,{<<"caller_id_number">>, kapps_call:caller_id_number(Call)}
                                ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
                                ,{<<"call_id">>, kapps_call:call_id(Call)}
                                ]),
    MediaDoc = kz_doc:update_pvt_parameters(BaseDoc, AccountDb),
    kz_datamgr:save_doc(AccountDb, MediaDoc).

recording_name(ALeg, BLeg) ->
    DateTime = kz_time:pretty_print_datetime(calendar:universal_time()),
    iolist_to_binary([DateTime, "_", ALeg, "_to_", BLeg, ".mp3"]).

-spec handle_offnet_timeout(dial_req()) -> {'ok', kapps_call:call()}.
handle_offnet_timeout(#dial_req{call=Call
                               ,call_timeout='undefined'
                               }) ->
    lager:info("call timeout exceeded"),
    kapps_call_command:hangup(Call),
    {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)};
handle_offnet_timeout(#dial_req{call=Call}) ->
    lager:debug("timed out waiting for call to be answered by endpoint(s)"),
    {'ok', kzt_util:update_call_status(?STATUS_NOANSWER, Call)}.

-spec handle_conference_timeout(dial_req()) -> {'ok', kapps_call:call()}.
handle_conference_timeout(#dial_req{call=Call
                                   ,call_timeout='undefined'
                                   }) ->
    lager:debug("time limit for conference exceeded"),
    kapps_call_command:park(Call),
    {'ok', kzt_util:update_call_status(?STATUS_COMPLETED, Call)};
handle_conference_timeout(#dial_req{call=Call}) ->
    lager:debug("timed out waiting for call to be answered by the conference"),
    {'ok', kzt_util:update_call_status(?STATUS_NOANSWER, Call)}.

-spec which_time(kz_term:api_integer(), pos_integer()) -> non_neg_integer().
which_time('undefined', Timelimit)
  when is_integer(Timelimit), Timelimit > 0 ->
    Timelimit;
which_time(Timeout, _)
  when is_integer(Timeout), Timeout > 0 ->
    Timeout;
which_time(_, _) -> 0.

-spec update_offnet_timers(dial_req()) -> dial_req().
update_offnet_timers(#dial_req{call_timeout='undefined'
                              ,call_time_limit='infinity'
                              }=OffnetReq) ->
    OffnetReq;
update_offnet_timers(#dial_req{call_timeout='undefined'
                              ,call_time_limit=CallTimeLimit
                              ,start=Start
                              }=OffnetReq) ->
    Left = kz_time:decr_timeout(CallTimeLimit, Start),
    OffnetReq#dial_req{call_time_limit=Left
                      ,start=kz_time:start_time()
                      };
update_offnet_timers(#dial_req{call_timeout='infinity'}=OffnetReq) ->
    OffnetReq;
update_offnet_timers(#dial_req{call_timeout=CallTimeout
                              ,start=Start
                              }=OffnetReq) ->
    Left = kz_time:decr_timeout(CallTimeout, Start),
    OffnetReq#dial_req{call_timeout=Left
                      ,start=kz_time:start_time()
                      }.
