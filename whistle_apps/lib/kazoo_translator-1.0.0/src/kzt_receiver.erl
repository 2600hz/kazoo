%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
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
         ,say_loop/6, say_loop/7
         ,play_loop/3, play_loop/4
         ,collect_dtmfs/4
        ]).

-record(offnet_req, {
          call :: whapps_call:call()
         ,hangup_dtmf :: api_binary()
         ,record_call :: boolean()
         ,call_timeout :: integer()
         ,call_time_limit :: integer()
         ,start :: wh_now()
         ,call_b_leg :: api_binary()
         }).
-type offnet_req() :: #offnet_req{}.

-define(DEFAULT_EVENT_WAIT, 10000). % 10s or 10000ms

collect_dtmfs(Call, FinishKey, Timeout, N) ->
    collect_dtmfs(Call, FinishKey, Timeout, N, kzt_util:get_digits_collected(Call)).
collect_dtmfs(Call, _FinishKey, _Timeout, N, Collected) when byte_size(Collected) =:= N ->
    {ok, Call};
collect_dtmfs(Call, FinishKey, Timeout, N, Collected) ->
    lager:debug("collect_dtmfs: n: ~p collected: ~s", [N, Collected]),
    case whapps_call_command:wait_for_dtmf(Timeout) of
        {ok, FinishKey} ->
            lager:debug("finish key pressed"),
            {ok, dtmf_finish, Call};
        {ok, <<>>} ->
            lager:debug("no dtmf pressed in time"),
            {ok, timeout, Call};
        {ok, DTMF} ->
            lager:debug("dtmf pressed: ~s", [DTMF]),
            collect_dtmfs(kzt_util:add_digit_collected(DTMF, Call)
                          ,FinishKey, Timeout, N, <<DTMF/binary, Collected/binary>>
                         );
        {error, channel_hungup} -> {stop, Call};
        {error, channel_destroy} -> {stop, Call};
        {error, E} ->
            lager:debug("error: ~p", [E]),
            {error, E, Call}
    end.

-spec say_loop/6 :: (whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                            {'ok', whapps_call:call()} |
                            {'error', _, whapps_call:call()}.
-spec say_loop/7 :: (whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), list() | 'undefined', ne_binary(), wh_timeout()) ->
                            {'ok', whapps_call:call()} |
                            {'error', _, whapps_call:call()}.
say_loop(Call, SayMe, Voice, Lang, Engine, N) ->
    say_loop(Call, SayMe, Voice, Lang, undefined, Engine, N).

say_loop(Call, _SayMe, _Voice, _Lang, _Terminators, _Engine, N) when N =< 0 -> {ok, Call};
say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N) ->
    NoopId = whapps_call_command:tts(SayMe, Voice, Lang, Terminators, Engine, Call),
    case wait_for_noop(Call, NoopId) of
        {ok, C} -> say_loop(C, SayMe, Voice, Lang, Terminators, Engine, decr_loop_counter(N));
        {error, _, _}=ERR -> ERR
    end.

-spec play_loop/3 :: (whapps_call:call(), ne_binary(), wh_timeout()) ->
                             {'ok', whapps_call:call()} |
                             {'error', _, whapps_call:call()}.
-spec play_loop/4 :: (whapps_call:call(), ne_binary(), list() | 'undefined', wh_timeout()) ->
                             {'ok', whapps_call:call()} |
                             {'error', _, whapps_call:call()}.
play_loop(Call, PlayMe, N) ->
    play_loop(Call, PlayMe, 'undefined', N).
play_loop(Call, _, _, 0) ->
    {ok, Call};
play_loop(Call, PlayMe, Terminators, N) ->
    lager:debug("terminators: ~p loop: ~p", [Terminators, N]),
    NoopId = whapps_call_command:play(PlayMe, Terminators, Call),
    case wait_for_noop(Call, NoopId) of
        {ok, C} -> play_loop(C, PlayMe, Terminators, decr_loop_counter(N));
        {error, _, _}=ERR -> ERR
    end.

-spec decr_loop_counter/1 :: (wh_timeout()) -> wh_timeout().
decr_loop_counter(infinity) -> infinity;
decr_loop_counter(N) when is_integer(N), N > 0 -> N-1;
decr_loop_counter(_) -> 0.

-spec wait_for_noop/2 :: (whapps_call:call(), ne_binary()) ->
                                 {'ok', whapps_call:call()} |
                                 {'error', noop_error(), whapps_call:call()}.
wait_for_noop(Call, NoopId) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {ok, JObj} -> process_noop_event(Call, NoopId, JObj);
        {error, timeout} ->
            case whapps_call_command:b_call_status(Call) of
                {ok, _} -> wait_for_noop(Call, NoopId);
                {error, E} -> {error, E, Call}
            end
    end.

-type noop_error() :: 'channel_destroy' | 'channel_hungup'.
-spec process_noop_event/3 :: (whapps_call:call(), ne_binary(), wh_json:object()) ->
                                      {'ok', whapps_call:call()} |
                                      {'error', noop_error(), whapps_call:call()}.
process_noop_event(Call, NoopId, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {error, channel_destroy, Call};
        {<<"call_event">>, <<"CHANNEL_HANGUP">>} -> {error, channel_hungup, Call};
        {<<"call_event">>, <<"DTMF">>} ->
            DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
            lager:debug("adding dtmf tone '~s' to collection", [DTMF]),
            wait_for_noop(kzt_util:add_digit_collected(DTMF, Call), NoopId);
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case wh_json:get_value(<<"Application-Response">>, JObj) of
                NoopId -> {ok, Call};
                _ -> wait_for_noop(Call, NoopId)
            end;
        _ -> wait_for_noop(Call, NoopId)
    end.

-spec wait_for_offnet/1 :: (whapps_call:call()) -> {'ok', whapps_call:call()}.
wait_for_offnet(Call) ->
    HangupDTMF = kzt_util:get_hangup_dtmf(Call),
    RecordCall = kzt_util:get_record_call(Call),

    CallTimeLimit = kzt_util:get_call_time_limit(Call) * 1000,
    CallTimeout = kzt_util:get_call_timeout(Call) * 1000,

    wait_for_offnet_events(#offnet_req{
                              call=Call
                              ,hangup_dtmf=HangupDTMF
                              ,record_call=RecordCall
                              ,call_timeout=CallTimeout
                              ,call_time_limit=CallTimeLimit
                              ,start=erlang:now()
                             }
                          ).

-spec wait_for_hangup/1 :: (whapps_call:call()) -> {'ok', whapps_call:call()}.
wait_for_hangup(Call) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT) of
        {ok, JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
                    RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
                    lager:debug("offnet resp finished: ~s(~s)", [RespCode, RespMsg]),
                    {ok, kzt_util:update_call_status(call_status(RespMsg), Call)};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
                    lager:debug("channel was destroyed"),
                    {ok, kzt_util:update_call_status(call_status(wh_json:get_value(<<"Hangup-Cause">>, JObj)), Call)};
                _Type ->
                    wait_for_hangup(Call)
            end;
        {error, timeout} ->
            lager:debug("timeout waiting for hangup...seems peculiar"),
            {ok, kzt_util:update_call_status(?STATUS_COMPLETED, Call)}
    end.

call_status(<<"ORIGINATOR_CANCEL">>) -> ?STATUS_COMPLETED;
call_status(<<"NORMAL_CLEARING">>) -> ?STATUS_COMPLETED;
call_status(<<"SUCCESS">>) -> ?STATUS_COMPLETED;
call_status(<<"NO_ANSWER">>) -> ?STATUS_NOANSWER;
call_status(<<"USER_BUSY">>) -> ?STATUS_BUSY;
call_status(_Status) ->
    lager:debug("unhandled call status: ~p", [_Status]),
    ?STATUS_FAILED.

-spec wait_for_offnet_events/1 :: (offnet_req()) -> {'ok', whapps_call:call()}.
wait_for_offnet_events(#offnet_req{
                          call_timeout=CallTimeout
                          ,call_time_limit=CallTimeLimit
                         }=OffnetReq
                      ) ->
    RecvTimeout = which_time(CallTimeout, CallTimeLimit),

    case whapps_call_command:receive_event(RecvTimeout) of
        {ok, JObj} -> process_offnet_event(OffnetReq, JObj);
        {error, timeout} -> handle_offnet_timeout(OffnetReq)
    end.

process_offnet_event(#offnet_req{
                        call=Call
                        ,hangup_dtmf=HangupDTMF
                        ,call_b_leg=CallBLeg
                       }=OffnetReq
                     ,JObj
                    ) ->
    CallId = whapps_call:call_id(Call),

    case {wh_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"resource">>, <<"offnet_resp">>}, _} ->
            RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
            RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
            lager:debug("offnet resp: ~s(~s)", [RespMsg, RespCode]),
            {ok, kzt_util:update_call_status(call_status(RespMsg), Call)};
        {{<<"call_event">>, <<"DTMF">>}, CallId} ->
            case (DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj)) =:= HangupDTMF of
                false ->
                    lager:debug("caller pressed dtmf tone '~s', adding to collection", [DTMF]),

                    wait_for_offnet_events(
                      update_offnet_timers(
                        OffnetReq#offnet_req{
                          call=kzt_util:add_digit_collected(DTMF, Call)
                         }));
                true ->
                    lager:debug("recv'd hangup DTMF '~s'", [HangupDTMF]),
                    whapps_call_command:hangup(Call)
            end;
        {{<<"call_event">>, <<"LEG_CREATED">>}, CallId} ->
            BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
            lager:debug("b-leg created: ~s", [BLeg]),
            Srv = kzt_util:get_amqp_listener(Call),
            lager:debug("adding binding to ~p", [Srv]),

            _ = gen_listener:add_binding(Srv, call, [{callid, BLeg}
                                                     ,{restrict_to, [events]}
                                                    ]),

            wait_for_offnet_events(
              update_offnet_timers(
                OffnetReq#offnet_req{
                  call_b_leg=BLeg
                  ,call=
                      lists:foldl(fun({V, F}, CallAcc) -> F(V, CallAcc) end
                                  ,Call
                                  ,[{BLeg, fun kzt_util:set_dial_call_sid/2}
                                    ,{?STATUS_RINGING, fun kzt_util:update_call_status/2}
                                    ,{?STATUS_RINGING, fun kzt_util:set_dial_call_status/2}
                                   ]
                                 )
                 }));
        {{<<"call_event">>, <<"CHANNEL_BRIDGE">>}, CallId} ->
            MediaJObj = maybe_start_recording(OffnetReq),

            lager:debug("b-leg bridged: ~s", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]),

            wait_for_offnet_events(
              update_offnet_timers(
                OffnetReq#offnet_req{
                  call_timeout=undefined
                  ,call=
                      lists:foldl(fun({V, F}, CallAcc) -> F(V, CallAcc) end
                                  ,Call
                                  ,[{MediaJObj, fun kzt_util:set_media_meta/2}
                                    ,{?STATUS_ANSWERED, fun kzt_util:update_call_status/2}
                                    ,{?STATUS_ANSWERED, fun kzt_util:set_dial_call_status/2}
                                   ]
                                 )
                 }));
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj),
            lager:debug("caller channel finished: ~s", [HangupCause]),

            DialStatus = case kzt_util:get_dial_call_status(Call) of
                             ?STATUS_ANSWERED -> ?STATUS_COMPLETED;
                             ?STATUS_RINGING -> ?STATUS_NOANSWER;
                             _ -> ?STATUS_FAILED
                         end,

            {ok, lists:foldl(fun({V, F}, CallAcc) -> F(V, CallAcc) end
                             ,Call
                             ,[{call_status(HangupCause), fun kzt_util:update_call_status/2}
                               ,{DialStatus, fun kzt_util:set_dial_call_status/2}
                              ])};
        {{_Cat, _Name}, _CallId} ->
            lager:debug("unhandled event for ~s: ~s: ~s: ~s"
                        ,[_CallId, _Cat, _Name, wh_json:get_value(<<"Application-Name">>, JObj)]
                       ),
            wait_for_offnet_events(update_offnet_timers(OffnetReq))
    end.

maybe_start_recording(#offnet_req{record_call=false}) -> undefined;
maybe_start_recording(#offnet_req{record_call=true
                                  ,call=Call
                                  ,call_time_limit=CallTimeLimit
                                 }) ->
    OtherLegId = kzt_util:get_dial_call_sid(Call),
    RecordingName = recording_name(whapps_call:call_id(Call), OtherLegId),

    lager:debug("starting recording '~s'", [RecordingName]),
    {ok, MediaJObj} = recording_meta(Call, RecordingName),
    whapps_call_command:record_call(RecordingName, <<"start">>
                                    ,CallTimeLimit, Call
                                   ),
    MediaJObj.

recording_meta(Call, MediaName) ->
    AcctDb = whapps_call:account_db(Call),
    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list
                   ([{<<"name">>, MediaName}
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

-spec handle_offnet_timeout/1 :: (offnet_req()) -> {'ok', whapps_call:call()}.
handle_offnet_timeout(#offnet_req{
                         call=Call
                         ,call_timeout=undefined
                        }) ->
    lager:debug("time limit for call exceeded"),
    whapps_call_command:hangup(Call),
    wait_for_hangup(kzt_util:update_call_status(?STATUS_NOANSWER, Call));
handle_offnet_timeout(#offnet_req{call=Call}) ->
    {ok, kzt_util:update_call_status(?STATUS_NOANSWER, Call)}.

-spec which_time/2 :: (integer(), integer() | 'undefined') -> non_neg_integer().
which_time(TimeLimit, undefined) when TimeLimit > 0 -> TimeLimit;
which_time(_, undefined) -> 0;
which_time(_, Timeout) when Timeout > 0 -> Timeout;
which_time(_, _) -> 0.

-spec update_offnet_timers/1 :: (offnet_req()) -> offnet_req().
update_offnet_timers(#offnet_req{
                        call_timeout=undefined
                        ,call_time_limit=CallTimeLimit
                        ,start=Start
                       }=OffnetReq) ->
    OffnetReq#offnet_req{
      call_time_limit = CallTimeLimit - wh_util:elapsed_ms(Start)
      ,start=erlang:now()
     };
update_offnet_timers(#offnet_req{
                        call_timeout=CallTimeout
                        ,start=Start
                       }=OffnetReq) ->
    OffnetReq#offnet_req{
      call_timeout = CallTimeout - wh_util:elapsed_ms(Start)
      ,start=erlang:now()
     }.
