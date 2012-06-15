%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Aims to recreate the TwiML interaction, converting the TwiML XML
%%% to Whistle JSON commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%% @todo
%%%   Finish support for Number tag
%%%-------------------------------------------------------------------
-module(wht_twiml).

-export([does_recognize/1, exec/2, req_params/1]).

-include("wht.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(NAME, <<"wht_twiml">>).
-define(VERSION, <<"0.2.0">>).

-define(STATUS_QUEUED, <<"queued">>).
-define(STATUS_RINGING, <<"ringing">>).
-define(STATUS_ANSWERED, <<"in-progress">>).
-define(STATUS_COMPLETED, <<"completed">>).
-define(STATUS_BUSY, <<"busy">>).
-define(STATUS_FAILED, <<"failed">>).
-define(STATUS_NOANSWER, <<"no-answer">>).
-define(STATUS_CANCELED, <<"canceled">>).

-spec does_recognize/1 :: (string()) -> {'true', term()} | 'false'.
does_recognize(Cmds) ->
    case xmerl_scan:string(Cmds) of
        {#xmlElement{name='Response'}=Cs, _} -> {true, Cs};
        _E ->
            lager:debug("don't recognize: ~p", [_E]),
            false
    end.

-spec exec/2 :: (whapps_call:call(), #xmlElement{}) -> exec_return().
exec(Call, #xmlElement{name='Response', content=Elements}) ->
    try exec_response(Call, Elements) of
        Resp -> Resp
    catch
        _C:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to exec: ~p: ~p", [_C, _R]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            {stop, update_call_status(Call, ?STATUS_FAILED)}
    end.

-spec exec_response/2 :: (whapps_call:call(), [#xmlText{} | #xmlElement{},...] | []) -> exec_return().
exec_response(Call, [#xmlText{}|T]) ->
    exec_response(Call, T);
exec_response(Call, [#xmlElement{name=Name, content=Content}=El|T]) ->
    case exec_element(Call, Name, Content, El) of
        {ok, Call1} -> exec_response(Call1, T);
        Other -> Other
    end;
exec_response(Call, []) ->
    {stop, update_call_status(Call, ?STATUS_COMPLETED)}.

-spec exec_element/4 :: (whapps_call:call(), atom(), [#xmlText{},...] | [], #xmlElement{}) -> exec_element_return().

exec_element(Call, 'Dial', [#xmlText{value=DialMe, type=text}], #xmlElement{attributes=Attrs}) ->
    dial_number(Call, DialMe, Attrs);
exec_element(Call, 'Dial', [#xmlElement{name='Number'}=El1], #xmlElement{attributes=Attrs}) ->
    dial_number(Call, El1, Attrs);
exec_element(Call, 'Dial', [#xmlElement{}|_]=Numbers, #xmlElement{attributes=Attrs}) ->
    dial_ring_group(Call, Numbers, Attrs);

exec_element(Call, 'Record', [#xmlText{}], #xmlElement{attributes=Attrs}) ->
    record_call(Call, Attrs);

exec_element(Call, 'Gather', [#xmlText{}|T], El) ->
    exec_element(maybe_answer_call(Call), 'Gather', T, El);
exec_element(Call, 'Gather', [#xmlElement{name=Name, content=Content}=El1|T], El) ->
    Call1 = maybe_answer_call(Call),

    case exec_gather_element(Call1, Name, Content, El1) of
        {ok, Digit, Call2} ->
            lager:debug("maybe recv dtmf ~s for gather", [Digit]),
            exec_element(whapps_call:kvs_store(digits_collected, Digit, Call2), 'Gather', T, El);
        {ok, Call2} ->
            exec_element(Call2, 'Gather', T, El);
        {error, channel_hungup, Call2} ->
            {stop, Call2};
        {error, channel_destroy, Call2} ->
            {stop, Call2};
        {error, _, Call2} ->
            exec_element(Call2, 'Gather', T, El)
    end;
exec_element(Call, 'Gather', [], #xmlElement{attributes=Attrs}) ->
    gather(Call, Attrs);

exec_element(Call, 'Play', [#xmlText{value=PlayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    play(Call, PlayMe, Attrs);

exec_element(Call, 'Say', [#xmlText{value=SayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    maybe_stop(Call, say(Call, SayMe, Attrs), {ok, Call});

exec_element(Call, 'Redirect', [_|_]=Texts, #xmlElement{attributes=Attrs}) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),

    Url = wh_util:join_binary([wh_util:to_binary(Frag) || #xmlText{value=Frag} <- Texts], <<>>),

    Call2 = set_variables(Call1, Texts),

    NewUri = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call1), Url),
    Method = wht_util:http_method(props:get_value(method, Props, post)),
    BaseParams = wh_json:from_list(req_params(Call1) ),

    {request, Call2, NewUri, Method, BaseParams};

exec_element(Call, 'Pause', _, #xmlElement{attributes=Attrs}) ->
    pause(Call, Attrs);

exec_element(Call, 'Variable', _, #xmlElement{attributes=Attrs}) ->
    Props = attrs_to_proplist(Attrs),
    {ok, set_variable(Call, props:get_value(key, Props), props:get_value(value, Props))};

exec_element(Call, 'Hangup', _, _) ->
    Call1 = maybe_answer_call(Call),
    whapps_call_command:hangup(Call1),
    {stop, update_call_status(Call1, ?STATUS_COMPLETED)};

exec_element(Call, 'Reject', _, #xmlElement{attributes=Attrs}) ->
    Props = attrs_to_proplist(Attrs),
    Reason = reject_reason(props:get_value(reason, Props)),

    play_reject_reason(Call, Reason), 
    whapps_call_command:response(reject_code(Reason), Reason, Call),
    {stop, update_call_status(Call, ?STATUS_BUSY)}.

%%-------------------------------------------------------------------------
%% @doc Variable
%%   name  | the name of the variable
%%   value | the value of the variable
%%-------------------------------------------------------------------------
-spec set_variable/3 :: (whapps_call:call(), wh_json:json_string(), wh_json:json_term()) -> whapps_call:call().
-spec set_variables/2 :: (whapps_call:call(), list()) -> whapps_call:call().
set_variable(Call, Key, Value) ->
    wht_translator:set_user_vars([{wh_util:to_binary(Key)
                                   ,wh_util:to_binary(Value)
                                  }], Call).

set_variables(Call, Els) ->
    case [begin
              Props = attrs_to_proplist(Attrs),
              {wh_util:to_binary(props:get_value(key, Props))
               ,wh_util:to_binary(props:get_value(value, Props))
              }
          end
          || #xmlElement{name='Variable', attributes=Attrs} <- Els
         ] of
        [] -> Call;
        Vars -> wht_translator:set_user_vars(Vars, Call)
    end.

%%-------------------------------------------------------------------------
%% @doc Record
%%   action             | relative or absolute URL | current URL
%%   method             | GET, POST                | POST
%%   timeout            | positive integer, secs   | 5
%%   finishOnKey        | any digit, #, *          | 1234567890*#
%%   maxLength          | positive integer, secs   | 3600 (1 hour)
%%   transcribe         | true, false              | false (not supported)
%%   transcribeCallback | relative or absolute URL | none
%%   playBeep           | true, false              | true
%%-------------------------------------------------------------------------
record_call(Call, Attrs) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),
    lager:debug("RECORD with attrs: ~p", [Attrs]),

    %% TODO: remove cf dependency
    Action = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call1)
                                  ,props:get_value(action, Props)
                                 ),

    Method = wht_util:http_method(props:get_value(method, Props, post)),

    %% Transcribe = props:get_is_true(transcribe, Props, false),
    %% TranscribeCallback = props:get_value(transcribeCallback, Props),

    case props:get_is_true(playBeep, Props, true) of
        true -> play_tone(Call1);
        false -> ok
    end,

    MediaName = media_name(whapps_call:call_id(Call1)),
    case whapps_call_command:b_record(MediaName
                                      ,props:get_value(finishOnKey, Props, ?ANY_DIGIT)
                                      ,wh_util:to_binary(props:get_value(maxLength, Props, <<"3600">>))
                                      ,props:get_value(timeout, Props, <<"5">>)
                                      ,Call1
                                     ) of
        {ok, Msg} ->
            case wh_json:get_integer_value(<<"Length">>, Msg, 0) of
                0 -> lager:debug("recorded message length: 0, continuing"), ok;
                L ->
                    RecordingId = maybe_save_recording(Call1, MediaName, true),
                    DTMFs = wh_json:get_value(<<"Digit-Pressed">>, Msg),

                    lager:debug("recorded message length: ~bs, stored as ~s", [L, RecordingId]),
                    BaseParams = wh_json:from_list([{<<"RecordingUrl">>, recorded_url(Call1, RecordingId, true)}
                                                    ,{<<"RecordingDuration">>, L}
                                                    ,{<<"Digits">>, DTMFs}
                                                    | req_params(Call1)
                                                   ]),
                    {request, update_call_status(Call1, ?STATUS_ANSWERED), Action, Method, BaseParams}
            end;
        {error, _E} ->
            %% TODO: when call hangs up, try to save message
            lager:debug("failed to record message: ~p", [_E]),
            {stop, update_call_status(Call1, ?STATUS_FAILED)}
    end.

-spec gather/2 :: (whapps_call:call(), proplist()) -> exec_return().
gather(Call, Attrs) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),

    Timeout = wh_util:to_integer(props:get_value(timeout, Props, 5)) * 1000,
    FinishOnKey = props:get_value(finishOnKey, Props, <<"#">>),

    InitDigit = whapps_call:kvs_fetch(digits_collected, <<>>, Call1),

    case props:get_value(numDigits, Props) of
        undefined -> collect_until_terminator(Call1, InitDigit, FinishOnKey, Timeout, Props);
        MaxDigits -> collect_digits(Call1, InitDigit, wh_util:to_integer(MaxDigits), FinishOnKey, Timeout, Props)
    end.

-spec exec_gather_element/4 :: (whapps_call:call(), atom(), list(), #xmlElement{}) -> {'ok', binary(), whapps_call:call()} |
                                                                                      {'ok', whapps_call:call()} |
                                                                                      {'error', atom() | wh_json:json_object(), whapps_call:call()}.
exec_gather_element(Call, 'Say', [#xmlText{value=SayMe, type=text}], #xmlElement{attributes=Attrs}) ->    
    Result = say(Call, SayMe, Attrs, ?ANY_DIGIT),
    lager:debug("say returned: ~p", [Result]),
    maybe_stop(Call, Result, {ok, Call});

exec_gather_element(Call, 'Play', [#xmlText{value=PlayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    Call1 = case play(Call, PlayMe, Attrs, ?ANY_DIGIT) of
                {ok, Call0} -> Call0;
                {stop, Call0} -> Call0;
                {request, Call0, _, _, _} -> Call0
            end,
    case whapps_call_command:wait_for_application_or_dtmf(<<"play">>, infinity) of
        {dtmf, Digit} ->
            lager:debug("gather/play recv DTMF ~s", [Digit]),
            {ok, Digit, Call1};
        {ok, _} ->
            lager:debug("gather/play finished"),
            {ok, <<>>, Call1};
        {error, E} ->
            {error, E, Call1}
    end;
exec_gather_element(Call, 'Pause', _, #xmlElement{attributes=Attrs}) ->
    pause(Call, Attrs);
exec_gather_element(Call, _Action, _, _) ->
    lager:debug("unhandled nested action ~s in Gather", [_Action]),
    {ok, Call}.

%%-------------------------------------------------------------------------
%% @doc Dial
%%   action       | relative or absolute URL | no default action
%%   method       | GET, POST                | POST
%%   timeout      | positive integer, secs   | 30
%%   hangupOnStar | true, false              | false
%%   timeLimit    | positive integer, secs   | 14400
%%   callerId     | e164                     | caller's caller-id-number
%%   record       | true, false              | false
%%
%%   value        | DID to dial              | DID
%%                | <Number>                 |
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc Number
%%   sendDigits   | digits and pause ('w') characters | "ww123" or "123"
%%
%%   value        | DID to dial                       | DID
%%-------------------------------------------------------------------------
-spec dial_number/3 :: (whapps_call:call(), ne_binary() | #xmlElement{}, proplist()) -> exec_element_return().
dial_number(Call, #xmlElement{name='Number', content=[#xmlText{value=DialMe, type=text}]}, Attrs) ->
    lager:debug("DIAL number tag: ~s", [DialMe]),
    Props = attrs_to_proplist(Attrs),

    Call1 = send_call(Call, DialMe, Props),
    case props:get_value(sendDigits, Props) of
        undefined ->
            lager:debug("no sendDigits attributes, waiting for call to end..."),
            finish_dial(Call1, Props);
        ?NE_BINARY = SendDigits ->
            lager:debug("sendDigits: ~s", [SendDigits]),
            send_digits(Call1, SendDigits),
            finish_dial(Call1, Props)
    end;
dial_number(Call0, DialMe, Attrs) ->
    lager:debug("DIAL number: ~s", [DialMe]),
    Props = attrs_to_proplist(Attrs),

    Call1 = send_call(Call0, DialMe, Props),
    finish_dial(Call1, Props).

dial_ring_group(Call, Numbers, Attrs) ->
    lager:debug("DIAL ring group"),
    Props = attrs_to_proplist(Attrs),

    try build_ring_group_endpoints(Numbers) of
        [] ->
            lager:debug("no endpoints were created for ring group"),
            {ok, Call};
        EPs ->
            lager:debug("endpoints generated: ~p", [EPs]),

            case ring_group_bridge_req(Call, EPs, Props) of
                {ok, JObj} ->
                    RecordCall = wh_util:is_true(props:get_value(record, Props, false)),
                    StarHangup = wh_util:is_true(props:get_value(hangupOnStar, Props, false)),
                    lager:debug("call bridged, do we need record: ~s or allow *-hangup: ~s", [RecordCall, StarHangup]),
                    lager:debug("bridge resp: ~p", [JObj]),
                    {ok, update_call_status(Call, ?STATUS_ANSWERED)};
                {error, JObj} ->
                    lager:debug("error bridging: ~p", [JObj]),
                    {stop, update_call_status(Call, ?STATUS_FAILED)};
                {stop, _}=Stop ->
                    Stop
            end
    catch
        error:function_clause ->
            lager:debug("invalid tag in list of numbers"),
            {stop, update_call_status(Call, ?STATUS_FAILED)}
    end.

-spec ring_group_bridge_req/3 :: (whapps_call:call(), wh_json:json_objects(), proplist()) -> {'ok' | 'error' | 'stop', wh_json:json_object()}.
ring_group_bridge_req(Call, EPs, Props) ->
    Timeout = wh_util:to_integer(props:get_value(timeout, Props, 30)),
    CallerID = props:get_value(callerId, Props, whapps_call:caller_id_number(Call)),

    CCVs = [{<<"Account-ID">>, whapps_call:account_id(Call)}],

    Req = [{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Endpoints">>, EPs}
           ,{<<"Timeout">>, Timeout}
           ,{<<"Dial-Endpoint-Method">>, wapi_dialplan:dial_method_simultaneous()}
           ,{<<"Ignore-Early-Media">>, <<"true">>}
           ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Outgoing-Caller-ID-Number">>, CallerID}
           | wh_api:default_headers(?NAME, ?VERSION)
          ],
    whapps_call_command:send_command(Req, Call),
    wait_for_bridge_start(Call, Timeout).

-spec build_ring_group_endpoints/1 :: ([#xmlElement{},...]|[]) -> wh_json:json_objects().
build_ring_group_endpoints(Numbers) ->
    build_ring_group_endpoints(Numbers, []).
build_ring_group_endpoints([], Acc) ->
    Acc;
build_ring_group_endpoints([#xmlElement{name='Number', content=[#xmlText{value=DialMe, type=text}]}|Numbers], Acc) ->
    lager:debug("adding ~s to ring group endpoints", [DialMe]),
    EP = wh_json:from_list([{<<"Invite-Format">>, <<"route">>}
                            ,{<<"Route">>, <<"loopback/", DialMe/binary, "/context_2">>}
                            ,{<<"To-DID">>, DialMe}
                           ]),
    build_ring_group_endpoints(Numbers, [EP|Acc]).

send_digits(Call, SendDigits) ->
    lager:debug("sending DTMFs: ~s", [SendDigits]),
    Req = [{<<"Application-Name">>, <<"send_dtmf">>}
           ,{<<"DTMFs">>, SendDigits}
           ,{<<"Duration">>, 500}
          ],
    whapps_call_command:send_command(Req, Call).

send_call(Call0, DialMe, Props) ->
    Timeout = wh_util:to_integer(props:get_value(timeout, Props, 30)),
    RecordCall = wh_util:is_true(props:get_value(record, Props, false)),
    StarHangup = wh_util:is_true(props:get_value(hangupOnStar, Props, false)),
    CallerID = props:get_value(callerId, Props, whapps_call:caller_id_number(Call0)),

    Call1 = lists:foldl(fun({V, F}, C) -> F(V, C) end, Call0, [{list_to_binary([DialMe, "@norealm"]), fun whapps_call:set_request/2}
                                                               ,{CallerID, fun whapps_call:set_caller_id_number/2}
                                                              ]),

    %% remove reliance on cf_offnet...
    ok = cf_offnet:offnet_req(wh_json:from_list([{<<"Timeout">>, Timeout}
                                                 | offnet_data(RecordCall, StarHangup)
                                                ])
                              ,Call1
                             ),
    Call1.

finish_dial(Call, Props) ->
    TimeLimit = wh_util:to_integer(props:get_value(timeLimit, Props, 14400)),
    RecordCall = wh_util:is_true(props:get_value(record, Props, false)),
    StarHangup = wh_util:is_true(props:get_value(hangupOnStar, Props, false)),

    %% wait for the bridge to end
    Start = erlang:now(),
    OffnetProp = wait_for_offnet(Call, RecordCall, StarHangup, TimeLimit),
    Elapsed = wh_util:elapsed_s(Start),

    RecordingId = maybe_save_recording(Call, props:get_value(media_name, OffnetProp), RecordCall),

    OtherLeg = props:get_value(other_leg, OffnetProp),
    Status = props:get_value(call_status, OffnetProp),
    lager:debug("other leg ~s done in ~bs: ~s", [OtherLeg, Elapsed, Status]),

    case props:get_value(action, Props) of
        undefined ->
            %% if action is defined, no commands after Dial are reachable;
            %% since its not defined, we fall through to the next TwiML command
            maybe_stop(Call, ok, {ok, Call});
        Action ->
            BaseParams = wh_json:from_list([{"DialCallStatus", Status}
                                            ,{"DialCallSid", OtherLeg}
                                            ,{<<"DialCallDuration">>, Elapsed}
                                            ,{<<"RecordingUrl">>, recorded_url(Call, RecordingId, RecordCall)}
                                            | req_params(Call)
                                           ]),
            Uri = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call), Action),
            Method = wht_util:http_method(props:get_value(method, Props, post)),
            {request, Call, Uri, Method, BaseParams}
    end.

-spec pause/2 :: (whapps_call:call(), proplist()) -> {'ok', whapps_call:call()}.
pause(Call, Attrs) ->
    Props = attrs_to_proplist(Attrs),
    Length = props:get_integer_value(length, Props, 1) * 1000,

    Call1 = maybe_answer_call(Call),

    lager:debug("PAUSE: for ~b ms", [Length]),

    receive
    after Length -> {ok, Call1}
    end.

-spec play/3 :: (whapps_call:call(), ne_binary(), proplist()) -> exec_element_return().
-spec play/4 :: (whapps_call:call(), ne_binary(), proplist(), list() | binary()) -> exec_element_return().
play(Call, PlayMe, Attrs) ->
    Call1 = maybe_answer_call(Call),
    lager:debug("PLAY: ~s", [PlayMe]),
    Res = case get_loop_count(props:get_value(loop, attrs_to_proplist(Attrs), 1)) of
              %% TODO: play music in a continuous loop
              0 -> whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call1);
              1 -> whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call1);
              N when N > 1 -> play_loop(Call1, wh_util:to_binary(PlayMe), N)
          end,
    maybe_stop(Call1, Res, {ok, Call1}).

play(Call, PlayMe, Attrs, Terminators) ->
    Call1 = maybe_answer_call(Call),
    lager:debug("PLAY: ~s with terminators: ~p", [PlayMe, Terminators]),
    Res = case get_loop_count(props:get_value(loop, attrs_to_proplist(Attrs), 1)) of
              %% TODO: play music in a continuous loop
              0 -> whapps_call_command:play(wh_util:to_binary(PlayMe), Terminators, Call1);
              1 -> whapps_call_command:play(wh_util:to_binary(PlayMe), Terminators, Call1);
              N when N > 1 -> play_loop(Call1, wh_util:to_binary(PlayMe), Terminators, N)
          end,
    maybe_stop(Call1, Res, {ok, Call1}).

-spec say/3 :: (whapps_call:call(), ne_binary(), proplist()) -> exec_element_return().
-spec say/4 :: (whapps_call:call(), ne_binary(), proplist(), list() | binary()) -> exec_element_return().
say(Call, SayMe, Attrs) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),
    Voice = get_voice(props:get_value(voice, Props)),
    Lang = get_lang(props:get_value(language, Props)),

    lager:debug("SAY: ~s using voice ~s, in lang ~s", [SayMe, Voice, Lang]),

    case get_loop_count(wh_util:to_integer(props:get_value(loop, Props, 1))) of
        0 -> say_loop(Call1, fun() -> whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1) end, infinity);
        1 -> whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1);
        N -> say_loop(Call1, fun() -> whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1) end, N)
    end.

say(Call, SayMe, Attrs, Terminators) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),
    Voice = get_voice(props:get_value(voice, Props)),
    Lang = get_lang(props:get_value(language, Props)),

    lager:debug("SAY: ~s using voice ~s, in lang ~s", [SayMe, Voice, Lang]),

    case get_loop_count(wh_util:to_integer(props:get_value(loop, Props, 1))) of
        0 -> say_loop(Call1, fun() -> whapps_call_command:tts(wh_util:to_binary(SayMe), Voice, Lang, Terminators, Call1) end, infinity);
        1 -> whapps_call_command:tts(wh_util:to_binary(SayMe), Voice, Lang, Terminators, Call1);
        N -> say_loop(Call1, fun() -> whapps_call_command:tts(wh_util:to_binary(SayMe), Voice, Lang, Terminators, Call1) end, N)
    end.

collect_digits(Call, InitDigit, MaxDigits, FinishOnKey, Timeout, Props) ->
    lager:debug("GATHER: ~p max, finish: ~p with timeout ~p and init ~s", [MaxDigits, FinishOnKey, Timeout, InitDigit]),
    MaxDigitsBin = wh_util:to_binary(MaxDigits),
    case whapps_call_command:collect_digits(MaxDigitsBin, Timeout, 2000
                                            ,undefined, [FinishOnKey], Call
                                           ) of
        {ok, DTMFs} ->
            lager:debug("recv DTMFs: ~s", [DTMFs]),

            NewUri = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = wht_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, <<InitDigit/binary, DTMFs/binary>>}
                                            | req_params(Call)
                                           ]),

            {request, Call, NewUri, Method, BaseParams};
        {error, _E} ->
            lager:debug("failed to collect ~b digits, error: ~p", [MaxDigits, _E]),
            {stop, Call}
    end.

collect_until_terminator(Call, InitDigit, FinishOnKey, Timeout, Props) ->
    collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, [InitDigit]).

collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, DTMFs) ->
    case whapps_call_command:wait_for_dtmf(Timeout) of
        {ok, FinishOnKey} ->
            Digits = lists:reverse(DTMFs),
            lager:debug("recv finish key ~s, responding with ~p", [FinishOnKey, Digits]),
            NewUri = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = wht_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, iolist_to_binary(Digits)} | req_params(Call)]),

            {request, Call, NewUri, Method, BaseParams};
        {ok, <<>>} ->
            Digits = lists:reverse(DTMFs),
            lager:debug("timeout waiting for digits, working with what we got: '~s'", [Digits]),
            NewUri = wht_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = wht_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, Digits} | req_params(Call)]),

            {request, Call, NewUri, Method, BaseParams};
        {ok, Digit} ->
            lager:debug("recv dtmf ~s", [Digit]),
            collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, [Digit | DTMFs]);
        {error, _E} ->
            lager:debug("failed to collect unlimited digits, error: ~p", [_E]),
            {stop, Call}
    end.

say_loop(Call, F, infinity) ->
    case maybe_stop(Call, F(), ok) of
        ok -> say_loop(Call, F, infinity);
        Result -> Result
    end;
say_loop(Call, F, N) ->
    say_loop(Call, F, ok, N).

say_loop(_, _, Resp, 0) -> Resp;
say_loop(Call, F, _, N) ->
    Resp = F(),
    case maybe_stop(Call, Resp, Resp) of
        {stop, _}=Stop -> Stop;
        Result -> say_loop(Call, F, Result, N-1)
    end.

reject_reason(X) when not is_binary(X) ->
    reject_reason(wh_util:to_binary(X));
reject_reason(<<"busy">> = B) -> B;
reject_reason(_) -> <<"rejected">>.

play_reject_reason(Call, <<"busy">>) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"480">>, <<"620">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"500">>}
                             ]),
    whapps_call_command:tones([Tone], Call);
play_reject_reason(_Call, <<"rejected">>) ->
    ok.

reject_code(<<"busy">>) ->
    <<"486">>;
reject_code(<<"rejected">>) ->
    <<"503">>.

play_loop(_, _, 0) -> ok;
play_loop(Call, PlayMe, N) ->
    _ = whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call),
    play_loop(Call, PlayMe, N-1).

play_loop(_, _, _, 0) -> ok;
play_loop(Call, PlayMe, Terminators, N) ->
    NoopId = whapps_call_command:audio_macro([
                                              {play, wh_util:to_binary(PlayMe), Terminators}
                                             ]
                                             ,Terminators
                                             ,Call
                                            ),
    lager:debug("play loop ~b with terminators: ~p: noop: ~p", [N, Terminators, NoopId]),
    play_loop(Call, PlayMe, Terminators, N-1).

attrs_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].

maybe_answer_call(Call) ->
    case wh_util:is_true(whapps_call:kvs_fetch(<<"is_answered">>, false, Call)) of
        true -> Call;
        _ ->
            whapps_call_command:answer(Call),
            whapps_call:kvs_store_proplist([{<<"is_answered">>, true}
                                            ,{<<"call_status">>, ?STATUS_ANSWERED}
                                           ], Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% Return the Result and the Other Leg's Call-ID
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet/4 :: (whapps_call:call(), boolean(), boolean(), pos_integer()) -> proplist().
wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit) ->
    wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit * 1000, erlang:now(), []).

wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit, Start, Acc) ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    [{call_status, call_status(wh_json:get_value(<<"Response-Message">>, JObj), wh_json:get_value(<<"Response-Code">>, JObj))}
                     | Acc
                    ];
                {<<"call_event">>, <<"DTMF">>} when HangupOnStar ->
                    case wh_json:get_value(<<"DTMF-Digit">>, JObj) of
                        <<"*">> ->
                            lager:debug("recv '*' DTMF, hanging up"),
                            whapps_call_command:hangup(true, Call);
                        _DTMF ->
                            lager:debug("ignore '~s' DTMF", [_DTMF]),
                            wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit - wh_util:elapsed_ms(Start), erlang:now(), Acc)
                    end;
                {<<"call_event">>, <<"LEG_CREATED">>} ->
                    BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("b-leg created: ~s", [BLeg]),
                    wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit - wh_util:elapsed_ms(Start), erlang:now(), [{other_leg, BLeg}|Acc]);
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>} when RecordCall ->
                    OtherLegCallID = props:get_value(other_leg, Acc),
                    MediaName = media_name(whapps_call:call_id(Call), OtherLegCallID),

                    lager:debug("channel bridged, start recording the call: ~s", [MediaName]),

                    whapps_call_command:record_call(MediaName, <<"start">>, <<"remote">>, TimeLimit, Call),
                    wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit - wh_util:elapsed_ms(Start), erlang:now(), [{media_name, MediaName}|Acc]);
                _Type ->
                    lager:debug("ignore ~p", [_Type]),
                    wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit - wh_util:elapsed_ms(Start), erlang:now(), Acc)
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit - wh_util:elapsed_ms(Start), erlang:now(), Acc)
    after
        TimeLimit ->
            lager:debug("time limit for call exceeded"),
            whapps_call_command:hangup(true, Call),
            wait_for_offnet(Acc)
    end.

wait_for_offnet(Acc) ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    [{call_status, call_status(wh_json:get_value(<<"Response-Message">>, JObj), wh_json:get_value(<<"Response-Code">>, JObj))}
                     | Acc
                    ];
                _ ->
                    wait_for_offnet(Acc)
            end;
        _ ->
            wait_for_offnet(Acc)
    end.

-spec wait_for_bridge_start/2 :: (whapps_call:call(), integer()) -> {'ok' | 'error' | 'stop', wh_json:json_object()}.
wait_for_bridge_start(Call, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case wait_for_bridge_event(wh_util:get_event_type(JObj), JObj) of
                {error, _}=Err -> Err;
                {ok, _}=OK -> OK;
                ignore ->
                    wait_for_bridge_start(Call, Timeout - wh_util:elapsed_ms(Start))
            end;
        _Type ->
            lager:debug("ignore ~p", [_Type]),
            wait_for_bridge_start(Call, Timeout - wh_util:elapsed_ms(Start))
    after Timeout ->
            lager:debug("time limit for call exceeded"),
            whapps_call_command:hangup(true, Call),
            {stop, Call}
    end.

wait_for_bridge_event({<<"error">>,_}, EvtJObj) ->
    case wh_json:get_value(<<"Application-Name">>, EvtJObj) of
        <<"bridge">> -> {error, EvtJObj};
        _App ->
            lager:debug("error on application ~s", [_App]),
            ignore
    end;
wait_for_bridge_event({<<"call_event">>, <<"CHANNEL_BRIDGE">>}, EvtJObj) ->
    lager:debug("call bridge started: ~p", [EvtJObj]),
    {ok, EvtJObj};
wait_for_bridge_event({<<"call_event">>, <<"CHANNEL_DESTROY">>}, EvtJObj) ->
    lager:debug("call destroyed: ~p", [EvtJObj]),
    {ok, EvtJObj};
wait_for_bridge_event(_Type, _EvtJObj) ->
    lager:debug("ignored call event: ~p ~p", [_Type, _EvtJObj]),
    ignore.

call_status(<<"SUCCESS">>, _) ->
    <<"completed">>;
call_status(<<"NO_ANSWER">>, _) ->
    <<"no-answer">>;
call_status(_, _) ->
    <<"failed">>.

-spec offnet_data/2 :: (boolean(), boolean()) -> proplist().
offnet_data(true, _) ->
    [{<<"Media">>, <<"process">>}
    ];
offnet_data(false, true) ->
    [{<<"Media">>, <<"process">>}
    ];
offnet_data(false, false) ->
    [].

media_name(ALeg) ->
    DateTime = wh_util:pretty_print_datetime(calendar:universal_time()),
    list_to_binary([DateTime, "_", ALeg, ".mp3"]).

media_name(ALeg, BLeg) ->
    DateTime = wh_util:pretty_print_datetime(calendar:universal_time()),
    list_to_binary([DateTime, "_", ALeg, "_to_", BLeg, ".mp3"]).

recorded_url(_Call, _DocId, false) ->
    undefined;
recorded_url(Call, DocId, true) ->
    list_to_binary([<<"/v1/accounts/">>, whapps_call:account_id(Call), <<"/media/">>, DocId]).

maybe_save_recording(_Call, _MediaName, false) ->
    undefined;
maybe_save_recording(Call, MediaName, true) ->
    MediaDoc = [{<<"name">>, MediaName}
                ,{<<"description">>, <<"recording ", MediaName/binary>>}
                ,{<<"content_type">>, <<"audio/mp3">>}
                ,{<<"media_source">>, <<"recorded">>}
                ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
               ],
    AcctDb = whapps_call:account_db(Call),
    case couch_mgr:save_doc(AcctDb, wh_json:from_list(MediaDoc)) of
        {ok, JObj} -> store_recording(Call, MediaName, JObj);
        _E -> lager:debug("error saving media doc: ~p", [_E])
    end.

-spec store_recording/3 :: (whapps_call:call(), ne_binary(), wh_json:json_object()) -> ne_binary().
store_recording(Call, MediaName, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),

    Rev = wh_json:get_value(<<"_rev">>, JObj),
    StoreUrl = list_to_binary([couch_mgr:get_url(), AccountDb
                               ,"/", MediaId
                               ,"/", MediaName
                               ,"?rev=", Rev
                              ]),
    {ok, _} = whapps_call_command:b_store(MediaName, StoreUrl, Call),
    MediaId.

-spec update_call_status/2 :: (whapps_call:call(), ne_binary()) -> whapps_call:call().
update_call_status(Call, Status) ->
    whapps_call:kvs_store(<<"call_status">>, Status, Call).

-spec req_params/1 :: (whapps_call:call()) -> proplist().
req_params(Call) ->
    [{<<"CallSid">>, whapps_call:call_id(Call)}
     ,{<<"AccountSid">>, whapps_call:account_id(Call)}
     ,{<<"From">>, whapps_call:from_user(Call)}
     ,{<<"To">>, whapps_call:to_user(Call)}
     ,{<<"CallStatus">>, whapps_call:kvs_fetch(<<"call_status">>, ?STATUS_RINGING, Call)}
     ,{<<"ApiVersion">>, <<"2010-04-01">>}
     ,{<<"Direction">>, <<"inbound">>}
     ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
    ].

-spec play_tone/1 :: (whapps_call:call()) -> 'ok'.
play_tone(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    whapps_call_command:tones([Tone], Call).

maybe_stop(Call, {error, channel_hungup}, _) -> {stop, Call};
maybe_stop(_, {error, _R}, Result) ->
    lager:debug("error in result, but continuing anyway: ~p", [_R]),
    Result;
maybe_stop(_, _, Result) -> Result.

get_voice(undefined) -> <<"female">>;
get_voice(<<"man">>) -> <<"male">>;
get_voice(<<"woman">>) -> <<"female">>.

get_lang(undefined) -> <<"en-US">>;
get_lang(<<"en">>) -> <<"en-US">>;
get_lang(<<"en-gb">>) -> <<"en-GB">>;
get_lang(<<"es">> = ES) -> ES;
get_lang(<<"fr">> = FR) -> FR;
get_lang(<<"de">> = DE) -> DE.

%% contstrain loop to 10
-spec get_loop_count/1 :: (integer() | binary()) -> 0..10.
get_loop_count(N) when not is_integer(N) -> get_loop_count(wh_util:to_integer(N));
get_loop_count(N) when N =< 0 -> 0;
get_loop_count(N) when N =< 10 -> N;
get_loop_count(_) -> 10.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

