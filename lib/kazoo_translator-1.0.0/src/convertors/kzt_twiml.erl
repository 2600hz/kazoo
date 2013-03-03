%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml).

-include("../kzt.hrl").

-export([exec/2
         ,base_req_params/1
        ]).

-spec exec/2 :: (whapps_call:call(), #xmlElement{} | text()) ->
                        {'error', whapps_call:call()} |
                        {'req', whapps_call:call()} |
                        {'stop', whapps_call:call()}.
exec(Call, #xmlElement{name='Response', content=Els}) ->
    exec_elements(Call, Els);
exec(Call, Resp) ->
    try xmerl_scan:string(Resp) of
        {#xmlElement{name='Response', content=Els}, _} ->
            exec_elements(Call, Els);
        _Other ->
            lager:debug("failed to exec twiml: ~p", [_Other]),
            {error, Call}
    catch
        _E:_R ->
            lager:debug("xml extraction fail: ~s: ~p", [_E, _R]),
            {error, Call}
    end.

-spec exec_elements/2 :: (whapps_call:call(), [#xmlElement{},...]) ->
                                 {'error', whapps_call:call()} |
                                 {'req', whapps_call:call()} |
                                 {'stop', whapps_call:call()}.
exec_elements(Call, []) -> {ok, Call};
exec_elements(Call, [El|Els]) ->
    try exec_element(Call, El) of
        {ok, Call1} -> exec_elements(Call1, Els);
        {req, _Call}=REQ -> REQ;
        {error, _Call}=ERR -> ERR;
        {stop, _Call}=STOP -> STOP
    catch
        throw:{unknown_element, Name} ->
            lager:debug("unknown element in response: ~s", [Name]),
            {error, kzt_util:add_error(Call, <<"unknown_element">>, Name)};
        _E:_R ->
            lager:debug("'~s' when execing el ~p: ~p", [_E, El, _R]),
            {error, Call}
    end.

-spec exec_element/2 :: (whapps_call:call(), #xmlElement{}) ->
                                {'ok', whapps_call:call()} |
                                {'req', whapps_call:call()} |
                                {'stop', whapps_call:call()} |
                                {'error', whapps_call:call()}.
exec_element(Call, #xmlElement{name='Dial'
                               ,content=Endpoints
                               ,attributes=Attrs
                              }) ->
    dial(Call, Endpoints, Attrs);
exec_element(Call, #xmlElement{name='Record'
                               ,content=[] % nothing inside the tags please
                               ,attributes=Attrs
                              }) ->
    record_call(Call, Attrs);
exec_element(Call, #xmlElement{name='Gather'
                                  ,content=SubActions
                                  ,attributes=Attrs
                                 }) ->
    gather(Call, SubActions, Attrs);
exec_element(Call, #xmlElement{name='Play'
                                  ,content=ToPlay
                                  ,attributes=Attrs
                                 }) ->
    case play(Call, ToPlay, Attrs) of
        {ok, _}=OK -> OK;
        {error, _E, Call1} ->
            lager:debug("play stopped with error ~p", [_E]),
            {error, Call1}
    end;
exec_element(Call, #xmlElement{name='Say'
                                  ,content=ToSay
                                  ,attributes=Attrs
                                 }) ->
    case say(Call, ToSay, Attrs) of
        {ok, _}=OK -> OK;
        {error, _E, Call1} ->
            lager:debug("say stopped with error ~p", [_E]),
            {error, Call1}
    end;
exec_element(Call, #xmlElement{name='Redirect'
                                  ,content=RedirectUrl
                                  ,attributes=Attrs
                                 }) ->
    redirect(Call, RedirectUrl, Attrs);
exec_element(Call, #xmlElement{name='Pause'
                                  ,content=[]
                                  ,attributes=Attrs
                                 }) ->
    pause(Call, Attrs);
exec_element(Call, #xmlElement{name='Variable'
                                  ,content=[]
                                  ,attributes=Attrs
                                 }) ->
    set_variable(Call, Attrs);
exec_element(Call, #xmlElement{name='Hangup'
                                  ,content=[]
                                  ,attributes=[]
                                 }) ->
    hangup(Call);
exec_element(Call, #xmlElement{name='Reject'
                               ,content=[]
                               ,attributes=Attrs
                              }) ->
    reject(Call, Attrs);
exec_element(_Call, #xmlElement{name=Unknown
                               ,content=_Content
                               ,attributes=_Attrs
                              }) ->
    throw({unknown_element, Unknown}).

-spec base_req_params/1 :: (whapps_call:call()) -> wh_proplist().
base_req_params(Call) ->
    [{<<"CallSid">>, whapps_call:call_id(Call)}
     ,{<<"AccountSid">>, whapps_call:account_id(Call)}
     ,{<<"From">>, whapps_call:from_user(Call)}
     ,{<<"To">>, whapps_call:to_user(Call)}
     ,{<<"CallStatus">>, whapps_call:kvs_fetch(<<"call_status">>, ?STATUS_RINGING, Call)}
     ,{<<"ApiVersion">>, <<"2010-04-01">>}
     ,{<<"Direction">>, <<"inbound">>}
     ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
    ].

%%------------------------------------------------------------------------------
%% Verbs
%%------------------------------------------------------------------------------
dial(Call, [#xmlText{value=DialMe, type=text}], Attrs) ->
    whapps_call_command:answer(Call),
    lager:debug("dial '~s'", [DialMe]),

    Props = kzt_util:attributes_to_proplist(Attrs),

    Timeout = timeout_s(Props),
    RecordCall = should_record_call(Props),
    HangupDTMF = hangup_dtmf(Props),

    Setters = [{fun whapps_call:set_request/2, request_id(DialMe, Call)}
               ,{fun whapps_call:set_caller_id_number/2, caller_id(Props, Call)}
               ,{fun kzt_util:set_hangup_dtmf/2, HangupDTMF}
               ,{fun kzt_util:set_record_call/2, RecordCall}
               ,{fun kzt_util:set_call_timeout/2, Timeout}
               ,{fun kzt_util:set_call_time_limit/2, timelimit_s(Props)}
              ],
    Call1 = lists:foldl(fun({F, V}, C) when is_function(F, 2) -> F(V, C) end
                        ,Call
                        ,Setters
                       ),

    OffnetProps = wh_json:from_list(
                    [{<<"Timeout">>, Timeout}
                     ,{<<"Media">>, media_processing(RecordCall, HangupDTMF)}
                    ]),

    ok = kzt_util:offnet_req(OffnetProps, Call1),

    {ok, Call2} = kzt_receiver:wait_for_offnet(Call1),

    case kzt_util:get_call_status(Call2) of
        ?STATUS_COMPLETED -> {stop, Call2};
        _Status ->
            lager:debug("offnet failed: ~s", [_Status]),
            {ok, Call2} % will progress to next TwiML element
    end.

-spec hangup/1 :: (whapps_call:call()) -> {'stop', whapps_call:call()}.
hangup(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hangup(Call),
    {stop, kzt_util:update_call_status(Call, ?STATUS_COMPLETED)}.

reject(Call, Attrs) ->
    Props = kzt_util:attributes_to_proplist(Attrs),

    Reason = reject_reason(Props),
    Code = reject_code(Reason),

    _ = whapps_call_command:response(Code, Reason, reject_prompt(Props), Call),
    {stop, kzt_util:update_call_status(Call, reject_status(Code))}.

pause(Call, Attrs) ->
    whapps_call_command:answer(Call),
    Props = kzt_util:attributes_to_proplist(Attrs),

    PauseFor = pause_for(Props),
    lager:debug("pause for ~b ms", [PauseFor]),
    timer:sleep(PauseFor),
    {ok, Call}.

set_variable(Call, Attrs) ->
    whapps_call_command:answer(Call),
    Props = kzt_util:attributes_to_proplist(Attrs),
    {ok, kzt_translator:set_user_vars(
           [{props:get_binary_value(key, Props), props:get_binary_value(value, Props)}]
           ,Call
          )}.

-spec set_variables/2 :: (whapps_call:call(), list()) -> whapps_call:call().
set_variables(Call, Els) when is_list(Els) ->
    lists:foldl(fun(#xmlElement{attributes=Attrs}, C) ->
                        set_variable(C, Attrs);
                   (_, C) -> C
                end, Call, Els).

-spec say/3 :: (whapps_call:call(), list(), list()) ->
                       {'ok', whapps_call:call()} |
                       {'error', _, whapps_call:call()}.
say(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),
    SayMe = xml_text_to_binary(XmlText, whapps_config:get_integer(<<"pivot">>, <<"tts_text_size">>, ?TTS_SIZE_LIMIT)),

    Props = kzt_util:attributes_to_proplist(Attrs),

    Voice = get_voice(Props),
    Lang = get_lang(Props),
    Engine = get_engine(Props),

    lager:debug("SAY: ~s using voice ~s, in lang ~s, and engine ~s", [SayMe, Voice, Lang, Engine]),

    case loop_count(Props) of
        0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Engine, infinity);
        N when N > 0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Engine, N)
    end.

-spec play/3 :: (whapps_call:call(), list(), list()) ->
                        {'ok', whapps_call:call()} |
                        {'error', _, whapps_call:call()}.                          
play(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),
    PlayMe = xml_text_to_binary(XmlText),
    lager:debug("playing '~s'", [PlayMe]),

    Props = kzt_util:attributes_to_proplist(Attrs),

    case loop_count(Props) of
        0 -> kzt_receiver:play_loop(Call, PlayMe, infinity);
        N when N > 0 -> kzt_receiver:play_loop(Call, PlayMe, N)
    end.

redirect(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),

    Props = kzt_util:attributes_to_proplist(Attrs),

    CurrentUri = kzt_util:get_voice_uri(Call),
    RedirectUri = xml_text_to_binary(XmlText),

    Call1 = case xml_elements(XmlText) of
                [] -> Call;
                Els -> set_variables(Call, Els)
            end,

    NewUri = kzt_util:resolve_uri(CurrentUri, RedirectUri),
    Method = kzt_util:http_method(Props),

    lager:debug("redirect using ~s to ~s from ~s", [Method, NewUri, CurrentUri]),

    Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
               ,{fun kzt_util:set_voice_uri/2, NewUri}
              ],
    {req, lists:foldl(fun({F, V}, C) -> F(V, C) end, Call1, Setters)}.

gather(Call, SubActions, Attrs) ->
    case exec_elements(kzt_util:clear_digits_collected(Call)
                       ,xml_elements(SubActions)
                      )
    of
        {stop, C} -> gather(C, Attrs);
        Other ->
            lager:debug("other: ~p", [Other]),
            Other
    end.

gather(Call, Attrs) ->
    whapps_call_command:answer(Call),

    Props = kzt_util:attributes_to_proplist(Attrs),

    Timeout = timeout_s(Props, 5) * 1000,
    FinishKey = finish_dtmf(Props),

    gather(Call, FinishKey, Timeout, Props, num_digits(Props)).

gather(Call, FinishKey, Timeout, Props, N) ->
    case kzt_receiver:collect_dtmfs(Call, FinishKey, Timeout, N) of
        {ok, timeout, C} -> gather_finished(C, Props);
        {ok, dtmf_finish, C} -> gather_finished(C, Props);
        {ok, C} -> gather_finished(C, Props);
        {error, _E, _C}=ERR -> ERR;
        {stop, _C}=STOP -> STOP
    end.

gather_finished(Call, Props) ->
    case kzt_util:get_digits_collected(Call) of
        <<>> ->
            lager:debug("caller entered no digits, continuing"),
            {ok, kzt_util:clear_digits_collected(Call)};
        _DTMFs ->
            CurrentUri = kzt_util:get_voice_uri(Call),
            NewUri = kzt_util:resolve_uri(CurrentUri, action_url(Props)),
            Method = kzt_util:http_method(Props),

            lager:debug("redirect w/ ~s using ~s to ~s from ~s", [_DTMFs, Method, NewUri, CurrentUri]),

            Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
                       ,{fun kzt_util:set_voice_uri/2, NewUri}
                      ],
            {req, lists:foldl(fun({F, V}, C) -> F(V, C) end, Call, Setters)}
    end.

record_call(Call, Props) ->
    ok.

%%------------------------------------------------------------------------------
%% Nouns
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
-spec xml_text_to_binary/1 :: ([#xmlText{},...]|[]) -> binary().
xml_text_to_binary(Vs) when is_list(Vs) ->
    iolist_to_binary([V || #xmlText{value=V, type='text'} <- Vs]).

xml_text_to_binary(Vs, Size) when is_list(Vs), is_integer(Size), Size > 0 ->
    B = xml_text_to_binary(Vs),
    case byte_size(B) > Size of
        true -> erlang:binary_part(B, 0, Size);
        false -> B
    end.

-spec xml_elements/1 :: (list()) -> [] | [#xmlElement{},...].
xml_elements(Els) -> [El || #xmlElement{}=El <- Els].

-spec caller_id/2 :: (wh_proplist(), whapps_call:call()) -> ne_binary().
caller_id(Props, Call) ->
    wh_util:to_binary(
      props:get_value(callerId, Props, whapps_call:caller_id_number(Call))
     ).

timelimit_s(Props) -> props:get_integer_value(timeLimit, Props, 14400).
loop_count(Props) -> props:get_integer_value(loop, Props, 1).

request_id(N, Call) -> iolist_to_binary([N, $@, whapps_call:from_realm(Call)]).
should_record_call(Props) -> wh_util:is_true(props:get_value(record, Props, false)).

-spec hangup_dtmf/1 :: (wh_proplist() | api_binary()) -> api_binary().
hangup_dtmf(Props) when is_list(Props) ->
    case props:get_value(hangupOnStar, Props) of
        true -> <<"*">>;
        _ -> hangup_dtmf(props:get_binary_value(hangupOn, Props))
    end;
hangup_dtmf(DTMF) ->
    case lists:member(DTMF, ?ANY_DIGIT) of
        true -> DTMF;
        false -> undefined
    end.

finish_dtmf(Props) when is_list(Props) ->
    case props:get_binary_value(finishOnKey, Props) of
        undefined -> <<"#">>;
        DTMF ->
            true = lists:member(DTMF, ?ANY_DIGIT),
            DTMF
    end.

media_processing(false, undefined) -> <<"bypass">>;
media_processing(_ShouldRecord, _HangupDTMF) -> <<"process">>.

-spec get_voice/1 :: (wh_proplist()) -> ne_binary().
get_voice(Props) ->
    case props:get_binary_value(voice, Props) of
        <<"man">> -> <<"male">>;
        <<"male">> -> <<"male">>;
        <<"woman">> -> <<"female">>;
        <<"female">> -> <<"female">>;
        undefined -> <<"male">>
    end.

-spec get_lang/1 :: (wh_proplist()) -> ne_binary().
get_lang(Props) ->
    case props:get_binary_value(language, Props) of
        undefined -> <<"en-US">>;
        <<"en">> -> <<"en-US">>;
        <<"en-gb">> -> <<"en-GB">>;
        <<"es">> -> <<"es">>;
        <<"fr">> -> <<"fr">>;
        <<"de">> -> <<"de">>;
        <<"it">> -> <<"it">>
    end.

-spec get_engine/1 :: (wh_proplist()) -> ne_binary().
get_engine(Props) ->
    case props:get_binary_value(engine, Props) of
        undefined -> whapps_config:get_binary(?TTS_CONFIG_CAT, <<"tts_provider">>, <<"flite">>);
        Engine -> Engine
    end.

%% limit pause to 1 hour (3600000 ms)
-spec pause_for/1 :: (wh_proplist()) -> 1000..3600000.
pause_for(Props) ->
    case props:get_integer_value(length, Props) of
        undefined -> 1000;
        N when is_integer(N), N > 0, N =< 3600 -> N * 1000;
        N when is_integer(N), N > 3600 -> 3600000
    end.

action_url(Props) -> props:get_value(action, Props).

reject_reason(Props) ->
    case props:get_binary_value(reason, Props) of
        undefined -> <<"rejected">>;
        <<"rejected">> -> <<"rejected">>;
        <<"busy">> -> <<"busy">>
    end.

reject_code(<<"busy">>) -> <<"486">>;
reject_code(<<"rejected">>) -> <<"503">>.

reject_status(<<"486">>) -> ?STATUS_BUSY;
reject_status(<<"503">>) -> ?STATUS_NOANSWER.

reject_prompt(Props) ->
    props:get_binary_value(prompt, Props).

-spec timeout_s/1 :: (wh_proplist()) -> pos_integer().
timeout_s(Props) ->
    timeout_s(Props, 30).
timeout_s(Props, Default) ->
    case props:get_integer_value(timeout, Props, Default) of
        N when is_integer(N), N > 3600 -> 3600;
        N when is_integer(N), N > 0 -> N
    end.

-spec num_digits/1 :: (wh_proplist()) -> wh_timeout().
num_digits(Props) ->
    case props:get_integer_value(numDigits, Props) of
        undefined -> infinity;
        N when is_integer(N), N > 0 -> N
    end.
