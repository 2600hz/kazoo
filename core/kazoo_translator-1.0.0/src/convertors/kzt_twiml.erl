%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml).

-include("../kzt.hrl").

-export([exec/2
         ,parse_cmds/1
         ,req_params/1
         ,exec_gather_els/3
        ]).

-spec parse_cmds(iolist()) -> {'ok', xml_els()} |
                              {'error', 'not_parsed'}.
parse_cmds(XMLString) ->
    try xmerl_scan:string(wh_util:to_list(XMLString)) of
        {#xmlElement{name='Response'}=XML, _} -> {'ok', XML};
        _E ->
            {'error', 'not_parsed'}
    catch
        _E:_R ->
            {'error', 'not_parsed'}
    end.

-spec exec(whapps_call:call(), xml_el() | text()) ->
                  {'error', whapps_call:call()} |
                  {'request', whapps_call:call()} |
                  {'stop', whapps_call:call()}.
exec(Call, #xmlElement{name='Response', content=Els}) ->
    exec_elements(Call, Els);
exec(Call, Resp) ->
    try xmerl_scan:string(Resp) of
        {#xmlElement{name='Response', content=Els}, _} ->
            exec_elements(Call, Els);
        _Other ->
            lager:debug("failed to scan XML: ~p", [_Other]),
            {'error', Call}
    catch
        _E:_R ->
            lager:debug("xml extraction fail: ~s: ~p: ~p", [_E, _R, Resp]),
            {'error', Call}
    end.

-spec exec_elements(whapps_call:call(), xml_els()) ->
                           {'error', whapps_call:call()} |
                           {'request', whapps_call:call()} |
                           {'stop', whapps_call:call()}.
exec_elements(Call, []) -> {'ok', Call};
exec_elements(Call, [#xmlText{}=_El|Els]) ->
    exec_elements(Call, Els);
exec_elements(Call, [El|Els]) ->
    try exec_element(Call, El) of
        {'ok', Call1} -> exec_elements(Call1, Els);
        {'request', _Call}=REQ -> REQ;
        {'error', _Call}=ERR -> ERR;
        {'stop', _Call}=STOP -> STOP
    catch
        'throw':{'unknown_element', Name} ->
            lager:debug("unknown element in response: ~s", [Name]),
            {'error', kzt_util:add_error(Call, <<"unknown_element">>, Name)};
        _E:_R ->
            lager:debug("'~s' when execing el ~p: ~p", [_E, El, _R]),
            wh_util:log_stacktrace(),
            {'error', Call}
    end.

-spec exec_element(whapps_call:call(), xml_el()) ->
                          {'ok', whapps_call:call()} |
                          {'request', whapps_call:call()} |
                          {'stop', whapps_call:call()} |
                          {'error', whapps_call:call()}.
exec_element(Call, #xmlElement{name='Dial'
                               ,content=Endpoints
                               ,attributes=Attrs
                              }) ->
    kzt_twiml_dial:exec(Call, Endpoints, Attrs);
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
        {'ok', _}=OK -> OK;
        {'error', _E, Call1} ->
            lager:debug("play stopped with error ~p", [_E]),
            {'error', Call1}
    end;
exec_element(Call, #xmlElement{name='Say'
                               ,content=ToSay
                               ,attributes=Attrs
                              }) ->
    case kzt_twiml_say:exec(Call, ToSay, Attrs) of
        {'ok', _}=OK -> OK;
        {'error', _E, Call1} ->
            lager:debug("say stopped with error ~p", [_E]),
            {'error', Call1}
    end;
exec_element(Call, #xmlElement{name='Redirect'
                               ,content=Url
                               ,attributes=Attrs
                              }) ->
    redirect(Call, Url, Attrs);
exec_element(Call, #xmlElement{name='Pause'
                               ,content=[]
                               ,attributes=Attrs
                              }) ->
    pause(Call, Attrs);
exec_element(Call, #xmlElement{name='Set'
                               ,content=Els
                              }) ->
    set_variables(Call, Els);
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
    throw({'unknown_element', Unknown});
exec_element(Call, _Xml) ->
    lager:debug("unhandled XML object: ~p", [_Xml]),
    {'ok', Call}.

-spec req_params(whapps_call:call()) -> wh_proplist().
req_params(Call) ->
    props:filter_undefined(
      [{<<"CallSid">>, whapps_call:call_id(Call)}
       ,{<<"AccountSid">>, whapps_call:account_id(Call)}
       ,{<<"From">>, whapps_call:from_user(Call)}
       ,{<<"FromRealm">>, whapps_call:from_realm(Call)}
       ,{<<"To">>, whapps_call:to_user(Call)}
       ,{<<"ToRealm">>, whapps_call:to_realm(Call)}
       ,{<<"CallStatus">>, kzt_util:get_call_status(Call)}
       ,{<<"ApiVersion">>, <<"2010-04-01">>}
       ,{<<"Direction">>, <<"inbound">>}
       ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
       ,{<<"CallerNumber">>, whapps_call:caller_id_number(Call)}
       ,{<<"RecordingUrl">>, kzt_util:get_recording_url(Call)}
       ,{<<"RecordingDuration">>, kzt_util:get_recording_duration(Call)}
       ,{<<"RecordingSid">>, kzt_util:get_recording_sid(Call)}
       ,{<<"Digits">>, kzt_util:get_digit_pressed(Call)}
       ,{<<"TranscriptionSid">>, kzt_util:get_transcription_sid(Call)}
       ,{<<"TranscriptionText">>, kzt_util:get_transcription_text(Call)}
       ,{<<"TranscriptionStatus">>, kzt_util:get_transcription_status(Call)}
       ,{<<"TranscriptionUrl">>, kzt_util:get_transcription_url(Call)}
      ]).

%%------------------------------------------------------------------------------
%% Verbs
%%------------------------------------------------------------------------------
-spec hangup(whapps_call:call()) ->
                    {'stop', whapps_call:call()}.
hangup(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hangup(Call),
    {'stop', kzt_util:update_call_status(?STATUS_COMPLETED, Call)}.

-spec reject(whapps_call:call(), xml_attribs()) ->
                    {'stop', whapps_call:call()}.
reject(Call, Attrs) ->
    Props = kz_xml:attributes_to_proplist(Attrs),

    Reason = kzt_twiml_util:reject_reason(Props),
    Code = kzt_twiml_util:reject_code(Reason),

    lager:debug("rejecting call with ~s(~s)", [Reason, Code]),
    _ = whapps_call_command:response(Code, Reason, kzt_twiml_util:reject_prompt(Props), Call),
    {'stop', kzt_util:update_call_status(kzt_twiml_util:reject_status(Code), Call)}.

-spec pause(whapps_call:call(), xml_attribs()) ->
                   {'ok', whapps_call:call()}.
pause(Call, Attrs) ->
    whapps_call_command:answer(Call),
    Props = kz_xml:attributes_to_proplist(Attrs),

    PauseFor = kzt_twiml_util:pause_for(Props),
    lager:debug("pause for ~b ms", [PauseFor]),
    timer:sleep(PauseFor),
    {'ok', Call}.

-spec set_variable(whapps_call:call(), xml_attribs()) ->
                          {'ok', whapps_call:call()}.
set_variable(Call, Attrs) ->
    whapps_call_command:answer(Call),
    Props = kz_xml:attributes_to_proplist(Attrs),
    {'ok', kzt_translator:set_user_vars(
             [{props:get_binary_value('key', Props), props:get_binary_value('value', Props)}]
             ,Call
            )}.

-spec set_variables(whapps_call:call(), xml_els()) -> whapps_call:call().
set_variables(Call, Els) when is_list(Els) ->
    lists:foldl(fun(#xmlElement{name='Variable'
                                ,attributes=Attrs
                               }, C) ->
                        set_variable(C, Attrs);
                   (_, C) -> C
                end, Call, Els).

-spec play(whapps_call:call(), xml_els() | xml_texts(), xml_attribs()) ->
                  {'ok', whapps_call:call()} |
                  {'error', _, whapps_call:call()}.
play(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),
    PlayMe = kz_xml:texts_to_binary(XmlText),
    lager:info("PLAY '~s'", [PlayMe]),

    Props = kz_xml:attributes_to_proplist(Attrs),
    Terminators = kzt_twiml_util:get_terminators(Props),

    case kzt_twiml_util:loop_count(Props) of
        0 -> kzt_receiver:play_loop(Call, PlayMe, Terminators, 'infinity');
        N when N > 0 -> kzt_receiver:play_loop(Call, PlayMe, Terminators, N)
    end.

-spec redirect(whapps_call:call(), xml_els() | xml_texts(), xml_attribs()) ->
                      {'request', whapps_call:call()}.
redirect(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),

    Props = kz_xml:attributes_to_proplist(Attrs),

    CurrentUri = kzt_util:get_voice_uri(Call),

    RedirectUri = kz_xml:texts_to_binary(XmlText),

    Call1 = case kz_xml:elements(XmlText) of
                [] -> Call;
                Els -> set_variables(Call, Els)
            end,

    NewUri = kzt_util:resolve_uri(CurrentUri, RedirectUri),
    Method = kzt_util:http_method(Props),

    Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
               ,{fun kzt_util:set_voice_uri/2, NewUri}
              ],
    {'request', lists:foldl(fun({F, V}, C) -> F(V, C) end, Call1, Setters)}.

-spec exec_gather_els(pid(), whapps_call:call(), xml_els()) -> 'ok'.
exec_gather_els(_Parent, _Call, []) ->
    lager:info("finished gather sub elements");
exec_gather_els(Parent, Call, [SubAction|SubActions]) ->
    whapps_call:put_callid(Call),

    case exec_element(Call, SubAction) of
        {'stop', _} -> lager:debug("sub els stopping");
        {'error', _} -> lager:debug("sub els erroring");
        {'ok', Call1} -> exec_gather_els(Parent, Call1, SubActions)
    end.

-spec exec_gather_els(whapps_call:call(), xml_els()) ->
                             {'ok', whapps_call:call()}.
exec_gather_els(Call, SubActions) ->
    {_Pid, _Ref}=PidRef =
        spawn_monitor(?MODULE, 'exec_gather_els', [self(), Call, SubActions]),
    lager:debug("started to exec gather els: ~p(~p)", [_Pid, _Ref]),
    {'ok', kzt_util:set_gather_pidref(PidRef, Call)}.

-spec gather(whapps_call:call(), xml_els(), xml_attribs()) ->
                    kzt_receiver:collect_dtmfs_return().
gather(Call, [], Attrs) -> gather(Call, Attrs);
gather(Call, SubActions, Attrs) ->
    lager:info("GATHER: exec sub actions"),
    {'ok', C} = exec_gather_els(kzt_util:clear_digits_collected(Call)
                                ,kz_xml:elements(SubActions)
                               ),
    gather(C, Attrs).

-spec gather(whapps_call:call(), xml_attribs()) ->
                    kzt_receiver:collect_dtmfs_return().
gather(Call, Attrs) ->
    whapps_call_command:answer(Call),

    Props = kz_xml:attributes_to_proplist(Attrs),

    Timeout = kzt_twiml_util:timeout_s(Props, 5) * ?MILLISECONDS_IN_SECOND,
    FinishKey = kzt_twiml_util:finish_dtmf(Props),

    gather(Call, FinishKey, Timeout, Props, kzt_twiml_util:num_digits(Props)).

-spec gather(whapps_call:call(), api_binary(), wh_timeout(), wh_proplist(), pos_integer()) ->
                    {'ok', whapps_call:call()} |
                    {'request', whapps_call:call()} |
                    {'error', _, whapps_call:call()} |
                    {'stop', whapps_call:call()}.
gather(Call, FinishKey, Timeout, Props, N) ->
    case kzt_receiver:collect_dtmfs(Call, FinishKey, Timeout, N, fun on_first_dtmf/1) of
        {'ok', 'timeout', C} -> gather_finished(C, Props);
        {'ok', 'dtmf_finish', C} -> gather_finished(C, Props);
        {'ok', C} -> gather_finished(C, Props);
        {'stop', _C}=STOP -> STOP
    end.

-spec on_first_dtmf(whapps_call:call()) -> 'ok' | 'stop'.
on_first_dtmf(Call) ->
    case kzt_util:get_gather_pidref(Call) of
        'undefined' -> 'ok';
        {Pid, Ref} ->
            erlang:demonitor(Ref, ['flush']),
            lager:debug("first dtmf recv, stopping ~p(~p)", [Pid, Ref]),
            exit(Pid, 'kill')
    end.

-spec gather_finished(whapps_call:call(), wh_proplist()) ->
                             {'ok', whapps_call:call()} |
                             {'request', whapps_call:call()}.
gather_finished(Call, Props) ->
    case kzt_util:get_digits_collected(Call) of
        <<>> ->
            lager:info("caller entered no digits, continuing"),
            {'ok', kzt_util:clear_digits_collected(Call)};
        _DTMFs ->
            lager:info("caller entered DTMFs: ~s", [_DTMFs]),
            CurrentUri = kzt_util:get_voice_uri(Call),
            NewUri = kzt_util:resolve_uri(CurrentUri, kzt_twiml_util:action_url(Props)),
            Method = kzt_util:http_method(Props),

            Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
                       ,{fun kzt_util:set_voice_uri/2, NewUri}
                      ],
            {'request', whapps_call:exec(Setters, Call)}
    end.

record_call(Call, Attrs) ->
    Props = kz_xml:attributes_to_proplist(Attrs),
    Timeout = kzt_twiml_util:timeout_s(Props, 5),
    FinishOnKey = kzt_twiml_util:get_finish_key(Props),
    MaxLength = kzt_twiml_util:get_max_length(Props),

    MediaName = media_name(Call),

    lager:info("RECORD: ~s for at most ~b s", [MediaName, MaxLength]),

    case props:is_true('playBeep', Props, 'true') of
        'true' -> play_beep(Call);
        'false' -> 'ok'
    end,

    whapps_call_command:record(MediaName, FinishOnKey, MaxLength, 200, Timeout, Call),

    case kzt_receiver:record_loop(Call, Timeout) of
        {'ok', Call1} -> finish_record_call(Call1, Props, MediaName);
        {'empty', Call1} -> {'ok', Call1};
        _E -> lager:debug("call record failed: ~p", [_E]), {'stop', Call}
    end.

-spec finish_record_call(whapps_call:call(), wh_proplist(), ne_binary()) ->
                                {'request', whapps_call:call()}.
finish_record_call(Call, Props, MediaName) ->
    CurrentUri = kzt_util:get_voice_uri(Call),
    NewUri = kzt_util:resolve_uri(CurrentUri, kzt_twiml_util:action_url(Props)),
    Method = kzt_util:http_method(Props),

    lager:info("recording of ~s finished; using method '~s' to ~s from ~s", [MediaName, Method, NewUri, CurrentUri]),

    Setters = [{fun kzt_util:set_voice_uri_method/2, Method}
               ,{fun kzt_util:set_voice_uri/2, NewUri}
              ],

    RecordingUrl = props:get_value('recordingUrl', Props, NewUri),
    Setters1 =
        case should_store_recording(RecordingUrl) of
            'false' ->
                lager:info("not storing the recording"),
                Setters;
            {'true', 'local'} ->
                {'ok', MediaJObj} = kzt_receiver:recording_meta(Call, MediaName),
                StoreUrl = wapi_dialplan:local_store_url(Call, MediaJObj),

                lager:info("storing ~s locally to ~s", [MediaName, StoreUrl]),

                whapps_call_command:store(MediaName, StoreUrl, Call),
                [{fun kzt_util:set_recording_url/2, StoreUrl}
                 | Setters
                ];
            {'true', Url} ->
                StoreUrl = wapi_dialplan:offsite_store_url(Url, MediaName),

                lager:info("storing ~s offsite to ~s", [MediaName, StoreUrl]),

                whapps_call_command:store(MediaName, StoreUrl, Call),
                [{fun kzt_util:set_recording_url/2, StoreUrl}
                 | Setters]
        end,
    {'request', lists:foldl(fun({F, V}, C) -> F(V, C) end, Call, Setters1)}.

-spec should_store_recording(api_binary()) -> {'true', ne_binary() | 'local'} | 'false'.
should_store_recording(Url) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' when is_binary(Url) -> {'true', Url};
        'true' -> {'true', 'local'};
        'false' when is_binary(Url) -> {'true', Url};
        'false' -> 'false'
    end.

play_beep(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    whapps_call_command:tones([Tone], Call).

media_name(Call) ->
    Format = whapps_config:get(<<"callflow">>, [<<"call_recording">>, <<"extension">>], <<"mp3">>),
    <<"call_recording_", (whapps_call:call_id(Call))/binary, ".", Format/binary>>.

%%------------------------------------------------------------------------------
%% Nouns
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
