%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Aims to recreate the TwiML interaction, converting the TwiML XML
%%% to Kazoo JSON commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%% @todo
%%%   Finish support for Number tag
%%%   Record verb
%%%-------------------------------------------------------------------
-module(kzt_twiml).

-export([does_recognize/1, exec/2, req_params/1]).

-include("kzt.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(NAME, <<"kzt_twiml">>).
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
exec_element(Call, 'Dial', [#xmlElement{name='Conference'}=El1], #xmlElement{attributes=Attrs}) ->
    dial_conference(Call, El1, Attrs);
exec_element(Call, 'Dial', [#xmlElement{}|_]=Numbers, #xmlElement{attributes=Attrs}) ->
    dial_ring_group(Call, Numbers, Attrs);

exec_element(Call, 'Record', [], #xmlElement{attributes=Attrs}) ->
    record_call(Call, Attrs);
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

    NewUri = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call1), Url),
    Method = kzt_util:http_method(props:get_value(method, Props, post)),

    lager:debug("Redirect: using ~s to ~s(~s)", [Method, NewUri, Url]),

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
    _ = whapps_call_command:response(reject_code(Reason), Reason, Call),
    {stop, update_call_status(Call, ?STATUS_BUSY)}.

%%-------------------------------------------------------------------------
%% @doc Variable
%%   name  | the name of the variable
%%   value | the value of the variable
%%-------------------------------------------------------------------------
-spec set_variable/3 :: (whapps_call:call(), wh_json:json_string(), wh_json:json_term()) -> whapps_call:call().
-spec set_variables/2 :: (whapps_call:call(), list()) -> whapps_call:call().
set_variable(Call, Key, Value) ->
    kzt_translator:set_user_vars([{wh_util:to_binary(Key)
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
        Vars -> kzt_translator:set_user_vars(Vars, Call)
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

    case props:get_is_true(playBeep, Props, true) of
        true -> play_tone(Call1);
        false -> ok
    end,

    MediaName = media_name(whapps_call:call_id(Call1)),
    {ok, MediaJObj} = store_recording_meta(Call1, MediaName),

    MaxLength = props:get_integer_value(maxLength, Props, 3600),
    Timeout = props:get_integer_value(timeout, Props, 5),

    case whapps_call_command:b_record(MediaName, terminators(props:get_value(finishOnKey, Props))
                                      ,MaxLength, 200, Timeout, Call1) of
        {ok, Msg} ->
            lager:debug("record msg: ~p", [Msg]),
            case record_terminated_by(Msg) of
                {hangup, Length} when Length < 1000 ->
                    lager:debug("hangup ended call, but length(~b ms) is < 1000ms", [Length]),
                    _ = couch_mgr:del_doc(whapps_call:account_db(Call1), wh_json:get_value(<<"_id">>, MediaJObj)),
                    {ok, Call1};
                {hangup, Length} ->
                    lager:debug("hangup ended call, saving recording"),
                    store_and_send_recording(Call1, MediaJObj, undefined, Props, Length);
                {silence, Length} when Length < Timeout * 1000 ->
                    lager:debug("silence hits ended call and length (~b ms) is less than timeout (~b ms)", [Length, Timeout * 1000]),
                    _ = couch_mgr:del_doc(whapps_call:account_db(Call1), wh_json:get_value(<<"_id">>, MediaJObj)),
                    {ok, Call1};
                {silence, Length} ->
                    lager:debug("silence hits ended, but length (~b ms) is > timeout (~b ms)", [Length, Timeout * 1000]),
                    store_and_send_recording(Call1, MediaJObj, undefined, Props, Length);
                {terminator, _T, Length} when Length < 1000 ->
                    lager:debug("terminator(~s) ended call, but length(~b ms) is < 1000ms", [_T, Length]),
                    _ = couch_mgr:del_doc(whapps_call:account_db(Call1), wh_json:get_value(<<"_id">>, MediaJObj)),
                    {ok, Call1};
                {terminator, T, Length} ->
                    lager:debug("terminator ~s ended call, saving recording", [T]),
                    store_and_send_recording(Call1, MediaJObj, T, Props, Length)
            end;
        {error, R} ->
            lager:debug("error while attempting to record a new message: ~p", [R]),
            {stop, update_call_status(Call1, ?STATUS_CANCELED)}
    end.

-spec record_terminated_by/1 :: (wh_json:json_object()) ->
                                        {'silence', integer()} |
                                        {'hangup', integer()} |
                                        {'terminator', ne_binary(), integer()}.
record_terminated_by(JObj) ->
    Length = wh_json:get_integer_value(<<"Length">>, JObj),
    case wh_json:is_true(<<"Silence-Terminated">>, JObj) of
        true -> {silence, Length};
        false ->
            case wh_json:get_value(<<"Terminator">>, JObj) of
                undefined -> {hangup, Length};
                Term -> {terminator, Term, Length}
            end
    end.

-spec terminators/1 :: ('undefined' | string() | ne_binary()) -> [ne_binary(),...].
terminators(undefined) -> ?ANY_DIGIT;
terminators(?NE_BINARY = T) -> [ <<D>> || <<D>> <= T];
terminators([_|_]=L) -> terminators(wh_util:to_binary(L)).

-spec store_and_send_recording/5 :: (whapps_call:call(), wh_json:json_object(), ne_binary() | 'undefined'
                                     ,wh_proplist(), pos_integer()) -> request_return().
store_and_send_recording(Call, MediaJObj, DTMF, Props, Length) ->
    {RecordingId, _StoreJObj} = maybe_save_recording(Call, MediaJObj, true),

    lager:debug("recording of ~b ms finished, stored as ~s", [Length, RecordingId]),

    Action = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                  ,props:get_value(action, Props)
                                 ),
    Method = kzt_util:http_method(props:get_value(method, Props, post)),
    lager:debug("sending data to ~s(~s)", [Action, Method]),

    %% Transcribe = props:get_is_true(transcribe, Props, false),
    %% TranscribeCallback = props:get_value(transcribeCallback, Props),

    BaseParams = wh_json:from_list(
                   props:filter_undefined(
                     [{<<"RecordingUrl">>, recorded_url(Call, RecordingId, true)}
                      ,{<<"RecordingDuration">>, Length div 1000} % convert to seconds
                      ,{<<"Digits">>, DTMF}
                      | req_params(Call)
                     ]
                    )),
    {request, update_call_status(Call, ?STATUS_ANSWERED), Action, Method, BaseParams}.

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

-spec exec_gather_element/4 :: (whapps_call:call(), atom(), list(), #xmlElement{}) ->
                                       {'ok', binary(), whapps_call:call()} |
                                       {'ok', whapps_call:call()} |
                                       {'error', atom() | wh_json:json_object(), whapps_call:call()}.
exec_gather_element(Call, 'Say', [#xmlText{value=SayMe, type=text}], #xmlElement{attributes=Attrs}) ->    
    Result = say(Call, SayMe, Attrs, ?ANY_DIGIT),
    lager:debug("say returned: ~p", [Result]),
    maybe_stop(Call, Result, {ok, Call});

exec_gather_element(Call, 'Play', [#xmlText{value=PlayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    Call1 = case play(Call, PlayMe, Attrs, ?ANY_DIGIT) of
                {ok, Call0} -> Call0;
                {stop, Call0} -> Call0
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
%%                | <Conference>             |
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc Conference
%%   muted                  | true, false            | false
%%   beep                   | true, false            | true
%%   startConferenceOnEnter | true, false            | true
%%   endConferenceOnExit    | true, false            | false
%%   waitUrl                | URL, empty string      | Default hold music
%%   waitMethod             | GET, POST              | POST
%%   maxParticipants        | int <= 40              | 40
%%
%%   2600Hz extensions
%%   entryPin               | pin # to enter         | empty (no pin)
%%
%%   value                  | Name of the conference | string
%%-------------------------------------------------------------------------
dial_conference(Call
                ,#xmlElement{name='Conference'
                             ,content=[#xmlText{value=ConfRoom, type=text}]
                             ,attributes=ConfAttrs
                            }
                ,DialAttrs) ->
    lager:debug("DIAL conference room: ~s", [ConfRoom]),

    connect_caller_to_conference(Call, wh_util:to_binary(ConfRoom), attrs_to_proplist(ConfAttrs)),

    finish_dial(Call, DialAttrs).

-spec connect_caller_to_conference/3 :: (whapps_call:call(), ne_binary(), wh_proplist()) -> 'ok'.
connect_caller_to_conference(Call, ConfRoom, ConfProps) ->
    Command = [{<<"Call">>, whapps_call:to_json(Call)}
               ,{<<"Conference-Doc">>, conference_config(ConfRoom, ConfProps)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],

    wapi_conference:publish_discovery_req(Command),

    lager:debug("conference discovery sent for conf ~s", [ConfRoom]).

-spec conference_config/2 :: (ne_binary(), wh_proplist()) -> wh_json:json_object().
conference_config(ConfRoom, ConfProps) ->
    MaxParticipants = conf_max_participants(props:get_integer_value('maxParticipants', ConfProps, 40)),
    StartConferenceOnEnter = props:get_is_true('startConferenceOnEnter', ConfProps, true),

    ConfDoc = [{<<"name">>, ConfRoom}
               ,{<<"play_name_on_join">>, <<"true">>}
               ,{<<"conference_numbers">>, []}
               ,{<<"wait_for_moderator">>, <<"true">>}
               ,{<<"require_moderator">>, <<"true">>}
               ,{<<"max_members">>, MaxParticipants}
              ],

    case (StartConferenceOnEnter =:= true) of % if the conf can begin when the caller enters
        true -> wh_json:from_list([{<<"moderator">>, caller_config(ConfProps, false)} | ConfDoc]);
        false -> wh_json:from_list([{<<"member">>, caller_config(ConfProps, true)} | ConfDoc])
    end.

-spec caller_config/2 :: (wh_proplist(), boolean()) -> wh_json:json_object().
caller_config(ConfProps, JoinDeaf) ->
    WaitUrl = conf_wait_url(props:get_value('waitUrl', ConfProps)),
    WaitMethod = kzt_util:http_method(props:get_value('waitMethod', ConfProps, post)),
    EndConferenceOnExit = props:get_is_true('endOnConferenceOnExit', ConfProps, false),
    EntryPin = conf_entry_pin(props:get_value('entryPin', ConfProps)),

    wh_json:from_list([{<<"join_muted">>, props:get_is_true('muted', ConfProps, false)}
                       ,{<<"join_deaf">>, JoinDeaf}
                       ,{<<"pins">>, EntryPin}
                       ,{<<"play_name">>, WaitUrl =/= <<"none">>}
                       ,{<<"play_beep">>, props:get_is_true('beep', ConfProps, true)}
                       ,{<<"hold_music">>, WaitUrl}
                       ,{<<"hold_music_method">>, WaitMethod}
                       ,{<<"end_conference_on_hangup">>, EndConferenceOnExit}
                      ]).

-spec conf_entry_pin/1 :: ('undefined' | binary()) -> [] | [ne_binary()].
conf_entry_pin(undefined) -> [];
conf_entry_pin(<<>>) -> [];
conf_entry_pin(?NE_BINARY = Pin) -> [Pin].

-spec conf_max_participants/1 :: (1..40) -> 1..40.
conf_max_participants(X) when X < 1 -> throw({error, max_participants_too_low});
conf_max_participants(X) when X > 40 -> throw({error, max_participants_too_high});
conf_max_participants(X) when is_integer(X) -> X.

-define(DEFAULT_HOLD_MUSIC, <<"http://twimlets.com/holdmusic?Bucket=com.twilio.music.classical">>).
-spec conf_wait_url/1 :: ('undefined' | binary()) -> ne_binary().
conf_wait_url(undefined) -> ?DEFAULT_HOLD_MUSIC;
conf_wait_url(<<>>) -> <<"none">>;
conf_wait_url(Url) -> Url. % can return audio data or TwiML

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

-spec dial_ring_group/3 :: (whapps_call:call(), list(), proplist()) -> ok_return() | stop_return().
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

-spec ring_group_bridge_req/3 :: (whapps_call:call(), wh_json:json_objects(), proplist()) ->
                                         {'ok' | 'error', wh_json:json_object()} |
                                         stop_return().
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

    ok = kzt_util:offnet_req(wh_json:from_list([{<<"Timeout">>, Timeout}
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

    {RecordingId, _StoreJObj} = maybe_save_recording(Call, props:get_value(media_jobj, OffnetProp), RecordCall),

    lager:debug("offnet: ~p", [OffnetProp]),

    OtherLeg = props:get_value(other_leg, OffnetProp),
    Status = props:get_value(call_status, OffnetProp),
    lager:debug("other leg ~s done in ~bs: ~s", [OtherLeg, Elapsed, Status]),

    case props:get_value(action, Props) of
        undefined ->
            %% if action is defined, no commands after Dial are reachable;
            %% since its not defined, we fall through to the next TwiML command
            maybe_stop(Call, ok, {ok, Call});
        Action ->
            BaseParams = wh_json:from_list(
                           props:filter_empty([{"DialCallStatus", Status}
                                               ,{"DialCallSid", OtherLeg}
                                               ,{"ParentCallSid", whapps_call:call_id(Call)}
                                               ,{<<"DialCallDuration">>, Elapsed}
                                               ,{<<"RecordingUrl">>, recorded_url(Call, RecordingId, RecordCall)}
                                               ,{<<"RecordingDuration">>, Elapsed}
                                               | req_params(Call)
                                              ])
                          ),
            Uri = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call), Action),
            Method = kzt_util:http_method(props:get_value(method, Props, post)),
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

-spec play/3 :: (whapps_call:call(), ne_binary(), proplist()) -> ok_return() | stop_return().
-spec play/4 :: (whapps_call:call(), ne_binary(), proplist(), list() | binary()) ->
                        ok_return() | stop_return().
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

-spec say/3 :: (whapps_call:call(), ne_binary(), proplist()) -> ok_return() | stop_return().
-spec say/4 :: (whapps_call:call(), ne_binary(), proplist(), list() | binary()) ->
                       ok_return() | stop_return().
say(Call, SayMe, Attrs) ->
    Call1 = maybe_answer_call(Call),
    Props = attrs_to_proplist(Attrs),
    Voice = get_voice(props:get_value(voice, Props)),
    Lang = get_lang(props:get_value(language, Props)),

    lager:debug("SAY: ~s using voice ~s, in lang ~s", [SayMe, Voice, Lang]),

    Res = case get_loop_count(wh_util:to_integer(props:get_value(loop, Props, 1))) of
              0 ->
                  lager:debug("looping for ever (loop: 0)"),
                  say_loop(Call1, fun() ->
                                          whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1)
                                  end, infinity);
              1 ->
                  lager:debug("say once"),
            whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1);
              N ->
                  lager:debug("saying, then looping ~b more times", [N-1]),
                  say_loop(Call1, fun() ->
                                          whapps_call_command:b_tts(wh_util:to_binary(SayMe), Voice, Lang, Call1)
                                  end, N)
          end,
    maybe_stop(Call1, Res, {ok, Call1}).

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
        {ok, <<>>} ->
            lager:debug("no dtmfs received, moving to next verb"),
            {ok, Call};
        {ok, DTMFs} ->
            lager:debug("recv DTMFs: ~s", [DTMFs]),

            NewUri = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = kzt_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, <<InitDigit/binary, DTMFs/binary>>}
                                            | req_params(Call)
                                           ]),

            {request, Call, NewUri, Method, BaseParams};
        {error, _E} ->
            lager:debug("failed to collect ~b digits, error: ~p", [MaxDigits, _E]),
            {stop, Call}
    end.

collect_until_terminator(Call, <<>>, FinishOnKey, Timeout, Props) ->
    collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, []);
collect_until_terminator(Call, InitDigit, FinishOnKey, Timeout, Props) ->
    collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, [InitDigit]).

collect_until_terminator_1(Call, FinishOnKey, Timeout, Props, DTMFs) ->
    case whapps_call_command:wait_for_dtmf(Timeout) of
        {ok, FinishOnKey} ->
            Digits = lists:reverse(DTMFs),
            lager:debug("recv finish key ~s, responding with ~p", [FinishOnKey, Digits]),
            NewUri = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = kzt_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, iolist_to_binary(Digits)} | req_params(Call)]),

            {request, Call, NewUri, Method, BaseParams};
        {ok, <<>>} when DTMFs =/= [] ->
            Digits = lists:reverse(DTMFs),
            lager:debug("timeout waiting for digits, working with what we got: '~s'", [Digits]),
            NewUri = kzt_util:resolve_uri(whapps_call:kvs_fetch(<<"voice_uri">>, Call)
                                          ,props:get_value(action, Props)
                                         ),
            Method = kzt_util:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, Digits} | req_params(Call)]),

            {request, Call, NewUri, Method, BaseParams};
        {ok, <<>>} ->
            lager:debug("no dtmfs collected, going to next verb"),
            {ok, Call};
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

-spec play_loop/3 :: (whapps_call:call(), ne_binary(), non_neg_integer()) -> 'ok'.
-spec play_loop/4 :: (whapps_call:call(), ne_binary(), list(), non_neg_integer()) -> 'ok'.
play_loop(_, _, 0) -> ok;
play_loop(Call, PlayMe, N) ->
    _ = whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call),
    play_loop(Call, PlayMe, N-1).

play_loop(_, _, _, 0) -> ok;
play_loop(Call, PlayMe, Terminators, N) ->
    NoopId = whapps_call_command:audio_macro([{play, wh_util:to_binary(PlayMe), Terminators}]
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
-spec wait_for_offnet/4 :: (whapps_call:call(), boolean(), boolean(), pos_integer()) ->
                                   wh_proplist().
wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit) ->
    wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit * 1000, erlang:now(), []).

wait_for_offnet(Call, HangupOnStar, RecordCall, TimeLimit, Start, Acc) ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
                    RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
                    [{call_status, call_status(RespMsg, RespCode)} | Acc];
                {<<"call_event">>, <<"DTMF">>} when HangupOnStar ->
                    case wh_json:get_value(<<"DTMF-Digit">>, JObj) of
                        <<"*">> ->
                            lager:debug("recv '*' DTMF, hanging up"),
                            whapps_call_command:hangup(true, Call);
                        _DTMF ->
                            lager:debug("ignore '~s' DTMF", [_DTMF]),
                            wait_for_offnet(Call, HangupOnStar, RecordCall
                                            ,TimeLimit - wh_util:elapsed_ms(Start)
                                            ,erlang:now(), Acc
                                           )
                    end;
                {<<"call_event">>, <<"LEG_CREATED">>} ->
                    BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("b-leg created: ~s", [BLeg]),
                    wait_for_offnet(Call, HangupOnStar, RecordCall
                                    ,TimeLimit - wh_util:elapsed_ms(Start)
                                    ,erlang:now(), [{other_leg, BLeg}|Acc]
                                   );
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>} when RecordCall ->
                    OtherLegCallID = props:get_value(other_leg, Acc),
                    MediaName = media_name(whapps_call:call_id(Call), OtherLegCallID),

                    lager:debug("channel bridged, start recording the call: ~s", [MediaName]),

                    {ok, MediaJObj} = store_recording_meta(Call, MediaName),
                    whapps_call_command:record_call(MediaName
                                                    ,<<"start">>
                                                    ,TimeLimit
                                                    ,Call
                                                   ),
                    wait_for_offnet(Call, HangupOnStar, RecordCall
                                    ,TimeLimit - wh_util:elapsed_ms(Start)
                                    ,erlang:now(), [{media_jobj, MediaJObj}|Acc]
                                   );
                _Type ->
                    lager:debug("ignore ~p", [_Type]),
                    wait_for_offnet(Call, HangupOnStar, RecordCall
                                    ,TimeLimit - wh_util:elapsed_ms(Start)
                                    ,erlang:now(), Acc
                                   )
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_offnet(Call, HangupOnStar, RecordCall
                            ,TimeLimit - wh_util:elapsed_ms(Start)
                            ,erlang:now(), Acc
                           )
    after
        TimeLimit ->
            lager:debug("time limit for call exceeded"),
            whapps_call_command:hangup(true, Call),
            wait_for_hangup(Acc)
    end.

-spec wait_for_hangup/1 :: (wh_proplist()) -> wh_proplist().
wait_for_hangup(Acc) ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    RespMsg = wh_json:get_value(<<"Response-Message">>, JObj),
                    RespCode = wh_json:get_value(<<"Response-Code">>, JObj),
                    [{call_status, call_status(RespMsg, RespCode)} | Acc];
                _ ->
                    wait_for_hangup(Acc)
            end;
        _ ->
            wait_for_hangup(Acc)
    end.

-spec wait_for_bridge_start/2 :: (whapps_call:call(), pos_integer()) ->
                                         {'ok' | 'error', wh_json:json_object()} |
                                         {'stop', whapps_call:call()}.
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

-spec recorded_url/1 :: (ne_binary()) -> ne_binary().
-spec recorded_url/3 :: (whapps_call:call(), ne_binary(), boolean()) -> 'undefined' | ne_binary().
recorded_url(_Call, _DocId, false) ->
    undefined;
recorded_url(Call, DocId, true) ->
    Db = whapps_call:account_db(Call),

    case whapps_util:amqp_pool_request([{<<"Media-Name">>, <<Db/binary, "/", DocId/binary>>}
                                        ,{<<"Stream-Type">>, <<"new">>}
                                        ,{<<"Protocol">>, <<"http">>}
                                        ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                                        ,{<<"Msg-ID">>, wh_util:to_binary(wh_util:current_tstamp())}
                                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)]
                                       ,fun wapi_media:publish_req/1
                                       ,fun wapi_media:resp_v/1
                                      ) of
        {error, _R} ->
            lager:debug("failed to get media URL: ~p", [_R]),
            undefined;
        {ok, MediaResp} ->
            lager:debug("stream url: ~s", [wh_json:get_value(<<"Stream-URL">>, MediaResp)]),
            recorded_url(wh_json:get_value(<<"Stream-URL">>, MediaResp))
    end.

%% if the media_req returns a SHOUT URL, strip it and return an http protocol
recorded_url(<<"shout://", Url/binary>>) ->
    <<"http://", Url/binary>>;
recorded_url(Url) -> Url.

-spec maybe_save_recording/3 :: (whapps_call:call(), wh_json:json_object(), boolean()) ->
                                        {ne_binary(), wh_json:json_object()} |
                                        'undefined'.
maybe_save_recording(_Call, _MediaDoc, false) ->
    undefined;
maybe_save_recording(Call, MediaDoc, true) ->
    store_recording(Call, wh_json:get_value(<<"name">>, MediaDoc), MediaDoc).

-spec store_recording/3 :: (whapps_call:call(), ne_binary(), wh_json:json_object()) ->
                                   {ne_binary(), wh_json:json_object()}.
store_recording(Call, MediaName, JObj) ->
    StoreUrl = store_url(Call, JObj),
    {ok, StoreJObj} = whapps_call_command:b_store(MediaName, StoreUrl, Call),
    {wh_json:get_value(<<"_id">>, JObj), StoreJObj}.

-spec store_url/2 :: (whapps_call:call(), wh_json:json_object()) -> ne_binary().
store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),

    Rev = wh_json:get_value(<<"_rev">>, JObj),
    list_to_binary([couch_mgr:get_url(), AccountDb
                    ,"/", MediaId
                    ,"/", MediaName
                    ,"?rev=", Rev
                   ]).

-spec store_recording_meta/2 :: (whapps_call:call(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                     {'error', any()}.
store_recording_meta(Call, MediaName) ->
    AcctDb = whapps_call:account_db(Call),
    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list([{<<"name">>, MediaName}
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

-spec maybe_stop/3 :: (whapps_call:call(), term(), Result) -> {stop, whapps_call:call()} | Result.
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
get_lang(<<"es">>) -> <<"es">>;
get_lang(<<"fr">>) -> <<"fr">>;
get_lang(<<"de">>) -> <<"de">>.

%% contstrain loop to 10
-spec get_loop_count/1 :: (integer() | binary()) -> 0..10.
get_loop_count(N) when not is_integer(N) -> get_loop_count(wh_util:to_integer(N));
get_loop_count(N) when N =< 0 -> 0;
get_loop_count(N) when N =< 10 -> N;
get_loop_count(_) -> 10.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

