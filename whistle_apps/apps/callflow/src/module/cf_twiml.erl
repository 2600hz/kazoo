%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Aims to recreate the TwiML interaction, converting the TwiML XML
%%% to Whistle JSON commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_twiml).

-export([process_call/2]).

-include("../callflow.hrl").
-include("cf_killio.hrl").

process_call(Call, {Resp, _}) ->
    case Resp#xmlElement.name =:= 'Response' of
        true -> process_response(Call, Resp#xmlElement.content);
        false ->
            lager:debug("root element not Response tag, ~p", [Resp#xmlElement.name]),
            cf_exe:continue(Call)
    end.

process_response(Call, [#xmlText{}|T]) ->
    process_response(Call, T);
process_response(Call, [#xmlElement{name=Name, content=Content}=El|T]) ->
    lager:debug("hname: ~p, content: ~p", [Name, Content]),
    process_element(Call, Name, Content, El),
    process_response(Call, T);
process_response(Call, []) ->
    cf_exe:continue(Call).

%%-------------------------------------------------------------------------
%% @doc Dial
%%   action       | relative or absolute URL | no default action
%%   method       | GET, POST                | POST
%%   timeout      | positive integer, secs   | 30
%%   hangupOnStar | true, false              | false
%%   timeLimit    | positive integer, secs   | 14400
%%   callerId     | e164                     | caller's caller-id-number
%%   record       | true, false              | false
%%-------------------------------------------------------------------------
process_element(Call, 'Dial', [#xmlText{value=DialMe, type=text}], #xmlElement{attributes=Attrs}) ->
    lager:debug("DIAL: ~s", [DialMe]),
    Props = attrs_to_proplist(Attrs),

    Timeout = wh_util:to_integer(props:get_value(timeout, Props, 30)),
    StarHangup = wh_util:is_true(props:get_value(hangupOnStar, Props, false)),

    TimeLimit = wh_util:to_integer(props:get_value(timeLimit, Props, 14400)),
    CallerID = props:get_value(callerId, Props, whapps_call:caller_id_number(Call)),
    RecordCall = wh_util:is_true(props:get_value(record, Props, false)),

    Call1 = lists:foldl(fun({V, F}, C) -> F(V, C) end, Call, [{list_to_binary([DialMe, "@norealm"]), fun whapps_call:set_request/2}
                                                              ,{CallerID, fun whapps_call:set_caller_id_number/2}
                                                             ]),

    cf_offnet:offnet_req(wh_json:from_list([{<<"Timeout">>, Timeout}
                                            | offnet_data(RecordCall, StarHangup)
                                           ])
                         ,Call1),

    %% wait for the bridge to end
    Start = erlang:now(),
    OffnetProp = wait_for_offnet(Call1, RecordCall, StarHangup, TimeLimit),
    Elapsed = wh_util:elapsed_s(Start),

    RecordingId = maybe_save_recording(Call1, props:get_value(media_name, OffnetProp), RecordCall),

    OtherLeg = props:get_value(other_leg, OffnetProp),
    Status = props:get_value(call_status, OffnetProp),
    lager:debug("other leg ~s done in ~bs: ~s", [OtherLeg, Elapsed, Status]),

    case props:get_value(action, Props) of
        undefined -> ok;
        Action ->
            BaseParams = wh_json:from_list([{"DialCallStatus", Status}
                                            ,{"DialCallSid", OtherLeg}
                                            ,{<<"DialCallDuration">>, Elapsed}
                                            ,{<<"RecordingUrl">>, recorded_url(Call, RecordingId, RecordCall)}
                                            | cf_killio:req_params(Call1)
                                           ]),
            Uri = cf_killio:resolve_uri(whapps_call:kvs_fetch(voice_uri, Call1), Action),
            Method = cf_killio:http_method(props:get_value(method, Props, post)),
            cf_killio:send_req(Call1, Uri, Method, BaseParams)
    end;

process_element(Call, 'Gather', [#xmlText{}|T], El) ->
    process_element(Call, 'Gather', T, El);
process_element(Call, 'Gather', [#xmlElement{name=Name, content=Content}=El1|T], El) ->
    process_element(Call, Name, Content, El1),
    process_element(Call, 'Gather', T, El);
process_element(Call, 'Gather', [], #xmlElement{attributes=Attrs}) ->
    Props = attrs_to_proplist(Attrs),

    lager:debug("attrs: ~p", [Props]),
    MaxDigitsBin = wh_util:to_binary(props:get_value(numDigits, Props)),
    MaxDigits = wh_util:to_integer(MaxDigitsBin),
    Timeout = wh_util:to_integer(props:get_value(timeout, Props, 5)),
    FinishOnKey = props:get_value(finishOnKey, Props, <<"#">>),

    case whapps_call_command:collect_digits(MaxDigitsBin, Timeout, 2000
                                            ,undefined, [FinishOnKey], Call
                                           ) of
        {ok, DTMFs} when byte_size(DTMFs) =:= MaxDigits ->
            lager:debug("recv DTMFs: ~s", [DTMFs]),

            NewUri = cf_killio:resolve_uri(whapps_call:kvs_fetch(voice_uri, Call)
                                           ,props:get_value(action, Props)
                                          ),
            Method = cf_killio:http_method(props:get_value(method, Props, post)),
            BaseParams = wh_json:from_list([{<<"Digits">>, DTMFs} | cf_killio:req_params(Call)]),

            cf_killio:send_req(Call, NewUri, Method, BaseParams);
        {ok, _DTMFs} ->
            lager:debug("failed to collect ~b digits, got ~s", [MaxDigits, _DTMFs])
    end;

process_element(Call, 'Play', [#xmlText{value=PlayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    lager:debug("PLAY: ~s", [PlayMe]),
    case props:get_value(loop, attrs_to_proplist(Attrs), 1) of
        0 ->
            %% play music in a loop
            whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call);
        1 ->
            whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call);
        N when N > 1 ->
            play_loop(Call, N, wh_util:to_binary(PlayMe))
    end;
process_element(Call, 'Say', [#xmlText{value=SayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    Props = attrs_to_proplist(Attrs),
    Voice = props:get_value(voice, Props, <<"man">>),
    Lang = props:get_value(language, Props, <<"en">>),
    Loop = props:get_value(loop, Props, 1),

    lager:debug("SAY: ~s using voice ~s, in lang ~s, ~b times", [SayMe, Voice, Lang, Loop]),

    whapps_call_command:b_say(wh_util:to_binary(SayMe), Call).

play_loop(_, _, 0) ->
    ok;
play_loop(Call, PlayMe, N) ->
    _ = whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call),
    play_loop(Call, PlayMe, N-1).

attrs_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].

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
        {amqp_msg, {struct, _}=JObj} ->
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
        {amqp_msg, {struct, _}=JObj} ->
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
