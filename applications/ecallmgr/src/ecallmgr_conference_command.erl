%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016 2600Hz INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_command).

-export([exec_cmd/3]).

-include("ecallmgr.hrl").

-spec exec_cmd(atom(), ne_binary(), kz_json:object()) ->
                      'ok' |
                      'error' |
                      ecallmgr_util:send_cmd_ret() |
                      [ecallmgr_util:send_cmd_ret(),...].
exec_cmd(Node, ConferenceId, JObj) ->
    exec_cmd(Node, ConferenceId, JObj, kz_json:get_value(<<"Conference-ID">>, JObj)).

exec_cmd(Node, ConferenceId, JObj, ConferenceId) ->
    App = kz_json:get_value(<<"Application-Name">>, JObj),
    case get_conf_command(App, Node, ConferenceId, JObj) of
        {'error', Msg} -> throw({'msg', Msg});
        {_, _}=Cmd -> api(Node, ConferenceId, Cmd)
    end;
exec_cmd(_Node, _ConferenceId, JObj, _DestId) ->
    lager:debug("command ~s not meant for us (~s) but for ~s", [kz_json:get_value(<<"Application-Name">>, JObj)
                                                               ,_ConferenceId
                                                               ,_DestId]).

api(Node, ConferenceId, {AppName, AppData}) ->
    Command = kz_util:to_list(list_to_binary([ConferenceId, " ", AppName, " ", AppData])),
    freeswitch:api(Node, 'conference', Command).

-spec get_conf_command(ne_binary(), atom(), ne_binary(), kz_json:object()) ->
                              fs_app() | fs_apps() |
                              {'return', 'error' | ne_binary()} |
                              {'error', ne_binary()}.

%% The following conference commands can operate on the entire conference

get_conf_command(<<"lock">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:lock_v(JObj) of
        'false' ->
            {'error', <<"conference lock failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"lock">>, <<>>}
    end;

get_conf_command(<<"unlock">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:unlock_v(JObj) of
        'false' ->
            {'error', <<"conference unlock failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"unlock">>, <<>>}
    end;

get_conf_command(<<"record">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:record_v(JObj) of
        'false' ->
            {'error', <<"conference record failed to execute as JObj did not validate.">>};
        'true' ->
            MediaName = kz_json:get_value(<<"Media-Name">>, JObj),
            RecordingName = ecallmgr_util:recording_filename(MediaName),
            {<<"recording">>, [<<"start ">>, RecordingName]}
    end;

get_conf_command(<<"recordstop">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:recordstop_v(JObj) of
        'false' -> {'error', <<"conference recordstop failed validation">>};
        'true' ->
            MediaName = ecallmgr_util:recording_filename(kz_json:get_binary_value(<<"Media-Name">>, JObj)),
            {<<"recording">>, [<<"stop ">>, MediaName]}
    end;

get_conf_command(<<"tones">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:tones_v(JObj) of
        'false' -> {'error', <<"conference tones failed to validate">>};
        'true' ->
            Tones = kz_json:get_value(<<"Tones">>, JObj, []),
            FSTones = [begin
                           Vol = case kz_json:get_value(<<"Volume">>, Tone) of
                                     'undefined' -> [];
                                     %% need to map V (0-100) to FS values
                                     V -> list_to_binary(["v=", kz_util:to_list(V), ";"])
                                 end,
                           Repeat = case kz_json:get_value(<<"Repeat">>, Tone) of
                                        'undefined' -> [];
                                        R -> list_to_binary(["l=", kz_util:to_list(R), ";"])
                                    end,
                           Freqs = string:join([ kz_util:to_list(V) || V <- kz_json:get_value(<<"Frequencies">>, Tone) ], ","),
                           On = kz_util:to_list(kz_json:get_value(<<"Duration-ON">>, Tone)),
                           Off = kz_util:to_list(kz_json:get_value(<<"Duration-OFF">>, Tone)),
                           kz_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
                       end || Tone <- Tones],
            Arg = "tone_stream://" ++ string:join(FSTones, ";"),
            {<<"play">>, Arg}
    end;

%% The following conference commands can optionally specify a participant
get_conf_command(<<"play">>, _Focus, ConferenceId, JObj) ->
    case kapi_conference:play_v(JObj) of
        'false' ->
            {'error', <<"conference play failed to execute as JObj did not validate.">>};
        'true' ->
            UUID = kz_json:get_ne_value(<<"Call-ID">>, JObj, ConferenceId),
            Media = list_to_binary(["'", ecallmgr_util:media_path(kz_json:get_value(<<"Media-Name">>, JObj), UUID, JObj), "'"]),
            Args = case kz_json:get_binary_value(<<"Participant-ID">>, JObj) of
                       'undefined' -> Media;
                       Participant -> list_to_binary([Media, " ", Participant])
                   end,
            {<<"play">>, Args}
    end;

get_conf_command(<<"play_macro">>, _Focus, _ConferenceId, JObj) ->
    Participant = kz_json:get_binary_value(<<"Participant-ID">>, JObj, <<>>),
    Macro = kz_json:get_value(<<"Media-Macro">>, JObj, []),
    Paths = lists:map(fun ecallmgr_util:media_path/1, Macro),
    Media = list_to_binary(["'file_string://", kz_util:join_binary(Paths, <<"!">>), "'", " ", Participant]),
    {<<"play">>, Media};

get_conf_command(<<"stop_play">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:stop_play_v(JObj) of
        'false' ->
            {'error', <<"conference stop_play failed to execute as JObj did not validate.">>};
        'true' ->
            Affects = kz_json:get_binary_value(<<"Affects">>, JObj, <<"all">>),
            Args = case kz_json:get_binary_value(<<"Participant-ID">>, JObj) of
                       undefined -> Affects;
                       Participant -> list_to_binary([Affects, " ", Participant])
                   end,
            {<<"stop">>, Args}
    end;

get_conf_command(Say, _Focus, _ConferenceId, JObj)
  when Say =:= <<"say">>;
       Say =:= <<"tts">> ->
    case kapi_conference:say_v(JObj) of
        'false' -> {'error', <<"conference say failed to validate">>};
        'true'->
            SayMe = kz_json:get_value(<<"Text">>, JObj),

            case kz_json:get_binary_value(<<"Participant-ID">>, JObj) of
                'undefined' -> {<<"say">>, ["'", SayMe, "'"]};
                Id -> {<<"saymember">>, [Id, " '", SayMe, "'"]}
            end
    end;

%% The following conference commands require a participant
get_conf_command(<<"kick">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:kick_v(JObj) of
        'false' ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"hup">>, kz_json:get_binary_value(<<"Participant-ID">>, JObj, <<"last">>)}
    end;

get_conf_command(<<"mute_participant">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:mute_participant_v(JObj) of
        'false' ->
            {'error', <<"conference mute_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"mute">>, kz_json:get_binary_value(<<"Participant-ID">>, JObj, <<"last">>)}
    end;

get_conf_command(<<"deaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:deaf_participant_v(JObj) of
        'false' ->
            {'error', <<"conference deaf_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"deaf">>, kz_json:get_binary_value(<<"Participant-ID">>, JObj)}
    end;

get_conf_command(<<"participant_energy">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:participant_energy_v(JObj) of
        'false' ->
            {'error', <<"conference participant_energy failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([kz_json:get_binary_value(<<"Participant-ID">>, JObj)
                                  ," ", kz_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;

get_conf_command(<<"relate_participants">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:relate_participants_v(JObj) of
        'false' ->
            {'error', <<"conference relate_participants failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([kz_json:get_binary_value(<<"Participant-ID">>, JObj)
                                  ," ", kz_json:get_binary_value(<<"Other-Participant">>, JObj)
                                  ," ", relationship(kz_json:get_binary_value(<<"Relationship">>, JObj))
                                  ]),
            {<<"relate">>, Args}
    end;

get_conf_command(<<"set">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:set_v(JObj) of
        'false' ->
            {'error', <<"conference set failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([kz_json:get_binary_value(<<"Parameter">>, JObj)
                                  ," ", kz_json:get_binary_value(<<"Value">>, JObj)
                                  ]),
            {<<"set">>, Args}
    end;

get_conf_command(<<"undeaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:undeaf_participant_v(JObj) of
        'false' ->
            {'error', <<"conference undeaf_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"undeaf">>, kz_json:get_binary_value(<<"Participant-ID">>, JObj)}
    end;

get_conf_command(<<"unmute_participant">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:unmute_participant_v(JObj) of
        'false' ->
            {'error', <<"conference unmute failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"unmute">>, kz_json:get_binary_value(<<"Participant-ID">>, JObj)}
    end;

get_conf_command(<<"participant_volume_in">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:participant_volume_in_v(JObj) of
        'false' ->
            {'error', <<"conference participant_volume_in failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([kz_json:get_binary_value(<<"Participant-ID">>, JObj)
                                  ," ", kz_json:get_binary_value(<<"Volume-In-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_in">>, Args}
    end;

get_conf_command(<<"participant_volume_out">>, _Focus, _ConferenceId, JObj) ->
    case kapi_conference:participant_volume_out_v(JObj) of
        'false' ->
            {'error', <<"conference participant_volume_out failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([kz_json:get_binary_value(<<"Participant-ID">>, JObj)
                                  ," ", kz_json:get_binary_value(<<"Volume-Out-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_out">>, Args}
    end;

get_conf_command(Cmd, _Focus, _ConferenceId, _JObj) ->
    lager:debug("unknown conference command ~s", [Cmd]),
    {'error', list_to_binary([<<"unknown conference command: ">>, Cmd])}.

-spec relationship(ne_binary()) -> ne_binary().
relationship(<<"mute">>) -> <<"nospeak">>;
relationship(<<"deaf">>) -> <<"nohear">>;
relationship(_) -> <<"clear">>.
