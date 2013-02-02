%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012 VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_command).

-export([exec/3]).

-include("ecallmgr.hrl").

-spec exec(atom(), ne_binary(), wh_json:object()) -> 'ok'.
exec(Focus, ConferenceId, JObj) ->
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case get_conf_command(App, Focus, ConferenceId, JObj) of
        {'error', _Msg}=E ->
            send_response(App, E, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {noop, Conference} ->
            send_response(App, {noop, Conference}, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {AppName, AppData} ->
            Command = wh_util:to_list(list_to_binary([ConferenceId, " ", AppName, " ", AppData])),
            Focus =/= undefined andalso lager:debug("execute on node ~s: conference ~s", [Focus, Command]),
            Result = freeswitch:api(Focus, 'conference', Command),
            send_response(App, Result, wh_json:get_value(<<"Server-ID">>, JObj), JObj)
    end.

-spec get_conf_command(ne_binary(), atom(), ne_binary(), wh_json:object()) -> {ne_binary(), binary()} |
                                                                                    {'error', ne_binary()} |
                                                                                    {'noop', wh_json:object()}.
get_conf_command(<<"deaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:deaf_participant_v(JObj) of
        false ->
            {'error', <<"conference deaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"deaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"participant_energy">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference participant_energy failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;
get_conf_command(<<"kick">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:kick_v(JObj) of
        false ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        true ->
            {<<"hup">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(<<"participants">>, undefined, ConferenceId, _) ->
    {error, <<"Non-Existant ID ", ConferenceId/binary>>};
get_conf_command(<<"participants">>, Focus, ConferenceId, JObj) ->
    case wapi_conference:participants_req_v(JObj) of
        false ->
            {'error', <<"conference participants failed to execute as JObj did not validate.">>};
        true ->
            {noop, wh_json:get_value(ConferenceId, ecallmgr_conference_listener:conferences_on_node(Focus))}
    end;
get_conf_command(<<"lock">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:lock_v(JObj) of
        false ->
            {'error', <<"conference lock failed to execute as JObj did not validate.">>};
        true ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(<<"mute_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:mute_participant_v(JObj) of
        false ->
            {'error', <<"conference mute_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"mute">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(<<"play">>, _Focus, ConferenceId, JObj) ->
    case wapi_conference:play_v(JObj) of
        false ->
            {'error', <<"conference play failed to execute as JObj did not validate.">>};
        true ->
            UUID = wh_json:get_ne_value(<<"Call-ID">>, JObj, ConferenceId),
            Media = list_to_binary(["'", ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID, JObj), "'"]),
            Args = case wh_json:get_value(<<"Participant">>, JObj) of
                       undefined -> Media;
                       Participant -> list_to_binary([Media, " ", Participant])
                   end,
            {<<"play">>, Args}
    end;
get_conf_command(<<"record">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:record_v(JObj) of
        false ->
            {'error', <<"conference record failed to execute as JObj did not validate.">>};
        true ->
            MediaName = ecallmgr_util:recording_filename(wh_json:get_binary_value(<<"Media-Name">>, JObj)),
            {<<"record">>, MediaName}
    end;
get_conf_command(<<"relate_participants">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:relate_participants_v(JObj) of
        false ->
            {'error', <<"conference relate_participants failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Other-Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Relationship">>, JObj, <<"clear">>)
                                  ]),
            {<<"relate">>, Args}
    end;
get_conf_command(<<"set">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:set_v(JObj) of
        false ->
            {'error', <<"conference set failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Parameter">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Value">>, JObj)
                                  ]),
            {<<"set">>, Args}
    end;
get_conf_command(<<"stop_play">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:stop_play_v(JObj) of
        false ->
            {'error', <<"conference stop_play failed to execute as JObj did not validate.">>};
        true ->
            Affects = wh_json:get_binary_value(<<"Affects">>, JObj, <<"all">>),
            Args = case wh_json:get_binary_value(<<"Participant">>, JObj) of
                       undefined -> Affects;
                       Participant -> list_to_binary([Affects, " ", Participant])
                   end,
            {<<"stop">>, Args}
    end;
get_conf_command(<<"undeaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:undeaf_participant_v(JObj) of
        false ->
            {'error', <<"conference undeaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"undeaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"unlock">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:unlock_v(JObj) of
        false ->
            {'error', <<"conference unlock failed to execute as JObj did not validate.">>};
        true ->
            {<<"unlock">>, <<>>}
    end;
get_conf_command(<<"unmute_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:unmute_participant_v(JObj) of
        false ->
            {'error', <<"conference unmute failed to execute as JObj did not validate.">>};
        true ->
            {<<"unmute">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"participant_volume_in">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_volume_in_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_in failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-In-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_in">>, Args}
    end;
get_conf_command(<<"participant_volume_out">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_volume_out_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_out failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-Out-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_out">>, Args}
    end.

-spec send_response(ne_binary(), tuple(), api_binary(), wh_json:object()) -> ok.
send_response(_, _, undefined, _) -> ok;
send_response(_, {ok, <<"Non-Existant ID", _/binary>> = Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(Msg, <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(<<"participants">>, {noop, Conference}, RespQ, Command) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            ,{<<"Participants">>, wh_json:get_value(<<"Participants">>, Conference, wh_json:new())}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(_, {ok, Response}, RespQ, Command) ->
    case binary:match(Response, <<"not found">>) of
        nomatch -> ok;
        _Else ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
                     ,{<<"Error-Message">>, binary:replace(Response, <<"\n">>, <<>>)}
                     ,{<<"Request">>, Command}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(RespQ, Error)
    end;
send_response(_, {error, Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(wh_util:to_binary(Msg), <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(_, timeout, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, <<"Node Timeout">>}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error).
