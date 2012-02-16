%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_command).

-export([exec_cmd/3]).

-include("ecallmgr.hrl").

-spec exec_cmd/3 :: (atom(), ne_binary(), wh_json:json_object()) -> 'ok'.
exec_cmd(Node, ConferenceId, JObj) ->
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case get_conf_command(Node, ConferenceId, JObj, App) of
        {'error', Msg}=E ->
            _ = ecallmgr_util:fs_log(Node, "whistle error while building command ~s: ~s", [App, Msg]),
            send_response(App, E, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {noop, JObj} ->
            send_response(App, {noop, JObj}, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {AppName, AppData} ->
            Command = wh_util:to_list(list_to_binary([ConferenceId, AppName, " ", AppData])),
            Result = freeswitch:api(Node, 'conference', Command),
            send_response(App, Result, wh_json:get_value(<<"Server-ID">>, JObj), JObj)
    end.

-spec get_conf_command/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) -> {ne_binary(), binary()} | 
                                                                                         {'error', ne_binary()} |
                                                                                         {'noop', wh_json:json_object()}.
get_conf_command(_Node, _ConferenceId, JObj, <<"deaf_participant">>) ->
    case wapi_conference:deaf_participant_v(JObj) of
        false -> 
            {'error', <<"conference deaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"deaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_energy">>) ->
    case wapi_conference:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference participant_energy failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"kick">>) ->
    case wapi_conference:kick_v(JObj) of
        false ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        true ->
            {<<"hup">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(Node, ConferenceId, JObj, <<"participants">>) ->
    case wapi_conference:participants_v(JObj) of
        false ->
            {'error', <<"conference participants failed to execute as JObj did not validate.">>};
        true ->
            {noop, wh_json:get_value(ConferenceId, ecallmgr_conference_listener:conferences_on_node(Node))}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"lock">>) ->
    case wapi_conference:lock_v(JObj) of
        false ->
            {'error', <<"conference lock failed to execute as JObj did not validate.">>};
        true ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"mute_participant">>) ->
    case wapi_conference:mute_participant_v(JObj) of
        false ->
            {'error', <<"conference mute_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(_Node, ConferenceId, JObj, <<"play">>) ->
    case wapi_conference:play_v(JObj) of
        false ->
            {'error', <<"conference play failed to execute as JObj did not validate.">>};
        true ->
            UUID = wh_json:get_ne_value(<<"Call-ID">>, JObj, ConferenceId),
            Media = list_to_binary(["'", ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID), "'"]),
            Args = case wh_json:get_value(<<"Participant">>, JObj) of
                       undefined -> Media;                       
                       Participant -> list_to_binary([Media, " ", Participant])
                   end,
            {<<"play">>, Args}
    end;
get_conf_command(_Node, ConferenceId, JObj, <<"record">>) ->
    case wapi_conference:record_v(JObj) of
        false ->
            {'error', <<"conference record failed to execute as JObj did not validate.">>};
        true ->
            UUID = wh_json:get_binary_value(<<"Call-ID">>, JObj, ConferenceId),
            MediaName = wh_json:get_binary_value(<<"Media-Name">>, JObj),
            Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),
            {<<"record">>, Media}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"relate_participants">>) ->
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
get_conf_command(_Node, _ConferenceId, JObj, <<"set">>) ->
    case wapi_conference:set_v(JObj) of
        false ->
            {'error', <<"conference set failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Parameter">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Value">>, JObj)
                                  ]),
            {<<"set">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"stop_play">>) ->
    case wapi_conference:stop_play_v(JObj) of
        false ->
            {'error', <<"conference stop_play failed to execute as JObj did not validate.">>};
        true ->
            Affects = wh_json:get_binary_value(<<"Affects">>, JObj, <<>>),
            Args = case wh_json:get_binary_value(<<"Participant">>, JObj) of
                       undefined -> Affects;
                       Participant -> list_to_binary([Affects, " ", Participant])
                   end,
            {<<"stop">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"undeaf_participant">>) ->
    case wapi_conference:undeaf_participant_v(JObj) of
        false ->
            {'error', <<"conference undeaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"undeaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"unlock">>) ->
    case wapi_conference:unlock_v(JObj) of
        false ->
            {'error', <<"conference unlock failed to execute as JObj did not validate.">>};
        true ->
            {<<"unlock">>, <<>>}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"unmute">>) ->
    case wapi_conference:unmute_v(JObj) of
        false ->
            {'error', <<"conference unmute failed to execute as JObj did not validate.">>};
        true ->
            {<<"unmute">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_volume_in">>) ->
    case wapi_conference:participant_volume_in_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_in failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-In-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_in">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_volume_out">>) ->
    case wapi_conference:participant_volume_out_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_out failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-Out-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_out">>, Args}
    end.

-spec send_response/4 :: (ne_binary(), tuple(), undefined | ne_binary(), wh_json:json_object()) -> ok.
send_response(_, _, undefined, _) ->
    ok;
send_response(_, {ok, <<"Non-Existant ID", _/binary>> = Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(Msg, <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(<<"find">>, {noop, Conference}, RespQ, Command) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            | wh_api:default_headers(<<>>, <<"conference">>, <<"find">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(<<"participants">>, {noop, Conference}, RespQ, Command) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            ,{<<"Participants">>, wh_json:get_value(<<"Participants">>, Conference, wh_json:new())}
            | wh_api:default_headers(<<>>, <<"conference">>, <<"participants_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(_, {ok, Response}, RespQ, Command) ->
    case binary:match(Response, <<"not found">>) of
        nomatch -> ok;
        _Else ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
                     ,{<<"Error-Message">>, binary:replace(Response, <<"\n">>, <<>>)}
                     ,{<<"Request">>, Command}
                     | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(RespQ, Error)
    end;
send_response(_, {error, Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(wh_util:to_binary(Msg), <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(_, timeout, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, <<"Node Timeout">>}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error).
