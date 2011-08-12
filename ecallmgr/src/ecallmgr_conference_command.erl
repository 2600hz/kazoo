%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------

-module(ecallmgr_conference_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-spec(exec_cmd/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), ControlPID :: pid()) -> ok | timeout | tuple(error, bad_reply | string())).
exec_cmd(Node, CallId, JObj, _) ->
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
    case get_fs_app(Node, ConfName, JObj, AppName) of
        {error, _Msg}=Err ->
            Err;
        Args ->
            case api(Node, <<"conference">>, <<ConfName/binary, " ", Args/binary>>) of
                {ok, Reply} when AppName =:= <<"participants">> ->
                    spawn(fun() -> participants_response(Reply, ConfName, CallId, wh_json:get_value(<<"Server-ID">>, JObj)) end), ok;
                {ok, _} -> ok;
		timeout -> timeout;
                {error, _} -> {error, bad_reply}
            end
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/4 ::
        (Node :: atom(), ConfName :: binary(), JObj :: json_object(), Application :: binary()) ->
                           binary() | tuple(error, string())).
get_fs_app(_Node, ConfName, _JObj, _Application) when not is_binary(ConfName) ->
    {error, "invalid conference id"};
get_fs_app(_Node, _ConfName, JObj, <<"participants">>) ->
    case wh_api:conference_participants_req_v(JObj) of
	false ->
            {error, "conference participants failed to execute as JObj did not validate."};
	true ->
            <<"list">>
    end;
get_fs_app(_Node, ConfName, JObj, <<"play">>) ->
    case wh_api:conference_play_req_v(JObj) of
	false ->
            {error, "conference play failed to execute as JObj did not validate."};
	true ->
            Media = <<$', (media_path(wh_json:get_value(<<"Media-Name">>, JObj), ConfName))/binary, $'>>,
            case wh_json:get_value(<<"Participant-ID">>, JObj) of
                ParticipantId when is_binary(ParticipantId) ->
                    <<"play ", Media/binary, " ", ParticipantId/binary>>;
                _ ->
                    <<"play ", Media/binary>>
            end
    end;
get_fs_app(_Node, _ConfName, JObj, <<"deaf">>) ->
    case wh_api:conference_deaf_req_v(JObj) of
	false ->
            {error, "conference deaf failed to execute as JObj did not validate."};
	true ->
            <<"deaf ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"undeaf">>) ->
    case wh_api:conference_undeaf_req_v(JObj) of
	false ->
            {error, "conference undeaf failed to execute as JObj did not validate."};
	true ->
            <<"undeaf ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"mute">>) ->
    case wh_api:conference_mute_req_v(JObj) of
	false ->
            {error, "conference mute failed to execute as JObj did not validate."};
	true ->
            <<"mute ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"unmute">>) ->
    case wh_api:conference_unmute_req_v(JObj) of
	false ->
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"unmute ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"kick">>) ->
    case wh_api:conference_kick_req_v(JObj) of
	false ->
            {error, "conference kick failed to execute as JObj did not validate."};
	true ->
            <<"kick ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"move">>) ->
    case wh_api:conference_move_req_v(JObj) of
	false ->
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"transfer ", (wh_json:get_value(<<"Participant-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _UUID, _JObj, _App) ->
    ?LOG_SYS("Unknown App ~p: ~p", [_App, _JObj]),
    {error, "Application unknown"}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(api/3 :: (Node :: atom(), AppName :: binary() | string(), Args :: binary() | string()) -> tuple(ok, binary()) | timeout | {error, string()}).
api(Node, AppName, Args) ->
    App = wh_util:to_atom(AppName, true),
    Arg = wh_util:to_list(Args),
    ?LOG_SYS("FS-API -> Node: ~p Api: ~s ~s", [Node, App, Arg]),
    freeswitch:api(Node, App, Arg, 5000).

-spec(media_path/2 :: (MediaName :: binary(), UUID :: binary()) -> binary()).
media_path(MediaName, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, UUID) of
        {error, _} ->
            MediaName;
        {ok, Url} ->
            get_fs_playback(Url)
    end.

-spec(get_fs_playback/1 :: (Url :: binary()) -> binary()).
get_fs_playback(Url) when byte_size(Url) >= 4 ->
    case binary:part(Url, 0, 4) of
        <<"http">> ->
            {ok, Settings} = file:consult(?SETTINGS_FILE),
            RemoteAudioScript = props:get_value(remote_audio_script, Settings, <<"/tmp/fetch_remote_audio.sh">>),
            <<"shell_stream://", (wh_util:to_binary(RemoteAudioScript))/binary, " ", Url/binary>>;
        _Else ->
            Url
    end;
get_fs_playback(Url) ->
    Url.

-spec(participants_response/4 :: (Participants :: binary(), ConfName :: binary(), CallId :: binary(), ServerId :: binary()) -> no_return()).
participants_response(Participants, ConfName, CallId, ServerId) ->
    ParticipantList = try parse_participants(Participants) catch _:_ -> [] end,
    Response = [
                {<<"Application-Name">>, <<"participants">>}
                ,{<<"Participants">>, ParticipantList}
                ,{<<"Conference-ID">>, ConfName}
                ,{<<"Call-ID">>, CallId}
                | wh_api:default_headers(ServerId, <<"conference">>, <<"participants">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = wh_api:conference_participants_resp(Response),
    amqp_util:conference_publish(Payload, events, ConfName).

parse_participants(Participants) ->
    CSV = wh_util:to_list(Participants),
    lists:foldr(fun(Line, Acc) ->
                        [{struct, parse_participant(Line)}|Acc]
                end, [], string:tokens(CSV, "\n")).

parse_participant(Line) ->
    [Id, _, CallId, CidName, CidNum, Status, VolIn, _, VolOut, Energy] =
        string:tokens(Line, ";"),
    [
      {<<"Participant-ID">>, wh_util:to_binary(Id)}
     ,{<<"Call-ID">>, wh_util:to_binary(CallId)}
     ,{<<"CID-Name">>, wh_util:to_binary(CidName)}
     ,{<<"CID-Number">>, wh_util:to_binary(CidNum)}
     ,{<<"Status">>, wh_util:to_binary(Status)}
     ,{<<"Volume-In">>, wh_util:to_binary(VolIn)}
     ,{<<"Volume-Out">>, wh_util:to_binary(VolOut)}
     ,{<<"Energy-Threshold">>, wh_util:to_binary(Energy)}
    ].
