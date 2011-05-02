%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------

-module(ecallmgr_conference_command).

-export([exec_cmd/3]).

-import(logger, [log/2, format_log/3]).

-include("ecallmgr.hrl").

-spec(exec_cmd/3 :: (Node :: atom(), UUID :: binary(), JObj :: json_object()) -> ok | timeout | tuple(error, bad_reply | string())).
exec_cmd(Node, CallId, JObj) ->
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
    case get_fs_app(Node, ConfName, JObj, AppName) of
        {error, _Msg}=Err ->
            Err;
        Args ->
            case api(Node, <<"conference">>, <<ConfName/binary, " ", Args/binary>>) of
                {ok, Reply} when AppName =:= <<"members">> ->
                    spawn(fun() -> members_response(Reply, ConfName, CallId, wh_json:get_value(<<"Server-ID">>, JObj)) end), ok;
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
get_fs_app(_Node, _ConfName, JObj, <<"members">>) ->
    case whistle_api:conference_members_req_v(JObj) of
	false ->
            {error, "conference members failed to execute as JObj did not validate."};
	true ->
            <<"list">>
    end;
get_fs_app(_Node, ConfName, JObj, <<"play">>) ->
    case whistle_api:conference_play_req_v(JObj) of
	false ->
            {error, "conference play failed to execute as JObj did not validate."};
	true ->
            Media = <<$', (media_path(wh_json:get_value(<<"Media-Name">>, JObj), ConfName))/binary, $'>>,
            case wh_json:get_value(<<"Member-ID">>, JObj) of
                MemberId when is_binary(MemberId) ->
                    <<"play ", Media/binary, " ", MemberId/binary>>;
                _ ->
                    <<"play ", Media/binary>>
            end
    end;
get_fs_app(_Node, _ConfName, JObj, <<"deaf">>) ->
    case whistle_api:conference_deaf_req_v(JObj) of
	false ->
            {error, "conference deaf failed to execute as JObj did not validate."};
	true ->
            <<"deaf ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"undeaf">>) ->
    case whistle_api:conference_undeaf_req_v(JObj) of
	false ->
            {error, "conference undeaf failed to execute as JObj did not validate."};
	true ->
            <<"undeaf ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"mute">>) ->
    case whistle_api:conference_mute_req_v(JObj) of
	false ->
            {error, "conference mute failed to execute as JObj did not validate."};
	true ->
            <<"mute ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"unmute">>) ->
    case whistle_api:conference_unmute_req_v(JObj) of
	false ->
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"unmute ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"kick">>) ->
    case whistle_api:conference_kick_req_v(JObj) of
	false ->
            {error, "conference kick failed to execute as JObj did not validate."};
	true ->
            <<"kick ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, <<"move">>) ->
    case whistle_api:conference_move_req_v(JObj) of
	false ->
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"transfer ", (wh_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _UUID, _JObj, _App) ->
    format_log(error, "CONFERENCE_COMMAND(~p): Unknown App ~p:~n~p~n", [self(), _App, _JObj]),
    {error, "Application unknown"}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(api/3 :: (Node :: atom(), AppName :: binary() | string(), Args :: binary() | string()) -> tuple(ok, binary()) | timeout | {error, string()}).
api(Node, AppName, Args) ->
    App = whistle_util:to_atom(AppName, true),
    Arg = whistle_util:to_list(Args),
    format_log(info, "CONFERENCE_COMMAND(~p): FS-API -> Node: ~p Api: ~p ~p~n", [self(), Node, App, Arg]),
    freeswitch:api(Node, App, Arg, 5000).

-spec(media_path/2 :: (MediaName :: binary(), UUID :: binary()) -> list()).
media_path(MediaName, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, UUID) of
        {error, _} ->
            MediaName;
        Url ->
            get_fs_playback(Url)
    end.

-spec(get_fs_playback/1 :: (Url :: binary()) -> binary()).
get_fs_playback(Url) when byte_size(Url) >= 4 ->
    case binary:part(Url, 0, 4) of
        <<"http">> ->
            <<"shell_stream:///tmp/fetch_remote_audio.sh ", Url/binary>>;
        _Else ->
            Url
    end;
get_fs_playback(Url) ->
    Url.

-spec(members_response/4 :: (Members :: binary(), ConfName :: binary(), CallId :: binary(), ServerId :: binary()) -> no_return()).
members_response(Members, ConfName, CallId, ServerId) ->
    format_log(info, "parse ~p", [Members]),
    MemberList = try
                     parse_members(Members)
                 catch
                     _:_ ->
                         []
                 end,
    Response = [
              {<<"Application-Name">>, <<"members">>}
             ,{<<"Members">>, MemberList}
             ,{<<"Conference-ID">>, ConfName}
             ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers("", <<"conference">>, <<"response">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "MEMBERS RESPONSE: ~p", [Response]),
    {ok, Payload} = whistle_api:conference_members_resp(Response),
    amqp_util:targeted_publish(ServerId, Payload, <<"application/json">>).

parse_members(Members) ->
    CSV = whistle_util:to_list(Members),
    lists:foldr(fun(Line, Acc) ->
                        [{struct, parse_member(Line)}|Acc]
                end, [], string:tokens(CSV, "\n")).

parse_member(Line) ->
    [Id, _, CallId, CidName, CidNum, Status, VolIn, _, VolOut, Energy] =
        string:tokens(Line, ";"),
    [
      {<<"Member-ID">>, whistle_util:to_binary(Id)}
     ,{<<"Call-ID">>, whistle_util:to_binary(CallId)}
     ,{<<"CID-Name">>, whistle_util:to_binary(CidName)}
     ,{<<"CID-Number">>, whistle_util:to_binary(CidNum)}
     ,{<<"Status">>, whistle_util:to_binary(Status)}
     ,{<<"Volume-In">>, whistle_util:to_binary(VolIn)}
     ,{<<"Volume-Out">>, whistle_util:to_binary(VolOut)}
     ,{<<"Energy-Threshold">>, whistle_util:to_binary(Energy)}
    ].
