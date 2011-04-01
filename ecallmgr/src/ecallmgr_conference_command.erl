%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------

-module(ecallmgr_conference_command).

-export([exec_cmd/4]).

-import(logger, [log/2, format_log/3]).

-include("ecallmgr.hrl").

-define(APP_NAME, <<"conference_command">>).
-define(APP_VERSION, <<"1.0">>).


-spec(exec_cmd/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), AmqpHost :: string()) -> ok | timeout | {error, string()}).
exec_cmd(Node, CallId, JObj, AmqpHost) ->
    AppName = whapps_json:get_value(<<"Application-Name">>, JObj),
    ConfName = whapps_json:get_value(<<"Conference-ID">>, JObj),
    case get_fs_app(Node, ConfName, JObj, AmqpHost, AppName) of
        {error, _Msg}=Err -> 
            Err;
        {_, noop} -> 
            ok;
        Args -> 
            case api(Node, <<"conference">>, <<ConfName/binary, " ", Args/binary>>) of
                {ok, Reply} when AppName =:= <<"members">> ->
                    members_response(Reply, ConfName, CallId, whapps_json:get_value(<<"Server-ID">>, JObj), AmqpHost);
                {ok, _} ->
                    ok;
                _ -> 
                    {error, bad_reply}
            end
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/5 :: 
        (Node :: atom(), ConfName :: binary(), JObj :: json_object(), AmqpHost
 :: string(), Application :: binary()) -> 
                           tuple(binary(), binary() | noop) | tuple(error, string())).
get_fs_app(_Node, ConfName, _JObj, _AmqpHost, _Application) when not is_binary(ConfName) ->
    {error, "invalid conference id"};
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"members">>) ->
    case whistle_api:conference_members_req_v(JObj) of
	false -> 
            {error, "conference members failed to execute as JObj did not validate."};
	true ->
            <<"list">>
    end;
get_fs_app(_Node, ConfName, JObj, AmqpHost, <<"play">>) ->
    case whistle_api:conference_play_req_v(JObj) of
	false -> 
            {error, "conference play failed to execute as JObj did not validate."};
	true ->
            Media = <<$', (media_path(whapps_json:get_value(<<"Media-Name">>, JObj), ConfName, AmqpHost))/binary, $'>>,
            case whapps_json:get_value(<<"Member-ID">>, JObj) of
                MemberId when is_binary(MemberId) ->
                    <<"play ", Media/binary, " ", MemberId/binary>>;
                _ ->
                    <<"play ", Media/binary>>                                      
            end
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"deaf">>) ->
    case whistle_api:conference_deaf_req_v(JObj) of
	false -> 
            {error, "conference deaf failed to execute as JObj did not validate."};
	true ->
            <<"deaf ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"undeaf">>) ->
    case whistle_api:conference_undeaf_req_v(JObj) of
	false -> 
            {error, "conference undeaf failed to execute as JObj did not validate."};
	true ->
            <<"undeaf ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"mute">>) ->
    case whistle_api:conference_mute_req_v(JObj) of
	false -> 
            {error, "conference mute failed to execute as JObj did not validate."};
	true ->
            <<"mute ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"unmute">>) ->
    case whistle_api:conference_unmute_req_v(JObj) of
	false -> 
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"unmute ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"kick">>) ->
    case whistle_api:conference_kick_req_v(JObj) of
	false -> 
            {error, "conference kick failed to execute as JObj did not validate."};
	true ->
            <<"kick ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _ConfName, JObj, _AmqpHost, <<"move">>) ->
    case whistle_api:conference_move_req_v(JObj) of
	false -> 
            {error, "conference unmute failed to execute as JObj did not validate."};
	true ->
            <<"transfer ", (whapps_json:get_value(<<"Member-ID">>, JObj))/binary>>
    end;
get_fs_app(_Node, _UUID, _JObj, _AmqpHost, _App) ->
    format_log(error, "CONFERENCE_COMMAND(~p): Unknown App ~p:~n~p~n", [self(), _App, _JObj]),
    {error, "Application unknown"}.
          
%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(api/3 :: (Node :: atom(), AppName :: binary() | string(), Args :: binary() | string()) -> ok | timeout | {error, string()}).
api(Node, AppName, Args) ->
    App = whistle_util:to_atom(AppName, true),
    Arg = whistle_util:to_list(Args),
    format_log(info, "CONFERENCE_COMMAND(~p): FS-API -> Node: ~p Api: ~p ~p~n", [self(), Node, App, Arg]),
    freeswitch:api(Node, App, Arg, 5000).

-spec(media_path/3 :: (MediaName :: binary(), UUID :: binary(), AmqpHost :: binary()) -> list()).
media_path(MediaName, UUID, AmqpHost) ->
    case ecallmgr_media_registry:lookup_media(MediaName, UUID, AmqpHost) of
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
      
members_response(Members, ConfName, CallId, ServerId, AmqpHost) ->
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
    {ok, Json} = whistle_api:conference_members_resp(Response),
    amqp_util:targeted_publish(AmqpHost, ServerId, Json, <<"application/json">>).    

parse_members(Members) ->
    CSV = whistle_util:to_list(Members),
    lists:foldr(fun(Line, Acc) ->
                        [{struct, parse_member(Line)}|Acc]
                end, [], string:tokens(CSV, "\n")).

parse_member(Line) ->
    [Id, _, CallId, CidName, CidNum, Status, VolIn, VolOut, Energy] =
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
