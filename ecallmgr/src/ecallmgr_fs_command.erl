%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Basically a passthrough for FS commands relating to calls
%%% @end
%%% Created :  8 Apr 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_command).

-export([exec_cmd/3]).

-include("ecallmgr.hrl").

-spec(exec_cmd/3 :: (Node :: atom(), UUID :: binary(), JObj :: json_object()) -> ok | tuple(error, string())).
exec_cmd(Node, UUID, JObj) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    case DestID =:= UUID of
	true ->
	    true = wh_api:fs_req_v(JObj),
	    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
	    AppArgs = wh_json:get_value(<<"Args">>, JObj),
	    {ok, _} = freeswitch:api(Node, AppName, AppArgs),
	    ok;
	false ->
	    {error, "Command not for this node"}
    end.
