%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Basically a passthrough for FS commands relating to calls
%%% @end
%%% Created :  8 Apr 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-spec exec_cmd/4 :: (atom(), ne_binary(), json_object(), pid()) -> fs_api_ret().
exec_cmd(Node, UUID, JObj, _ControlPid) ->
    UUID = wh_json:get_value(<<"Call-ID">>, JObj),
    true = wapi_fs:req_v(JObj),
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    AppArgs = wh_json:get_value(<<"Args">>, JObj),
    ?LOG(UUID, "Executing fs api ~s with ~s", [AppName, AppArgs]),
    freeswitch:api(Node, wh_util:to_atom(AppName), wh_util:to_list(AppArgs)).
