%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_command).

-export([set/3, unset/3
        ,bg_set/3, bg_unset/3
        ,export/3, bridge_export/3
        ,record_call/3
        ]).

-include("ecallmgr.hrl").

-define(FS_CMD_SET_MULTIVAR, 'kz_uuid_setvar_multi').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% set channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
set(_, _, []) -> 'ok';
set(Node, UUID, Props) ->
    case maybe_export_vars(Node, UUID, Props) of
        [] -> 'ok';
        NewProps -> AppArgs = ecallmgr_util:process_fs_kv(Node, UUID, NewProps, 'set'),
                    api(Node, UUID, ?FS_CMD_SET_MULTIVAR, AppArgs)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% set channel and call variables in FreeSWITCH (in background)
%% @end
%%--------------------------------------------------------------------
-spec bg_set(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
bg_set(_, _, []) -> 'ok';
bg_set(Node, UUID, Props) ->
    case maybe_export_vars(Node, UUID, Props) of
        [] -> 'ok';
        NewProps -> AppArgs = ecallmgr_util:process_fs_kv(Node, UUID, NewProps, 'set'),
                    bgapi(Node, UUID, ?FS_CMD_SET_MULTIVAR, AppArgs)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% unset channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec unset(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
unset(_, _, []) -> 'ok';
unset(Node, UUID, Props) ->
    AppArgs = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'unset'),
    api(Node, UUID, ?FS_CMD_SET_MULTIVAR, AppArgs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% unset channel and call variables in FreeSWITCH (in background)
%% @end
%%--------------------------------------------------------------------
-spec bg_unset(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
bg_unset(_, _, []) -> 'ok';
bg_unset(Node, UUID, Props) ->
    AppArgs = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'unset'),
    bgapi(Node, UUID, ?FS_CMD_SET_MULTIVAR, AppArgs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% export channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec export(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
export(_, _, []) -> 'ok';
export(Node, UUID, Props) ->
    Exports = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'export'),
    lager:debug("~p sendmsg export ~p ~p", [Node, UUID, Exports]),
    _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                       ,{"execute-app-name", "kz_export"}
                                       ,{"execute-app-arg", ecallmgr_util:fs_args_to_binary(Exports)}
                                       ]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bridge_export(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
bridge_export(_, _, []) -> 'ok';
bridge_export(Node, UUID, Props) ->
    Exports = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'export'),
    lager:debug("~p sendmsg bridge_export ~p ~p", [Node, UUID, Exports]),
    _ = [freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "bridge_export"}
                                        ,{"execute-app-arg", AppArg}
                                        ]) || AppArg <- Exports],
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_call(atom(), ne_binary(), kz_proplist()) -> ecallmgr_util:send_cmd_ret().
record_call(Node, UUID, Args) ->
    lager:debug("execute on node ~p: uuid_record(~p)", [Node, Args]),
    case freeswitch:api(Node, 'uuid_record', Args) of
        {'ok', _Msg}=Ret ->
            lager:debug("executing uuid_record returned: ~p", [_Msg]),
            Ret;
        {'error', <<"-ERR ", E/binary>>} ->
            lager:debug("error executing uuid_record: ~p", [E]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                 ,",kazoo_application_response="
                                 ,"'",binary:replace(E, <<"\n">>, <<>>),"'"
                                 ]),
            lager:debug("publishing event: ~p", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            {'error', E};
        {'error', _Reason}=Error ->
            lager:debug("error executing uuid_record: ~p", [_Reason]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                 ,",kazoo_application_response=timeout"
                                 ]),
            lager:debug("publishing event: ~p", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            Error
    end.

-spec maybe_export_vars(atom(), ne_binary(), kz_proplist()) -> kz_proplist().
maybe_export_vars(Node, UUID, Props) ->
    lists:foldl(fun({<<"Hold-Media">> = K, V}, Acc) ->
                        export(Node, UUID, [{K, V}]),
                        Acc;
                   ({<<"ringback">> = K, V}, Acc) ->
                        export(Node, UUID, [{K, V}]),
                        Acc;
                   (KV, Acc) -> [KV| Acc]
                end, [], Props).

-spec api(atom(), atom(), binary()) -> ecallmgr_util:send_cmd_ret().
-spec api(atom(), atom(), binary(), binary() | list()) -> ecallmgr_util:send_cmd_ret().
api(Node, Cmd, Args) ->
    freeswitch:api(Node, Cmd, Args).

api(_, _, _, []) -> 'ok';
api(Node, UUID, Cmd, Args)
  when is_list(Args)->
    api(Node, Cmd, list_to_binary([UUID, " ", ecallmgr_util:fs_args_to_binary(Args)]));
api(Node, UUID, Cmd, Args) ->
    api(Node, Cmd, <<UUID/binary, " ", Args/binary>>).

-spec bgapi(atom(), binary(), binary()) -> ecallmgr_util:send_cmd_ret().
-spec bgapi(atom(), binary(), binary(), binary() | list()) -> ecallmgr_util:send_cmd_ret().
bgapi(Node, Cmd, Args) ->
    freeswitch:bgapi(Node, Cmd, Args).

bgapi(_, _, _, []) -> 'ok';
bgapi(Node, UUID, Cmd, Args)
  when is_list(Args)->
    bgapi(Node, Cmd, list_to_binary([UUID, " ", ecallmgr_util:fs_args_to_binary(Args)]));
bgapi(Node, UUID, Cmd, Args) ->
    bgapi(Node, Cmd, <<UUID/binary, " ", Args/binary>>).
