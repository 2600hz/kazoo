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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% set channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
set(_, _, []) -> 'ok';
set(Node, UUID, Props) ->
    NewProps = maybe_export_vars(Node, UUID, Props),
    AppArgs = process_fs_kv(Node, UUID, NewProps, 'set'),
    send_command(Node, UUID, AppArgs, fun api/3).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% set channel and call variables in FreeSWITCH (in background)
%% @end
%%--------------------------------------------------------------------
-spec bg_set(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
bg_set(_, _, []) -> 'ok';
bg_set(Node, UUID, Props) ->
    NewProps = maybe_export_vars(Node, UUID, Props),
    AppArgs = process_fs_kv(Node, UUID, NewProps, 'set'),
    send_command(Node, UUID, AppArgs, fun bgapi/3).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% unset channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec unset(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
unset(_, _, []) -> 'ok';
unset(Node, UUID, Props) ->
    AppArgs = process_fs_kv(Node, UUID, Props, 'unset'),
    send_command(Node, UUID, AppArgs, fun api/3).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% unset channel and call variables in FreeSWITCH (in background)
%% @end
%%--------------------------------------------------------------------
-spec bg_unset(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
bg_unset(_, _, []) -> 'ok';
bg_unset(Node, UUID, Props) ->
    AppArgs = process_fs_kv(Node, UUID, Props, 'unset'),
    send_command(Node, UUID, AppArgs, fun bgapi/3).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% export channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
export(_, _, []) -> 'ok';
export(Node, UUID, Props) ->
    Exports = process_fs_kv(Node, UUID, Props, 'export'),
    lager:debug("~p sendmsg export ~p ~p", [Node, UUID, Exports]),
    _ = [freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]) || AppArg <- Exports],
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bridge_export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
bridge_export(_, _, []) -> 'ok';
bridge_export(Node, UUID, Props) ->
    Exports = process_fs_kv(Node, UUID, Props, 'export'),
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
-spec record_call(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
record_call(Node, UUID, Args) ->
    lager:debug("execute on node ~p: uuid_record(~p)", [Node, Args]),
    case freeswitch:api(Node, 'uuid_record', Args) of
        {'ok', _Msg}=Ret ->
            lager:debug("executing uuid_record returned: ~p", [_Msg]),
            Ret;
        {'error', <<"-ERR ", E/binary>>} ->
            lager:debug("error executing uuid_record: ~p", [E]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response="
                                  ,"'",binary:replace(E, <<"\n">>, <<>>),"'"
                                 ]),
            lager:debug("publishing event: ~p", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            {'error', E};
        {'error', _Reason}=Error ->
            lager:debug("error executing uuid_record: ~p", [_Reason]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response=timeout"
                                 ]),
            lager:debug("publishing event: ~p", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            Error
    end.

%% format channel and call variables
-spec process_fs_kv(atom(), ne_binary(), wh_proplist(), atom()) -> [binary()].
process_fs_kv(_, _, [], _) -> [];
process_fs_kv(Node, UUID, [{K, V}|KVs], Action) ->
    X1 = format_fs_kv(K, V, UUID, Action),
    lists:foldl(fun(Prop, Acc) ->
                    process_fs_kv_fold(Node, UUID, Prop, Action, Acc)
                end, X1, KVs).

process_fs_kv_fold(_, _, {_Key, 'undefined'}, _, Acc) -> Acc;
process_fs_kv_fold(Node, UUID, {K, V}, Action, Acc) when is_binary(V) ->
    {Key, Value} = ecallmgr_util:get_fs_key_and_value(K, V, UUID),
    %% NOTE: uuid_setXXX does not support vars:
    %%   switch_channel.c:1287 Invalid data (XXX contains a variable)
    %%   so issue a set command if it is present.
    case binary:match(Key, <<"${">>) =:= 'nomatch'
        andalso binary:match(Value, <<"${">>) =:= 'nomatch'
    of
        'true' -> [format_fs_kv(K, V, UUID, Action), ";" | Acc];
        'false' ->
            AppArg = format_fs_kv(K, V, UUID, Action),
            _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                                ,{"execute-app-name", "set"}
                                                ,{"execute-app-arg", AppArg}
                                               ]),
            Acc
    end;
process_fs_kv_fold(_, _, _, _, Acc) ->
    Acc.

-spec format_fs_kv(ne_binary(), binary(), ne_binary(), atom()) -> [binary()].
format_fs_kv(<<"Hold-Media">>, _, _, 'unset') ->
    [<<"hold_music=">>];
format_fs_kv(<<"ringback">>, _, _, 'unset') ->
    [<<"ringback=">>];
format_fs_kv(<<"Auto-Answer">>, _, _, 'unset') ->
    [<<"alert_info=">>, <<"Auto-Answer=">>];
format_fs_kv(Key, Value, UUID, 'unset') ->
    {K, _} = ecallmgr_util:get_fs_key_and_value(Key, Value, UUID),
    [<<K/binary, "=">>];
format_fs_kv(<<"ringback">>, Media, _, _) ->
    [<<"ringback=", Media/binary>>, <<"transfer_ringback=", Media/binary>>];
format_fs_kv(<<"Auto-Answer">> = Key, Value, _, _) ->
    [<<"alert_info=", "intercom", ";", Key/binary, "=", Value/binary>>];
format_fs_kv(Key, Value, UUID, _) ->
    {K, V} = ecallmgr_util:get_fs_key_and_value(Key, Value, UUID),
    [<<K/binary, "=", V/binary>>].

-spec maybe_export_vars(atom(), ne_binary(), wh_proplist()) -> wh_proplist().
maybe_export_vars(Node, UUID, Props) ->
    lists:foldl(fun({<<"Hold-Media">> = K, V}, Acc) ->
                        export(Node, UUID, [{K, V}]),
                        Acc;
                    ({<<"ringback">> = K, V}, Acc) ->
                        export(Node, UUID, [{K, V}]),
                        Acc;
                    (KV, Acc) -> [KV| Acc]
                end, [], Props).

-spec send_command(atom(), ne_binary(), [binary()], function()) -> ecallmgr_util:send_cmd_ret().
send_command(_, _, [], _) -> 'ok';
send_command(Node, UUID, AppArgs, Fun) when is_list(AppArgs) ->
    Set = list_to_binary([UUID, " ", AppArgs]),
    lager:debug("~p api uuid_setvar_multi ~p", [Node, Set]),
    Fun(Node, 'uuid_setvar_multi', Set).

%% this should be considered temporary
%% TODO fetch set capabilities from freeswitch:version

-spec api(atom(), atom(), binary()) -> ecallmgr_util:send_cmd_ret().
-spec api(atom(), atom(), binary(), boolean()) -> ecallmgr_util:send_cmd_ret().
api(_, _, []) -> 'ok';
api(Node, Cmd, Args) ->
    KazooDPTools = ecallmgr_config:get_boolean(<<"use_kazoo_dptools">>, 'false', Node),
    api(Node, Cmd, Args, KazooDPTools).

api(Node, Cmd, Args, 'false') ->
    freeswitch:api(Node, Cmd, Args);
api(Node, Cmd, Args, 'true') ->
    NewCmd = wh_util:to_atom(<<"kz_", (wh_util:to_binary(Cmd))/binary>>, 'true'),
    freeswitch:api(Node, NewCmd, Args).

-spec bgapi(atom(), atom(), binary()) -> ecallmgr_util:send_cmd_ret().
-spec bgapi(atom(), atom(), binary(), boolean()) -> ecallmgr_util:send_cmd_ret().
bgapi(_, _, []) -> 'ok';
bgapi(Node, Cmd, Args) ->
    KazooDPTools = ecallmgr_config:get_boolean(<<"use_kazoo_dptools">>, 'false', Node),
    bgapi(Node, Cmd, Args, KazooDPTools).

bgapi(Node, Cmd, Args, 'false') ->
    freeswitch:bgapi(Node, Cmd, Args);
bgapi(Node, Cmd, Args, 'true') ->
    NewCmd = wh_util:to_atom(<<"kz_", (wh_util:to_binary(Cmd))/binary>>, 'true'),
    freeswitch:bgapi(Node, NewCmd, Args).

