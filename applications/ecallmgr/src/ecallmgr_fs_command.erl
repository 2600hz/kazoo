%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_command).

-export([set/3, set/4
         ,unset/3, unset/4
         ,bg_set/3, bg_set/4
         ,bg_unset/3, bg_unset/4
         ,export/3, bridge_export/3
         ,record_call/3
        ]).

-include("ecallmgr.hrl").

-spec set(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
-spec set(atom(), ne_binary(), text(), text()) -> ecallmgr_util:send_cmd_ret().

set(_, _, []) -> 'ok';
set(Node, UUID, [{K,V}]) ->
    Set = list_to_binary([UUID, " ", K, " ", V]),
    lager:debug("~s api uuid_setvar ~s", [Node, Set]),
    api(Node, 'uuid_setvar', Set);
set(Node, UUID, [{K,V}|KVs]) ->
    X1 = ecallmgr_util:get_fs_kv(K, V, UUID),
    Multiset = lists:foldl(fun({Key, Val}, Acc) ->
                                   X = ecallmgr_util:get_fs_kv(Key, Val, UUID),
                                   [wh_util:to_list(X), ";" | Acc]
                           end, [wh_util:to_list(X1)], KVs),
    Set = list_to_binary([UUID, " ", lists:reverse(Multiset)]),
    lager:debug("~s api uuid_setvar_multi ~s", [Node, Set]),
    api(Node, 'uuid_setvar_multi', Set).

set(Node, UUID, K, V) -> set(Node, UUID, [{K, V}]).


-spec bg_set(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
-spec bg_set(atom(), ne_binary(), text(), text()) -> ecallmgr_util:send_cmd_ret().

bg_set(_, _, []) -> 'ok';
bg_set(Node, UUID, [{K,V}]) ->
    Set = list_to_binary([UUID, " ", K, " ", V]),
    lager:debug("~s api uuid_setvar ~s", [Node, Set]),
    bgapi(Node, 'uuid_setvar', Set);
bg_set(Node, UUID, [{K,V}|KVs]) ->
    Multiset = lists:foldl(fun({Key, Val}, Acc) ->
                                   [Val, "=", Key, ";" | Acc]
                           end, [V, "=", K], KVs),
    Set = list_to_binary([UUID, " ", lists:reverse(Multiset)]),
    lager:debug("~s api kz_uuid_setvar_multi ~s", [Node, Set]),
    bgapi(Node, 'uuid_setvar_multi', Set).

bg_set(Node, UUID, K, V) -> bg_set(Node, UUID, [{K, V}]).

-spec unset(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
-spec unset(atom(), ne_binary(), text(), text()) -> ecallmgr_util:send_cmd_ret().

unset(_, _, []) -> 'ok';
unset(Node, UUID, [K]) ->
    Set = list_to_binary([UUID, " ", K]),
    lager:debug("~s api uuid_setvar ~s", [Node, Set]),
    api(Node, 'uuid_setvar', Set);
unset(Node, UUID, [K|KVs]) ->
    Multiset = lists:foldl(fun(K1, Acc) ->
                                   [K1, "=;" | Acc]
                           end, [K, "=;"], KVs),
    Set = list_to_binary([UUID, " ", Multiset]),
    lager:debug("~s api kz_uuid_setvar_multi ~s", [Node, Set]),
    api(Node, 'uuid_setvar_multi', Set).

unset(Node, UUID, K, V) -> unset(Node, UUID, [{K, V}]).

-spec bg_unset(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
-spec bg_unset(atom(), ne_binary(), text(), text()) -> ecallmgr_util:send_cmd_ret().

bg_unset(_, _, []) -> 'ok';
bg_unset(Node, UUID, [K]) ->
    Set = list_to_binary([UUID, " ", K]),
    lager:debug("~s api uuid_setvar ~s", [Node, Set]),
    bgapi(Node, 'uuid_setvar', Set);
bg_unset(Node, UUID, [K|KVs]) ->
    Multiset = lists:foldl(fun(K1, Acc) ->
                                   [K1, "=;" | Acc]
                           end, [K, "=;"], KVs),
    Set = list_to_binary([UUID, " ", Multiset]),
    lager:debug("~s api kz_uuid_setvar_multi ~s", [Node, Set]),
    bgapi(Node, 'uuid_setvar_multi', Set).

bg_unset(Node, UUID, K, V) -> bg_unset(Node, UUID, [{K, V}]).

-define(EXPORT_DELIMITER, <<"|">>).
-spec export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
export(_, _, []) -> 'ok';
export(Node, UUID, [{K,V}|Exports]) ->
    Export = <<K/binary, "=", V/binary>>,
    lager:debug("~s sendmsg export ~s ~s", [Node, UUID, Export]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", "export"}
                                    ,{"execute-app-arg", wh_util:to_list(Export)}
                                   ]),
    export(Node, UUID, Exports).

-spec bridge_export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
bridge_export(_, _, []) -> 'ok';
bridge_export(Node, UUID, [{K,V}|Exports]) ->
    Export = <<K/binary, "=", V/binary>>,
    lager:debug("~s sendmsg bridge_export ~s ~s", [Node, UUID, Export]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", "bridge_export"}
                                    ,{"execute-app-arg", wh_util:to_list(Export)}
                                   ]),
    bridge_export(Node, UUID, Exports).

record_call(Node, UUID, Args) ->
    lager:debug("execute on node ~s: uuid_record(~s)", [Node, Args]),
    case freeswitch:api(Node, 'uuid_record', Args) of
        {'ok', _Msg}=Ret ->
            lager:debug("executing uuid_record returned: ~s", [_Msg]),
            Ret;
        {'error', <<"-ERR ", E/binary>>} ->
            lager:debug("error executing uuid_record: ~s", [E]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response="
                                  ,"'",binary:replace(E, <<"\n">>, <<>>),"'"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            {'error', E};
        {'error', _Reason}=Error ->
            lager:debug("error executing uuid_record: ~p", [_Reason]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response=timeout"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = ecallmgr_util:send_cmd(Node, UUID, "application", Evt),
            Error
    end.

%% this should be considered temporary
%% TODO fetch set capabilities from freeswitch:version

-spec api(atom(), atom(), any()) -> any().
-spec api(atom(), atom(), any(), boolean()) -> any().

api(Node, Cmd, Args) ->
    KazooDPTools = ecallmgr_config:get_boolean(<<"use_kazoo_dptools">>, 'false', Node),
    api(Node, Cmd, Args, KazooDPTools).

api(Node, Cmd, Args, 'false') ->
    freeswitch:api(Node, Cmd, Args);
api(Node, Cmd, Args, 'true') ->
    NewCmd = wh_util:to_atom(<<"kz_", (wh_util:to_binary(Cmd))/binary>>, 'true'),
    freeswitch:api(Node, NewCmd, Args).

-spec bgapi(atom(), atom(), any()) -> any().
-spec bgapi(atom(), atom(), any(), boolean()) -> any().

bgapi(Node, Cmd, Args) ->
    KazooDPTools = ecallmgr_config:get_boolean(<<"use_kazoo_dptools">>, 'false', Node),
    bgapi(Node, Cmd, Args, KazooDPTools).

bgapi(Node, Cmd, Args, 'false') ->
    freeswitch:bgapi(Node, Cmd, Args);
bgapi(Node, Cmd, Args, 'true') ->
    NewCmd = wh_util:to_atom(<<"kz_", (wh_util:to_binary(Cmd))/binary>>, 'true'),
    freeswitch:bgapi(Node, NewCmd, Args).

