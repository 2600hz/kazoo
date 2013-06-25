%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_command).

-export([set/3, set/4
         ,export/3
        ]).

-include("ecallmgr.hrl").

-spec set(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
-spec set(atom(), ne_binary(), text(), text()) -> ecallmgr_util:send_cmd_ret().

set(_, _, []) -> 'ok';
set(Node, UUID, [{K,V}]) ->
    freeswitch:api(Node, 'uuid_setvar', list_to_binary([UUID, " ", K, " ", V]));
set(Node, UUID, [{K,V}|KVs]) ->
    Multiset = lists:foldl(fun({Key, Val}, Acc) ->
                                   [Val, "=", Key, ";" | Acc]
                           end, [V, "=", K], KVs),
    freeswitch:api(Node, 'uuid_setvar_multi', list_to_binary([UUID, " ", lists:reverse(Multiset)])).

set(Node, UUID, K, V) -> set(Node, UUID, [{K, V}]).

-define(EXPORT_DELIMITER, <<"|">>).
-spec export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
export(_, _, []) -> 'ok';
export(Node, UUID, [{K,V}|Exports]) ->
    Export = <<K/binary, "=", V/binary>>,
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", "export"}
                                    ,{"execute-app-arg", wh_util:to_list(Export)}
                                   ]),
    export(Node, UUID, Exports).
