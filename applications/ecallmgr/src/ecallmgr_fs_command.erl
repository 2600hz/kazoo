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
    freeswitch:api(Node, 'uuid_setvar', Set);
set(Node, UUID, [{K,V}|KVs]) ->
    Multiset = lists:foldl(fun({Key, Val}, Acc) ->
                                   [Val, "=", Key, ";" | Acc]
                           end, [V, "=", K], KVs),
    Set = list_to_binary([UUID, " ", lists:reverse(Multiset)]),
    lager:debug("~s api uuid_setvar_mulit ~s", [Node, Set]),
    freeswitch:api(Node, 'uuid_setvar_multi', Set).

set(Node, UUID, K, V) -> set(Node, UUID, [{K, V}]).

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
