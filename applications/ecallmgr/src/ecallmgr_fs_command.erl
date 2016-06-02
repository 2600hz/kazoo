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
set(Node, UUID, [{<<"Auto-Answer">> = K, V}]) ->
    do_set(Node, UUID, [{<<"Alert-Info">>, <<"intercom">>}, {K, V}], fun api/3);
set(Node, UUID, [{<<"Auto-Answer-Suppress-Notify">> = K, V}]) ->
    do_set(Node, UUID, [{<<"Alert-Info">>, <<"intercom">>}
                       ,{<<"Auto-Answer">>, <<"true">>}
                       ,{K, V}
                       ], fun api/3);
set(Node, UUID, [{<<"Hold-Media">>, Value}]) ->
    Media = ecallmgr_util:media_path(Value, 'extant', UUID, wh_json:new()),
    AppArg = wh_util:to_list(<<"hold_music=", Media/binary>>),
    %% NOTE: due to how we handle hold_music we need to export
    %%    this var rather than set...
    _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]),
    'ok';
set(Node, UUID, [{<<"ringback">>, Media}]) ->
    AppArgs = [<<"ringback=", Media/binary>>,<<"transfer_ringback=", Media/binary>>],
    _ = [freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]) || AppArg <- AppArgs],
    'ok';
set(Node, UUID, [{K, V}]) when is_binary(V) ->
    do_set(Node, UUID, [{K, V}], fun api/3);
set(_, _, [{_, _}]) ->
    'ok';
set(Node, UUID, [{_, _}|_]=Props) ->
    Multiset = lists:foldl(fun(Prop, Acc) ->
                              set_fold(Node, UUID, Prop, Acc)
                           end, [], Props),
    do_set(Node, UUID, Multiset, fun api/3).

set(Node, UUID, K, V) -> set(Node, UUID, [{K, V}]).

set_fold(_, _, {_Key, 'undefined'}, Acc) -> Acc;
set_fold(Node, UUID, {<<"Hold-Media">>, Value}, Acc) ->
    Media = ecallmgr_util:media_path(Value, 'extant', UUID, wh_json:new()),
    AppArg = wh_util:to_list(<<"hold_music=", Media/binary>>),
    %% NOTE: due to how we handle hold_music we need to export
    %%    this var rather than set...
    _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]),
    Acc;
set_fold(_, _, {<<"Auto-Answer">> = K, V}, Acc) ->
    [{<<"Alert-Info">>, <<"intercom">>} , {K, V} | Acc];
set_fold(_, _, {<<"Auto-Answer-Suppress-Notify">> = K, V}, Acc) ->
    [{<<"Alert-Info">>, <<"intercom">>}
    ,{<<"Auto-Answer">>, <<"true">>}
    ,{K, V} | Acc];
set_fold(_, _, {K, V}, Acc) when is_binary(V) ->
    [{K, V} | Acc];
set_fold(_, _, _, Acc) ->
    Acc.

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
export(Node, UUID, Props) ->
    KVs = lists:foldl(fun export_fold/2, [], Props),
    do_export(Node, UUID, KVs).

-spec export_fold({ne_binary(), ne_binary()}, wh_proplist()) -> wh_proplist().
export_fold({<<"Auto-Answer">>, _}=KV, Acc) ->
    [{<<"Alert-Info">>, <<"intercom">>}, KV | Acc];
export_fold({<<"Auto-Answer-Suppress-Notify">>, _}=KV, Acc) ->
    [{<<"Alert-Info">>, <<"intercom">>}
    ,{<<"Auto-Answer">>, <<"true">>}
    ,KV | Acc ];
export_fold(KV, Acc) ->
    [KV | Acc].

-spec do_export(atom(), ne_binary(), wh_proplist()) -> ecallmgr_util:send_cmd_ret().
do_export(_, _, []) -> 'ok';
do_export(Node, UUID, [{K,V}|Exports]) ->
    do_export_val(Node, UUID, ecallmgr_util:get_fs_key_and_value(K, V, UUID)),
    do_export(Node, UUID, Exports).

-spec do_export_val(atom(), ne_binary(), {ne_binary(), ne_binary()} | 'skip') -> ecallmgr_util:send_cmd_ret().
do_export_val(_Node, _UUID, 'skip') -> 'ok';
do_export_val(Node, UUID, {K,V}) ->
    Export = <<K/binary, "=", V/binary>>,
    lager:debug("~s sendmsg export ~s ~s", [Node, UUID, Export]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", "export"}
                                    ,{"execute-app-arg", wh_util:to_list(Export)}
                                   ]).

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

-spec do_set(atom(), ne_binary(), wh_proplist(), function()) -> ecallmgr_util:send_cmd_ret().
do_set(Node, UUID, [{K,V}], Fun) ->
    {Key, Value} = ecallmgr_util:get_fs_key_and_value(K, V, UUID),
    Set = list_to_binary([UUID, " ", Key, " ", Value]),
    lager:debug("~s api uuid_setvar ~s", [Node, Set]),
    Fun(Node, 'uuid_setvar', Set);
do_set(Node, UUID, [{K,V}|KVs], Fun) ->
    {K1, V1} = ecallmgr_util:get_fs_key_and_value(K, V, UUID),
    X1 = [K1, "=", V1],
    Multiset = lists:foldl(fun(Prop, Acc) ->
                                   do_set_fold(Node, UUID, Prop, Acc)
                           end, X1, KVs),
    Set = list_to_binary([UUID, " ", Multiset]),
    lager:debug("~s api uuid_setvar_multi ~s", [Node, Set]),
    Fun(Node, 'uuid_setvar_multi', Set).

do_set_fold(Node, UUID, {K, V}, Acc) ->
    {Key, Value} = ecallmgr_util:get_fs_key_and_value(K, V, UUID),

    %% NOTE: uuid_setXXX does not support vars:
    %%   switch_channel.c:1287 Invalid data (XXX contains a variable)
    %%   so issue a set command if it is present.
    case binary:match(Key, <<"${">>) =:= 'nomatch'
        andalso binary:match(Value, <<"${">>) =:= 'nomatch'
    of
        'true' -> [[Key, "=", Value], ";" | Acc];
        'false' ->
            AppArg = wh_util:to_list(<<Key/binary, "=", Value/binary>>),
            _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                                ,{"execute-app-name", "set"}
                                                ,{"execute-app-arg", AppArg}
                                               ]),
            Acc
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

