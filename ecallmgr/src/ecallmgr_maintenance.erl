%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_maintenance).

-export([show_calls/0]).
-export([add_fs_node/1]).
-export([list_fs_nodes/0]).
-export([remove_fs_node/1]).
-export([show_channels/0]).
-export([sync_channels/0]).
-export([sync_channels/1]).
-export([flush_node_channels/1]).
-export([flush_registrar/0]).
-export([status/0]).

-include("ecallmgr.hrl").

-spec add_fs_node/1 :: (string() | binary() | atom()) -> 'ok'.
add_fs_node(Node) when not is_atom(Node) ->
    add_fs_node(wh_util:to_atom(Node, true));
add_fs_node(Node) ->
    ecallmgr_fs_nodes:add(Node).

-spec remove_fs_node/1 :: (string() | binary() | atom()) -> 'ok'.
remove_fs_node(Node) when not is_atom(Node) ->
    remove_fs_node(wh_util:to_atom(Node, true));
remove_fs_node(Node) ->
    ecallmgr_fs_nodes:remove(Node).

-spec list_fs_nodes/0 :: () -> [atom(),...] | [].
list_fs_nodes() ->
    ecallmgr_fs_nodes:connected().

-spec show_channels/0 :: () -> 'no_return'.
show_channels() ->
    case ecallmgr_fs_nodes:show_channels() of
        [] -> io:format("no channels~n", []);
        [Channel|_]=Channels ->
            Headers = string:join([wh_util:to_list(K) || {K, _} <- wh_json:to_proplist(Channel)], ","),
            io:format("~s~n", [Headers]),
            do_show_channels(Channels)
    end,
    no_return.

do_show_channels([]) ->
    ok;
do_show_channels([Channel|Channels]) ->
    Values = string:join([wh_util:to_list(V) || {_, V} <- wh_json:to_proplist(Channel)], ","),
    io:format("~s~n", [Values]),
    do_show_channels(Channels).

-spec sync_channels/0 :: () -> 'ok'.
-spec sync_channels/1 :: (string() | binary() | atom()) -> 'ok'.

sync_channels() ->
    ecallmgr_fs_nodes:sync_channels().

sync_channels(Node) ->
    ecallmgr_fs_nodes:sync_channels(Node).

-spec flush_node_channels/1 :: (string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    ecallmgr_fs_nodes:flush_node_channels(Node).

-spec flush_registrar/0 :: () -> 'ok'.
flush_registrar() ->
    wh_cache:flush_local(?ECALLMGR_REG_CACHE).

-spec show_calls/0 :: () -> 'no_return'.
show_calls() ->
    EventWorkers = gproc:lookup_pids({p, l, call_events_processes}),
    io:format("Call Event Process: ~p ~n", [length(EventWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [EventWorker
                                       ,ecallmgr_call_events:node(EventWorker)
                                       ,ecallmgr_call_events:callid(EventWorker)
                                      ])
         || EventWorker <- EventWorkers],
    ControlWorkers = gproc:lookup_pids({p, l, call_control}),
    io:format("Call Control Process: ~p~n", [length(ControlWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [ControlWorker
                                       ,ecallmgr_call_control:node(ControlWorker)
                                       ,ecallmgr_call_control:callid(ControlWorker)
                                      ])
         || ControlWorker <- ControlWorkers],    
    no_return.

-spec status/0 :: () -> no_return.
status() ->
    FolsomMetrics = folsom_metrics:get_metrics(),
    FsMetrics = lists:foldl(fun(FMetric, Acc) ->
                                    case binary:split(FMetric, <<".">>, [global]) of
                                        [Type, <<"freeswitch">>, <<"nodes">>, EncodedNode, StatKey] ->
                                            Metric = <<"freeswitch.nodes.", EncodedNode/binary, ".",StatKey/binary>>,
                                            StatMod = wh_util:to_atom(Type),
                                            Node = cowboy_http:urldecode(EncodedNode),
                                            Stat = case StatMod of
                                                       wh_timer ->
                                                           DateTime = calendar:now_to_datetime(wh_timer:get_timestamp(Metric)),
                                                           {wh_timer:get(Metric), wh_util:pretty_print_datetime(DateTime)};
                                                       _ ->
                                                           StatMod:get(Metric)
                                                   end,
                                            dict:update(Node, fun(List) -> [{StatKey, Stat} | List] end, [{StatKey, Stat}], Acc);
                                        _ -> Acc
                                    end
                            end
                            ,dict:new()
                            ,FolsomMetrics
                           ),
    lists:foreach(fun(Node) ->
                          Stats = dict:fetch(Node, FsMetrics),
                          io:format("----- ~s stats -----~n", [Node]),
                          lists:foreach(fun({Key, {Val1, Val2}}) when is_binary(Val2) ->
                                                io:format("~s: ~p (~s)~n", [Key, Val1, Val2]);
                                           ({Key, {Val1, Val2}}) ->
                                                io:format("~s: ~p (~s)~n", [Key, Val1, Val2]);
                                           ({Key, Val}) ->
                                                io:format("~s: ~p~n", [Key, Val])
                                        end
                                        ,Stats
                                       ),
                          io:format("~n")
                  end
                  ,dict:fetch_keys(FsMetrics)
                 ),
    no_return.
