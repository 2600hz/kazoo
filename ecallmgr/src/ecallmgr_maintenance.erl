%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
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

-spec show_channels/0 :: () -> 'ok'.
show_channels() ->
    case ecallmgr_fs_nodes:show_channels() of
        [] -> "no channels";
        [Channel|_]=Channels ->
            Header = string:join([wh_util:to_list(K) || {K, _} <- wh_json:to_proplist(Channel)], ","),
            do_show_channels(Channels, io_lib:format("~s~n", [Header]))
    end.

do_show_channels([], String) ->
    lists:flatten(String);
do_show_channels([Channel|Channels], String) ->
    Values = string:join([wh_util:to_list(V) || {_, V} <- wh_json:to_proplist(Channel)], ","),
    do_show_channels(Channels, io_lib:format("~s~s~n", [String, Values])).

-spec sync_channels/0 :: () -> 'ok'.
-spec sync_channels/1 :: (string() | binary() | atom()) -> 'ok'.

sync_channels() ->
    ecallmgr_fs_nodes:sync_channels().

sync_channels(Node) ->
    ecallmgr_fs_nodes:sync_channels(Node).

-spec flush_node_channels/1 :: (string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    ecallmgr_fs_nodes:flush_node_channels(Node).

-spec show_calls/0 :: () -> 'ok'.
show_calls() ->
    EventWorkers = gproc:lookup_pids({p, l, call_events}),
    lager:info("Call Event Process: ~p ~n", [length(EventWorkers)]),
    _ = [lager:info("    ~p: ~s ~s~n", [EventWorker
                                        ,ecallmgr_call_events:node(EventWorker)
                                        ,ecallmgr_call_events:callid(EventWorker)
                                       ])
         || EventWorker <- EventWorkers],
    ControlWorkers = gproc:lookup_pids({p, l, call_control}),
    lager:info("Call Control Process: ~p~n", [length(ControlWorkers)]),
    _ = [lager:info("    ~p: ~s ~s~n", [ControlWorker
                                        ,ecallmgr_call_control:node(ControlWorker)
                                        ,ecallmgr_call_control:callid(ControlWorker)
                                       ])
         || ControlWorker <- ControlWorkers],    
    ok.
    


