%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_maintenance).

-export([show_calls/0]).
-export([add_fs_node/1]).
-export([list_fs_nodes/0]).
-export([remove_fs_node/1]).
-export([show_channels/0]).
-export([sync_channels/0
         ,sync_channels/1
        ]).
-export([sync_conferences/0
         ,sync_conferences/1
        ]).
-export([flush_node_channels/1]).
-export([flush_node_conferences/1]).
-export([flush_registrar/0]).
-export([flush_util/0]).
-export([status/0]).

-include("ecallmgr.hrl").

-spec add_fs_node(string() | binary() | atom()) -> 'ok'.
add_fs_node(Node) when not is_atom(Node) ->
    add_fs_node(wh_util:to_atom(Node, 'true'));
add_fs_node(Node) -> ecallmgr_fs_nodes:add(Node).

-spec remove_fs_node(string() | binary() | atom()) -> 'ok'.
remove_fs_node(Node) when not is_atom(Node) ->
    remove_fs_node(wh_util:to_atom(Node, 'true'));
remove_fs_node(Node) -> ecallmgr_fs_nodes:remove(Node).

-spec list_fs_nodes() -> [atom(),...] | [].
list_fs_nodes() -> ecallmgr_fs_nodes:connected().

-spec show_channels() -> 'no_return'.
show_channels() ->
    case ecallmgr_fs_channel:show_all() of
        [] -> io:format("no channels~n", []);
        [Channel|_]=Channels ->
            Headers = wh_util:join_binary([K
                                           || {K, _} <- wh_json:to_proplist(Channel)
                                          ], <<",">>),
            io:format("~s~n", [Headers]),
            do_show_channels(Channels)
    end,
    'no_return'.

do_show_channels([]) -> 'ok';
do_show_channels([Channel|Channels]) ->
    Values = string:join([wh_util:to_list(V) || {_, V} <- wh_json:to_proplist(Channel)], ","),
    io:format("~s~n", [Values]),
    do_show_channels(Channels).

-spec sync_channels() -> 'ok'.
sync_channels() ->
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
        ],
    'ok'.

-spec sync_channels(text()) -> 'ok'.
sync_channels(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec sync_conferences() -> 'ok'.
sync_conferences() ->
    _ = [ecallmgr_fs_conferences:sync_conferences(N)
         || N <- ecallmgr_fs_nodes:connected()
        ],
    'ok'.

-spec sync_conferences(text()) -> 'ok'.
sync_conferences(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    ecallmgr_fs_conferences:sync_conferences(N),
    'ok'.

-spec flush_node_channels(string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    ecallmgr_fs_nodes:flush_node_channels(Node).

-spec flush_node_conferences(string() | binary() | atom()) -> 'ok'.
flush_node_conferences(Node) ->
    ecallmgr_fs_conferences:flush_node(Node).

-spec flush_registrar() -> 'ok'.
flush_registrar() ->
    wh_cache:flush_local(?ECALLMGR_REG_CACHE).

-spec flush_util() -> 'ok'.
flush_util() ->
    wh_cache:flush_local(?ECALLMGR_UTIL_CACHE).

-spec show_calls() -> 'no_return'.
show_calls() ->
    EventWorkers = gproc:lookup_pids({'p', 'l', 'call_events_processes'}),
    io:format("Call Event Process: ~p ~n", [length(EventWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [EventWorker
                                       ,ecallmgr_call_events:node(EventWorker)
                                       ,ecallmgr_call_events:callid(EventWorker)
                                      ])
         || EventWorker <- EventWorkers
        ],
    ControlWorkers = gproc:lookup_pids({'p', 'l', 'call_control'}),
    io:format("Call Control Process: ~p~n", [length(ControlWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [ControlWorker
                                       ,ecallmgr_call_control:node(ControlWorker)
                                       ,ecallmgr_call_control:callid(ControlWorker)
                                      ])
         || ControlWorker <- ControlWorkers
        ],
    'no_return'.

-spec status() -> 'no_return'.
status() ->
    ecallmgr_fs_nodes:status(),
    'no_return'.
