-module(ecallmgr).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, add_fs_node/1, rm_fs_node/1, diagnostics/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    ecallmgr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    start_deps(),
    application:start(ecallmgr).

start_deps() ->
    ecallmgr_deps:ensure(),

    case application:get_env(reloader) of
	{ok, true} -> reloader:start();
	_ -> ok
    end,

    logger:start_link(),
    wh_util:ensure_started(sasl),
    wh_util:ensure_started(crypto),
    wh_util:ensure_started(riak_err),
    wh_util:ensure_started(whistle_amqp),
    wh_util:ensure_started(ibrowse).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(ecallmgr).

add_fs_node(Node) ->
    ecallmgr_fs_handler:add_fs_node(Node).

rm_fs_node(Node) ->
    ecallmgr_fs_handler:rm_fs_node(Node).

%% list of handlers to retrieve diagnostics from
diagnostics() ->
    [{freeswitch_nodes, ecallmgr_fs_handler:diagnostics()}].
