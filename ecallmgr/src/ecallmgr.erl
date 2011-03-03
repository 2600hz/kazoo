-module(ecallmgr).

-author('James Aimonetti <james@2600hz.com>').
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
    logger:start(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(riak_err),
    ensure_started(whistle_amqp),
    ensure_started(ibrowse).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(ecallmgr).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

add_fs_node(Node) ->
    ecallmgr_fs_handler:add_fs_node(Node).

rm_fs_node(Node) ->
    ecallmgr_fs_handler:rm_fs_node(Node).

%% list of handlers to retrieve diagnostics from
diagnostics() ->
    [{freeswitch_nodes, ecallmgr_fs_handler:diagnostics()}].
