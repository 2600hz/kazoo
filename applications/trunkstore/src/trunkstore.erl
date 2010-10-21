-module(trunkstore).

-author('James Aimonetti <james@2600hz.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    trunkstore_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(amqp),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    ensure_started(ibrowse),
    ensure_started(couchbeam),
    trunkstore_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    trunkstore_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(amqp),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    ensure_started(ibrowse),
    ensure_started(couchbeam),
    application:start(trunkstore).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(trunkstore).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
