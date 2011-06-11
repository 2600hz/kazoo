-module(log_server).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    log_server_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    ensure_started(log_roller_server),
    log_server_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    log_server_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    ensure_started(log_roller_server),
    log_server_sup:start_link().

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(log_server).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
