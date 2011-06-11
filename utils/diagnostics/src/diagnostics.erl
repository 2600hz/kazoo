-module(diagnostics).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    diagnostics_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    application:start(diagnostics).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(diagnostics).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
