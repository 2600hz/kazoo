-module(rscrpt).

-author('James Aimonetti <james@sfvoip.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    rscrpt_deps:ensure(),
    ensure_started(sasl),
    ensure_started(os_mon),
    rscrpt_sup:start_link().

%% @spec start() -> ok
%% @doc Start the rscrpt server.
start() ->
    rscrpt_deps:ensure(),
    ensure_started(sasl),
    ensure_started(os_mon),
    application:start(rscrpt).

%% @spec stop() -> ok
%% @doc Stop the rscrpt server.
stop() ->
    application:stop(rscrpt).
