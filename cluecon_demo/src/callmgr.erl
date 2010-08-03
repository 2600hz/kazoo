-module(callmgr).

-author('James Aimonetti <james@2600hz.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    callmgr_deps:ensure(),
    ensure_started(sasl),
    ensure_started(amqp),
    callmgr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    callmgr_deps:ensure(),
    ensure_started(sasl),
    ensure_started(amqp),
    application:start(callmgr).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(callmgr).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
