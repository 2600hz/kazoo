-module(ecallmgr).

-author('James Aimonetti <james@2600hz.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ecallmgr_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(amqp),
    ensure_started(ibrowse),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    ecallmgr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    ecallmgr_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(amqp),
    ensure_started(ibrowse),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    application:start(ecallmgr).

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
