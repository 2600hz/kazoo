-module(whistle_couch).
-behaviour(application).

-author('James Aimonetti <james@2600hz.com>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%%
start(_Type, _Args) ->
    start_deps(),
    whistle_couch_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    whistle_couch_sup:start_link().

%% @spec start() -> ok
%% @doc Start the couch server.
start() ->
    start_deps(),
    application:start(whistle_couch).

start_deps() ->
    whistle_couch_deps:ensure(?MODULE),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(ibrowse),
    ensure_started(couchbeam),
    ensure_started(dynamic_compile),
    ensure_started(log_roller).

%% @spec stop() -> ok
%% @doc Stop the couch server.
stop() ->
    application:stop(whistle_couch).

stop(_) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
