-module(trunkstore).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, start_deps/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
-spec(start_link/0 :: () -> tuple(ok, pid()) | ignore | tuple(error, term())).
start_link() ->
    start_deps(),
    Res = trunkstore_sup:start_link(),
    spawn(fun() -> [ ts_responder_sup:start_handler() || _ <- [1,2,3] ] end),
    Res.

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    application:start(trunkstore).

start_deps() ->
    trunkstore_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp),
    ensure_started(whistle_couch),
    ensure_started(ibrowse).

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
