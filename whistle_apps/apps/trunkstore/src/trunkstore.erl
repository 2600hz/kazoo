-module(trunkstore).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, start_deps/0]).

-include("ts.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    start_deps(),
    trunkstore_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    application:start(trunkstore).

start_deps() ->
    trunkstore_deps:ensure(),
    wh_util:ensure_started(sasl),
    wh_util:ensure_started(crypto),
    wh_util:ensure_started(whistle_amqp),
    wh_util:ensure_started(whistle_couch),
    wh_util:ensure_started(ibrowse).

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(trunkstore).
