-module(whistle_couch).
-behaviour(application).

-author('James Aimonetti <james@2600hz.org>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%%
start(_Type, _Args) ->
    _ = start_deps(),
    whistle_couch_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    whistle_couch_sup:start_link().

%% @spec start() -> ok
%% @doc Start the couch server.
start() ->
    _ = start_deps(),
    application:start(whistle_couch).

start_deps() ->
    whistle_couch_deps:ensure(?MODULE),
    ok = wh_util:ensure_started(sasl),
    ok = wh_util:ensure_started(crypto),
    ok = wh_util:ensure_started(riak_err),
    ok = wh_util:ensure_started(ibrowse),
    ok = wh_util:ensure_started(couchbeam).

%% @spec stop() -> ok
%% @doc Stop the couch server.
stop() ->
    application:stop(whistle_couch).

stop(_) ->
    ok.
