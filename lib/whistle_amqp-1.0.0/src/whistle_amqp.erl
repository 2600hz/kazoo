-module(whistle_amqp).
-behaviour(application).

-author('James Aimonetti <james@2600hz.org>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%%
start(_Type, _Args) ->
    _ = start_deps(),
    whistle_amqp_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    whistle_amqp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the amqp server.
start() ->
    _ = start_deps(),
    application:start(whistle_amqp).

start_deps() ->
    whistle_amqp_deps:ensure(?MODULE),
    ok = wh_util:ensure_started(sasl),
    ok = wh_util:ensure_started(riak_err),
    ok = wh_util:ensure_started(amqp_client).

%% @spec stop() -> ok
%% @doc Stop the amqp server.
stop() ->
    application:stop(whistle_amqp).

stop(_) ->
    ok.
