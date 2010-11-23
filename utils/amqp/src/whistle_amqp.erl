-module(whistle_amqp).
-behaviour(application).

-author('James Aimonetti <james@2600hz.com>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%%
start(_Type, _Args) ->
    whistle_amqp_deps:ensure(?MODULE),
    whistle_amqp_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    whistle_amqp_deps:ensure(?MODULE),
    whistle_amqp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the amqp server.
start() ->
    whistle_amqp_deps:ensure(?MODULE),
    application:start(whistle_amqp).

%% @spec stop() -> ok
%% @doc Stop the amqp server.
stop() ->
    application:stop(whistle_amqp).

stop(_) ->
    ok.
