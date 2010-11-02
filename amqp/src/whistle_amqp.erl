-module(whistle_amqp).

-author('James Aimonetti <james@2600hz.com>').

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    io:format("Start_link~n", []),
    whistle_amqp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the amqp server.
start() ->
    io:format("Start~n", []),
    application:start(whistle_amqp).

%% @spec stop() -> ok
%% @doc Stop the amqp server.
stop() ->
    application:stop(whistle_amqp).
