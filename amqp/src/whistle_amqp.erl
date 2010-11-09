-module(whistle_amqp).
-behaviour(application).

-author('James Aimonetti <james@2600hz.com>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%% 
start(Type, Args) ->
    io:format("AMQP start with type ~p and args ~p~n", [Type, Args]),
    whistle_amqp_deps:ensure(?MODULE),
    whistle_amqp_sup:start_link().
    

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    io:format("AMQP Start link~n", []),
    whistle_amqp_deps:ensure(?MODULE),
    whistle_amqp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the amqp server.
start() ->
    io:format("AMQP Start~n", []),
    whistle_amqp_deps:ensure(?MODULE),
    application:start(whistle_amqp).

%% @spec stop() -> ok
%% @doc Stop the amqp server.
stop() ->
    application:stop(whistle_amqp).

stop(_) ->
    ok.
