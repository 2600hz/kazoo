%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011 James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created :  Thu, 13 Jan 2011 22:12:40 GMT: James Aimonetti <james@2600hz.org>

-module(registrar).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    registrar_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    application:start(registrar).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(whistle_amqp). % amqp wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(registrar).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
