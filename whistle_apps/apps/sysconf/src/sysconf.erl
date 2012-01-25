%%% @author Edouard Swiac <edouard@2600hz.org> 
%%% @copyright (C) 2011 Edouard Swiac
%%% @doc
%%% 
%%% @end

-module(sysconf).

-author('Edouard Swiac <edouard@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    sysconf_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    application:start(sysconf).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(whistle_amqp). % amqp wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(sysconf).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
