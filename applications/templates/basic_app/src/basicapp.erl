%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created :  4 Dec 2010 at 16:12: James Aimonetti <james@2600hz.org>

-module(basicapp).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, set_amqp_host/1, set_couch_host/1]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    basicapp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    start_deps(),
    application:start(basicapp).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(whistle_amqp), % amqp wrapper
    ensure_started(dynamic_compile), % for logging
    ensure_started(log_roller), % for distrubuted logging
    ensure_started(whistle_couch). % couch wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(basicapp).

set_amqp_host(H) ->
    H. % add any servers that need amqp hosts updated here

set_couch_host(H) ->
    H. % add any servers that need couch hosts updated here

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
