%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Its a party and your invite'd...
%%% @end
%%% Created : 27 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conference).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    conference_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    start_deps(),
    application:start(partyline).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(whistle_amqp), % amqp wrapper
    ensure_started(whistle_couch). % couch wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    ok = application:stop(partyline).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
