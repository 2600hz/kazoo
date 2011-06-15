%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Carriers routing WhApp
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>

-module(carriers).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    carriers_sup:start_link().

%% @spec start() -> ok
%% @doc Start the carriers whapp.
start() ->
    start_deps(),
    application:start(carriers).

start_deps() ->
    carriers_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp),
    ensure_started(couchbeam),
    ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the carriers server.
stop() ->
    application:stop(carriers).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
