%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% stepswitch routing WhApp
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>

-module(stepswitch).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    stepswitch_sup:start_link().

%% @spec start() -> ok
%% @doc Start the stepswitch whapp.
start() ->
    start_deps(),
    application:start(stepswitch).

start_deps() ->
    stepswitch_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp),
    ensure_started(couchbeam),
    ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the stepswitch server.
stop() ->
    application:stop(stepswitch).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
