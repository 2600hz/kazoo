%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% HANGUPS logger
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(hangups).

-author('James Aimonetti <james@2600hz.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    hangups_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    start_deps(),
    application:start(hangups).

start_deps() ->
    hangups_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    ensure_started(couchbeam),
    ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the hangups server.
stop() ->
    application:stop(hangups).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
