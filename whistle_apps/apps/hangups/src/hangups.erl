%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% HANGUPS logger
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(hangups).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, set_amqp_host/1, set_couch_host/1]).

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
    ensure_started(whistle_amqp).

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

set_amqp_host(H) ->
    hangups_listener:set_amqp_host(H).

set_couch_host(H) ->
    hangups_listener:set_couch_host(H).
