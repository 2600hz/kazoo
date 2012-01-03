%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% CDR logger
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(cdr).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    cdr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    start_deps(),
    application:start(cdr).

start_deps() ->
    cdr_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp).

%% @spec stop() -> ok
%% @doc Stop the cdr server.
stop() ->
    application:stop(cdr).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
