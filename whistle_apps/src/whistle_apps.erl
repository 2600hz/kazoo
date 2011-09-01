%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% WHISTLE_APPS logger
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(whistle_apps).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    whistle_apps_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    start_deps(),
    application:start(whistle_apps).

start_deps() ->
    whistle_apps_deps:ensure(),
    reloader:start(),
    logger:start_link(),

    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(ibrowse),
    ensure_started(riak_err),
    ensure_started(couchbeam),
    ensure_started(whistle_amqp),
    ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the whistle_apps server.
stop() ->
    application:stop(whistle_apps).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
