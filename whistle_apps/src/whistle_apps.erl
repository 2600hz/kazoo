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

    case application:get_env(reloader) of
	{ok, true} -> reloader:start();
	_ -> ok
    end,

    logger:start_link(),

    wh_util:ensure_started(sasl),
    wh_util:ensure_started(crypto),
    wh_util:ensure_started(ibrowse),
    wh_util:ensure_started(riak_err),
    wh_util:ensure_started(couchbeam),
    wh_util:ensure_started(whistle_amqp),
    wh_util:ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the whistle_apps server.
stop() ->
    application:stop(whistle_apps).
