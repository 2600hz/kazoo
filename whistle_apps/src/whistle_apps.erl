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
    application:start(whistle_apps, permanent).

start_deps() ->
    whistle_apps_deps:ensure(),

    lager:start(),

    case application:get_env(reloader) of
        {ok, true} -> reloader:start();
        _ -> ok
    end,

    ok = wh_util:ensure_started(sasl),
    ok = wh_util:ensure_started(crypto),
    ok = wh_util:ensure_started(ibrowse),
    ok = wh_util:ensure_started(gproc),
    ok = wh_util:ensure_started(riak_err),
    ok = wh_util:ensure_started(couchbeam),
    ok = wh_util:ensure_started(whistle_amqp),
    ok = wh_util:ensure_started(whistle_stats).

%% @spec stop() -> ok
%% @doc Stop the whistle_apps server.
stop() ->
    application:stop(whistle_apps).
