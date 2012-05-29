%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% WHISTLE_APPS logger
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whistle_apps).

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
    ok = wh_util:ensure_started(gproc),
    ok = wh_util:ensure_started(whistle_amqp),
    ok = wh_util:ensure_started(whistle_stats).

%% @spec stop() -> ok
%% @doc Stop the whistle_apps server.
stop() ->
    application:stop(whistle_apps).
