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
    _ = start_deps(),
    whistle_apps_sup:start_link().

%% @spec start() -> ok
%% @doc Start the whistle_apps server.
start() ->
    _ = start_deps(),
    application:start('whistle_apps', 'permanent').

-spec start_deps() -> list().
start_deps() ->
    whistle_apps_deps:ensure(),

    lager:start(),

    case application:get_env('reloader') of
        {'ok', 'true'} -> reloader:start();
        _ -> 'ok'
    end,

    [wh_util:ensure_started(A) || A <- ['sasl', 'crypto', 'gproc', 'whistle_amqp', 'whistle_stats']].

%% @spec stop() -> ok
%% @doc Stop the whistle_apps server.
stop() ->
    application:stop('whistle_apps').
