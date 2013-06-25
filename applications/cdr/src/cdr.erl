%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cdr).

-export([start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    cdr_sup:start_link().

%% @spec stop() -> ok
%% @doc Stop the cdr server.
stop() -> 'ok'.

-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE),
    _ = [wh_util:ensure_started(App) || App <- ['sasl', 'crypto', 'whistle_amqp']],
    'ok'.
