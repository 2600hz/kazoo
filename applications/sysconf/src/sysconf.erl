%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf).

-author('Edouard Swiac <edouard@2600hz.org>').
-export([start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    sysconf_sup:start_link().

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(App) || App <- ['sasl', 'crypto', 'whistle_amqp']],
    'ok'.

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() -> 'ok'.
