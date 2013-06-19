%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Rating engine whapp
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hotornot).

-author('James Aimonetti <james@2600hz.org>').
-export([start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    hotornot_sup:start_link().

start_deps() ->
    _ = whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(A) || A <- ['sasl', 'crypto', 'whistle_amqp']].

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() -> 'ok'.
