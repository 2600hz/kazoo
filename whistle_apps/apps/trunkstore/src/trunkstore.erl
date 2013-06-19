%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore).

-author('James Aimonetti <james@2600hz.org>').
-export([start_link/0
         ,stop/0
         ,start_deps/0
        ]).

-include("ts.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    trunkstore_sup:start_link().

start_deps() ->
    whistle_apps_deps:ensure(?MODULE),
    [wh_util:ensure_started(A) || A <- ['sasl', 'crypto', 'whistle_amqp']].

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() -> 'ok'.
