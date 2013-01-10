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
-export([start/0
         ,start_link/0
         ,stop/0
         ,start_deps/0
        ]).

-include("ts.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    _ = start_deps(),
    trunkstore_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    application:start(trunkstore).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE),
    [wh_util:ensure_started(A) || A <- [sasl, crypto, whistle_amqp, ibrowse]].

%% @spec stop() -> ok
%% @doc Stop the callmgr server.
stop() ->
    application:stop(trunkstore).
