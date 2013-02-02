%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_couch).

-behaviour(application).

-export([start/2, start/0
         ,start_link/0
         ,start_deps/0
         ,stop/0, stop/1
        ]).

-define(DEPS, [sasl, crypto, public_key, ibrowse, couchbeam]).

%%
start(_Type, _Args) ->
    _ = start_deps(),
    whistle_couch_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    whistle_couch_sup:start_link().

%% @spec start() -> ok
%% @doc Start the couch server.
start() ->
    _ = start_deps(),
    application:start(whistle_couch).

-spec start_deps/0 :: () -> any().
start_deps() ->
    whistle_couch_deps:ensure(?MODULE),
    [wh_util:ensure_started(Dep) || Dep <- ?DEPS].

%% @spec stop() -> ok
%% @doc Stop the couch server.
stop() ->
    application:stop(whistle_couch).

stop(_) ->
    ok.
