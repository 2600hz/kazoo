%%%-------------------------------------------------------------------
%%% @copyright (C) 2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(skel).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    skel_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    application:start(skel).

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(skel).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(App) || App <- [sasl, crypto, whistle_amqp]].
