%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(voxeo).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    voxeo_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    application:start(voxeo).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    wh_util:ensure_started(sasl), % logging
    wh_util:ensure_started(crypto), % random and for the SSL application
    wh_util:ensure_started(public_key), %% needed by SSL application
    wh_util:ensure_started(ssl), %% for auth with XMPP
    wh_util:ensure_started(exmpp), % XMPP service
    wh_util:ensure_started(whistle_amqp). % amqp wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(voxeo).
