%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Various ways to notify things of stuff
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(notify).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0]).

-include("notify.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    _ = start_deps(),
    notify_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app
%% @end
%%--------------------------------------------------------------------
-spec start/0 :: () -> 'ok'.
start() ->
    _ = start_deps(),
    application:start(notify).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> 'ok'.
stop() ->
    ok = application:stop(notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> _.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(App) || App <- [sasl, crypto, whistle_amqp]].
