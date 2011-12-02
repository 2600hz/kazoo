%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Various ways to notify things of stuff
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
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
    start_deps(),
    notify_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app
%% @end
%%--------------------------------------------------------------------
-spec start/0 :: () -> 'ok'.
start() ->
    start_deps(),
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
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    wh_util:ensure_started(sasl), % logging
    wh_util:ensure_started(crypto), % random
    wh_util:ensure_started(whistle_amqp), % amqp wrapper
    wh_util:ensure_started(whistle_couch). % couch wrapper
