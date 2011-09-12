%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% stepswitch routing WhApp
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(stepswitch).

-include_lib("whistle/include/wh_types.hrl").

-export([start/0, start_link/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    start_deps(),
    stepswitch_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app
%% @end
%%--------------------------------------------------------------------
-spec start/0 :: () -> ok.
start() ->
    start_deps(),
    application:start(stepswitch).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> ok.
stop() ->
    application:stop(stepswitch).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> ok.
start_deps() ->
    whistle_apps_deps:ensure(),
    wh_util:ensure_started(sasl),
    wh_util:ensure_started(crypto),
    wh_util:ensure_started(whistle_amqp),
    wh_util:ensure_started(couchbeam),
    wh_util:ensure_started(whistle_couch).
