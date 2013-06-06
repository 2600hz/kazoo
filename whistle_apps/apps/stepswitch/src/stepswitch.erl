%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% stepswitch routing WhApp
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
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
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    stepswitch_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app
%% @end
%%--------------------------------------------------------------------
-spec start() -> 'ok'.
start() ->
    _ = start_deps().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(),
    _ = [wh_util:ensure_started(App) || App <- [sasl, crypto, whistle_amqp]],
    ok.
