%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr).

-include("ecallmgr.hrl").

-export([start_link/0, start/0]).
-export([stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    ecallmgr_sup:start_link().

-spec start() -> 'ok' | {'error', term()}.
start() ->
    _ = start_deps(),
    application:start(ecallmgr, permanent).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    application:stop(ecallmgr).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    lager:start(),
    _ = [wh_util:ensure_started(App) || App <- [sasl, crypto, whistle_amqp, gproc, ibrowse, whistle_stats]],
    ok.
