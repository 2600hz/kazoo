%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_amqp).

-export([start_link/0, start/0]).
-export([stop/0]).

-include("amqp_util.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    _ = start_deps(),
    wh_amqp_sup:start_link().

-spec start/0 :: () -> 'ok' | {'error', _}.
start() ->
    _ = start_deps(),
    application:start(whistle_amqp, permanent).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> 'ok'.
stop() ->
    application:stop(whistle_amqp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    _ = [wh_util:ensure_started(App) || App <- [sasl, riak_error, amqp_client]],
    ok.
