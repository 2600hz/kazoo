%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    OK = blackhole_sup:start_link(),
    _ = blackhole_bindings:init(),
    OK.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    cowboy:stop_listener('socketio_http_listener'),
    exit(whereis('blackhole_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    %whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                               ,'inets'
                                               ,'lager'
                                               ,'whistle_amqp'
                                               ,'whistle_couch'
                                               ,'kazoo_bindings'
                                               ,'ranch'
                                               ,'cowboy'
                                               ,'public_key'
                                               ,'ssl'
                                               ,'socketio'
                                               ]],
    'ok'.
