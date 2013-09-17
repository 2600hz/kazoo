%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013 VoIP, INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_apps).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0
         ,start/0
         ,stop/0
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    whistle_apps_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the application
%% @end
%%--------------------------------------------------------------------
-spec start() -> 'ok' | {'error', _}.
start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    exit(whereis('whistle_apps_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(),
    case application:get_env('reloader') of
        {'ok', 'true'} -> reloader:start();
        _ -> 'ok'
    end,
    [wh_util:ensure_started(A) || A <- ['sasl'
                                        ,'crypto'
                                        ,'gproc'
                                        ,'lager'
                                        ,'whistle_amqp'
                                        ,'whistle_couch'
                                       ]],
    'ok'.
