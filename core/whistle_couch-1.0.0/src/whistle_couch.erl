%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_couch).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0
         ,start/0
         ,stop/0
        ]).

-define(DEPS, ['sasl'
               ,'crypto'
               ,'public_key'
               ,'ibrowse'
               ,'couchbeam'
               ,'lager'
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
    _ = declare_exchanges(),
    whistle_couch_sup:start_link().

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
    exit(whereis('whistle_couch_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    _ = [wh_util:ensure_started(App) || App <- ?DEPS],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_conf:declare_exchanges(),
    wapi_self:declare_exchanges().
