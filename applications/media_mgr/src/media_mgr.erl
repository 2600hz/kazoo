%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(media_mgr).

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
    _ = declare_exchanges(),
    media_mgr_sup:start_link().

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
    _ = wh_media_proxy:stop(),
    exit(whereis('media_mgr_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                                ,'lager'
                                                ,'whistle_amqp'
                                                ,'whistle_couch'
                                               ]],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_media:declare_exchanges(),
    wapi_self:declare_exchanges().
