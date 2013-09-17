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
-export([start_link/0
         ,start/0
         ,stop/0
        ]).

-include("notify.hrl").

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
    notify_sup:start_link().

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
    exit(whereis('notify_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> _.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(App) || App <- ['crypto'
                                            ,'lager'
                                            ,'whistle_amqp'
                                            ,'whistle_couch'
                                           ]].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    wapi_self:declare_exchanges().
