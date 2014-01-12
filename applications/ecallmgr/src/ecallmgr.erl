%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr).

-include("ecallmgr.hrl").

-export([start_link/0]).
-export([start/0]).
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
    _ = declare_exchanges(),
    ecallmgr_sup:start_link().

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
    exit(whereis('ecallmgr_sup'), 'shutdown'),
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
                                                ,'gproc'
                                                ,'ibrowse'
                                                ,'whistle_amqp'
                                                ,'whistle_stats'
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
    _ = wapi_authn:declare_exchanges(),
    _ = wapi_authz:declare_exchanges(),
    _ = wapi_call:declare_exchanges(),
    _ = wapi_conference:declare_exchanges(),
    _ = wapi_dialplan:declare_exchanges(),
    _ = wapi_media:declare_exchanges(),
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_rate:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    _ = wapi_resource:declare_exchanges(),
    _ = wapi_route:declare_exchanges(),
    _ = wapi_sysconf:declare_exchanges(),
    wapi_self:declare_exchanges().
