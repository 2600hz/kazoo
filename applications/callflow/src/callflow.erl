%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(callflow).

-include("callflow.hrl").

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
    _ = declare_exchanges(),
    callflow_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    exit(whereis('callflow_sup'), 'shutdown'),
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
    _ = [wh_util:ensure_started(App) || App <- ['crypto', 'whistle_amqp', 'whistle_couch']],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_acdc_agent:declare_exchanges(), %% TODO: decouple
    _ = wapi_acdc_queue:declare_exchanges(), %% TODO: decouple
    _ = wapi_call:declare_exchanges(),
    _ = wapi_callflow:declare_exchanges(),
    _ = wapi_conf:declare_exchanges(),
    _ = wapi_conference:declare_exchanges(),
    _ = wapi_dialplan:declare_exchanges(),
    _ = wapi_fax:declare_exchanges(), %% TODO: decouple
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_offnet_resource:declare_exchanges(),
    _ = wapi_pivot:declare_exchanges(), %% TODO: decouple
    _ = wapi_route:declare_exchanges(),
    wapi_self:declare_exchanges().
