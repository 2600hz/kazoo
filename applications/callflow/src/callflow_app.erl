%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(callflow_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    callflow_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
    exit(whereis('callflow_sup'), 'shutdown'),
    'ok'.


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
    _ = wapi_presence:declare_exchanges(),
    wapi_self:declare_exchanges().
