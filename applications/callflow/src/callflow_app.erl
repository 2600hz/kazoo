%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(callflow_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    callflow_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_call:declare_exchanges(),
    _ = kapi_callflow:declare_exchanges(),
    _ = kapi_conf:declare_exchanges(),
    _ = kapi_conference:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_fax:declare_exchanges(), %% TODO: decouple
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_pivot:declare_exchanges(), %% TODO: decouple
    _ = kapi_route:declare_exchanges(),
    _ = kapi_presence:declare_exchanges(),
    _ = kapi_metaflow:declare_exchanges(),
    kapi_self:declare_exchanges().
