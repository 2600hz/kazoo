%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz
%%% @doc
%%% stepswitch routing WhApp entry module
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_app).

-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    _ = kapps_maintenance:bind({'refresh', ?KZ_OFFNET_DB}, 'stepswitch_maintenance', 'refresh'),
    stepswitch_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind({'refresh', ?KZ_OFFNET_DB}, 'stepswitch_maintenance', 'refresh'),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_authn:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_sms:declare_exchanges(),
    kapi_self:declare_exchanges().
