%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    _ = kapps_maintenance:bind({'refresh', ?KZ_WEBHOOKS_DB}, 'webhooks_maintenance', 'reset_webhooks_list'),
    webhooks_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind({'refresh', ?KZ_WEBHOOKS_DB}, 'webhooks_maintenance', 'reset_webhooks_list'),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_self:declare_exchanges().
