%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_app).

-behaviour(application).

-include("webhooks.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    kapps_maintenance:bind_and_register_views(?APP, 'webhooks_maintenance', 'register_views'),
    webhooks_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('register_views', 'webhooks_maintenance', 'register_views'),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_self:declare_exchanges().
