%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    cccp_util:register_views(),
    _ = kapps_maintenance:bind('register_views', 'cccp_util', 'register_views'),
    _ = declare_exchanges(),
    cccp_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_self:declare_exchanges(),
    kapi_call:declare_exchanges(),
    kapi_route:declare_exchanges(),
    kapi_resource:declare_exchanges(),
    kapi_offnet_resource:declare_exchanges().
