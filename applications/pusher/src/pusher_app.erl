%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pusher_app).

-behaviour(application).

-include("pusher.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    pusher_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_pusher:declare_exchanges(),
    _ = kapi_registration:declare_exchanges(),
    kapi_self:declare_exchanges().
