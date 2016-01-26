%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(pusher_app).

-behaviour(application).

-include_lib("pusher.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    pusher_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
   exit(whereis('pusher_sup'), 'shutdown'),
   'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_call:declare_exchanges(),
    _ = wapi_pusher:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    wapi_self:declare_exchanges().
