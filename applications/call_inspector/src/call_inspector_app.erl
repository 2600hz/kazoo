%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(call_inspector_app).

-behaviour(application).

-include_lib("kazoo_types/include/kz_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    call_inspector_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_self:declare_exchanges(),
    kapi_inspector:declare_exchanges().
