%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(konami_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc
%% Implement the application start behaviour.
%% @end
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    konami_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%% Implement the application stop behaviour.
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_metaflow:declare_exchanges(),
    kapi_self:declare_exchanges().
