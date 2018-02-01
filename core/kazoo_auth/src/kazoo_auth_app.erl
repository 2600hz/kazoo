%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_auth_app).

-behaviour(application).

-include("kazoo_auth.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kazoo_auth_maintenance:refresh(),
    _ = kazoo_auth_maintenance:register_common_providers(),
    _ = kazoo_auth_maintenance:ensure_secret(),
    kazoo_auth_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
