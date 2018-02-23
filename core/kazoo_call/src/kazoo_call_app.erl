%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz INC
%%% @doc
%%%
%%%
%%% @author Karl Anderson
%%% @end
%%%-------------------------------------------------------------------
-module(kazoo_call_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%% Application callbacks

%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_call_sup:start_link().

%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
