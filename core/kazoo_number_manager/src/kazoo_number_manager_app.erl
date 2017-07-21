%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_number_manager_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    declare_exchanges(),
    kazoo_number_manager_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_discovery:declare_exchanges(),
    kapi_self:declare_exchanges().
