%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("trunkstore/src/ts.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    trunkstore_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    kapi_self:declare_exchanges().
