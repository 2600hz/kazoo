%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    crossbar_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> 'true'.
stop(_State) ->
    cowboy:stop_listener('api_resource'),
    cowboy:stop_listener('api_resource_ssl'),
    crossbar_bindings:flush(),
    exit(whereis('crossbar_sup'), 'shutdown').
