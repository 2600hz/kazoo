%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    OK = blackhole_sup:start_link(),
    _ = blackhole_bindings:init(),
    OK.

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
    cowboy:stop_listener('blackhole'),
    cowboy:stop_listener('socketio_http_listener'),
    exit(whereis('blackhole_sup'), 'shutdown'),
    'ok'.
