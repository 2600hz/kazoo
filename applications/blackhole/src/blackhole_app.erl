%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_app).

-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    OK = blackhole_sup:start_link(),
    _ = blackhole_bindings:init(), %% FIXME: the OTP way to supervise this?
    OK.

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = cowboy:stop_listener('blackhole'),
    _ = cowboy:stop_listener('blackhole_http_listener'),
    'ok'.
