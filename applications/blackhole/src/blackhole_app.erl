%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_app).
-behaviour(application).
-include_lib("kazoo/include/kz_types.hrl").

-export([start/2, stop/1]).

-define(MODULES, [bh_cmd, bh_call]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    OK = blackhole_sup:start_link(),
    _ = blackhole_bindings:init(), %% FIXME: the OTP way to supervise this?
    load_binding_modules(),
    OK.

load_binding_modules() -> [ M:init() || M <- ?MODULES].

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = cowboy:stop_listener('blackhole'),
    _ = cowboy:stop_listener('blackhole_http_listener'),
    _ = blackhole_limit:stop(),
    _ = blackhole_counters:stop(),
    _ = blackhole_bindings:flush(),
    'ok'.
