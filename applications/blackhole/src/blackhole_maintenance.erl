%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_maintenance).


-export([start_module/1]).
-export([stop_module/1]).
-export([running_modules/0]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module(text()) -> 'ok'.
start_module(Module) ->
    case blackhole_bindings:init_mod(Module) of
        'ok' -> maybe_autoload_module(kz_util:to_binary(Module));
        {'error', Error} -> io:format("failed to start ~s: ~p~n", [Module, Error])
    end.

-spec maybe_autoload_module(ne_binary()) -> 'ok'.
maybe_autoload_module(Module) ->
    Mods = blackhole_config:autoload_modules(),
    case lists:member(Module, Mods) of
        'true' ->
            io:format("module ~s started~n", [Module]);
        'false' ->
            persist_module(Module, Mods),
            io:format("started and added ~s to autoloaded modules~n", [Module])
    end.

-spec persist_module(ne_binary(), ne_binaries()) -> 'ok'.
persist_module(Module, Mods) ->
    blackhole_config:set_default_autoload_modules(
      [kz_util:to_binary(Module)
       | lists:delete(kz_util:to_binary(Module), Mods)
      ]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_module(text()) -> 'ok'.
stop_module(Module) ->
    case blackhole_bindings:flush_mod(Module) of
        'ok' ->
            Mods = blackhole_config:autoload_modules(),
            blackhole_config:set_default_autoload_modules(lists:delete(kz_util:to_binary(Module), Mods)),
            io:format("stopped and removed ~s from autoloaded modules~n", [Module]);
        {'error', Error} -> io:format("failed to stop ~s: ~p~n", [Module, Error])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec running_modules() -> atoms().
running_modules() -> blackhole_bindings:modules_loaded().
