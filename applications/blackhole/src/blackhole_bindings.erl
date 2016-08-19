%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% See kazoo_bindings, this is an interface
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Ben Wann
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(blackhole_bindings).

%% API
-export([bind/4, unbind/4, map/2, filter/1, modules_loaded/0, init/0]).

-include("blackhole.hrl").

-type payload() :: bh_context:context() | ne_binary().

-type map_results() :: list().
-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    kazoo_bindings:map(Routing, Payload).

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result()].

-spec bind(ne_binary() | ne_binaries(), atom(), atom(), any()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun, Payload) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:bind(Binding, Module, Fun, Payload).

-spec unbind(ne_binary() | ne_binaries(), atom(), atom(), any()) -> 'ok'.
unbind([_|_]=Bindings, Module, Fun, Payload) ->
    _ = [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings],
    'ok';
unbind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:unbind(Binding, Module, Fun, Payload).

filter(Predicate) ->
    kazoo_bindings:filter(Predicate).

-spec modules_loaded() -> atoms().
modules_loaded() -> kazoo_bindings:modules_loaded().

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing blackhole bindings"),
    kz_util:put_callid(?LOG_SYSTEM_ID),
    _ = [init_mod(Mod)
         || Mod <- kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES)
        ],
    'ok'.

init_mod(ModuleName) ->
    lager:debug("initializing module: ~p", [ModuleName]),
    maybe_init_mod(ModuleName).

maybe_init_mod(ModuleName) ->
    lager:debug("trying to init module: ~p", [ModuleName]),
    try (kz_util:to_atom(ModuleName, 'true')):init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p.", [ModuleName, _E, _R])
    end.
