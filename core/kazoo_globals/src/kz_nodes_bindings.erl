%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_nodes_bindings).

-export([bind/1, bind/2, bind/3
        ,unbind/1, unbind/2, unbind/3
        ,request/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(DEFAULT_MODULE, 'kz_nodes').
-define(DEFAULT_FUNCTION, 'request').
-define(CALLBACK_ARITY, 1).

-type bind_result() :: kazoo_bindings:bind_result().

-spec bind(atom()) -> bind_result().
bind(App) ->
    bind(App, App).

-spec bind(atom(), atom()) -> bind_result().
bind(App, Module) ->
    bind(App, Module, ?DEFAULT_FUNCTION).

-spec bind(atom(), atom(), atom()) -> bind_result().
bind(App, Module, Function)
  when is_atom(App)
       andalso is_atom(Module)
       andalso is_atom(Function)
       ->
    Binding = routing_key(App),
    case erlang:function_exported(Module, Function, ?CALLBACK_ARITY) of
        'true' ->
            kazoo_bindings:bind(Binding, Module, Function);
        'false' ->
            kazoo_bindings:bind(Binding, ?DEFAULT_MODULE, ?DEFAULT_FUNCTION)
    end.

-spec unbind(atom()) -> kazoo_bindings:unbind_result().
unbind(App) ->
    unbind(App, App).

-spec unbind(atom(), atom()) -> kazoo_bindings:unbind_result().
unbind(App, Module) ->
    unbind(App, Module, ?DEFAULT_FUNCTION).

-spec unbind(atom(), atom(), atom()) -> kazoo_bindings:unbind_result().
unbind(App, Module, Function)
  when is_atom(App)
       andalso is_atom(Module)
       andalso is_atom(Function)
       ->
    Binding = routing_key(App),
    case erlang:function_exported(Module, Function, ?CALLBACK_ARITY) of
        'true' ->
            kazoo_bindings:unbind(Binding, Module, Function);
        'false' ->
            kazoo_bindings:unbind(Binding, ?DEFAULT_MODULE, ?DEFAULT_FUNCTION)
    end.

routing_key(App) ->
    <<"kz_nodes.request.", (kz_term:to_binary(App))/binary>>.

-spec request(atom()) -> kazoo_bindings:fold_results().
request(App) when is_atom(App) ->
    case ets:info(kazoo_bindings:table_id()) of
        'undefined' ->
            lager:debug("kazoo bindings ets table not ready"),
            [];
        _ ->
            Routing = routing_key(App),
            kazoo_bindings:fold(Routing, [[{'app', App}]])
    end.
