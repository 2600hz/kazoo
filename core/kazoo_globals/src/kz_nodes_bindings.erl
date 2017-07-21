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
-spec bind(atom(), atom()) -> bind_result().
-spec bind(atom(), atom(), atom()) -> bind_result().
bind(App) ->
    bind(App, App).

bind(App, Module) ->
    bind(App, Module, ?DEFAULT_FUNCTION).

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
-spec unbind(atom(), atom()) -> kazoo_bindings:unbind_result().
-spec unbind(atom(), atom(), atom()) -> kazoo_bindings:unbind_result().
unbind(App) ->
    unbind(App, App).

unbind(App, Module) ->
    unbind(App, Module, ?DEFAULT_FUNCTION).

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
