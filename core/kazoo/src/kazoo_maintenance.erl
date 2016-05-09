%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_maintenance).

-export([nodes/0]).
-export([hotload/1
         ,hotload_app/1
        ]).
-export([syslog_level/1
         ,error_level/1
         ,console_level/1
        ]).
-export([gc_all/0, gc_pids/1
         ,gc_top_mem_consumers/0, gc_top_mem_consumers/1
         ,top_mem_consumers/0, top_mem_consumers/1
         ,etop/0
        ]).

-include("include/kz_types.hrl").
-include("include/kz_databases.hrl").

-spec gc_all/0 :: () -> 'ok'.
-spec gc_pids/1 :: ([pid(),...]) -> 'ok'.
-spec gc_top_mem_consumers/0 :: () -> 'ok'.
-spec gc_top_mem_consumers/1 :: (pos_integer()) -> 'ok'.
-spec top_mem_consumers/0 :: () -> {kz_proplist_kv(pid(), integer()), kz_proplist_kv(pid(), integer())}.
-spec top_mem_consumers/1 :: (pos_integer()) -> {kz_proplist_kv(pid(), integer()), kz_proplist_kv(pid(), integer())}.
-spec etop/0 :: () -> 'ok'.

syslog_level(Level) ->
    kz_util:change_syslog_log_level(kz_term:to_atom(Level)).

error_level(Level) ->
    kz_util:change_error_log_level(kz_term:to_atom(Level)).

console_level(Level) ->
    kz_util:change_console_log_level(kz_term:to_atom(Level)).

nodes() ->
    kz_nodes:status().

hotload(Module) when is_atom(Module) ->
    _ = code:soft_purge(Module),
    case code:load_file(Module) of
        {'module', _} -> 'ok';
        {'error' , Reason} ->
            io:format("ERROR: unable to hotload ~s: ~s~n", [Module, Reason]),
            'no_return'
    end;
hotload(Module) ->
    hotload(kz_term:to_atom(Module, 'true')).

hotload_app(App) when is_atom(App) ->
    case application:get_key(App, 'modules') of
        {'ok', Modules} ->
            io:format("found ~b modules to reload for ~s~n", [length(Modules), App]),
            _ = [hotload(Module) || Module <- Modules],
            io:format("app ~s modules reloaded~n", [App]);
        'undefined' ->
            io:format("app ~s not found (is it running? typo?)~n", [App])
    end;
hotload_app(App) ->
    hotload_app(kz_term:to_atom(App, 'true')).

gc_all() ->
    gc_pids(processes()).
gc_pids(Ps) ->
    _ = [begin erlang:garbage_collect(P), timer:sleep(500) end || P <- Ps],
    'ok'.

gc_top_mem_consumers() ->
    gc_top_mem_consumers(10).
gc_top_mem_consumers(N) ->
    {Top, _} = top_mem_consumers(N),
    gc_pids([P || {P,_} <- Top]).

top_mem_consumers() ->
    top_mem_consumers(10).
top_mem_consumers(Len) when is_integer(Len), Len > 0 ->
    lists:split(Len, lists:reverse(lists:keysort(2, [{P, erlang:process_info(P, total_heap_size)} || P <- processes()]))).

etop() ->
    etop:start([{output, text}]),
    ok.
