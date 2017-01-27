%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_maintenance).

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

        ,ets_info/0
        ,mem_info/0
        ]).

-include("include/kz_types.hrl").
-include("include/kz_databases.hrl").

-spec syslog_level(text()) -> 'ok'.
syslog_level(Level) ->
    kz_util:change_syslog_log_level(kz_term:to_atom(Level)).

-spec error_level(text()) -> 'ok'.
error_level(Level) ->
    kz_util:change_error_log_level(kz_term:to_atom(Level)).

-spec console_level(text()) -> 'ok'.
console_level(Level) ->
    kz_util:change_console_log_level(kz_term:to_atom(Level)).

-spec hotload(text() | atom()) -> 'ok' | 'no_return'.
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

-spec hotload_app(text() | atom()) -> 'ok'.
hotload_app(App) when is_atom(App) ->
    case application:get_key(App, 'modules') of
        {'ok', Modules} ->
            io:format("found ~b modules to reload for ~s~n", [length(Modules), App]),
            lists:foreach(fun hotload/1, Modules),
            io:format("app ~s modules reloaded~n", [App]);
        'undefined' ->
            io:format("app ~s not found (is it running? typo?)~n", [App])
    end;
hotload_app(App) ->
    hotload_app(kz_term:to_atom(App, 'true')).

-spec gc_all() -> 'ok'.
-spec gc_pids([pid(),...]) -> 'ok'.
gc_all() ->
    gc_pids(processes()).
gc_pids(Ps) ->
    lists:foreach(fun (P) -> erlang:garbage_collect(P), timer:sleep(500) end, Ps).

-spec gc_top_mem_consumers() -> 'ok'.
-spec gc_top_mem_consumers(pos_integer()) -> 'ok'.
gc_top_mem_consumers() ->
    gc_top_mem_consumers(10).
gc_top_mem_consumers(N) ->
    {Top, _} = top_mem_consumers(N),
    gc_pids([P || {P,_} <- Top]).

-type consumers() :: {kz_proplist_kv(pid(), integer()), kz_proplist_kv(pid(), integer())}.
-spec top_mem_consumers() -> consumers().
-spec top_mem_consumers(pos_integer()) -> consumers().
top_mem_consumers() ->
    top_mem_consumers(10).
top_mem_consumers(Len) when is_integer(Len), Len > 0 ->
    SortHeapDesc =
        lists:reverse(
          lists:keysort(2
                       ,[{P, erlang:process_info(P, 'total_heap_size')} || P <- processes()]
                       )
         ),
    lists:split(Len, SortHeapDesc).

-spec etop() -> 'ok'.
etop() ->
    etop:start([{'output', 'text'}]),
    'ok'.

-spec ets_info() -> 'ok'.
ets_info() ->
    io:format("ETS table memory usage:~n"),
    [print_table(T) || T <- sort_tables(ets:all())],
    'ok'.

-spec sort_tables([ets:tid()]) -> [{ets:tid(), integer()}].
sort_tables(Ts) ->
    lists:reverse(
      lists:keysort(2
                   ,[{T, table_size(T)} || T <- Ts]
                   )
     ).

-spec table_size(ets:tid()) -> integer().
table_size(T) ->
    words_to_bytes(ets:info(T, 'memory')).

words_to_bytes(Words) ->
    Words * erlang:system_info('wordsize').

-spec print_table({ets:tid(), integer()}) -> 'ok'.
print_table({T, Mem}) ->
    io:format("  ~-25s: ~6s~n", [kz_term:to_list(T), kz_util:pretty_print_bytes(Mem, 'truncated')]).

-spec mem_info() -> 'ok'.
mem_info() ->
    io:format(" VM Memory Info:~n"),
    [print_memory_type(Info) || Info <- erlang:memory()],
    'ok'.

-spec print_memory_type({erlang:memory_type(), integer()}) -> 'ok'.
print_memory_type({Type, Size}) ->
    io:format("  ~-15s : ~6s~n", [Type, kz_util:pretty_print_bytes(Size, 'truncated')]).
