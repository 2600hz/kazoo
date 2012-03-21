-module(whistle_maintenance).

-export([gc_all/0]).
-export([top_mem_consumers/0, top_mem_consumers/1]).
-export([etop/0]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-spec gc_all/0 :: () -> 'ok'.
gc_all() ->
    _ = [begin erlang:garbage_collect(P), timer:sleep(500) end || P <- processes()],
    ok.

-spec top_mem_consumers/0 :: () -> {wh_proplist_kv(pid(), integer()), wh_proplist_kv(pid(), integer())}.
-spec top_mem_consumers/1 :: (pos_integer()) -> {wh_proplist_kv(pid(), integer()), wh_proplist_kv(pid(), integer())}.
top_mem_consumers() ->
    top_mem_consumers(10).
top_mem_consumers(Len) when is_integer(Len), Len > 0 ->
    lists:split(Len, lists:reverse(lists:keysort(2, [{P, erlang:process_info(P, total_heap_size)} || P <- processes()]))).

-spec etop/0 :: () -> 'ok'.
etop() ->
    etop:start([{output, text}]),
    ok.
