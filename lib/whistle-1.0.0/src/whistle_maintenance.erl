%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_maintenance).

-export([nodes/0]).
-export([syslog_level/1
         ,error_level/1
         ,console_level/1
        ]).
-export([gc_all/0, gc_pids/1
         ,gc_top_mem_consumers/0, gc_top_mem_consumers/1
         ,top_mem_consumers/0, top_mem_consumers/1
         ,etop/0
         ,ibrowse_cleanup/0
        ]).

-include("../include/wh_types.hrl").
-include("../include/wh_databases.hrl").

-spec gc_all/0 :: () -> 'ok'.
-spec gc_pids/1 :: ([pid(),...]) -> 'ok'.
-spec gc_top_mem_consumers/0 :: () -> 'ok'.
-spec gc_top_mem_consumers/1 :: (pos_integer()) -> 'ok'.
-spec top_mem_consumers/0 :: () -> {wh_proplist_kv(pid(), integer()), wh_proplist_kv(pid(), integer())}.
-spec top_mem_consumers/1 :: (pos_integer()) -> {wh_proplist_kv(pid(), integer()), wh_proplist_kv(pid(), integer())}.
-spec etop/0 :: () -> 'ok'.

syslog_level(Level) ->
    wh_util:change_syslog_log_level(wh_util:to_atom(Level)).

error_level(Level) ->
    wh_util:change_error_log_level(wh_util:to_atom(Level)).

console_level(Level) ->
    wh_util:change_console_log_level(wh_util:to_atom(Level)).

nodes() ->
    wh_nodes:status().

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

ibrowse_cleanup() ->
    [ibrowse_cleanup(K, P) || {{req_id_pid, _Ref}=K, P} <- ets:tab2list(ibrowse_stream)].
ibrowse_cleanup(K, P) ->
    case erlang:is_process_alive(P) of
        true -> ok;
        false -> ets:delete(ibrowse_stream, K)
    end.
            
