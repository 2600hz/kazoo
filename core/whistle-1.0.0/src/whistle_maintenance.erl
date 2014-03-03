%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_maintenance).

-export([nodes/0]).
-export([hotload/1]).
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

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

syslog_level(Level) ->
    wh_util:change_syslog_log_level(wh_util:to_atom(Level)).

error_level(Level) ->
    wh_util:change_error_log_level(wh_util:to_atom(Level)).

console_level(Level) ->
    wh_util:change_console_log_level(wh_util:to_atom(Level)).

nodes() ->
    wh_nodes:status().

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
    hotload(wh_util:to_atom(Module, 'true')).

-spec gc_all() -> 'ok'.
-spec gc_pids(pids()) -> 'ok'.
-spec gc_pids(pids(), pos_integer()) -> 'ok'.
gc_all() ->
    gc_pids(processes()).
gc_pids(Ps) ->
    gc_pids(Ps, 500).
gc_pids(Ps, Sleep) ->
    _ = [begin erlang:garbage_collect(P), timer:sleep(Sleep) end || P <- Ps],
    'ok'.

-spec gc_top_mem_consumers() -> 'ok'.
-spec gc_top_mem_consumers(pos_integer()) -> 'ok'.
gc_top_mem_consumers() ->
    gc_top_mem_consumers(10).
gc_top_mem_consumers(N) ->
    {Top, _} = top_mem_consumers(N),
    gc_pids([P || {P,_} <- Top]).

-type pid_info() :: {pid(), non_neg_integer()}.
-type pid_infos() :: [pid_info(),...] | [].
-spec top_mem_consumers() -> {pid_infos(), pid_infos()}.
-spec top_mem_consumers(pos_integer()) -> {pid_infos(), pid_infos()}.
top_mem_consumers() ->
    top_mem_consumers(10).
top_mem_consumers(Len) when is_integer(Len), Len > 0 ->
    lists:split(Len
                ,lists:reverse(
                   lists:keysort(2, [{P, erlang:process_info(P, 'total_heap_size')}
                                     || P <- processes()
                                    ])
                  )).

-spec etop() -> 'ok'.
etop() ->
    etop:start([{'output', 'text'}]),
    'ok'.

-spec ibrowse_cleanup() -> 'ok'.
ibrowse_cleanup() ->
    [ibrowse_cleanup(K, P)
     || {{'req_id_pid', _Ref}=K, P} <- ets:tab2list('ibrowse_stream')
    ],
    'ok'.

-spec ibrowse_cleanup({'req_id_pid', reference()}, pid()) -> 'ok'.
ibrowse_cleanup(K, P) ->
    case erlang:is_process_alive(P) of
        'true' -> 'ok';
        'false' ->
            ets:delete('ibrowse_stream', K),
            'ok'
    end.

