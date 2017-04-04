%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_maintenance).

-export([crash/0]).
-export([debug_dump/0]).
-export([ets_info/0]).
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

-spec crash() -> 'ok'.
crash() ->
    _ = erlang:halt("crash requested"),
    'ok'.

-spec debug_dump() -> 'ok'.
debug_dump() ->
    FolderName = "/tmp/" ++ wh_util:to_list(node()) ++ "_" ++ wh_util:to_list(wh_util:current_tstamp()),
    'ok' = file:make_dir(FolderName),
    _ = debug_dump_process(FolderName),
    _ = debug_dump_memory(FolderName),
    _ = debug_dump_ports(FolderName),
    _ = debug_dump_ets(FolderName),
    'ok'.

-spec debug_dump_memory(list()) -> 'ok'.
debug_dump_memory(FolderName) ->
    MemoryLog = FolderName ++ "/memory_log",
    _ = [log_memory_type(Info, MemoryLog) || Info <- erlang:memory()],
    'ok'.

-spec debug_dump_process(list()) -> 'ok'.
debug_dump_process(FolderName) ->
    ProcessLog = FolderName ++ "/process_info",
    Bytes = [erlang:process_info(Pid)|| Pid <- erlang:processes()],
    'ok' = file:write_file(ProcessLog, io_lib:format("~p~n", [Bytes])),
    'ok'.

-spec debug_dump_ets(list()) -> 'ok'.
debug_dump_ets(FolderName) ->
    EtsLog = FolderName ++ "/ets_log",
    _ = [log_table(T, EtsLog) || T <- sort_tables(ets:all())],
    EtsFolder = FolderName ++ "/ets",
    'ok' = file:make_dir(EtsFolder),
    _ = debug_dump_ets_details(EtsFolder, ets:all()),
    'ok'.

-spec debug_dump_ets_details(list(), [ets:tab()]) -> 'ok'.
debug_dump_ets_details(_, []) -> 'ok';
debug_dump_ets_details(EtsFolder, [Tab|Tabs]) ->
    TabInfoLog = EtsFolder ++ "/" ++ wh_util:to_list(ets:info(Tab, 'name')) ++ "_info",
    'ok' = file:write_file(TabInfoLog, io_lib:format("~p~n", [ets:info(Tab)])),
    TabDumpLog = EtsFolder ++ "/" ++ wh_util:to_list(ets:info(Tab, 'name')) ++ "_dump",
    catch ets:tab2file(Tab, TabDumpLog),
    debug_dump_ets_details(EtsFolder, Tabs).

-spec debug_dump_ports(list()) -> 'ok'.
debug_dump_ports(FolderName) ->
    PortLog = FolderName ++ "/ports_info",
    Bytes = [erlang:port_info(Port) || Port <- erlang:ports()],
    'ok' = file:write_file(PortLog, io_lib:format("~p~n", [Bytes])),
    'ok'.

-spec log_table({ets:tab(), integer()}, file:name_all()) -> 'ok' | {'error', _}.
log_table({T, Mem}, Filename) ->
    Bytes = io_lib:format("  ~-25s: ~6s~n", [wh_util:to_list(T), wh_util:pretty_print_bytes(Mem, 'truncated')]),
    'ok' = file:write_file(Filename, Bytes, ['append']).

-spec log_memory_type({erlang:memory_type(), integer()}, file:name_all()) -> 'ok' | {'error', _}.
log_memory_type({Type, Size}, Filename) ->
    Bytes = io_lib:format("  ~-15s : ~6s~n", [Type, wh_util:pretty_print_bytes(Size, 'truncated')]),
    'ok' = file:write_file(Filename, Bytes, ['append']).

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
    io:format("  ~-25s: ~6s~n", [wh_util:to_list(T), wh_util:pretty_print_bytes(Mem, 'truncated')]).

-spec syslog_level(text()) -> 'ok'.
syslog_level(Level) ->
    wh_util:change_syslog_log_level(wh_util:to_atom(Level)).

-spec error_level(text()) -> 'ok'.
error_level(Level) ->
    wh_util:change_error_log_level(wh_util:to_atom(Level)).

-spec console_level(text()) -> 'ok'.
console_level(Level) ->
    wh_util:change_console_log_level(wh_util:to_atom(Level)).

-spec nodes() -> 'no_return'.
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

-spec hotload_app(text() | atom()) -> 'ok'.
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
    hotload_app(wh_util:to_atom(App, 'true')).

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
