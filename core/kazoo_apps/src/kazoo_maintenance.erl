%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_maintenance).

-export([crash/0]).
-export([debug_dump/0]).
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

        ,ets_info/0
        ,mem_info/0
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-spec crash() -> no_return().
crash() ->
    _ = debug_dump(),
    erlang:halt("crash requested").

-spec debug_dump() -> 'ok'.
debug_dump() ->
    FolderName = "/tmp/" ++ kz_term:to_list(node()) ++ "_" ++ kz_term:to_list(kz_time:now_s()),
    'ok' = file:make_dir(FolderName),
    _ = debug_dump_process_info(FolderName),
    _ = debug_dump_memory(FolderName),
    _ = debug_dump_ports(FolderName),
    _ = debug_dump_ets(FolderName),
    %% Do this last since it takes the longest,
    %% that way if the admin is impatient we have
    %% some complete data
    _ = debug_dump_process_status(FolderName),
    'ok'.

-spec debug_dump_memory(string()) -> 'ok'.
debug_dump_memory(FolderName) ->
    MemoryLog = FolderName ++ "/memory_log",
    _ = [log_memory_type(Info, MemoryLog) || Info <- erlang:memory()],
    'ok'.

-spec debug_dump_process_info(string()) -> 'ok'.
debug_dump_process_info(FolderName) ->
    InfoLog = FolderName ++ "/processes_info",
    'ok' = start_debug_file(InfoLog),
    debug_dump_process_info(InfoLog, erlang:processes()).

-spec debug_dump_process_info(string(), [pid()]) -> 'ok'.
debug_dump_process_info(_, []) -> 'ok';
debug_dump_process_info(InfoLog, [Pid|Pids]) ->
    Info = erlang:process_info(Pid),
    InfoBytes = io_lib:format("~p~n~p~n~n", [Pid, Info]),
    'ok' = file:write_file(InfoLog, InfoBytes, ['append']),
    debug_dump_process_info(InfoLog, Pids).

-spec debug_dump_process_status(string()) -> 'ok'.
debug_dump_process_status(FolderName) ->
    StatusLog = FolderName ++ "/processes_status",
    'ok' = start_debug_file(StatusLog),
    debug_dump_processes_status(StatusLog, erlang:processes()).

-spec debug_dump_processes_status(string(), [pid()]) -> 'ok'.
debug_dump_processes_status(_, []) -> 'ok';
debug_dump_processes_status(StatusLog, [Pid|Pids]) ->
    debug_dump_process_status(StatusLog, Pid),
    debug_dump_processes_status(StatusLog, Pids).

-spec debug_dump_process_status(string(), pid()) -> 'ok'.
debug_dump_process_status(StatusLog, Pid)  ->
    debug_dump_process_info(StatusLog, Pid, process_info(Pid)).

-spec debug_dump_process_info(string(), pid(), kz_term:api_proplist()) -> 'ok'.
debug_dump_process_info(_StatusLog, _Pid, 'undefined') -> 'ok';
debug_dump_process_info(StatusLog, Pid, Info) ->
    Dictionary = props:get_value('dictionary', Info, []),
    case props:get_value('$initial_call', Dictionary) =/= 'undefined' of
        'false' -> 'ok';
        'true' ->
            StatusBytes = io_lib:format("~p~n~p~n~n", [Pid, catch sys:get_status(Pid)]),
            'ok' = file:write_file(StatusLog, StatusBytes, ['append'])
    end.

-spec debug_dump_ets(string()) -> 'ok'.
debug_dump_ets(FolderName) ->
    EtsLog = FolderName ++ "/ets_log",
    _ = [log_table(T, EtsLog) || T <- sort_tables(ets:all())],
    EtsFolder = FolderName ++ "/ets",
    'ok' = file:make_dir(EtsFolder),
    _ = debug_dump_ets_details(EtsFolder, ets:all()),
    'ok'.

-spec debug_dump_ets_details(string(), [ets:tab()]) -> 'ok'.
debug_dump_ets_details(_, []) -> 'ok';
debug_dump_ets_details(EtsFolder, [Tab|Tabs]) ->
    TabInfoLog = EtsFolder ++ "/" ++ table_name(Tab) ++ "_info",
    'ok' = start_debug_file(TabInfoLog),
    'ok' = file:write_file(TabInfoLog, io_lib:format("~p~n", [ets:info(Tab)])),
    TabDumpLog = EtsFolder ++ "/" ++ table_name(Tab) ++ "_dump",
    TabList = (catch ets:tab2list(Tab)),
    'ok' = start_debug_file(TabDumpLog),
    'ok' = file:write_file(TabDumpLog, io_lib:format("~p~n", [TabList])),
    TabBinaryLog = EtsFolder ++ "/" ++ table_name(Tab) ++ "_binary",
    catch ets:tab2file(Tab, TabBinaryLog),
    debug_dump_ets_details(EtsFolder, Tabs).

-spec debug_dump_ports(string()) -> 'ok'.
debug_dump_ports(FolderName) ->
    PortLog = FolderName ++ "/ports_info",
    Bytes = [erlang:port_info(Port) || Port <- erlang:ports()],
    'ok' = start_debug_file(PortLog),
    'ok' = file:write_file(PortLog, io_lib:format("~p~n", [Bytes])).

-spec start_debug_file(file:name_all()) -> file:posix() | 'badarg' | 'terminated' | 'system_limit'.
start_debug_file(File) ->
    Timestamp = kz_time:current_unix_tstamp(),
    file:write_file(File, io_lib:format("Created: ~p~n~n", [Timestamp])).

-spec syslog_level(kz_term:text()) -> 'ok'.
syslog_level(Level) ->
    kz_log:change_syslog_log_level(kz_term:to_atom(Level)).

-spec error_level(kz_term:text()) -> 'ok'.
error_level(Level) ->
    kz_log:change_error_log_level(kz_term:to_atom(Level)).

-spec console_level(kz_term:text()) -> 'ok'.
console_level(Level) ->
    kz_log:change_console_log_level(kz_term:to_atom(Level)).

-spec hotload(kz_term:text() | atom()) -> 'ok' | 'no_return'.
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

-spec hotload_app(kz_term:text() | atom()) -> 'ok'.
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
gc_all() ->
    gc_pids(processes()).

-spec gc_pids([pid(),...]) -> 'ok'.
gc_pids(Ps) ->
    lists:foreach(fun (P) -> erlang:garbage_collect(P), timer:sleep(500) end, Ps).

-spec gc_top_mem_consumers() -> 'ok'.
gc_top_mem_consumers() ->
    gc_top_mem_consumers(10).

-spec gc_top_mem_consumers(pos_integer()) -> 'ok'.
gc_top_mem_consumers(N) ->
    {Top, _} = top_mem_consumers(N),
    gc_pids([P || {P,_} <- Top]).

-type consumers() :: {kz_term:proplist_kv(pid(), integer()), kz_term:proplist_kv(pid(), integer())}.

-spec top_mem_consumers() -> consumers().
top_mem_consumers() ->
    top_mem_consumers(10).

-spec top_mem_consumers(pos_integer()) -> consumers().
top_mem_consumers(Len) when is_integer(Len), Len > 0 ->
    SortHeapDesc =
        lists:reverse(
          lists:keysort(2
                       ,[{P, erlang:process_info(P, 'total_heap_size')} || P <- processes()]
                       )
         ),
    lists:split(Len, SortHeapDesc).

-spec ets_info() -> 'ok'.
ets_info() ->
    io:format("ETS table memory usage:~n"),
    _ = [print_table(T) || T <- sort_tables(ets:all())],
    'ok'.

-spec sort_tables([ets:tab()]) -> [{string(), integer()}].
sort_tables(Ts) ->
    lists:reverse(
      lists:keysort(2
                   ,[{table_name(T), table_size(T)} || T <- Ts]
                   )
     ).

-spec table_size(ets:tid()) -> integer().
table_size(T) ->
    kz_term:words_to_bytes(ets:info(T, 'memory')).

-spec print_table({string(), integer()}) -> 'ok'.
print_table({Name, Mem}) ->
    io:format("  ~-25s: ~6s~n", [Name
                                ,kz_util:pretty_print_bytes(Mem, 'truncated')
                                ]).

-spec log_table({string(), integer()}, file:name_all()) -> 'ok'.
log_table({Name, Mem}, Filename) ->
    Bytes = io_lib:format("  ~-25s: ~6s~n", [Name
                                            ,kz_util:pretty_print_bytes(Mem, 'truncated')]),
    'ok' = file:write_file(Filename, Bytes, ['append']).

-spec mem_info() -> 'ok'.
mem_info() ->
    io:format(" VM Memory Info:~n"),
    [print_memory_type(Info) || Info <- erlang:memory()],
    'ok'.

-spec print_memory_type({erlang:memory_type(), integer()}) -> 'ok'.
print_memory_type({Type, Size}) ->
    io:format("  ~-15s : ~6s~n", [Type, kz_util:pretty_print_bytes(Size, 'truncated')]).

-spec log_memory_type({erlang:memory_type(), integer()}, file:name_all()) -> 'ok'.
log_memory_type({Type, Size}, Filename) ->
    Bytes = io_lib:format("  ~-15s : ~6s~n", [Type, kz_util:pretty_print_bytes(Size, 'truncated')]),
    'ok' = file:write_file(Filename, Bytes, ['append']).

-spec table_name(ets:tab()) -> string().
table_name(Tab) ->
    kz_term:to_list(ets:info(Tab, 'name')).
