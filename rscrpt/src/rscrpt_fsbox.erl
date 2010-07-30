%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Returns a Key-Value list of data about the box; things like free
%%% memory, disk usage, avg CPU usage, etc...
%%% @end
%%% Created : 27 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(rscrpt_fsbox).

-import(rscrpt_logger, [log/2, format_log/3]).

%% API
-export([get_box_update/0]).

%% send updates to the queue about resources available from this box
get_box_update() ->
    Memory = get_memory_update(),
    Disk = get_disk_update(),
    Cpu = get_cpu_update(),
    Nic = get_nic_update(),
    lists:concat([Memory, Disk, Cpu, Nic]).

get_nic_update() ->
    [{hostname, net_adm:localhost()}].

get_cpu_update() ->
    [{cpu_avg_1, cpu_sup:avg1()}
     ,{cpu_avg_5, cpu_sup:avg5()}
     ,{cpu_avg_15, cpu_sup:avg15()}
    ].

get_disk_update() ->
    KVs = disksup:get_disk_data(),
    {_, TotalDisk, PerUsed} = lists:keyfind("/", 1, KVs),
    [{total_disk, TotalDisk}, {used_disk_percentage, PerUsed}].

get_memory_update() ->
    Mem = memsup:get_system_memory_data(),
    Free = proplists:get_value(cached_memory, Mem) + proplists:get_value(buffered_memory, Mem) + proplists:get_value(free_memory, Mem), % tested vs lists:foldl - 20:1 speed boost
    [{total_memory, proplists:get_value(system_total_memory, Mem)}
     ,{free_memory, Free}].
