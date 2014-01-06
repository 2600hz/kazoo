%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_vm_metrics.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% convert erlang system metrics to proplists
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_vm_metrics).

-export([get_system_info/0,
         get_statistics/0,
         get_memory/0,
         get_process_info/0,
         get_port_info/0,
         get_ets_info/0,
         get_dets_info/0
        ]).

%% exported for eunit test
-export([convert_system_info/2]).

-include("folsom.hrl").


% api

get_memory() ->
    erlang:memory().

get_statistics() ->
    [{Key, convert_statistics(Key, get_statistics(Key))} || Key <- ?STATISTICS].

get_system_info() ->
    [{Key, convert_system_info(Key, get_system_info(Key))} || Key <- ?SYSTEM_INFO].

get_process_info() ->
    [{pid_port_fun_to_atom(Pid), get_process_info(Pid)} || Pid <- processes()].

get_port_info() ->
    [{pid_port_fun_to_atom(Port), get_port_info(Port)} || Port <- erlang:ports()].

get_ets_info() ->
    [{Tab, get_ets_dets_info(ets, Tab)} || Tab <- ets:all()].

get_dets_info() ->
    [{Tab, get_ets_dets_info(dets, Tab)} || Tab <- dets:all()].

% internal functions

% wrap system_info and statistics in a try/catch in case keys are missing
% in old/new versions of erlang

get_system_info(Key) ->
    try erlang:system_info(Key) catch
                                    error:badarg->undefined
                                end.

get_statistics(Key) ->
    try erlang:statistics(Key) catch
                                   error:badarg->undefined
                               end.

%% conversion functions for erlang:statistics(Key)

convert_statistics(context_switches, {ContextSwitches, 0}) ->
    ContextSwitches;
convert_statistics(garbage_collection, {NumberofGCs, WordsReclaimed, 0}) ->
    [{number_of_gcs, NumberofGCs}, {words_reclaimed, WordsReclaimed}];
convert_statistics(io, {Input, Output}) ->
    [Input, Output];
convert_statistics(reductions, {TotalReductions, ReductionsSinceLastCall}) ->
    [{total_reductions, TotalReductions},
     {reductions_since_last_call, ReductionsSinceLastCall}];
convert_statistics(runtime, {TotalRunTime, TimeSinceLastCall}) ->
    [{total_run_time, TotalRunTime}, {time_since_last_call, TimeSinceLastCall}];
convert_statistics(wall_clock, {TotalWallclockTime, WallclockTimeSinceLastCall}) ->
    [{total_wall_clock_time, TotalWallclockTime},
     {wall_clock_time_since_last_call, WallclockTimeSinceLastCall}];
convert_statistics(_, Value) ->
    Value.

%% conversion functions for erlang:system_info(Key)

convert_system_info(allocated_areas, List) ->
    [convert_allocated_areas(Value) || Value <- List];
convert_system_info(allocator, {_,_,_,List}) ->
    List;
convert_system_info(c_compiler_used, {Compiler, Version}) ->
    [{compiler, Compiler}, {version, convert_c_compiler_version(Version)}];
convert_system_info(cpu_topology, undefined) ->
    undefined;
convert_system_info(cpu_topology, List) when is_list(List) ->
    [{Type, convert_cpu_topology(Item, [])} || {Type, Item} <- List];
convert_system_info(cpu_topology, {logical,Item}) ->
    convert_system_info(cpu_topology, [{processor,[{core,{logical,Item}}]}]);
convert_system_info(dist_ctrl, List) ->
    lists:map(fun({Node, Socket}) ->
                      {ok, Stats} = inet:getstat(Socket),
                      {Node, Stats}
              end, List);
convert_system_info(driver_version, Value) ->
    list_to_binary(Value);
convert_system_info(machine, Value) ->
    list_to_binary(Value);
convert_system_info(otp_release, Value) ->
    list_to_binary(Value);
convert_system_info(scheduler_bindings, Value) ->
    tuple_to_list(Value);
convert_system_info(system_version, Value) ->
    list_to_binary(Value);
convert_system_info(system_architecture, Value) ->
    list_to_binary(Value);
convert_system_info(version, Value) ->
    list_to_binary(Value);
convert_system_info(_, Value) ->
    Value.

convert_allocated_areas({Key, Value1, Value2}) ->
    {Key, [Value1, Value2]};
convert_allocated_areas({Key, Value}) ->
    {Key, Value}.

convert_c_compiler_version({A, B, C}) ->
    list_to_binary(io_lib:format("~p.~p.~p", [A, B, C]));
convert_c_compiler_version({A, B}) ->
    list_to_binary(io_lib:format("~p.~p", [A, B]));
convert_c_compiler_version(A) ->
    list_to_binary(io_lib:format("~p", [A])).

convert_cpu_topology([{core, Value}| Tail], Acc) when is_tuple(Value) ->
    convert_cpu_topology(Tail, lists:append(Acc, [{core, tuple_to_list(Value)}]));
convert_cpu_topology([{core, Value}| Tail], Acc) when is_list(Value) ->
    convert_cpu_topology(Tail, lists:append(Acc, [{core, convert_cpu_topology(Value, [])}]));
convert_cpu_topology([{thread, Value}| Tail], Acc) ->
    convert_cpu_topology(Tail, lists:append(Acc, [{thread, tuple_to_list(Value)}]));
convert_cpu_topology([{node, Value}| Tail], Acc) ->
    convert_cpu_topology(Tail, lists:append(Acc, [{node, convert_cpu_topology(Value, [])}]));
convert_cpu_topology([{processor, Value}| Tail], Acc) ->
    convert_cpu_topology(Tail, lists:append(Acc, [{processor, convert_cpu_topology(Value, [])}]));
convert_cpu_topology({Key, Value}, _) ->
    [{Key, Value}];
convert_cpu_topology([], Acc) ->
    Acc.

get_process_info(Pid) ->
    Info = [process_info(Pid, Key) || Key <- ?PROCESS_INFO],
    lists:flatten([convert_pid_info(Item) || Item <- Info]).

get_port_info(Port) ->
    Stat = get_socket_getstat(Port),
    SockName = get_socket_sockname(Port),
    Opts = get_socket_opts(Port),
    Info = get_erlang_port_info(Port),
    Protocol = get_socket_protocol(Port),
    Status = get_socket_status(Port),
    Type = get_socket_type(Port),

    lists:flatten(lists:append([
                                Stat,
                                SockName,
                                Opts,
                                Info,
                                Protocol,
                                Status,
                                Type
                               ])).

get_socket_getstat(Socket) ->
    case catch inet:getstat(Socket) of
        {ok, Info} ->
            Info;
        _ ->
            []
    end.

get_socket_status(Socket) ->
    case catch prim_inet:getstatus(Socket) of
        {ok, Status} ->
            [{status, Status}];
        _ ->
         []
    end.

get_erlang_port_info(Port) ->
    Info = erlang:port_info(Port),
    [convert_port_info(Item) || Item <- Info].

get_socket_type(Socket) ->
    case catch prim_inet:gettype(Socket) of
        {ok, Type} ->
            [{type, tuple_to_list(Type)}];
        _ ->
         []
    end.

get_socket_opts(Socket) ->
    [get_socket_opts(Socket, Key) || Key <- ?SOCKET_OPTS].

get_socket_opts(Socket, Key) ->
    case catch inet:getopts(Socket, [Key]) of
        {ok, Opt} ->
            Opt;
        _ ->
            []
    end.

get_socket_protocol(Socket) ->
    case erlang:port_info(Socket, name) of
        {name, "tcp_inet"} ->
            [{protocol, tcp}];
        {name, "udp_inet"} ->
            [{protocol, udp}];
        {name,"sctp_inet"} ->
            [{protocol, sctp}];
        _ ->
            []
    end.

get_socket_sockname(Socket) ->
    case catch inet:sockname(Socket) of
        {ok, {Ip, Port}} ->
            [{ip, ip_to_binary(Ip)}, {port, Port}];
        _ ->
            []
    end.

get_ets_dets_info(Type, Tab) ->
    case Type:info(Tab) of
        undefined -> [];
        Entries when is_list(Entries) ->
            [{Key, pid_port_fun_to_atom(Value)} || {Key, Value} <- Entries]
    end.

ip_to_binary(Tuple) ->
    iolist_to_binary(string:join(lists:map(fun integer_to_list/1, tuple_to_list(Tuple)), ".")).

convert_port_info({name, Name}) ->
    {name, list_to_binary(Name)};
convert_port_info({links, List}) ->
    {links, [pid_port_fun_to_atom(Item) || Item <- List]};
convert_port_info({connected, Pid}) ->
    {connected, pid_port_fun_to_atom(Pid)};
convert_port_info(Item) ->
    Item.

convert_pid_info({current_function, MFA}) ->
    {current_function, tuple_to_list(MFA)};
convert_pid_info({Key, Term}) when is_pid(Term) or is_port(Term) or is_function(Term) ->
    {Key, pid_port_fun_to_atom(Term)};
convert_pid_info({links, List}) ->
    {links, [pid_port_fun_to_atom(Item) || Item <- List]};
convert_pid_info({suspending, List}) ->
    {suspending, [pid_port_fun_to_atom(Item) || Item <- List]};
convert_pid_info({monitors, List}) ->
    {monitors, [{Key, pid_port_fun_to_atom(Value)} || {Key, Value} <- List]};
convert_pid_info({monitored_by, List}) ->
    {monitored_by, [pid_port_fun_to_atom(Item) || Item <- List]};
convert_pid_info({binary, List}) ->
    {binary, [tuple_to_list(Item) || Item <- List]};
convert_pid_info({initial_call, MFA}) ->
    {inital_call, tuple_to_list(MFA)};
convert_pid_info(Item) ->
    Item.

pid_port_fun_to_atom(Term) when is_pid(Term) ->
    erlang:list_to_atom(pid_to_list(Term));
pid_port_fun_to_atom(Term) when is_port(Term) ->
    erlang:list_to_atom(erlang:port_to_list(Term));
pid_port_fun_to_atom(Term) when is_function(Term) ->
    erlang:list_to_atom(erlang:fun_to_list(Term));
pid_port_fun_to_atom(Term) ->
    Term.
