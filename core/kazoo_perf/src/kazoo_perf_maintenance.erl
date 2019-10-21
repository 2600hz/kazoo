%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Simple (and mostly naive) script that connects to a remote Erlang node,
%%% fetches memory and process statistics and prints them in a Sensu compatible way
%%% for metrics gathering (great for Graphite)
%%%
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_perf_maintenance).

-export([graphite_metrics/3
        ,json_metrics/0
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% API

-spec graphite_metrics(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> no_return.
graphite_metrics(Account, Cluster, Zone) ->
    Scheme = scheme(Account, Cluster, Zone),
    F = fun ({Metric, Measured}) -> graphite(Scheme, Metric, Measured) end,
    lists:foreach(F, collect()),
    no_return.

-spec json_metrics() -> no_return.
json_metrics() ->
    JObj = kz_json:from_list_recursive(
             [{<<"timestamp">>, get(timestamp)}
              | [{Metric, to_props(Metric, Measure)} || {Metric, Measure} <- collect()]
             ]),
    io:format("~s\n", [kz_json:encode(JObj)]),
    no_return.

%% Internals

measure(memory_statistics) -> erlang:memory();
measure(number_of_processes) -> erlang:system_info(process_count);
measure(context_switches) -> erlang:statistics(context_switches);
measure(garbage_collection) -> erlang:statistics(garbage_collection);
measure(io) -> erlang:statistics(io);
measure(scheduler_reductions) -> erlang:statistics(reductions);
measure(processes_in_run_queue_of_each_schedulers) -> erlang:statistics(run_queue);
measure(ets_tables_sizes) ->
    [{Tab, ets:info(Tab, size)} || Tab <- ets:all()];
measure(info) -> info_to_props(erlang:system_info(info)).

collect() ->
    {Mega, Secs, _Micro} = os:timestamp(),
    put(timestamp, Mega * 1000000 + Secs),
    Metrics = [context_switches
              ,ets_tables_sizes
              ,garbage_collection
              ,info
              ,io
              ,memory_statistics
              ,number_of_processes
              ,processes_in_run_queue_of_each_schedulers
              ,scheduler_reductions
              ],
    Malt = [1, {processes, length(Metrics)}],
    plists:map(fun (Metric) -> {Metric, measure(Metric)} end, Metrics, Malt).

scheme(Account, Cluster, Zone) ->
    [Service, Hostname0] = binary:split(kz_term:to_binary(node()), <<$@>>),
    Hostname = binary:replace(Hostname0, <<$.>>, <<"::">>, [global]),
    kz_term:iolist_join($., [Account, Cluster, Zone, Hostname, Service]).

print_metric(Scheme, Key, Value) ->
    io:format("~s.~s ~B ~B\n", [Scheme, Key, Value, get(timestamp)]).

graphite(Scheme, memory_statistics, MemoryMetrics) ->
    [print_metric(Scheme, "memory_" ++ atom_to_list(Key), Value)
     || {Key, Value} <- MemoryMetrics
    ];
graphite(Scheme, number_of_processes, LiveProcesses) ->
    print_metric(Scheme, processes, LiveProcesses);
graphite(Scheme, context_switches, {Switches, 0}) ->
    print_metric(Scheme, context_switches, Switches);
graphite(Scheme, garbage_collection, {NumberofGCs, WordsReclaimed, 0}) ->
    print_metric(Scheme, number_of_gcs, NumberofGCs),
    print_metric(Scheme, words_reclaimed, WordsReclaimed);
graphite(Scheme, io, {{input, Input}, {output, Output}}) ->
    print_metric(Scheme, input_io_bytes, Input),
    print_metric(Scheme, output_io_bytes, Output);
graphite(Scheme, scheduler_reductions, {TotalReductions, _ReductionsSinceLastCall}) ->
    print_metric(Scheme, reductions, TotalReductions);
graphite(Scheme, processes_in_run_queue_of_each_schedulers, RunQueue) ->
    print_metric(Scheme, run_queue, RunQueue);
graphite(Scheme, ets_tables_sizes, Tabs) ->
    [print_metric(Scheme, "tab_" ++ kz_term:to_list(maybe_from_ref(Tab)), Size)
     || {Tab, Size} <- Tabs,
        Size =/= 0
    ];
graphite(Scheme, info, Props) ->
    [print_metric(Scheme, <<"info__", Key/binary>>, IntValue)
     || {Key, Value} <- Props,
        nomatch =:= binary:match(Value, <<$,>>),
        IntValue <- [bin_to_integer(Value)],
        IntValue =/= not_an_int
    ].

to_props(memory_statistics, MemoryMetrics) ->
    [{"memory_" ++ atom_to_list(Key), Value}
     || {Key, Value} <- MemoryMetrics
    ];
to_props(number_of_processes, LiveProcesses) ->
    [{processes, LiveProcesses}
    ];
to_props(context_switches, {Switches, 0}) ->
    [{context_switches, Switches}
    ];
to_props(garbage_collection, {NumberofGCs, WordsReclaimed, 0}) ->
    [{number_of_gcs, NumberofGCs}
    ,{words_reclaimed, WordsReclaimed}
    ];
to_props(io, {{input, Input}, {output, Output}}) ->
    [{input_io_bytes, Input}
    ,{output_io_bytes, Output}
    ];
to_props(scheduler_reductions, {TotalReductions, _ReductionsSinceLastCall}) ->
    [{reductions, TotalReductions}
    ];
to_props(processes_in_run_queue_of_each_schedulers, RunQueue) ->
    [{run_queue, RunQueue}
    ];
to_props(ets_tables_sizes, Tabs) ->
    [{kz_term:to_binary(maybe_from_ref(Tab)), Size}
     || {Tab, Size} <- Tabs,
        Size =/= 0
    ];
to_props(info, Props) -> Props.

info_to_props(Info) ->
    [<<>>|Categories] = binary:split(Info, <<$=>>, [global]),
    lists:flatten(
      [split_by_category(Category)
       || Category <- Categories
      ]).

split_by_category(Category) ->
    [Name|Items] = binary:split(Category, <<$\n>>, [global]),
    [{binary:replace(NewKey, <<"___">>, <<"__">>, [global])
     ,cleanse_with($,, Value)
     }
     || Item <- Items,
        [Key, Value] <- [binary:split(Item, <<": ">>)],
        NewKey <- [<<(cleanse_with($_, Name))/binary, "__", (cleanse_with($_, Key))/binary>>]
    ].

cleanse_with(Sep, Bin) ->
    ToCleanse = [<<$\s>>, <<$.>>, <<$:>>, <<$[>>, <<$]>>],
    binary:replace(Bin, ToCleanse, <<Sep>>, [global]).

bin_to_integer(<<"true">>) -> 1;
bin_to_integer(<<"false">>) -> 0;
bin_to_integer(Value=?NE_BINARY) ->
    try binary_to_integer(Value)
    catch error:badarg -> not_an_int
    end.

maybe_from_ref(Tab)
  when is_reference(Tab) ->
    Bin1 = iolist_to_binary(io_lib:format("~p", [Tab])),
    Bin2 = binary:replace(Bin1, [<<$<>>,<<$>>>],<<>>, [global]),
    binary:replace(Bin2, <<$.>>, <<$_>>, [global]);
maybe_from_ref(Tab) ->
    Tab.
