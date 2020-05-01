%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc load testing various caching strategies
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_loadtest).

-export([start/0, start/1]).

-include("kz_data.hrl").

-dialyzer({'nowarn_function', spawn_worker/1}).

-spec start() -> 'ok'.
start() ->
    start(1000).

-spec start(pos_integer()) -> 'ok'.
start(Workers) when is_integer(Workers), Workers > 0 ->
    Strategies = ['none', 'async'
                 ,'stampede', 'stampede_async'
                 ],
    run_tests(Workers, Strategies).

run_tests(Workers, Strategies) ->
    DocIds = doc_ids(Workers),

    io:format("using ~p document IDs~n", [length(DocIds)]),

    _ = [run(Workers, S, DocIds) || S <- Strategies],
    'ok'.

-spec doc_ids(pos_integer()) -> kz_term:ne_binaries().
doc_ids(_Workers) ->
    %%    doc_ids_variable_len(_Workers).
    doc_ids_fixed_len().

%% fixed number of doc IDs to fetch against
-spec doc_ids_fixed_len() -> kz_term:ne_binaries().
doc_ids_fixed_len() ->
    {'ok', Js} = kz_datamgr:all_docs(<<"system_schemas">>, [{'limit', 3}]),
    [kz_doc:id(J) || J <- Js].

%% scales doc ids used against workers; more workers, more doc IDs
%% doc_ids_variable_len(Workers) ->
%%     {'ok', Js} = kz_datamgr:all_docs(<<"system_schemas">>),
%%     DocIds = [kz_doc:id(J) || J <- Js],

%%     LenDocIds = length(DocIds),
%%     NDocIds = (Workers div (LenDocIds div 2)) + 1,
%%     {Use, _} = lists:split(NDocIds, DocIds),
%%     Use.

run(Workers, Strategy, DocIds) ->
    start_traces(),
    kzs_cache:set_cache_strategy(Strategy),
    kz_datamgr:flush_cache_docs(<<"system_schemas">>),

    io:format("starting ~s...", [Strategy]),
    Start = kz_time:start_time(),

    PidRefs = [spawn_worker(DocIds) || _ <- lists:seq(1, Workers)],

    {Normal, Not, ElapsedMs} = wait(PidRefs),
    Stop = kz_time:start_time(),

    TotalMs = lists:sum(ElapsedMs),
    MaxMs = lists:max(ElapsedMs),
    MinMs = lists:min(ElapsedMs),

    io:format("~n  finished in ~pms~n  min/mean/max: ~p < ~p < ~p (~p)"
             ,[kz_time:elapsed_ms(Start, Stop)
              ,MinMs, TotalMs div Normal, MaxMs, TotalMs
              ]),
    Not > 0
        andalso io:format(" (~p failed)", [Not]),
    io:format("~n"),
    percentiles(length(ElapsedMs), lists:sort(ElapsedMs)),
    stop_traces().

-define(PATTERNS
       ,[{'kzs_cache', 'fetch_doc', 4}
        ,{'kz_cache', 'store_local', 4}
        ,{'kz_cache', 'store_local_async', 4}
        ]
       ).
start_traces() ->
    _Patterns = [erlang:trace_pattern(Pattern, [], ['call_count'])
                 || Pattern <- ?PATTERNS
                ],
    erlang:trace('all', 'true', ['call']).

stop_traces() ->
    io:format("  calls: "),
    _ = [io:format("~p: ~p ", [F, N])
         || {_M,F,_A}=MFA <- ?PATTERNS,
            {'call_count', N} <- [erlang:trace_info(MFA, 'call_count')],
            N > 0
        ],
    io:format("~n"),
    _ = [erlang:trace_pattern(Pattern, 'false', [])
         || Pattern <- ?PATTERNS
        ],
    erlang:trace('all', 'false', ['call']).

-spec spawn_worker(kz_term:ne_binaries()) -> kz_term:pid_ref().
spawn_worker(DocIds) ->
    spawn_monitor(fun() -> worker(DocIds) end).

-spec worker(kz_term:ne_binaries()) -> no_return().
worker(DocIds) ->
    [DocId | _] = kz_term:shuffle_list(DocIds),

    Start = kz_time:start_time(),
    {'ok', _} = kz_datamgr:open_cache_doc(<<"system_schemas">>, DocId),
    exit({'elapsed_ms', kz_time:elapsed_ms(Start)}).

wait(PidRefs) ->
    wait(PidRefs, {0,0,[]}).

wait([], Finished) -> Finished;
wait([{Pid, Ref}|PidRefs], {Normal, Not, Elapsed}) ->
    receive
        {'DOWN', Ref, 'process', Pid, {'elapsed_ms',MS}} ->
            wait(PidRefs, {Normal+1, Not, [MS | Elapsed]});
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            wait(PidRefs, {Normal, Not+1})
    end.

percentiles(Len, ElapsedMs) ->
    Percentiles = [50, 75, 90, 95, 99],
    Indexes = [{Percentile, index(Percentile, Len)} || Percentile <- Percentiles],
    io:format("  percentiles: "),
    print_percentile(Indexes, ElapsedMs, 1).

print_percentile([], _ElapsedMs, _Index) ->
    io:format("~n");
print_percentile([{Percentile, _Index} | Indexes], [Kth], I) ->
    io:format("~p(~p) ", [Kth, Percentile]),
    print_percentile(Indexes, [Kth], I+1);
print_percentile([{Percentile, Index} | Indexes], [Kth | ElapsedMs], Index) ->
    io:format("~p(~p) ", [Kth, Percentile]),
    print_percentile(Indexes, ElapsedMs, Index+1);
print_percentile(Indexes, [_Kth | ElapsedMs], Index) ->
    print_percentile(Indexes, ElapsedMs, Index+1).

index(Percentile, Len) ->
    kz_term:ceiling(Len * (Percentile / 100)).
