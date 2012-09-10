%% @doc
-module(meck_performance_test).

%% Interface exports
-export([run/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

run(N) ->
    meck:new(test),
    io:format("\t\tMin\tMax\tMed\tAvg~n"),
    io:format("expect/3\t~p\t~p\t~p\t~p~n",
              test_avg(meck, expect, [test, normal, fun() -> ok end], N)),
    io:format("expect/3+args\t~p\t~p\t~p\t~p~n",
              test_avg(meck, expect, [test, normal_args,
                                      fun(_, _) -> ok end], N)),
    io:format("expect/4\t~p\t~p\t~p\t~p~n",
              test_avg(meck, expect, [test, shortcut, 0, ok], N)),
    io:format("expect/4+args\t~p\t~p\t~p\t~p~n",
              test_avg(meck, expect, [test, shortcut_args, 2, ok], N)),

    meck:expect(test, shortcut_opaque, 0, self()),

    io:format("~n\t\tMin\tMax\tMed\tAvg~n"),
    io:format("normal\t\t~p\t~p\t~p\t~p~n",
              test_avg(test, normal, [], N)),
    io:format("normal_args\t~p\t~p\t~p\t~p~n",
              test_avg(test, normal_args, [a, b], N)),
    io:format("shortcut\t~p\t~p\t~p\t~p~n",
              test_avg(test, shortcut, [], N)),
    io:format("shortcut_args\t~p\t~p\t~p\t~p~n",
              test_avg(test, shortcut_args, [a, b], N)),
    io:format("shortcut_opaque\t~p\t~p\t~p\t~p~n",
              test_avg(test, shortcut_opaque, [], N)),
    meck:unload(test),

    meck:new(test),
    meck:expect(test, func, 1, ok),
    [test:func(I) || I <- lists:seq(1, 100)],
    io:format("~n\t\tMin\tMax\tMed\tAvg~n"),
    io:format("called\t\t~p\t~p\t~p\t~p~n",
              test_avg(meck, called, [test, func, 50], N)),
    meck:unload(test),
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    [Min, Max, Med, Avg].

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
