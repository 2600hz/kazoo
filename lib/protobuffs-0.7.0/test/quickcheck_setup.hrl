-ifdef(EQC).
-undef(PROPER).
%% Prefer EQC over PropEr and shim up functions that PropEr provides
%% into EQC equivalents.
-include_lib("eqc/include/eqc.hrl").
integer() ->
    int().
integer(A,B) ->
    choose(A,B).
non_neg_integer() ->
    ?SUCHTHAT(X, int(), X >= 0).
float() ->
    real().
union(List) ->
    elements(List).
boolean() ->
    elements([false, true]).
quickcheck(Prop, _) ->
    quickcheck(Prop).
-endif.

-ifdef(PROPER).
%% No EQC, so include PropEr when available.
-include_lib("proper/include/proper.hrl").
-import(proper, [quickcheck/1, quickcheck/2]).
-endif.

-ifndef(PROPER).
-ifndef(EQC).

%% If neither are available, stub things out so we can at least
%% compile and run the eunit.
-define(FORALL(A,B,C), ok).
integer(_,_) -> ok.
union(_) -> ok.
list(_) -> ok.
choose(_,_) -> ok.
oneof(_) ->  ok.
real() -> ok.
bool() -> ok.
binary() -> ok.
default(_,_) -> ok.
quickcheck(_,_) ->
    io:format(user, "QuickCheck and PropEr not available, skipping test! ~n~p~n~n", [erlang:get_stacktrace()]),
    true.
quickcheck(_) ->
    io:format(user, "QuickCheck and PropEr not available, skipping test! ~n~p~n~n", [erlang:get_stacktrace()]),
    true.
numtests(_,_) -> ok.
-endif.
-endif.
