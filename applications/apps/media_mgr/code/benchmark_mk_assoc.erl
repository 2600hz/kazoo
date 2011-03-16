%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(benchmark_mk_assoc).

-compile(export_all).

test(N) ->
    [{list,dict,ets},
     {method1(N), method2(N),method3(N)}].

method1(N) when N > 20000 ->
    false;
method1(N) ->
    {T, _} = timer:tc(?MODULE, make_assoc_list1, [N]),
    trunc(T/N).

method2(N) when N > 300000 ->
    false;
method2(N) ->
    random_seed(),
    D = dict:new(),
    {T, _} = timer:tc(?MODULE, make_random_list2, [N, D]),
    trunc(T/N).

method3(N) ->
    random_seed(),
    E = ets:new(sometable,[]),
    {T, _} = timer:tc(?MODULE, make_random_list3, [N, E]),
    ets:delete(E),
    trunc(T/N).

make_random_list3(0, E) -> E;
make_random_list3(N, E) ->
    %% Key = list_to_binary(random_string(4,6,$a, 26)),
    Key = (random_string(4,6,$a, 26)),
    case ets:lookup(E, Key) of
	[] -> 
	    Val = list_to_integer(random_string(2,6,$0,10)),
	    ets:insert(E, {Key, Val}),
	    make_random_list3(N-1 , E);
	_ ->
	    make_random_list3(N, E)
    end.

make_random_list2(0, D) -> dict:to_list(D);
make_random_list2(N, D) ->
    %% Key = list_to_binary(random_string(4,6,$a, 26)),
    Key = (random_string(4,6,$a, 26)),
    case dict:find(Key, D) of
	error ->
	    Val = list_to_integer(random_string(2,6,$0,10)),
	    D1 = dict:store(Key, Val, D),
	    make_random_list2(N-1 , D1);
	{ok, _} ->
	    make_random_list2(N, D)
    end.


make_assoc_list1(N) ->
    random_seed(),
    make_random_list1(N, []).

%% goes like N^2

make_random_list1(0, L) -> L;
make_random_list1(N, L) ->
    %% Key = list_to_binary(random_string(4,6,$a, 26)),
    Key = (random_string(4,6,$a, 26)),
    case is_defined(Key, L) of
	true ->
	    make_random_list1(N , L);
	false ->
	    Val = list_to_integer(random_string(2,6,$0,10)),
	    make_random_list1(N-1, [{Key,Val}|L])
    end.

is_defined(Key, [{Key,_}|_]) -> true;
is_defined(Key, [_|T])       -> is_defined(Key, T);
is_defined(_, [])            -> false.
    
random_string(Min, Extra, Start, K) -> 
    Length = Min + random:uniform(Extra),
    random_str(Length, Start, K, []).

random_str(0, _, _, D) -> 
    D;
random_str(N, Start, K, D) -> 
    random_str(N-1, Start, K, [random:uniform(K)+ Start - 1|D]).

random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).

%% 1>  benchmark_mk_assoc:test(20000).
%% [{list,dict,ets},{1149,82,42}]
%% 2> c(benchmark_mk_assoc).
%% {ok,benchmark_mk_assoc}
%% 3> benchmark_mk_assoc:test(30000).
%% [{list,dict,ets},{false,80,46}]
%% 4> benchmark_mk_assoc:test(100000).
%% [{list,dict,ets},{false,118,42}]
%% 5> benchmark_mk_assoc:test(200000).
%% [{list,dict,ets},{false,235,59}]
%% 6> c(benchmark_mk_assoc).
%% ./benchmark_mk_assoc.erl:15: syntax error before: '/'
%% ./benchmark_mk_assoc.erl:7: function method2/1 undefined
%% error
%% 7> c(benchmark_mk_assoc).
%% {ok,benchmark_mk_assoc}
%% 8> benchmark_mk_assoc:test(300000).
%% [{list,dict,ets},{false,251,49}]
%%  benchmark_mk_assoc:test(400000).
%% [{list,dict,ets},{false,false,59}]

%%  benchmark_mk_assoc:test(1000000).
%% [{list,dict,ets},{false,false,80}]

