%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(benchmark_assoc).
-compile(export_all).
-import(lib_misc, [unconsult/2]).

%% make a random list of [{<<Bin>>, Int}] length N

test1(K) when K < 200000->
    L = make_assoc_list(K),
    Mid = K div 2,
    {Key,_} = lists:nth(Mid, L),
    %% create the file
    File = "big.tmp",
    lib_misc:unconsult("big.tmp", L),
    Size = filelib:file_size(File),
    %% time the first update method
    {T1, _} = timer:tc(?MODULE, f1, [Key, File]),
    %% clean up
    file:delete(File),
    {consult, Size, T1};
test1(_) ->
    no.

test2(K) when K < 300000->
    L = make_assoc_list(K),
    Mid = K div 2,
    {Key,_} = lists:nth(Mid, L),
    %% create the file
    File = "big.tmp",
    file:write_file(File, [term_to_binary(L)]),
    Size = filelib:file_size(File),
    {T, _} = timer:tc(?MODULE, f2, [Key, File]),
    %% clean up
    file:delete(File),
    {binary, Size, T};
test2(_) ->
    no.
    
test3(K) ->
    L = make_assoc_list(K),
    %% io:format("L=~p~n",[L]),
    Mid = K div 2,
    {Key,_} = lists:nth(Mid, L),
    File = "big.dets",
    Big = ets:new(big, []),
    lists:foreach(fun(KV) -> ets:insert(Big, KV) end, L),
    %% Now iopen a dets table
    io:format("made ets~n"),
    _V = ets:foldr(fun(X,A) -> [X|A] end, [], Big),
    %% io:format("V=~p~n",[V]),
    {ok, newd} = dets:open_file(newd, [{file,File}]),
    ets:to_dets(Big, newd),
    _V1 = dets:foldr(fun(X,A) -> [X|A] end, [], newd),
    %% io:format("V1=~p~n",[V1]),
    ets:delete(Big),
    dets:close(newd),
    Size = filelib:file_size(File),
    {T, _} = timer:tc(?MODULE, f3, [Key, File]),
    %% clean up
    file:delete(File),
    {dets, Size, T}.


f1(Key, File)  ->
    update(File, Key, 20).

f2(Key, File) ->
    update1(File, Key, 20).

f3(Key, File) ->
    {ok, big} = dets:open_file(big, [{file,File}]),
    %% V1 = dets:foldr(fun(X,A) -> [X|A] end, [], big),
    %% io:format("V1=~p~n",[V1]),
    %% io:format("Key=~p~n",[Key]),
    case dets:lookup(big, Key) of
	[{Key,Val}] ->
	    %% io:format("oldval=~p~n",[Val]),
	    dets:insert(big, {Key, Val+20});
	LL ->
	    io:format("*** error ***:~p~n",[LL])
    end,
    dets:close(big).

make_assoc_list(N) ->
    random_seed(),
    make_random_list(N, []).

%% goes like N^2

make_random_list(0, L) -> L;
make_random_list(N, L) ->
    %% Key = list_to_binary(random_string(4,6,$a, 26)),
    Key = (random_string(4,6,$a, 26)),
    case is_defined(Key, L) of
	true ->
	    make_random_list(N , L);
	false ->
	    Val = list_to_integer(random_string(2,6,$0,10)),
	    make_random_list(N-1, [{Key,Val}|L])
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


update1(File, Key, Delta) ->
    {ok, Bin} = file:read_file(File),
    Terms = binary_to_term(Bin),
    Terms1 = do_update(Key, Delta, Terms),
    file:write_file(File, [term_to_binary(Terms1)]). 

update(File, Key, Delta) ->
    {ok, Terms} = file:consult(File),
    Terms1 = do_update(Key, Delta, Terms),
    unconsult(File, Terms1). 

do_update(Key, Delta, [{Key,Val}|T]) ->
    [{Key,Val+Delta}|T];
do_update(Key, Delta, [H|T]) ->
    [H|do_update(Key, Delta, T)];
do_update(Key, Delta, []) ->
    [{Key, Delta}].

yes() ->
    {ok, abc} = dets:open_file(abc, [{file,"foo.dets"}]),
    L = [{<<"abc">>,123},{<<"zrt123">>,3456},{<<"trui67f">>,22345}],
    lists:foreach(fun(I) -> dets:insert(abc, I) end, L),
    dets:close(abc),
    {ok, abc} = dets:open_file(abc, [{file,"foo.dets"}]),
    Val = dets:lookup(abc, <<"zrt123">>),
    dets:close(abc),
    Val.

yes1() ->
    Tab = ets:new(abc, []),
    L = [{<<"abc">>,123},{<<"zrt123">>,3456},{<<"trui67f">>,22345}],
    lists:foreach(fun(I) -> ets:insert(Tab, I) end, L),
    V1 = ets:lookup(Tab, <<"zrt123">>),
    io:format("V1=~p~n",[V1]),

    ets:delete(Tab).
    
