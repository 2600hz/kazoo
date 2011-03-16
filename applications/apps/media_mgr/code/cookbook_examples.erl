%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(cookbook_examples).

%% examples in the cookbook chapter
%% not generally useful for anything else than
%% understanding the cookbook chapter

-compile(export_all).
-import(lists, [foreach/2, map/2, reverse/1]).


time(Fun) ->
    timer:tc(lib_cookbook, eval_fun, [Fun]).

eval_fun(Fun) -> Fun().



fib1(0) -> 1;
fib1(1) -> 1;
fib1(N) -> fib1(N-1) + fib1(N-2).



fib2(N) -> fib2(N, 1, 1).

fib2(0, A, _) -> A;
fib2(N, A, B) -> fib2(N-1, B, A+B).



extract_people(Tree) ->
    extract_people(Tree, []).
    
extract_people({person,_,_}=Person, L) ->
    [Person|L];
extract_people([H|T], L) ->
    L1 = extract_people(H, L),
    extract_people(T, L1);
extract_people(Tuple, L) when tuple(Tuple) ->
    extract_people(tuple_to_list(Tuple), L);
extract_people(_, L) ->
    L.



swap_names({person,First,Last}) ->
    {person, Last, First};
swap_names([H|T]) ->
    [swap_names(H)|swap_names(T)];
swap_names(Tuple) when tuple(Tuple) ->
    L1 = tuple_to_list(Tuple),
    L2 = map(fun swap_names/1, L1),
    list_to_tuple(L2);
swap_names(X) ->
    X.



shopping_list() ->
    [["Eggs", 24, 1.2],
     ["Apples", 6, 2.1],
     ["Sausages", 5, 5.6],
     ["Fish", 3, 5.4]].

print_shopping_list() ->
    show_format("~p ~p ~p~n"),
    show_format("~s ~w ~f~n"),
    show_format("~7s ~2w ~4.2f~n"),
    show_format("~-7s ~2w ~3.1f~n").

show_format(Format) ->
    io:format("  ~s~n",[Format]),
    foreach(fun(Items) -> io:format(Format, Items) end, 
	    shopping_list()),
    io:format("~n").

		    

test_data1() ->
    {hello, 123,
     [foo,{a,b,[{person,"Xambro", "Skoplangard"},[a,b]],
	   def, {g,h,{i, [a, {person,"Zolab","Zinkeldoffle"},q]}}}]}.

    

binary_file_access() ->
    File = "test.bin",
    file:write_file(File, [<<"12345abcde">>]),
    {ok, Initial} = file:read_file(File),
    {ok, FileHandle} = file:open(File, [binary,raw,read,write]),
    {ok, B1} = file:pread(FileHandle, 1, 2), 
    {ok, [First,Last]} = file:pread(FileHandle, [{0,3},{7,3}]),
    file:pwrite(FileHandle, [{0,Last},{7,First}]),
    file:close(FileHandle),
    {ok, Final} = file:read_file(File),
    {{initial,Initial}, 
     {b1,B1}, 
     {first,First}, {last,Last}, {final,Final}}.

