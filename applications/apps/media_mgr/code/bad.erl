%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(bad).

%% There are lots's of delibeate errors in this file
%% so it's not in the makefile


foo(1,2) ->
    a;
foo(2,3,a) ->
    b.



foo(A, B) ->
    bar(A, dothis(X), B),
    baz(Y, X).



foo() ->
    case bar() of
	1 -> 
	    X = 1,
	    Y = 2;
	2 ->
	    X = 3
    end,
    b(X, Y).



foo() ->
    case bar() of
	1 -> 
	    X = 1,
	    Y = 2;
	2 ->
	    X = 3
    end,
    b(X).



foo(X, L) ->
    lists:map(fun(X) -> 2*X end, L).



foo(X, L) ->
    lists:map(fun(Z) -> 2*Z end, L).



foo(X) ->
    io:format("hello ~p~n, [X]).

