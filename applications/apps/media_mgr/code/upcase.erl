%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(upcase).
-export([start/0]).

start() ->
    case io:get_chars('', 8192) of
	eof ->
	    init:stop();
	Str ->
	    Str1 = up_case(Str),
	    io:put_chars(Str1),
	    start()
    end.

up_case([H|T]) when $a =< H, H =< $z ->
    [H-$a+$A|up_case(T)];
up_case([H|T]) ->
    [H|up_case(T)];
up_case([]) ->
    [].
