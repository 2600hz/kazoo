%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(error1).
-export([start/0]).

start() -> loop(0).

loop(M) ->
    io:format("error1 M=~p~n",[M]),
    receive
	K ->
	    1/K,
	    loop(M+1)
    after 500 ->
	    loop(M+1)
    end.
