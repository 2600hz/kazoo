%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(counter1).
-export([start/0, counter/1]).

start() -> 
    register(counter1,
	     spawn(fun() ->
			   io:format("Hello I'm a counter:~p~n",[self()]),
			   counter(0) 
		   end)).

counter(N) -> 
    receive    
	bump -> 
	    io:format("info=~p~n",[process_info(self())]),
	    counter(N+1);
	display ->       
	    io:format("Counter ~p count=~p~n", [self(), N]),
	    counter(N);
	die ->
	    io:format("I'm going to die:~p~n",[self()]),
	    1/0,
	    counter(N)
    end.

