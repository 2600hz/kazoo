%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(counter4).
-export([start/0]).

start() -> lib_misc:spawn_monitor(counter, true, fun() -> counter(0) end).

counter(N) -> 
    receive                 
	bump ->             
	    counter(N+1);
	display ->          
	    io:format("Count=~p~n", [N]),
	    counter(N);
	stop ->             
	    true
    end.
