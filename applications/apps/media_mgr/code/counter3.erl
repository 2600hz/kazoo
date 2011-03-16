%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(counter3).

-export([start/0, bump/1, read/1]).

start() -> spawn(fun() -> counter(0) end). %% (1)

bump(Pid) -> Pid ! bump.      %% (2) 



read(Pid) -> 
    Pid ! {self(), read}, 
    receive
	{Pid, N} ->       
	    N
    after
	1000 ->
	    noreply
    end.
    

counter(N) -> 
    receive      
	bump -> 
	    counter(N+1);
	{From, read} ->           %% (3)
	    From ! {self(), N},   %% (4)
	    counter(N);
	stop ->            
	    true
    end.

