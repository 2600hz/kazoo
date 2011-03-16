%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(counter2).
-export([start/0, read/1]).

start() -> spawn(fun() -> counter(0) end). %% (1)

read(Pid) ->                 %% (2)
    Pid ! {self(), read},   
    receive
	{Pid, N} ->         
	    N
    end.
    
counter(N) -> 
    receive      
	bump -> 
	    counter(N+1);
	{From, read} ->           %% (3)
	    From ! {self(), N},  
	    counter(N);
	stop ->            
	    true
    end.

