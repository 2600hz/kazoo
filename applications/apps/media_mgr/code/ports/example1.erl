%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(example1).
-export([start/0, stop/0]).
-export([twice/1, sum/2]).

start() ->
    spawn(fun() ->
		  register(example1, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "./example1"}, [{packet, 2}]),
		  loop(Port)
	  end).

stop() ->
    example1 ! stop.

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).

call_port(Msg) ->
    example1 ! {call, self(), Msg},
    receive
	{example1, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}}, 
	    receive
		{Port, {data, Data}} ->
		    Caller ! {example1, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated,Reason})
    end.

encode({twice, X}) -> [1, X];  
encode({sum, X, Y}) -> [2, X, Y]. 

decode([Int]) -> Int. 
