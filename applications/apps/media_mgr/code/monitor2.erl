%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(monitor2).
-export([start/0]).

start() ->
    spawn(fun() -> run() end).

run() ->
    process_flag(trap_exit, true),
    counter1:start(),
    link(whereis(counter1)),
    loop().

loop() ->
    receive
	{'EXIT', Pid, Why} ->
	    io:format("Monitor caught an exit from:~p Why=~p~n",
		      [Pid, Why]),
	    counter1:start(),
	    link(whereis(counter1)),
	    loop()
    end.

			 
			
