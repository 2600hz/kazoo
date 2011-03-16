%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(monitor3).
-export([start/0]).


start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> error1:start() end),
    loop(2, Pid).


loop(N, Pid) ->
    io:format("~p N=~p~n",[?MODULE, N]),
    Pid ! N,
    receive
	Any ->
	    io:format("~p received:~p~n",[?MODULE, Any]),
	    loop(N, Pid)
    after 1000 ->
	loop(N-1, Pid)
    end.
       
			 
			
