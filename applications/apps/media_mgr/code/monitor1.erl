%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(monitor1).
-export([start/1]).

start(Pid) ->
    spawn(fun() -> run(Pid) end).

run(Pid) ->
    link(Pid),
    loop(0).

loop(N) ->
    io:format("~p N=~p~n",[?MODULE, N]),
    receive
    after 2000 ->
	loop(N+1)
    end.
       
			 
			
