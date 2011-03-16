%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(mod_echo).

-export([start/3]).

start(MM, ArgC, ArgS) ->
    io:format("Echo starting arguments ArgC:~p ArgS:~p~n",
	      [ArgC, ArgS]),
    loop(MM).

loop(MM) ->
    receive
	{chan, MM, Any} ->
	    MM ! {send, Any}, 
	    loop(MM);
	{chan_closed, MM} ->
	    io:format("echo channel closed~n"),
	    exit(normal);
	Any ->
	    io:format("echo bad message:~p~n",[Any]),
	    loop(MM)
    end.
