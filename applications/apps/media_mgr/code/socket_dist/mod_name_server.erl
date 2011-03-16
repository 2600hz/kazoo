%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(mod_name_server).
-export([start_me_up/3]).

start_me_up(MM, _ArgsC, _ArgS) ->
    loop(MM).

loop(MM) ->
    receive
	{chan, MM, {store, K, V}} ->
	    kvs:store(K, V),
	    loop(MM);
	{chan, MM, {lookup, K}} ->
	    MM ! {send, kvs:lookup(K)},
	    loop(MM);
	{chan_closed, MM} ->
	    true
    end.
