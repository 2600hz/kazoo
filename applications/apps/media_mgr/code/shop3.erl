%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(shop3).
-export([total/1]).
 
total([{What, N}|T]) ->
    case (catch shop:cost(What)) of
	{'EXIT', _} ->
	    io:format("The shop does not sell ~p~n",[What]),
	    total(T);
	Cost ->
	    Cost * N + total(T)
    end;
total([]) ->
    0.

