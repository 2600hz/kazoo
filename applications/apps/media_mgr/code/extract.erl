%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(extract).
-export([attribute/2]).

attribute(File, Key) ->
    case beam_lib:chunks(File,[attributes]) of
	{ok, {_Module, [{attributes,L}]}} ->
	    case lookup(Key, L) of
		{ok, Val} ->  
		    Val;
		error ->
		    exit(badAttribute)
	    end;
	_ -> 
	    exit(badFile)
    end.

lookup(Key, [{Key,Val}|_]) -> {ok, Val};
lookup(Key, [_|T])         -> lookup(Key, T);
lookup(_, [])              -> error.
