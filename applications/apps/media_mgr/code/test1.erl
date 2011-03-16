%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(test1).
-export([foo/1]).

zip(X) ->
    % this is zip
    Y = "abc",
    case X of
	1 ->
	    2
    end.


foo(X) ->

    bar(X).



bar(X) ->
    42.
 
