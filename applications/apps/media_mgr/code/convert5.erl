%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(convert5).
%% I can remove thjis it's not in the book
-export([convert1/2, convert2/2]).


convert1({yards,  X}, meters) -> {meters, 0.9144 * X};
convert1({yards,  X}, feet)   -> {feet, 3 * X};
convert1({meters, X}, yards)  -> {yards,  1.0936133 * X};
convert1({meters, X}, feet)   -> {feet, 3.2808399 * X};
convert1({feet,   X}, meters) -> {meters, 0.3048 * X};
convert1({feet,   X}, yards)  -> {yards, 0.3333333 * X};
convert1({Tag, X}, Tag)       -> {Tag, X}.



convert2(In, Out) ->
    case In of
	{yards, X} ->
	    case Out of
		meters -> {meters, 0.9144 * X};
		feet   -> {feet, 3 * X}
	    end;
	{meters, X} ->
	    case Out of
		yards -> {yards,  1.0936133 * X};
		feet  -> {meters, 0.3048 * X}
	    end;
	{feet, X} ->
	    case Out of
		meters -> {meters, 0.3048 * X};
		yards  -> {yards, 0.3333333 * X}
	    end;
	{_Tag, Out} ->
	    %% I dont understahd this ...
	    In
    end.

		    
		    
		    
	    
