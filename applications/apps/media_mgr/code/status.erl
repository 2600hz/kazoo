%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(status).

-compile(export_all).
-import(lists, [filter/2, map/2, member/2]).

start() ->
    not_compiled().

not_compiled() ->
    E = lib_files_find:files(".", "*.erl",false),
    E1 = map(fun(I) -> filename:rootname(I) end, E),
    B = lib_files_find:files(".", "*.beam",false),
    B1 = map(fun(I) -> filename:rootname(I) end, B),
    filter(fun(I) -> not member(I, B1) end, E1).

		   
		     
