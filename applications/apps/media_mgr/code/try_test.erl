%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(try_test).
-compile(export_all).


generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> erlang:error(a).



demo1() ->
     [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
   try generate_exception(N) of
       Val -> {N, normal, Val}
   catch
       throw:X -> {N, caught, thrown, X};
       exit:X  -> {N, caught, exited, X};
       error:X -> {N, caught, error, X}
   end.



demo2() ->
    [{I, (catch generate_exception(I))} || I <- [1,2,3,4,5]].



demo3() ->
    try generate_exception(5) 
    catch
       error:X ->
	    {X, erlang:get_stacktrace()}
    end.

	

lookup(N) ->
          case(N) of
	     1 -> {'EXIT', a};
	     2 -> exit(a)
	  end.

