%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(area_server0).  
-export([loop/0]). 

loop() ->
    receive
	{rectangle, Width, Ht} -> 
	    io:format("Area of rectangle is ~p~n",[Width * Ht]),
	    loop();
	{circle, R} -> 
	    io:format("Area of circle is ~p~n", [3.14159 * R * R]),
	    loop();
	Other ->
	    io:format("I don't know what the area of a ~p is ~n",[Other]),
	    loop()
    end.




     

