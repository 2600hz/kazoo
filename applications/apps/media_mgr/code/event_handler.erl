%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(event_handler).
-export([make/1, add_handler/2, event/2]).

%% make a new event handler called Name
%% the handler function is no_op -- so we do nothing with the event
make(Name) ->
    register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

add_handler(Name, Fun) -> Name ! {add, Fun}.

%% generate an event
event(Name, X) -> Name ! {event, X}.

my_handler(Fun) ->
    receive
	{add, Fun1} ->
	    my_handler(Fun1);
	{event, Any} ->
	    (catch Fun(Any)),
	    my_handler(Fun)
    end.

no_op(_) -> void.
    
