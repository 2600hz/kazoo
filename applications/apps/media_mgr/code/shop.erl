%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(shop).
-export([cost/1]).


cost(oranges)   -> 5;
cost(newspaper) -> 8; 
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.
%END:function
