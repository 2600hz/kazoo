%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(convert1). %% (1)
-export([yards_to_meters/1, meters_to_yards/1]). %% (2)

yards_to_meters(Yards) ->   %% (3)
    0.9144 * Yards.

meters_to_yards(Meters) ->
    1.0936133 * Meters.
