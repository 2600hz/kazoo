%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(convert4).

-export([convert/2]).

convert({yards, X},  meters) -> {meters, 0.9144 * X};
convert({meters, X}, yards)  -> {yards,  1.0936133 * X};
convert({Tag, X}, Tag)       -> {Tag, X}.


