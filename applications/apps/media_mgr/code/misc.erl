%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(misc).
-compile(export_all).

perimeter({square, X})            -> 4 * X;
perimeter({rectangle, Width, Ht}) -> 2 * (Width+Ht);
perimeter({circle, R})            -> 2 * math:pi() * R;
perimeter({triangle, A, B, C})    -> A + B + C.



fac2(0) -> 1;
fac2(N) when is_integer(N), N > 0 -> N * fac2(N-1);
fac2(N) when is_integer(N), N < 0 -> exit({factorialNegativeArgument, N});
fac2(X) -> exit({factorialArgumentNotAnInteger, X}).



fac1(0) -> 1;
fac1(N) when N > 0 -> N * fac1(N-1).


    
    
