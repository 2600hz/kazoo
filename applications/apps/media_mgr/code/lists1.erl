%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lists1).
-compile(export_all).

%% Warning this module should be compiled for 
%% to check the systenmax of the modules only
%% No beam code should be created

map(_, [])     -> [];
map(F,  [H|T]) -> [F(H)|map(F, T)].



member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, [])    -> false.

    

partition(Pred, L) -> partition(Pred, L, [], []).

partition(Pred, [H|T], Ts, Fs) ->
    case Pred(H) of
	true  -> partition(Pred, T, [H|Ts], Fs);
	false -> partition(Pred, T, Ts, [H|Fs])
    end;
partition(_, [], Ts, Fs) ->
    {reverse(Ts), reverse(Fs)}.
%END:partition


reverse(L) -> reverse(L, []).

reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L)    -> L.



search(Key, [{Key,Val}|_]) -> {ok, Val};        %% (1)
search(Key, [_|T])         -> search(Key, T);    %% (2)
search(_, [])              -> error.             %% (3)

    
