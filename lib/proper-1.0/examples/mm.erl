%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc PropEr usage example: Static mastermind solver

-module(mm).
-export([mastermind/3, mastermind/4]).
-export([prop_all_combinations_are_produced/0,
	 prop_all_selections_are_produced/0,
	 prop_remove_insert_symmetry/0,
	 prop_delete_insert_all_symmetry/0,
	 prop_compatible_works/0,
	 prop_io_filters_are_symmetric/0,
	 prop_next_comb_produces_all_combinations_in_order/0,
	 prop_all_compatibles_are_produced/0,
	 prop_all_produced_solutions_are_valid/1,
	 prop_secret_combination_is_not_discarded/1,
	 prop_invalidated_instances_reject_original_secret/1]).

-include_lib("proper/include/proper.hrl").


%% -----------------------------------------------------------------------------
%% Problem statement
%% -----------------------------------------------------------------------------

%% Given a list of guesses for the secret combination in a game of Mastermind
%% and their corresponding score of black and white pegs, find the first
%% combination that is compatible with all the guess-score pairs (the order of
%% combinations is derived lexicographically from the order of colors).

%% Colors are represented as hex digits, but we allow the use of letters up to
%% 'z' - thus, there may be up to 36 colors ('0' - '9' and 'a' - 'z'). The
%% combinations are represented as strings of such characters. That is also the
%% expected format for the answer. If there is no combination compatible with
%% all the guesses, the program should return the string "-1".

%% The module should export a function mastermind/3, that takes the following
%% arguments:
%% 1) the length of the combinations (> 1)
%% 2) the number of colors (1..36)
%% 3) the list of guess-score pairs, in the format:
%%    {guess, num_black_pegs, num_white_pegs}

%% Expected output:
%% mm:mastermind(4, 10, [{"3157",1,2},{"1350",2,1},{"6120",0,2},{"2381",3,0}]).
%% "2351"
%% mm:mastermind(4, 10, [{"3557",1,2},{"1350",2,1},{"6120",0,2},{"2381",3,0}]).
%% "-1"
%% mm:mastermind(4, 10, [{"3557",1,2},{"1350",0,1},{"2575",2,1},{"5574",3,0}]).
%% "5576"
%% mm:mastermind(5, 10, [{"12345",1,0},{"02789",1,2},{"82900",3,0}]).
%% "22902"
%% mm:mastermind(5, 10, [{"23543",0,2},{"45674",1,2},{"67242",2,0}]).
%% "67375"
%% mm:mastermind(5, 10, [{"74562",0,0},{"11300",1,0}]).
%% "18888"
%% mm:mastermind(4, 10, [{"1234",1,0},{"0004",1,0},{"0222",0,0},{"4444",1,0},
%%			 {"5554",1,0},{"6664",2,0},{"6784",2,2}]).
%% "6874"
%% mm:mastermind(6, 10, [{"353523",0,5},{"294333",3,2},{"254672",2,1}]).
%% "534332"
%% mm:mastermind(6, 10, [{"097654",1,3},{"000465",1,1},{"011579",0,2},
%%			 {"227496",1,3},{"347963",4,1}]).
%% "467963"
%% mm:mastermind(6, 10, [{"006892",0,2},{"115258",2,2},{"357368",2,1}]).
%% "112365"
%% mm:mastermind(7, 10, [{"2104767",1,3},{"3541285",3,1},{"7567128",1,4},
%%			 {"0117285",1,4},{"1521775",2,2},{"3261781",4,0}]).
%% "3570781"
%% mm:mastermind(8, 10, [{"11244556",0,2},{"66756572",1,4},{"00026667",1,3},
%%			 {"03663775",1,3},{"22677262",0,3},{"67568688",7,0}]).
%% "67568689"
%% mm:mastermind(8, 10, [{"21244767",3,0},{"35455685",3,1},{"75687658",2,4}]).
%% "05258667"
%% mm:mastermind(8, 10, [{"76897034",5,0},{"76284933",3,2}]).
%% "06097033"
%% mm:mastermind(9, 10, [{"345352352",0,5},{"287639433",3,2},{"276235467",5,2},
%%			 {"523459878",0,5}]).
%% "082235466"
%% mm:mastermind(10, 10, [{"3476453523",0,5},{"2876394333",3,2},
%%			  {"2762354672",5,2},{"5234598781",0,5}]).
%% "0122374372"


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

%% Function: all_combinations/2
%% Produces all 'Len'-length combinations made up of colors selected from
%% 'ColorsList'.
all_combinations(Len, ColorsList) ->
    all_combinations_tr(Len, ColorsList, [[]]).

all_combinations_tr(0, _ColorsList, Acc) ->
    Acc;
all_combinations_tr(Left, ColorsList, Acc) ->
    NewAcc = [[Color|Rest]  || Color <- ColorsList, Rest <- Acc],
    all_combinations_tr(Left - 1, ColorsList, NewAcc).

%% Function: all_selections/2
%% Returns all possible selections of 'N' elements from list 'List'.
all_selections(0, _List) ->
    [[]];
all_selections(N, List) when N >= 1 ->
    Len = length(List),
    case N > Len of
	true ->
	    erlang:error(badarg);
	false ->
	    all_selections(N, List, Len)
    end.

all_selections(1, List, _Len) ->
    [[X] || X <- List];
all_selections(_Len, List, _Len) ->
    [List];
all_selections(Take, [Head|Tail], Len) ->
    [[Head|Rest] || Rest <- all_selections(Take - 1, Tail, Len - 1)]
    ++ all_selections(Take, Tail, Len - 1).

%% Function: all_selection_pos/2
%% Returns all possible selections of 'N' positions from a 'Len'-length list.
all_selection_pos(N, Len) ->
    all_selections(N, lists:seq(1,Len)).

%% Function: remove/2
%% Removes from a list, 'List', the elements at positions 'Positions'. Returns
%% both the resulting list and a list of the removed elements, in the same
%% order they were removed.
%% Note that the positions must be given in order.
remove(Positions, List) ->
    remove_tr(Positions, List, 1, [], []).

remove_tr([], List, _CurrPos, Kept, Removed) ->
    {lists:reverse(Kept) ++ List, lists:reverse(Removed)};
remove_tr([CurrPos|PosTail], [X|ListTail], CurrPos, Kept, Removed) ->
    remove_tr(PosTail, ListTail, CurrPos + 1, Kept, [X|Removed]);
remove_tr(Positions, [X|ListTail], CurrPos, Kept, Removed) ->
    remove_tr(Positions, ListTail, CurrPos + 1, [X|Kept], Removed).

%% Function: insert/3
%% Inserts into a list, 'List', the elements of 'ToInsert', in the corresponding
%% positions, 'Positions'.
%% Note that the positions must be given in order.
insert(Positions, ToInsert, List) ->
    insert_tr(Positions, ToInsert, List, 1, []).

insert_tr([], [], List, _CurrPos, Acc) ->
    lists:reverse(Acc) ++ List;
insert_tr([CurrPos|PosTail], [X|ToInsertTail], List, CurrPos, Acc) ->
    insert_tr(PosTail, ToInsertTail, List, CurrPos + 1, [X|Acc]);
insert_tr(Positions, ToInsert, [X|ListTail], CurrPos, Acc) ->
    insert_tr(Positions, ToInsert, ListTail, CurrPos + 1, [X|Acc]).

%% Function: delete/2
%% Removes from a list, 'List', a subsequence of that list, 'ToDelete'.
delete(List, ToDelete) ->
    delete_tr(List, ToDelete, []).

delete_tr(List, [], Acc) ->
    lists:reverse(Acc) ++ List;
delete_tr([_Same|ListTail], [_Same|ToDeleteTail], Acc) ->
    delete_tr(ListTail, ToDeleteTail, Acc);
delete_tr([X|Rest], ToDelete, Acc) ->
    delete_tr(Rest, ToDelete, [X|Acc]).

%% Function: insert_all/2
%% Returns all possible insertions of the elements of the first list inside the
%% second list.
insert_all([], List) ->
    [List];
insert_all([X|Rest], List) ->
    [L2 || L1 <- insert_all(Rest, List), L2 <- all_insertions(X, L1)].

%% Function: all_insertions/2
%% Returns all possible insertions of 'X' inside 'List'.
all_insertions(X, List) ->
    all_insertions_tr(X, [], List, []).

all_insertions_tr(X, Front, [], Acc) ->
    [Front ++ [X] | Acc];
all_insertions_tr(X, Front, Back = [BackHead|BackTail], Acc) ->
    all_insertions_tr(X, Front ++ [BackHead], BackTail,
		      [Front ++ [X] ++ Back | Acc]).

%% Function true_permutation/2
%% Returns true iff two permutations of the same list have no element in the
%% same position.
true_permutation([], []) ->
    true;
true_permutation([_Same|_NewTail], [_Same|_OldTail]) ->
    false;
true_permutation([_NewHead|NewTail], [_OldHead|OldTail]) ->
    true_permutation(NewTail, OldTail).


%% -----------------------------------------------------------------------------
%% Solver code
%% -----------------------------------------------------------------------------

%% Function: compatible/4
%% Tests whether combination A produces the given score when compared against
%% combination B. This is always the same as when combination B is compared
%% against combination A.
compatible(A, B, {Blacks,Whites}, Colors) ->
    correct_blacks(A, B, Blacks)
    andalso correct_sum(A, B, Blacks + Whites, Colors).

correct_blacks([], [], 0) -> true;
correct_blacks([], [], _N) -> false;
correct_blacks([_Same|_At], [_Same|_Bt], 0) -> false;
correct_blacks([_Same|At], [_Same|Bt], N) -> correct_blacks(At, Bt, N - 1);
correct_blacks([_Ah|At], [_Bh|Bt], N) -> correct_blacks(At, Bt, N).

correct_sum(A, B, N, Colors) ->
    AFreqs = collect_freqs(A, Colors),
    BFreqs = collect_freqs(B, Colors),
    Common = lists:zipwith(fun erlang:min/2, AFreqs, BFreqs),
    lists:sum(Common) =:= N.

collect_freqs(Combination, Colors) ->
    lists:foldl(fun(C,F) -> inc_freq(C,F) end, lists:duplicate(Colors,0),
		Combination).

inc_freq(Color, Freqs) ->
    {H,[OldFreq | T]} = lists:split(Color, Freqs),
    H ++ [OldFreq + 1] ++ T.

%% Function: score/2
%% Compares two combinations A and B and calculates the corresponding score.
%% A and B must be of the same length and color number. The order of the
%% arguments is not important (i.e. it is always score(A,B) = score(B,A)).
%% This implementation is sub-optimal on purpose.
score(A, B) ->
    {Blacks,AA,BB} = remove_sames(A, B),
    Whites = get_whites(AA, BB),
    {Blacks, Whites}.

remove_sames(A, B) ->
    remove_sames_tr(A, B, 0, [], []).

remove_sames_tr([], [], N, AccA, AccB) ->
    {N, AccA, AccB};
remove_sames_tr([_Same|At], [_Same|Bt], N, AccA, AccB) ->
    remove_sames_tr(At, Bt, N + 1, AccA, AccB);
remove_sames_tr([Ah|At], [Bh|Bt], N, AccA, AccB) ->
    remove_sames_tr(At, Bt, N, [Ah|AccA], [Bh|AccB]).

get_whites(A, B) ->
    SA = lists:sort(A),
    SB = lists:sort(B),
    get_whites_tr(SA, SB, 0).

get_whites_tr([], _B, N) ->
    N;
get_whites_tr(_A, [], N) ->
    N;
get_whites_tr([_Same|At], [_Same|Bt], N) ->
    get_whites_tr(At, Bt, N + 1);
get_whites_tr([Ah|At], B = [Bh|_Bt], N) when Ah < Bh ->
    get_whites_tr(At, B, N);
get_whites_tr(A = [Ah|_At], [Bh|Bt], N) when Ah > Bh ->
    get_whites_tr(A, Bt, N).

%% Function: mastermind/3
%% Main entry function, serves as input/output filter for an actual solver
%% function, which must return a list of combinations that are compatible with
%% every guess-score pair provided. Such a list needn't be sorted - actually,
%% it needn't even be complete (i.e. containing all plausible secret
%% combinations), but it must contain the minimum combination compatible with
%% the input, if such a combination exists (being complete, however, helps with
%% testing).
mastermind(Len, Colors, RawGuesses) ->
    mastermind(Len, Colors, RawGuesses, heur).

%% Function: mastermind/4
%% The last argument is used to select a particular solver - valid solvers are
%% 'simple', 'stream' and 'heur', default is 'heur'.
mastermind(Len, Colors, RawGuesses, SolverName) ->
    Guesses = [{parse(RawComb),{B,W}} || {RawComb,B,W} <- RawGuesses],
    case valid_input(Len, Colors, Guesses) of
	true  -> ok;
	false -> erlang:error(badarg)
    end,
    Solver = get_solver(SolverName),
    Result = case Solver(Len, Colors, Guesses) of
		 [] -> error;
		 L  -> lists:min(L)
	     end,
    export(Result).

parse(RawComb) ->
    [digit_to_integer(X) || X <- RawComb].

export(error) ->
    "-1";
export(Comb) ->
    [integer_to_digit(X) || X <- Comb].

digit_to_integer(X) when X >= $0, X =< $9 -> X - $0;
digit_to_integer(X) when X >= $a, X =< $z -> X - $a + 10;
digit_to_integer(X) when X >= $A, X =< $Z -> X - $A + 10.

integer_to_digit(X) when X >= 0, X =< 9   -> X + $0;
integer_to_digit(X) when X >= 10, X =< 35 -> X - 10 + $a.

valid_input(Len, Colors, Guesses) ->
    Len > 0 andalso Colors > 0
    andalso lists:all(fun(G) -> valid_guess(Len, Colors, G) end, Guesses).

valid_guess(Len, Colors, {Comb,{Blacks,Whites}}) ->
    Blacks >= 0 andalso Whites >= 0
    andalso (Blacks + Whites < Len
	     orelse Blacks + Whites =:= Len andalso Whites =/= 1)
    andalso length(Comb) =:= Len
    andalso lists:all(fun(X) -> X >= 0 andalso X =< Colors end, Comb).

get_solver(SolverName) ->
    case SolverName of
	simple -> fun simple_solver/3;
	stream -> fun stream_solver/3;
	heur   -> fun heur_solver/3
    end.

%% Function: simple_solver/3
%% Simple way to produce all combinations which are compatible with a given
%% list of guess-score pairs:
%% * create a list of all possible 'Len'-length combinations of 'Colors' colors
%% * filter the list with all provided guess-score pairs (for each pair, we
%%   remove from the list those combinations that are incompatible with it)
%% Note that the resulting list is always complete and sorted.
simple_solver(Len, Colors, Guesses) ->
    Combs = all_combinations(Len, lists:seq(0,Colors-1)),
    filter_guesses(Colors, Guesses, Combs).

filter_guesses(_Colors, _Guesses, []) ->
    [];
filter_guesses(_Colors, [], Combs) ->
    Combs;
filter_guesses(Colors, [{Guess,Score} | Rest], Combs) ->
    IsCompatible = fun(C) -> compatible(Guess, C, Score, Colors) end,
    NewCombs = lists:filter(IsCompatible, Combs),
    filter_guesses(Colors, Rest, NewCombs).

%% Function: stream_solver/3
%% Low-memory solver: lazily produces and checks all possible combinations in
%% order until it finds one that is compatible with all guess-score pairs.
%% Note that the resulting list is almost certainly incomplete, since we only
%% return the first instance we find.
stream_solver(Len, Colors, Guesses) ->
    stream_solver_tr(Colors, Guesses, lists:duplicate(Len,0)).

stream_solver_tr(_Colors, _Guesses, done) ->
    [];
stream_solver_tr(Colors, Guesses, Comb) ->
    case lists:all(fun({C,S}) -> compatible(C,Comb,S,Colors) end, Guesses) of
	true  -> [Comb];
	false -> stream_solver_tr(Colors, Guesses, next_comb(Colors,Comb))
    end.

next_comb(Colors, Comb) ->
    next_comb_tr(Colors - 1, lists:reverse(Comb), []).

next_comb_tr(_MaxColor, [], _Acc) ->
    done;
next_comb_tr(MaxColor, [MaxColor | Rest], Acc) ->
    next_comb_tr(MaxColor, Rest, [0 | Acc]);
next_comb_tr(_MaxColor, [X | Rest], Acc) ->
    lists:reverse(Rest) ++ [X+1] ++ Acc.

%% Function: heur_solver/3
%% More sophisticated solver (avoids the construction of all possible
%% combinations):
%% * if the guess list is empty, return [[0,0,...,0]], else:
%% * sort the guesses by applying a selectivity heuristic (guesses whose
%%   score will result in more combinations being rejected are prefered)
%% * take the first guess-score pair and produce all the combinations it's
%%   compatible with
%% * filter the list with the rest of the pairs
%% Note that the resulting list is always complete (except for the special case
%% when Guesses =:= []) but is not necessarily sorted.
heur_solver(Len, _Colors, []) ->
    [lists:duplicate(Len, 0)];
heur_solver(Len, Colors, Guesses) ->
    [First|Rest] = lists:sort(fun(A,B) -> more_selective(A,B,Colors) end,
			      Guesses),
    Combs = all_compatibles(Len, Colors, First),
    filter_guesses(Colors, Rest, Combs).

%% Function: more_selective/2
%% Selectivity heuristic used to sort guess-score pairs. We suspect that
%% guess-score pair A is more selective than B if:
%% 1) it has a greater total score
%% 2) it has more black pegs
%% 3) it has fewer distinct colors
%% The above criteria are processed in that exact order.
more_selective({CombA,{BlacksA,WhitesA}}, {CombB,{BlacksB,WhitesB}}, Colors) ->
    case sign((BlacksA + WhitesA) - (BlacksB + WhitesB)) of
	+1 -> true;
	-1 -> false;
	0  -> case sign(BlacksA - BlacksB) of
		  +1 -> true;
		  -1 -> false;
		  0  -> distinct_colors(CombA, Colors)
			=< distinct_colors(CombB, Colors)
	      end
    end.

sign(0)            -> 0;
sign(X) when X > 0 -> +1;
sign(X) when X < 0 -> -1.

distinct_colors(Comb, Colors) ->
    lists:foldl(fun(F,S) -> sign(F) + S end, 0, collect_freqs(Comb, Colors)).

%% Function: all_compatibles/3
%% Runs the 'all_whites' function for all possible selections of 'Blacks'
%% positions in the given combination.
all_compatibles(Len, Colors, {Comb,{Blacks,Whites}}) ->
    NonFixedLen = Len - Blacks,
    [C || BlackSelPos <- all_selection_pos(Blacks, Len),
	  C <- all_whites(NonFixedLen, Whites, Colors, Comb, BlackSelPos)].

all_whites(NonFixedLen, Whites, Colors, Comb, BlackSelPos) ->
    RejectedLen = NonFixedLen - Whites,
    {NonFixed,Fixed} = remove(BlackSelPos, Comb),
    UnsortedWhiteSels =
	[{Sel,lists:sort(Sel)} || Sel <- all_selections(Whites, NonFixed)],
    WhiteSels = lists:ukeysort(2, UnsortedWhiteSels),
    [insert(BlackSelPos, Fixed, C)
	|| {WhiteSel,_} <- WhiteSels,
	   C <- all_moves(NonFixed, WhiteSel, RejectedLen, Colors)].

all_moves(NonFixed, WhiteSel, RejectedLen, Colors) ->
    Rejected = delete(NonFixed, WhiteSel),
    RemainingColors = lists:seq(0,Colors-1) -- Rejected,
    AllCombs = all_combinations(RejectedLen, RemainingColors),
    UnsortedAllMoves = [L || C <- AllCombs,
			     L <- insert_all(WhiteSel, C),
			     true_permutation(L, NonFixed)],
    lists:usort(UnsortedAllMoves).


%% -----------------------------------------------------------------------------
%% Properties to check
%% -----------------------------------------------------------------------------

prop_all_combinations_are_produced() ->
    ?FORALL({Len, ColorsList},
	    {range(0,5), short_nd_list(integer())},
	    begin
		AllCombs = all_combinations(Len, ColorsList),
		NumAllCombs = pow(length(ColorsList), Len),
		lofl_check(AllCombs, NumAllCombs, Len, ColorsList)
		andalso no_duplicates(AllCombs)
	    end).

short_nd_list(ElemType) ->
    ?LET(L,
	 resize(7, list(ElemType)),
	 lists:usort(L)).

lofl_check(Lofl, NumLists, ListLen, ListElems) ->
    lofl_check(Lofl, NumLists, ListLen, ListElems, 0).

lofl_check([], NumLists, _ListLen, _ListElems, Acc) ->
    Acc =:= NumLists;
lofl_check([List|Rest], NumLists, ListLen, ListElems, Acc) ->
    list_check(List, ListLen, ListElems)
    andalso lofl_check(Rest, NumLists, ListLen, ListElems, Acc + 1).

list_check([], 0, _Elems) ->
    true;
list_check([], _Left, _Elems) ->
    false;
list_check([X|Rest], Left, Elems) ->
    lists:member(X, Elems)
    andalso list_check(Rest, Left - 1, Elems).

pow(X, Y) ->
    pow_tr(X, Y, 1).

pow_tr(_X, 0, Acc) ->
    Acc;
pow_tr(X, Y, Acc) ->
    pow_tr(X, Y - 1, X * Acc).

no_duplicates(L) -> length(L) =:= length(lists:usort(L)).

prop_all_selections_are_produced() ->
    ?FORALL(List,
	    short_ne_list(integer()),
	    begin
		Len = length(List),
		?FORALL(N,
			range(0,Len),
			begin
			    AllSels = all_selections(N, List),
			    NumAllSels = num_sels(N, Len),
			    lofl_check(AllSels, NumAllSels, N, List)
			end)
	    end).

short_list(ElemType) ->
    resize(10, list(ElemType)).

short_ne_list(ElemType) ->
    non_empty(short_list(ElemType)).

num_sels(N, Len) ->
    fact(Len) div fact(N) div fact(Len - N).

fact(0) ->
    1;
fact(N) when N >= 1 ->
    N * fact(N-1).

prop_remove_insert_symmetry() ->
    ?FORALL(List,
	    short_ne_list(integer()),
	    ?FORALL(Positions,
		    pos_selection(List),
		    begin
			{Kept,Removed} = remove(Positions,List),
			insert(Positions,Removed,Kept) =:= List
		    end)).

pos_selection(List) ->
    Len = length(List),
    ?LET(N,
	 range(0,Len),
	 oneof(all_selection_pos(N, Len))).

prop_delete_insert_all_symmetry() ->
    ?FORALL(List,
	    short_list(integer()),
	    ?FORALL(Subseq,
		    subsequence(List),
		    lists:member(List,
				 insert_all(Subseq,delete(List,Subseq))))).

subsequence(List) ->
    ?LET(L,
	 [{X,boolean()} || X <- List],
	 [Y || {Y,true} <- L]).

prop_compatible_works() ->
    ?FORALL({Colors,A,B},
	    two_combinations(),
	    compatible(A, B, score(A,B), Colors)).

combination(Len, Colors) ->
    vector(Len, range(0,Colors-1)).

two_combinations() ->
    ?LET({Len, Colors},
	 {range(0,30), range(1,36)},
	 {Colors, combination(Len,Colors), combination(Len,Colors)}).

prop_io_filters_are_symmetric() ->
    ?FORALL(L,
	    list(digit()),
	    collect(num_digits(length(L)),
		    export(parse(L)) =:= L)).

digit() -> union([range($0,$9), range($a,$z)]).

num_digits(X) when X >= 0, X =< 9 -> 1;
num_digits(X) when X >= 10 -> 1 + num_digits(X div 10).

prop_next_comb_produces_all_combinations_in_order() ->
    ?FORALL({Len, Colors},
	    {range(0,5), range(1,10)},
	    list_is_produced(Colors, lists:duplicate(Len,0),
			     all_combinations(Len,lists:seq(0,Colors-1)))).

list_is_produced(_Colors, done, []) ->
    true;
list_is_produced(Colors, Same, [Same | Rest]) ->
    list_is_produced(Colors, next_comb(Colors,Same), Rest);
list_is_produced(_Colors, _Comb, _List) ->
    false.

prop_all_compatibles_are_produced() ->
    ?FORALL({Len, Colors, Guess},
	    one_guess_instance(),
	    simple_solver(Len, Colors, [Guess])
	    =:= lists:sort(all_compatibles(Len, Colors, Guess))).

one_guess_instance() ->
    ?LET({Len, Colors},
	 {range(2,5), range(2,10)},
	 {Len, Colors, scored_guess(Len,Colors)}).

scored_guess(Len, Colors) ->
    ?LET(Score,
	 valid_score(Len),
	 {combination(Len,Colors), Score}).

valid_score(Len) ->
    ?LET(Blacks,
	 range(0,Len),
	 ?LET(Whites,
	      ?SUCHTHAT(W,
			range(0,Len-Blacks),
			W =/= 1 orelse Blacks + W =/= Len),
	      {Blacks,Whites})).

prop_all_produced_solutions_are_valid(SolverName) ->
    Solver = get_solver(SolverName),
    ?FORALL({Len, Colors, Guesses},
	    instance(),
	    begin
		Solutions = Solver(Len, Colors, Guesses),
		collect(Solutions =:= [],
			lists:all(fun(Solution) ->
				      lists:all(fun({C,Score}) ->
						    compatible(C,Solution,
							       Score,Colors)
						end,
						Guesses)
				  end,
				  Solutions))
	    end).

instance() ->
    ?LET({Len, Colors},
	 {range(2,5), range(2,10)},
	 {Len, Colors, short_list(scored_guess(Len,Colors))}).

%% Note that the next property is not necessarily true for solvers that don't
%% return complete lists.
prop_secret_combination_is_not_discarded(SolverName) ->
    Solver = get_solver(SolverName),
    ?FORALL({Len,Colors,Secret,Guesses},
	    full_non_trivial_instance(),
	    lists:member(Secret, Solver(Len,Colors,Guesses))).

full_non_trivial_instance() ->
    ?LET({Len, Colors},
	 {range(2,5), range(2,10)},
	 ?LET({Secret, Guesses},
	      {combination(Len,Colors), short_ne_list(combination(Len,Colors))},
	      {Len,Colors,Secret,[{G,score(G,Secret)} || G <- Guesses]})).

prop_invalidated_instances_reject_original_secret(SolverName) ->
    Solver = get_solver(SolverName),
    ?FORALL({Len,Colors,Secret,Guesses},
	    invalid_instance(),
	    not lists:member(Secret, Solver(Len,Colors,Guesses))).

invalid_instance() ->
    ?LET({Len,Colors,Secret,Guesses},
	 full_non_trivial_instance(),
	 ?LET(Pos,
	      range(1,length(Guesses)),
	      begin
		  {Comb,OldScore} = lists:nth(Pos,Guesses),
		  ?LET(NewScore,
		       ?SUCHTHAT(S, valid_score(Len), S =/= OldScore),
		       {Len,Colors,Secret,
			list_update(Pos,{Comb,NewScore},Guesses)})
	      end)).

list_update(Index, NewElem, List) ->
    {H,[_OldElem | T]} = lists:split(Index - 1, List),
    H ++ [NewElem] ++ T.
