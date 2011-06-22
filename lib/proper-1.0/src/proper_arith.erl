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
%%% @doc This module contains helper arithmetic, list handling and random
%%%	 functions.
%%% @private

-module(proper_arith).

-export([list_remove/2, list_update/3, list_insert/3, safe_map/2, safe_foldl/3,
	 safe_any/2, safe_zip/2, tuple_map/2, cut_improper_tail/1,
	 head_length/1, find_first/2, filter/2, partition/2, remove/2, insert/3,
	 unflatten/2]).
-export([rand_start/0, rand_reseed/0, rand_stop/0,
	 rand_int/1, rand_int/2, rand_non_neg_int/1,
	 rand_float/1, rand_float/2, rand_non_neg_float/1,
	 distribute/2, jumble/1, rand_choose/1, freq_choose/1]).

-include("proper_internal.hrl").


%%-----------------------------------------------------------------------------
%% List handling functions
%%-----------------------------------------------------------------------------

-spec list_remove(position(), [T]) -> [T].
list_remove(Index, List) ->
    {H,[_Elem | T]} = lists:split(Index - 1, List),
    H ++ T.

-spec list_update(position(), T, [T]) -> [T,...].
list_update(Index, NewElem, List) ->
    {H,[_OldElem | T]} = lists:split(Index - 1, List),
    H ++ [NewElem] ++ T.

-spec list_insert(position(), T, [T]) -> [T,...].
list_insert(Index, Elem, List) ->
    {H,T} = lists:split(Index - 1, List),
    H ++ [Elem] ++ T.

%% TODO: safe_map and cut_improper_tail can be combined into one generic list-
%%	 recursing function, with 3 function arguments: apply_to_proper_elems,
%%	 apply_to_improper_tail, combine
-spec safe_map(fun((T) -> S), maybe_improper_list(T,T | [])) ->
	  maybe_improper_list(S,S | []).
safe_map(Fun, List) ->
    safe_map_tr(Fun, List, []).

-spec safe_map_tr(fun((T) -> S), maybe_improper_list(T,T | []) | T, [S]) ->
	  maybe_improper_list(S,S | []).
safe_map_tr(_Fun, [], AccList) ->
    lists:reverse(AccList);
safe_map_tr(Fun, [Head | Tail], AccList) ->
    safe_map_tr(Fun, Tail, [Fun(Head) | AccList]);
safe_map_tr(Fun, ImproperTail, AccList) ->
    lists:reverse(AccList) ++ Fun(ImproperTail).

-spec safe_foldl(fun((T,A) -> A), A, maybe_improper_list(T,T | [])) -> A.
safe_foldl(_Fun, Acc, []) ->
    Acc;
safe_foldl(Fun, Acc, [X | Rest]) ->
    safe_foldl(Fun, Fun(X,Acc), Rest);
safe_foldl(Fun, Acc, ImproperTail) ->
    Fun(ImproperTail, Acc).

-spec safe_any(fun((T) -> boolean()), maybe_improper_list(T,T | [])) ->
	  boolean().
safe_any(_Pred, []) ->
    false;
safe_any(Pred, [X | Rest]) ->
    Pred(X) orelse safe_any(Pred, Rest);
safe_any(Pred, ImproperTail) ->
    Pred(ImproperTail).

-spec safe_zip([T], [S]) -> [{T,S}].
safe_zip(Xs, Ys) ->
    safe_zip_tr(Xs, Ys, []).

-spec safe_zip_tr([T], [S], [{T,S}]) -> [{T,S}].
safe_zip_tr([], _Ys, Acc) ->
    lists:reverse(Acc);
safe_zip_tr(_Xs, [], Acc) ->
    lists:reverse(Acc);
safe_zip_tr([X|Xtail], [Y|YTail], Acc) ->
    safe_zip_tr(Xtail, YTail, [{X,Y}|Acc]).

-spec tuple_map(fun((T) -> S), loose_tuple(T)) -> loose_tuple(S).
tuple_map(Fun, Tuple) ->
    list_to_tuple(lists:map(Fun, tuple_to_list(Tuple))).

-spec cut_improper_tail(maybe_improper_list(T,T | [])) -> [T] | {[T],T}.
cut_improper_tail(List) ->
    cut_improper_tail_tr(List, []).

-spec cut_improper_tail_tr(maybe_improper_list(T,T | []) | T, [T]) ->
	  [T] | {[T],T}.
cut_improper_tail_tr([], AccList) ->
    lists:reverse(AccList);
cut_improper_tail_tr([Head | Tail], AccList) ->
    cut_improper_tail_tr(Tail, [Head | AccList]);
cut_improper_tail_tr(ImproperTail, AccList) ->
    {lists:reverse(AccList), ImproperTail}.

-spec head_length(nonempty_improper_list(term(),term())) -> pos_integer().
head_length(List) ->
    head_length_tr(List, 0).

-spec head_length_tr(nonempty_improper_list(term(),term()) | term(),
		     non_neg_integer()) -> pos_integer().
head_length_tr([_Head | Tail], Len) ->
    head_length_tr(Tail, Len + 1);
head_length_tr(_ImproperTail, Len) ->
    Len.

-spec find_first(fun((T) -> boolean()), [T]) -> {position(),T} | 'none'.
find_first(Pred, List) ->
    find_first_tr(Pred, List, 1).

-spec find_first_tr(fun((T) -> boolean()), [T], position()) ->
	  {position(),T} | 'none'.
find_first_tr(_Pred, [], _Pos) ->
    none;
find_first_tr(Pred, [X | Rest], Pos) ->
    case Pred(X) of
	true  -> {Pos, X};
	false -> find_first_tr(Pred, Rest, Pos + 1)
    end.

-spec filter(fun((T) -> boolean()), [T]) -> {[T],[position()]}.
filter(Pred, List) ->
    {Trues,TrueLookup,_Falses,_FalseLookup} = partition(Pred, List),
    {Trues, TrueLookup}.

-spec partition(fun((T) -> boolean()), [T]) ->
	  {[T],[position()],[T],[position()]}.
partition(Pred, List) ->
    partition_tr(Pred, List, 1, [], [], [], []).

-spec partition_tr(fun((T) -> boolean()), [T], position(), [T], [position()],
		   [T], [position()]) -> {[T],[position()],[T],[position()]}.
partition_tr(_Pred, [], _Pos, Trues, TrueLookup, Falses, FalseLookup) ->
    {lists:reverse(Trues), lists:reverse(TrueLookup), lists:reverse(Falses),
     lists:reverse(FalseLookup)};
partition_tr(Pred, [X | Rest], Pos, Trues, TrueLookup, Falses, FalseLookup) ->
    case Pred(X) of
	true ->
	    partition_tr(Pred, Rest, Pos + 1, [X | Trues], [Pos | TrueLookup],
			 Falses, FalseLookup);
	false ->
	    partition_tr(Pred, Rest, Pos + 1, Trues, TrueLookup, [X | Falses],
			 [Pos | FalseLookup])
    end.

-spec remove([T], [position()]) -> [T].
remove(Xs, Positions) ->
    remove_tr(Xs, Positions, 1, []).

-spec remove_tr([T], [position()], position(), [T]) -> [T].
remove_tr(Xs, [], _Pos, Acc) ->
    lists:reverse(Acc) ++ Xs;
remove_tr([_X | XsTail], [Pos | PosTail], Pos, Acc) ->
    remove_tr(XsTail, PosTail, Pos + 1, Acc);
remove_tr([X | XsTail], Positions, Pos, Acc) ->
    remove_tr(XsTail, Positions, Pos + 1, [X | Acc]).

-spec insert([T], [position()], [T]) -> [T].
insert(Xs, Positions, Ys) ->
    insert_tr(Xs, Positions, Ys, 1, []).

-spec insert_tr([T], [position()], [T], position(), [T]) -> [T].
insert_tr([], [], Ys, _Pos, Acc) ->
    lists:reverse(Acc) ++ Ys;
insert_tr([X | XsTail], [Pos | PosTail], Ys, Pos, Acc) ->
    insert_tr(XsTail, PosTail, Ys, Pos + 1, [X | Acc]);
insert_tr(Xs, Positions, [Y | YsTail], Pos, Acc) ->
    insert_tr(Xs, Positions, YsTail, Pos + 1, [Y | Acc]).

-spec unflatten([T], [length()]) -> [[T]].
unflatten(List, Lens) ->
    {[],RevSubLists} = lists:foldl(fun remove_n/2, {List,[]}, Lens),
    lists:reverse(RevSubLists).

-spec remove_n(non_neg_integer(), {[T],[[T]]}) -> {[T],[[T]]}.
remove_n(N, {List,Acc}) ->
    {Front,Back} = lists:split(N, List),
    {Back, [Front | Acc]}.


%%-----------------------------------------------------------------------------
%% Random functions
%%-----------------------------------------------------------------------------

%% @doc Seeds the random number generator. This function should be run before
%% calling any random function from this module.
-spec rand_start() -> 'ok'.
rand_start() ->
    _ = random:seed(now()),
    %% TODO: read option for RNG bijections here
    ok.

-spec rand_reseed() -> 'ok'.
rand_reseed() ->
    %% TODO: This should use the pid of the process somehow, in case two
    %%       spawned functions call it simultaneously?
    _ = random:seed(now()),
    ok.

-spec rand_stop() -> 'ok'.
rand_stop() ->
    erase(random_seed),
    ok.

-spec rand_int(non_neg_integer()) -> integer().
rand_int(Const) ->
    round(rand_float(Const)).

-spec rand_non_neg_int(non_neg_integer()) -> non_neg_integer().
rand_non_neg_int(Const) ->
    trunc(rand_non_neg_float(Const)).

-spec rand_int(integer(), integer()) -> integer().
rand_int(Low, High) when is_integer(Low), is_integer(High), Low =< High ->
    Low + random:uniform(High - Low + 1) - 1.

-spec rand_float(non_neg_integer()) -> float().
rand_float(Const) ->
    X = rand_non_neg_float(Const),
    case random:uniform(2) of
	1 -> X;
	2 -> -X
    end.

-spec rand_non_neg_float(non_neg_integer()) -> float().
rand_non_neg_float(Const) when is_integer(Const), Const >= 0 ->
    case random:uniform() of
	1.0 -> rand_non_neg_float(Const);
	X   -> Const * zero_one_to_zero_inf(X)
    end.

-spec rand_float(float(), float()) -> float().
rand_float(Low, High) when is_float(Low), is_float(High), Low =< High ->
    Low + random:uniform() * (High - Low).

-spec zero_one_to_zero_inf(float()) -> float().
%% This function must return only non-negative values and map 0.0 to 0.0, but
%% may be undefined at 1.0.
%% TODO: read global options and decide here which bijection to use
zero_one_to_zero_inf(X) ->
    X / math:sqrt(1 - X*X).

-spec distribute(non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].
distribute(_Credits, 0) ->
    [];
distribute(Credits, People) ->
    jumble(distribute_tr(Credits, People, [])).

-spec distribute_tr(non_neg_integer(), pos_integer(), [non_neg_integer()]) ->
	  [non_neg_integer()].
distribute_tr(0, PeopleLeft, AccList) ->
    lists:duplicate(PeopleLeft, 0) ++ AccList;
distribute_tr(CreditsLeft, 1, AccList) ->
    [CreditsLeft | AccList];
distribute_tr(CreditsLeft, PeopleLeft, AccList) ->
    YourCut = rand_int(0, CreditsLeft),
    distribute_tr(CreditsLeft - YourCut, PeopleLeft - 1, [YourCut | AccList]).

-spec jumble([T]) -> [T].
%% @doc Produces a random permutation of a list.
jumble(List) ->
    jumble_tr(List, length(List), []).

-spec jumble_tr([T], non_neg_integer(), [T]) -> [T].
jumble_tr([], 0, Acc) ->
    Acc;
jumble_tr(List, Len, Acc) ->
    Pos = rand_int(0, Len - 1),
    {List1, [H|List2]} = lists:split(Pos, List),
    jumble_tr(List1 ++ List2, Len - 1, [H|Acc]).

-spec rand_choose([T,...]) -> {position(),T}.
rand_choose(Choices) when Choices =/= [] ->
    Pos = rand_int(1, length(Choices)),
    {Pos, lists:nth(Pos, Choices)}.

-spec freq_choose([{frequency(),T},...]) -> {position(),T}.
freq_choose(Choices) when Choices =/= []  ->
    AddFreq = fun({Freq,_},Acc) -> Freq + Acc end,
    SumFreq = lists:foldl(AddFreq, 0, Choices),
    freq_select(rand_int(1, SumFreq), Choices, 1).

-spec freq_select(frequency(), [{frequency(),T}], position()) -> {position(),T}.
freq_select(N, [{Freq,Choice} | Rest], Pos) ->
    case N =< Freq of
	true ->
	    {Pos,Choice};
	false ->
	    freq_select(N - Freq, Rest, Pos + 1)
    end.
