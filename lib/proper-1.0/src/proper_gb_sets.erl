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
%%% @doc Parametric wrapper to gb_sets module.
%%% @private

-module(proper_gb_sets).

-export([empty/0, is_empty/1, size/1, singleton/1, is_member/2,
	 insert/2, add/2, delete/2, delete_any/2, balance/1, union/2,
	 union/1, intersection/2, intersection/1, is_disjoint/2, difference/2,
	 is_subset/2, to_list/1, from_list/1, from_ordset/1, smallest/1,
	 largest/1, take_smallest/1, take_largest/1, iterator/1, next/1,
	 filter/2, fold/3, is_set/1]).
-export([new/0, is_element/2, add_element/2, del_element/2,
	 subtract/2]).

-export_type([gb_set/1, iterator/1]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type gb_set(_T) :: gb_set().
%% Based on the documentation alone, this is the best we can do.
-type iterator(_T) :: term().

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec empty() -> gb_set(_T).
empty() ->
    gb_sets:empty().

-spec new() -> gb_set(_T).
new() ->
    gb_sets:new().

-spec is_empty(gb_set(_T)) -> boolean().
is_empty(Set) ->
    gb_sets:is_empty(Set).

-spec size(gb_set(_T)) -> non_neg_integer().
size(Set) ->
    gb_sets:size(Set).

-spec singleton(T) -> gb_set(T).
singleton(X) ->
    gb_sets:singleton(X).

-spec is_element(T, gb_set(T)) -> boolean().
is_element(X, Set) ->
    gb_sets:is_element(X, Set).

-spec is_member(T, gb_set(T)) -> boolean().
is_member(X, Set) ->
    gb_sets:is_member(X, Set).

-spec insert(T, gb_set(T)) -> gb_set(T).
insert(X, Set) ->
    gb_sets:insert(X, Set).

-spec balance(gb_set(T)) -> gb_set(T).
balance(Set) ->
    gb_sets:balance(Set).

-spec add_element(T, gb_set(T)) -> gb_set(T).
add_element(X, Set) ->
    gb_sets:add_element(X, Set).

-spec add(T, gb_set(T)) -> gb_set(T).
add(X, Set) ->
    gb_sets:add(X, Set).

-spec from_list([T]) -> gb_set(T).
from_list(List) ->
    gb_sets:from_list(List).

-spec from_ordset(proper_ordsets:ordset(T)) -> gb_set(T).
from_ordset(Set) ->
    gb_sets:from_ordset(Set).

-spec del_element(T, gb_set(T)) -> gb_set(T).
del_element(X, Set) ->
    gb_sets:del_element(X, Set).

-spec delete_any(T, gb_set(T)) -> gb_set(T).
delete_any(X, Set) ->
    gb_sets:delete_any(X, Set).

-spec delete(T, gb_set(T)) -> gb_set(T).
delete(X, Set) ->
    gb_sets:delete(X, Set).

-spec take_smallest(gb_set(T)) -> {T, gb_set(T)}.
take_smallest(Set) ->
    gb_sets:take_smallest(Set).

-spec smallest(gb_set(T)) -> T.
smallest(Set) ->
    gb_sets:smallest(Set).

-spec take_largest(gb_set(T)) -> {T, gb_set(T)}.
take_largest(Set) ->
    gb_sets:take_largest(Set).

-spec largest(gb_set(T)) -> T.
largest(Set) ->
    gb_sets:largest(Set).

-spec to_list(gb_set(T)) -> [T].
to_list(Set) ->
    gb_sets:to_list(Set).

-spec iterator(gb_set(T)) -> iterator(T).
iterator(Set) ->
    gb_sets:iterator(Set).

-spec next(iterator(T)) -> {T, iterator(T)} | 'none'.
next(Iter) ->
    gb_sets:next(Iter).

-spec union(gb_set(T), gb_set(T)) -> gb_set(T).
union(Set1, Set2) ->
    gb_sets:union(Set1, Set2).

-spec union([gb_set(T)]) -> gb_set(T).
union(Sets) ->
    gb_sets:union(Sets).

-spec intersection(gb_set(T), gb_set(T)) -> gb_set(T).
intersection(Set1, Set2) ->
    gb_sets:intersection(Set1, Set2).

-spec intersection([gb_set(T),...]) -> gb_set(T).
intersection(Sets) ->
    gb_sets:intersection(Sets).

-spec is_disjoint(gb_set(T), gb_set(T)) -> boolean().
is_disjoint(Set1, Set2) ->
    gb_sets:is_disjoint(Set1, Set2).

-spec subtract(gb_set(T), gb_set(T)) -> gb_set(T).
subtract(Set1, Set2) ->
    gb_sets:subtract(Set1, Set2).

-spec difference(gb_set(T), gb_set(T)) -> gb_set(T).
difference(Set1, Set2) ->
    gb_sets:difference(Set1, Set2).

-spec is_subset(gb_set(T), gb_set(T)) -> boolean().
is_subset(Set1, Set2) ->
    gb_sets:is_subset(Set1, Set2).

-spec is_set(term()) -> boolean().
is_set(X) ->
    gb_sets:is_set(X).

-spec filter(fun((T) -> boolean()), gb_set(T)) -> gb_set(T).
filter(Pred, Set) ->
    gb_sets:filter(Pred, Set).

-spec fold(fun((T,A) -> A), A, gb_set(T)) -> A.
fold(Fun, Acc0, Set) ->
    gb_sets:fold(Fun, Acc0, Set).
