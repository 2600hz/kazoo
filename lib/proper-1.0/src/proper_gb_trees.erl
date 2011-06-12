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
%%% @doc Parametric wrapper to gb_trees module.
%%% @private

-module(proper_gb_trees).

-export([empty/0, is_empty/1, size/1, lookup/2, get/2, insert/3,
	 update/3, enter/3, delete/2, delete_any/2, balance/1,
	 is_defined/2, keys/1, values/1, to_list/1, from_orddict/1,
	 smallest/1, largest/1, take_smallest/1, take_largest/1,
	 iterator/1, next/1, map/2]).

-export_type([gb_tree/2, iterator/2]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type gb_tree(_K,_V) :: gb_tree().
%% Based on the documentation alone, this is the best we can do.
-type iterator(_K,_V) :: term().

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec empty() -> gb_tree(_K,_V).
empty() ->
    gb_trees:empty().

-spec is_empty(gb_tree(_K,_V)) -> boolean().
is_empty(Tree) ->
    gb_trees:is_empty(Tree).

-spec size(gb_tree(_K,_V)) -> non_neg_integer().
size(Tree) ->
    gb_trees:size(Tree).

-spec lookup(K, gb_tree(K,V)) -> 'none' | {'value', V}.
lookup(Key, Tree) ->
    gb_trees:lookup(Key, Tree).

-spec is_defined(K, gb_tree(K,_V)) -> boolean().
is_defined(Key, Tree) ->
    gb_trees:is_defined(Key, Tree).

-spec get(K, gb_tree(K,V)) -> V.
get(Key, Tree) ->
    gb_trees:get(Key, Tree).

-spec update(K, V, gb_tree(K,V)) -> gb_tree(K,V).
update(Key, Value, Tree) ->
    gb_trees:update(Key, Value, Tree).

-spec insert(K, V, gb_tree(K,V)) -> gb_tree(K,V).
insert(Key, Value, Tree) ->
    gb_trees:insert(Key, Value, Tree).

-spec enter(K, V, gb_tree(K,V)) -> gb_tree(K,V).
enter(Key, Value, Tree) ->
    gb_trees:enter(Key, Value, Tree).

-spec balance(gb_tree(K,V)) -> gb_tree(K,V).
balance(Tree) ->
    gb_trees:balance(Tree).

-spec from_orddict(proper_orddict:orddict(K,V)) -> gb_tree(K,V).
from_orddict(Dict) ->
    gb_trees:from_orddict(Dict).

-spec delete_any(K, gb_tree(K,V)) -> gb_tree(K,V).
delete_any(Key, Tree) ->
    gb_trees:delete_any(Key, Tree).

-spec delete(K, gb_tree(K,V)) -> gb_tree(K,V).
delete(Key, Tree) ->
    gb_trees:delete(Key, Tree).

-spec take_smallest(gb_tree(K,V)) -> {K, V, gb_tree(K,V)}.
take_smallest(Tree) ->
    gb_trees:take_smallest(Tree).

-spec smallest(gb_tree(K,V)) -> {K, V}.
smallest(Tree) ->
    gb_trees:smallest(Tree).

-spec take_largest(gb_tree(K,V)) -> {K, V, gb_tree(K,V)}.
take_largest(Tree) ->
    gb_trees:take_largest(Tree).

-spec largest(gb_tree(K,V)) -> {K, V}.
largest(Tree) ->
    gb_trees:largest(Tree).

-spec to_list(gb_tree(K,V)) -> [{K, V}].
to_list(Tree) ->
    gb_trees:to_list(Tree).

-spec keys(gb_tree(K,_V)) -> [K].
keys(Tree) ->
    gb_trees:keys(Tree).

-spec values(gb_tree(_K,V)) -> [V].
values(Tree) ->
    gb_trees:values(Tree).

-spec iterator(gb_tree(K,V)) -> iterator(K,V).
iterator(Tree) ->
    gb_trees:iterator(Tree).

-spec next(iterator(K,V)) -> 'none' | {K, V, iterator(K,V)}.
next(Iter) ->
    gb_trees:next(Iter).

-spec map(fun((K,V1) -> V2), gb_tree(K,V1)) -> gb_tree(K,V2).
map(Fun, Tree) ->
    gb_trees:map(Fun, Tree).
