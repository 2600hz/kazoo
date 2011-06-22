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
%%% @doc Parametric wrapper to sets module.
%%% @private

-module(proper_sets).

-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-export_type([set/1]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type set(_T) :: set().

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec new() -> set(_T).
new() ->
    sets:new().

-spec is_set(term()) -> boolean().
is_set(X) ->
    sets:is_set(X).

-spec size(set(_T)) -> non_neg_integer().
size(Set) ->
    sets:size(Set).

-spec to_list(set(T)) -> [T].
to_list(Set) ->
    sets:to_list(Set).

-spec from_list([T]) -> set(T).
from_list(L) ->
    sets:from_list(L).

-spec is_element(T, set(T)) -> boolean().
is_element(X, Set) ->
    sets:is_element(X, Set).

-spec add_element(T, set(T)) -> set(T).
add_element(X, Set) ->
    sets:add_element(X, Set).

-spec del_element(T, set(T)) -> set(T).
del_element(X, Set) ->
    sets:del_element(X, Set).

-spec union(set(T), set(T)) -> set(T).
union(Set1, Set2) ->
    sets:union(Set1, Set2).

-spec union([set(T)]) -> set(T).
union(Sets) ->
    sets:union(Sets).

-spec intersection(set(T), set(T)) -> set(T).
intersection(Set1, Set2) ->
    sets:intersection(Set1, Set2).

-spec intersection([set(T),...]) -> set(T).
intersection(Sets) ->
    sets:intersection(Sets).

-spec is_disjoint(set(T), set(T)) -> boolean().
is_disjoint(Set1, Set2) ->
    sets:is_disjoint(Set1, Set2).

-spec subtract(set(T), set(T)) -> set(T).
subtract(Set1, Set2) ->
    sets:subtract(Set1, Set2).

-spec is_subset(set(T), set(T)) -> boolean().
is_subset(S1, S2) ->
    sets:is_subset(S1, S2).

-spec fold(fun((T,A) -> A), A, set(T)) -> A.
fold(Fun, Acc, Set) ->
    sets:fold(Fun, Acc, Set).

-spec filter(fun((T) -> boolean()), set(T)) -> set(T).
filter(Pred, Set) ->
    sets:filter(Pred, Set).
