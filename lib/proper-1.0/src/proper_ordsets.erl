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
%%% @doc Parametric wrapper to ordsets module.
%%% @private

-module(proper_ordsets).

-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-export_type([ordset/1]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a
%% simple type because dialyzer can't handle parametric opaque types yet).
-type ordset(T) :: [T].

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%-----------------------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------------------

-spec new() -> ordset(_T).
new() ->
    ordsets:new().

-spec is_set(term()) -> boolean().
is_set(X) ->
    ordsets:is_set(X).

-spec size(ordset(_T)) -> non_neg_integer().
size(Set) ->
    ordsets:size(Set).

-spec to_list(ordset(T)) -> [T].
to_list(Set) ->
    ordsets:to_list(Set).

-spec from_list([T]) -> ordset(T).
from_list(List) ->
    ordsets:from_list(List).

-spec is_element(T, ordset(T)) -> boolean().
is_element(X, Set) ->
    ordsets:is_element(X, Set).

-spec add_element(T, ordset(T)) -> ordset(T).
add_element(X, Set) ->
    ordsets:add_element(X, Set).

-spec del_element(T, ordset(T)) -> ordset(T).
del_element(X, Set) ->
    ordsets:del_element(X, Set).

-spec union(ordset(T), ordset(T)) -> ordset(T).
union(Set1, Set2) ->
    ordsets:union(Set1, Set2).

-spec union([ordset(T)]) -> ordset(T).
union(Sets) ->
    ordsets:union(Sets).

-spec intersection(ordset(T), ordset(T)) -> ordset(T).
intersection(Set1, Set2) ->
    ordsets:intersection(Set1, Set2).

-spec intersection([ordset(T),...]) -> ordset(T).
intersection(Sets) ->
    ordsets:intersection(Sets).

-spec is_disjoint(ordset(T), ordset(T)) -> boolean().
is_disjoint(Set1, Set2) ->
    ordsets:is_disjoint(Set1, Set2).

-spec subtract(ordset(T), ordset(T)) -> ordset(T).
subtract(Set1, Set2) ->
    ordsets:subtract(Set1, Set2).

-spec is_subset(ordset(T), ordset(T)) -> boolean().
is_subset(Set1, Set2) ->
    ordsets:is_subset(Set1, Set2).

-spec fold(fun((T,S) -> S), S, ordset(T)) -> S.
fold(Fun, Acc0, Set) ->
    ordsets:fold(Fun, Acc0, Set).

-spec filter(fun((T) -> boolean()), ordset(T)) -> ordset(T).
filter(Pred, Set) ->
    ordsets:filter(Pred, Set).
