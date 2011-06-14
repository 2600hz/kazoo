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
%%% @doc Parametric wrapper to array module.
%%% @private

-module(proper_array).

-export([new/0, new/1, new/2, is_array/1, set/3, get/2, size/1,
	 sparse_size/1, default/1, reset/2, to_list/1, sparse_to_list/1,
	 from_list/1, from_list/2, to_orddict/1, sparse_to_orddict/1,
	 from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
	 foldr/3, sparse_foldl/3, sparse_foldr/3, fix/1, relax/1, is_fix/1,
	 resize/1, resize/2]).

-export_type([array/1]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type array(_T) :: array().

-type array_size() :: non_neg_integer().
-type array_indx() :: non_neg_integer().
-type indx_pairs(T) :: proper_orddict:orddict(array_indx(),T).

-type array_opt(T) :: 'fixed' | array_size()
                    | {'default', T} | {'fixed', boolean()}
                    | {'size', array_size()}.
-type array_opts(T) :: array_opt(T) | [array_opt(T)].

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec new() -> array(_T).
new() ->
    array:new().

-spec new(array_opts(T)) -> array(T).
new(Opts) ->
    array:new(Opts).

-spec new(array_size(), array_opts(T)) -> array(T).
new(Size, Opts) ->
    array:new(Size, Opts).

-spec is_array(term()) -> boolean().
is_array(X) ->
    array:is_array(X).

-spec size(array(_T)) -> array_size().
size(Array) ->
    array:size(Array).

-spec default(array(T)) -> T.
default(Array) ->
    array:default(Array).

-spec fix(array(T)) -> array(T).
fix(Array) ->
    array:fix(Array).

-spec is_fix(array(_T)) -> boolean().
is_fix(Array) ->
    array:is_fix(Array).

-spec relax(array(T)) -> array(T).
relax(Array) ->
    array:relax(Array).

-spec resize(array_size(), array(T)) -> array(T).
resize(Size, Array) ->
    array:resize(Size, Array).

-spec resize(array(T)) -> array(T).
resize(Array) ->
    array:resize(Array).

-spec set(array_indx(), T, array(T)) -> array(T).
set(Index, Value, Array) ->
    array:set(Index, Value, Array).

-spec get(array_indx(), array(T)) -> T.
get(Index, Array) ->
    array:get(Index, Array).

-spec reset(array_indx(), array(T)) -> array(T).
reset(Index, Array) ->
    array:reset(Index, Array).

-spec to_list(array(T)) -> [T].
to_list(Array) ->
    array:to_list(Array).

-spec sparse_to_list(array(T)) -> [T].
sparse_to_list(Array) ->
    array:sparse_to_list(Array).

-spec from_list([T]) -> array(T).
from_list(List) ->
    array:from_list(List).

-spec from_list([T], T) -> array(T).
from_list(List, Default) ->
    array:from_list(List, Default).

-spec to_orddict(array(T)) -> indx_pairs(T).
to_orddict(Array) ->
    array:to_orddict(Array).

-spec sparse_to_orddict(array(T)) -> indx_pairs(T).
sparse_to_orddict(Array) ->
    array:sparse_to_orddict(Array).

-spec from_orddict(indx_pairs(T)) -> array(T).
from_orddict(Dict) ->
    array:from_orddict(Dict).

-spec from_orddict(indx_pairs(T), T) -> array(T).
from_orddict(Dict, Default) ->
    array:from_orddict(Dict, Default).

-spec map(fun((array_indx(),T1) -> T2), array(T1)) -> array(T2).
map(Fun, Array) ->
    array:map(Fun, Array).

-spec sparse_map(fun((array_indx(),T1) -> T2), array(T1)) -> array(T2).
sparse_map(Fun, Array) ->
    array:sparse_map(Fun, Array).

-spec foldl(fun((array_indx(),T,A) -> A), A, array(T)) -> A.
foldl(Fun, Acc0, Array) ->
    array:foldl(Fun, Acc0, Array).

-spec sparse_foldl(fun((array_indx(),T,A) -> A), A, array(T)) -> A.
sparse_foldl(Fun, Acc0, Array) ->
    array:sparse_foldl(Fun, Acc0, Array).

-spec foldr(fun((array_indx(),T,A) -> A), A, array(T)) -> A.
foldr(Fun, Acc0, Array) ->
    array:foldr(Fun, Acc0, Array).

-spec sparse_foldr(fun((array_indx(),T,A) -> A), A, array(T)) -> A.
sparse_foldr(Fun, Acc0, Array) ->
    array:sparse_foldr(Fun, Acc0, Array).

-spec sparse_size(array(_T)) -> array_size().
sparse_size(Array) ->
    array:sparse_size(Array).
