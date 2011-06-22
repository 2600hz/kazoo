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
%%% @doc Parametric wrapper to orddict module.
%%% @private

-module(proper_orddict).

-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

-export_type([orddict/2]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type orddict(K,V) :: [{K,V}].

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec new() -> orddict(_K,_V).
new() ->
    orddict:new().

-spec is_key(K, orddict(K,_V)) -> boolean().
is_key(Key, Dict) ->
    orddict:is_key(Key, Dict).

-spec to_list(orddict(K,V)) -> [{K,V}].
to_list(Dict) ->
    orddict:to_list(Dict).

-spec from_list([{K,V}]) -> orddict(K,V).
from_list(List) ->
    orddict:from_list(List).

-spec size(orddict(_K,_V)) -> non_neg_integer().
size(Dict) ->
    orddict:size(Dict).

-spec fetch(K, orddict(K,V)) -> V.
fetch(Key, Dict) ->
    orddict:fetch(Key, Dict).

-spec find(K, orddict(K,V)) -> {'ok', V} | 'error'.
find(Key, Dict) ->
    orddict:find(Key, Dict).

-spec fetch_keys(orddict(K,_V)) -> [K].
fetch_keys(Dict) ->
    orddict:fetch_keys(Dict).

-spec erase(K, orddict(K,V)) -> orddict(K,V).
erase(Key, Dict) ->
    orddict:erase(Key, Dict).

-spec store(K, V, orddict(K,V)) -> orddict(K,V).
store(Key, Value, Dict) ->
    orddict:store(Key, Value, Dict).

%% TODO: This is too restricting.
-spec append(K, V, orddict(K,[V])) -> orddict(K,[V]).
append(Key, Value, Dict) ->
    orddict:append(Key, Value, Dict).

%% TODO: This is too restricting.
-spec append_list(K, [V], orddict(K,[V])) -> orddict(K,[V]).
append_list(Key, Values, Dict) ->
    orddict:append_list(Key, Values, Dict).

-spec update(K, fun((V) -> V), orddict(K,V)) -> orddict(K,V).
update(Key, Fun, Dict) ->
    orddict:update(Key, Fun, Dict).

-spec update(K, fun((V) -> V), V, orddict(K,V)) -> orddict(K,V).
update(Key, Fun, InitVal, Dict) ->
    orddict:update(Key, Fun, InitVal, Dict).

%% TODO: This is too restricting.
-spec update_counter(K, number(), orddict(K,number())) -> orddict(K,number()).
update_counter(Key, Incr, Dict) ->
    orddict:update_counter(Key, Incr, Dict).

-spec fold(fun((K,V,A) -> A), A, orddict(K,V)) -> A.
fold(Fun, Acc0, Dict) ->
    orddict:fold(Fun, Acc0, Dict).

-spec map(fun((K,V1) -> V2), orddict(K,V1)) -> orddict(K,V2).
map(Fun, Dict) ->
    orddict:map(Fun, Dict).

-spec filter(fun((K,V) -> boolean()), orddict(K,V)) -> orddict(K,V).
filter(Pred, Dict) ->
    orddict:filter(Pred, Dict).

-spec merge(fun((K,V,V) -> V), orddict(K,V), orddict(K,V)) -> orddict(K,V).
merge(Fun, Dict1, Dict2) ->
    orddict:merge(Fun, Dict1, Dict2).
