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
%%% @doc Parametric wrapper to dict module.
%%% @private

-module(proper_dict).

-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

-export_type([dict/2]).

%% This would normally contain the internal representation of the ADT.
%% This representation won't actually be used, so we could just use a dummy one.
%% As with specs, unbound type variables are not allowed in '-type' declarations
%% unless they begin with an underscore.

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type dict(_K,_V) :: dict().

%% Here are some valid symbolic calls that could be automatically produced using
%% this module's exported functions, for the type dict(atom(),integer()):
%% * {'$call',proper_dict,store,[aa,12,{'$call',proper_dict,new,[]}]}
%% * {'$call',proper_dict,filter,[Fun,{'$call',proper_dict,from_list,
%%                                     [[{a,1},{b,2}]]}]}
%% * {'$call',proper_dict,merge,[Fun,
%%                               {'$call',proper_dict,from_list,[[]]},
%%                               {'$call',proper_dict,update,
%%                                [aa,Fun,3,{'$call',proper_dict,new,[]}]}]}
%% Notice that PropEr will never produce a call like this one:
%%   {'$call',proper_dict,update,[aa,Fun,{'$call',proper_dict,new,[]}]}
%% which would raise an exception if we tried to evaluate it.

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec new() -> dict(_K,_V).
new() ->
    dict:new().

-spec is_key(K, dict(K,_V)) -> boolean().
is_key(Key, Dict) ->
    dict:is_key(Key, Dict).

-spec to_list(dict(K,V)) -> [{K,V}].
to_list(Dict) ->
    dict:to_list(Dict).

-spec from_list([{K,V}]) -> dict(K,V).
from_list(List) ->
    dict:from_list(List).

-spec size(dict(_K,_V)) -> non_neg_integer().
size(Dict) ->
    dict:size(Dict).

-spec fetch(K, dict(K,V)) -> V.
fetch(Key, Dict) ->
    dict:fetch(Key, Dict).

-spec find(K, dict(K,V)) -> {'ok', V} | 'error'.
find(Key, Dict) ->
    dict:find(Key, Dict).

-spec fetch_keys(dict(K,_V)) -> [K].
fetch_keys(Dict) ->
    dict:fetch_keys(Dict).

-spec erase(K, dict(K,V)) -> dict(K,V).
erase(Key, Dict) ->
    dict:erase(Key, Dict).

-spec store(K, V, dict(K,V)) -> dict(K,V).
store(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

%% NOTE: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
%% TODO: This is too restricting: the other values in the dictionary can be
%%	 arbitrary, we only care that the one being appended to is a list.
-spec append(K, V, dict(K,[V])) -> dict(K,[V]).
append(Key, Value, Dict) ->
    dict:append(Key, Value, Dict).

%% NOTE: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
%% TODO: This is too restricting: the other values in the dictionary can be
%%	 arbitrary, we only care that the one being appended to is a list.
-spec append_list(K, [V], dict(K,[V])) -> dict(K,[V]).
append_list(Key, Values, Dict) ->
    dict:append_list(Key, Values, Dict).

-spec update(K, fun((V) -> V), dict(K,V)) -> dict(K,V).
update(Key, Fun, Dict) ->
    dict:update(Key, Fun, Dict).

-spec update(K, fun((V) -> V), V, dict(K,V)) -> dict(K,V).
update(Key, Fun, InitVal, Dict) ->
    dict:update(Key, Fun, InitVal, Dict).

%% NOTE: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
%% TODO: This is too restricting: the other values in the dictionary can be
%%	 arbitrary, we only care that the one being updated is a number.
-spec update_counter(K, number(), dict(K,number())) -> dict(K,number()).
update_counter(Key, Number, Dict) ->
    dict:update_counter(Key, Number, Dict).

-spec fold(fun((K,V,A) -> A), A, dict(K,V)) -> A.
fold(Fun, Acc0, Dict) ->
    dict:fold(Fun, Acc0, Dict).

-spec map(fun((K,V1) -> V2), dict(K,V1)) -> dict(K,V2).
map(Fun, Dict) ->
    dict:map(Fun, Dict).

-spec filter(fun((K,V) -> boolean()), dict(K,V)) -> dict(K,V).
filter(Fun, Dict) ->
    dict:filter(Fun, Dict).

-spec merge(fun((K,V,V) -> V), dict(K,V), dict(K,V)) -> dict(K,V).
merge(Fun, Dict1, Dict2) ->
    dict:merge(Fun, Dict1, Dict2).
