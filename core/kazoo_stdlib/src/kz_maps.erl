%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc map utilities
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_maps).

%%==============================================================================
%% API functions
%%==============================================================================

-export([merge/2, merge/3]).
-export([get/2, get/3]).
-export([keys_to_atoms/1, keys_to_atoms/2]).
-export([exec/2]).

%% if you want specific merge functionality, first arg to merge/3
-export([merge_left/2, merge_right/2]).

-spec merge(map(), map()) -> map().
merge(MapLeft, MapRight) ->
    merge(fun merge_right/2, MapLeft, MapRight).

-type merge_arg_2() :: {'left' | 'right', term()} | {'both', term(), term()}.
-type merge_fun_result() :: 'undefined' | {'ok', term()}.
-type merge_fun() :: fun((term(), merge_arg_2()) -> merge_fun_result()).
-spec merge(merge_fun(), map(), map()) -> map().
merge(F, MapLeft, MapRight)
  when is_map(MapLeft)
       andalso is_map(MapRight)
       andalso is_function(F, 2)
       ->
    ListLeft = lists:sort(maps:to_list(MapLeft)),
    ListRight = lists:sort(maps:to_list(MapRight)),
    merge(F, ListLeft, ListRight, []).

merge(_F, [], [], Acc) ->
    maps:from_list(Acc);
merge(F, [{KX, VX}|Xs], [], Acc) ->
    merge(F, Xs, [], f(KX, F(KX, {'left', VX}), Acc));
merge(F, [], [{KY, VY}|Ys], Acc) ->
    merge(F, Ys, [], f(KY, F(KY, {'right', VY}), Acc));
merge(F, [{KX, VX}|Xs]=Left, [{KY, VY}|Ys]=Right, Acc) ->
    if
        KX < KY -> merge(F, Xs, Right, f(KX, F(KX, {'left', VX}), Acc));
        KX > KY -> merge(F, Left, Ys, f(KY, F(KY, {'right', VY}), Acc));
        KX =:= KY -> merge(F, Xs, Ys, f(KX, F(KX, {'both', VX, VY}), Acc))
    end.

-spec f(term(), merge_fun_result(), list()) -> list().
f(_K, 'undefined', Acc) -> Acc;
f(K, {'ok', R}, Acc) -> [{K, R} | Acc].

-spec merge_left(term(), merge_arg_2()) -> merge_fun_result().
merge_left(_K, {'left', V}) -> {'ok', V};
merge_left(_K, {'right', V}) -> {'ok', V};
merge_left(_K, {'both', MapLeft, MapRight})
  when is_map(MapLeft)
       andalso is_map(MapRight)
       ->
    {'ok', merge(fun merge_left/2, MapLeft, MapRight)};
merge_left(_K, {'both', Left, _Right}) -> {'ok', Left}.

-spec merge_right(term(), merge_arg_2()) -> merge_fun_result().
merge_right(_K, {'left', V}) -> {'ok', V};
merge_right(_K, {'right', V}) -> {'ok', V};
merge_right(_K, {'both', MapLeft, MapRight})
  when is_map(MapLeft)
       andalso is_map(MapRight)
       ->
    {'ok', merge(fun merge_right/2, MapLeft, MapRight)};
merge_right(_K, {'both', _Left, Right}) -> {'ok', Right}.

-spec get(term(), map()) -> term().
get(Keys, Map) ->
    get(Keys, Map, undefined).

-spec get(term(), map(), term()) -> term().
get([Key|Rest], Map, Default) ->
    case maps:get(Key, Map, {?MODULE, Default}) of
        {?MODULE, Default} ->
            Default;
        NestedMap ->
            get(Rest, NestedMap, Default)
    end;
get(NotAList, Value, Default)
  when not is_list(NotAList) ->
    get([NotAList], Value, Default);
get([], Value, _) -> Value.


-spec keys_to_atoms(map()) -> map().
keys_to_atoms(Map) ->
    keys_to_atoms(Map, 'true').

-spec keys_to_atoms(map(), boolean()) -> map().
keys_to_atoms(Map, 'true') ->
    maps:fold(fun keys_to_atoms_fold/3, #{}, Map);
keys_to_atoms(Map, 'false') ->
    maps:fold(fun(K, V, Acc) ->
                      Acc#{kz_term:to_atom(K, 'true') => V}
              end, #{}, Map).

-spec keys_to_atoms_fold(term(), any(), map()) -> map().
keys_to_atoms_fold(K, V, Acc) when is_map(V) ->
    Acc#{kz_term:to_atom(K, 'true') => keys_to_atoms(V)};
keys_to_atoms_fold(K, V, Acc) ->
    Acc#{kz_term:to_atom(K, 'true') => V}.

-spec exec(list(), map()) -> map().
exec(Routines, Map) ->
    lists:foldl(fun exec_fold/2, Map, Routines).

exec_fold(Fun, Map)
  when is_function(Fun, 1) ->
    Fun(Map);
exec_fold({Fun, Arg}, Map)
  when is_function(Fun, 2) ->
    Fun(Arg, Map);
exec_fold({Fun, Arg1, Arg2}, Map)
  when is_function(Fun, 3) ->
    Fun(Arg1, Arg2, Map).

%%==============================================================================
%% Internal functions
%%==============================================================================
