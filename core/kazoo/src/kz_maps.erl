%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% map utilities
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_maps).

%% ====================================================================
%% API functions
%% ====================================================================
-export([merge/2]).
-export([get/2, get/3]).
-export([keys_to_atoms/1, keys_to_atoms/2]).

-spec merge(map(), map()) -> map().
merge(Map1, Map1) -> Map1;
merge(Map1, Map2) ->
    M1 = maps:without(maps:keys(Map2), Map1),
    M2 = maps:without(maps:keys(Map1), Map2),
    M3 = maps:merge(M1, M2),
    M4 = maps:filter(fun(_, V) -> not is_map(V) end, maps:with(maps:keys(Map1), Map2)),
    M51 = maps:filter(fun(_, V) -> is_map(V) end, maps:with(maps:keys(Map2), Map1)),
    M52 = maps:filter(fun(_, V) -> is_map(V) end, maps:with(maps:keys(Map1), Map2)),
    M5 = maps:map(fun(K, V) -> merge(V, maps:get(K, M52)) end, M51),
    maps:merge(maps:merge(M3, M4), M5).


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

%% ====================================================================
%% Internal functions
%% ====================================================================


