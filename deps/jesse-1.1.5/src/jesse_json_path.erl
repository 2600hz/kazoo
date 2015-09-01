%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc Implementation of Key Value Coding style "queries" for commonly
%% used Erlang data structures.
-module(jesse_json_path).
-export([path/2, path/3, value/3, to_proplist/1, unwrap_value/1]).

-ifdef(namespaced_dicts).
-type kvc_dict() :: dict:dict().
-type kvc_gb_tree() :: gb_trees:tree().
-else.
-type kvc_dict() :: dict:dict().
-type kvc_gb_tree() :: gb_trees:gb_tree().
-endif.

-type elem_key_type() :: atom | binary | string | undefined.
-type elem_type() :: list | elem_key_type().
-type kvc_obj() :: kvc_obj_node() | [kvc_obj_node()] | list().
-type kvc_key() :: binary() | atom() | string().
-type proplist() :: [{kvc_key(), kvc_obj()}].
-type kvc_obj_node() :: proplist() | {struct, proplist()} | [{}] |
                          kvc_dict() | kvc_gb_tree() | term().
-type typed_proplist() :: {proplist() | {gb_tree, kvc_gb_tree()},
                           elem_type()}.

-export_type([proplist/0, kvc_key/0, kvc_obj/0]).

%% @doc Return the result of the query Path on P.
-spec path(kvc_key() | [kvc_key()], kvc_obj()) -> term() | [].
path(Path, P) ->
    path(Path, P, []).

-spec path(kvc_key() | [kvc_key()], kvc_obj(), term()) -> term().
path(Path, P, Default) when is_binary(Path) ->
    path(binary:split(Path, <<".">>, [global]), P, Default);
path(Path, P, Default) when is_atom(Path) ->
    path(atom_to_binary(Path, utf8), P, Default);
path(Path=[N | _], P, Default) when is_integer(N) ->
    path(iolist_to_binary(Path), P, Default);
path([], [], Default) ->
    Default;
path([], P, _Default) ->
    P;
path([K | Rest], P, Default) ->
    path(Rest, value(K, P, Default), Default).

%% @doc Return the immediate result of the query for key K in P.
-spec value(kvc_key(), kvc_obj(), term()) -> term().
value(K, P, Default) ->
    case proplist_type(P) of
        {Nested, list} ->
            R = make_ref(),
            case get_nested_values(K, Nested, R) of
                R ->
                    Default;
                V ->
                    V
            end;
        {{gb_tree, Tree}, Type} ->
            case gb_trees:lookup(normalize(K, Type), Tree) of
                none ->
                    Default;
                {value, V} ->
                    V
            end;
        {Proplist, Type} ->
            case lists:keyfind(normalize(K, Type), 1, Proplist) of
                false ->
                    Default;
                {_, V} ->
                    V
            end
    end.

%% @doc Normalize P to nested proplists.
-spec to_proplist(kvc_obj()) -> kvc_obj().
to_proplist(P) when is_atom(P) orelse is_bitstring(P) orelse is_number(P) orelse
                    is_pid(P) orelse is_port(P) orelse is_reference(P) ->
    %% ^^ do what we can to avoid checking is_tuple(P) directly so dialyzer
    %%    doesn't think we are cheating to find opaque types.
    P;
to_proplist({struct, L}) ->
    to_proplist_pl(L);
to_proplist({L}) ->
    to_proplist_pl(L);
to_proplist({}) ->
    [];
to_proplist([]) ->
    [];
to_proplist([{}]) ->
    [];
to_proplist(L=[{struct, _} | _]) ->
    to_proplist_l(L);
to_proplist(L=[{K, _} | _]) when not is_integer(K) ->
    %% gb_trees is an {integer(), _}
    to_proplist_pl(L);
to_proplist(L=[_ | _]) ->
    first_of(
      [fun to_proplist_gb_l/1,
       fun to_proplist_pl/1,
       fun to_proplist_l/1], L);
to_proplist(T) ->
    first_of(
      [fun to_proplist_gb/1,
       fun to_proplist_dict/1,
       fun identity/1], T).

%% @doc Unwrap data (remove mochijson2 and jiffy specific constructions,
%% and also handle `jsx' empty objects)
-spec unwrap_value(kvc_obj()) -> kvc_obj().
unwrap_value({struct, L}) -> L;
unwrap_value({L})         -> L;
unwrap_value({})          -> [];
unwrap_value([])          -> [];
unwrap_value([{}])        -> [];
unwrap_value(L)           -> L.

%% Internal API

to_proplist_l(L) ->
    [to_proplist(V) || V <- L].

to_proplist_pl(L=[{_, _} | _]) ->
    [{K, to_proplist(V)} || {K, V} <- L];
to_proplist_pl([]) ->
    [].

to_proplist_gb_l([H | T]) ->
    [to_proplist_gb(H) | to_proplist_l(T)].

to_proplist_gb(T) ->
    to_proplist(gb_trees:to_list(T)).

to_proplist_dict(T) ->
    to_proplist(dict:to_list(T)).

identity(T) ->
    T.

get_nested_values(<<"@max">>, L, _R) ->
    lists:max(L);
get_nested_values(<<"@min">>, L, _R) ->
    lists:min(L);
get_nested_values(<<"@sum">>, L, _R) ->
    lists:sum(L);
get_nested_values(<<"@count">>, L, _R) ->
    length(L);
get_nested_values(<<"@avg">>, [], R) ->
    R;
get_nested_values(<<"@avg">>, L, _R) ->
    {Count, Sum} = lists:foldl(
                     fun (N, {C, S}) -> {1 + C, N + S} end,
                     {0, 0},
                     L),
    Sum / Count;
get_nested_values(<<"@distinctUnionOfArrays">>, L, _R) ->
    lists:usort(lists:append(L));
get_nested_values(<<"@distinctUnionOfObjects">>, L, _R) ->
    lists:usort(L);
get_nested_values(<<"@unionOfArrays">>, L, _R) ->
    lists:append(L);
get_nested_values(<<"@unionOfObjects">>, L, _R) ->
    L;
get_nested_values(A, L, R) when is_atom(A) andalso A > '@' andalso A < 'A' ->
    get_nested_values(atom_to_binary(A, utf8), L, R);
get_nested_values(K="@" ++ _, L, R) ->
    get_nested_values(iolist_to_binary(K), L, R);
get_nested_values(K, [L | Rest], R) ->
    case value(K, L, R) of
        R ->
            get_nested_values(K, Rest, R);
        V ->
            [V | get_nested_values(K, Rest, R)]
    end;
get_nested_values(_K, [], _R) ->
    [].

-spec proplist_type(term()) -> typed_proplist().
proplist_type(P=[{K, _} | _]) ->
    {P, typeof_elem(K)};
proplist_type({struct, P=[{K, _} | _]}) ->
    {P, typeof_elem(K)};
proplist_type({P=[{K, _} | _]}) ->
    {P, typeof_elem(K)};
proplist_type({}) ->
    {[], undefined};
proplist_type([{}]) ->
    {[], undefined};
proplist_type(L) when is_list(L) ->
    {L, list};
proplist_type(V) ->
    first_of([fun proplist_type_d/1,
              fun proplist_type_gb/1,
              fun proplist_type_undefined/1], V).

proplist_type_d(D) ->
    proplist_type(dict:to_list(D)).

proplist_type_gb(D) ->
    {K, _V} = gb_trees:smallest(D),
    {{gb_tree, D}, typeof_elem(K)}.

proplist_type_undefined(_) ->
    {[], undefined}.


first_of([F | Rest], V) ->
    try F(V)
    catch error:_ ->
            first_of(Rest, V)
    end.


-spec typeof_elem(term()) -> elem_key_type().
typeof_elem(A) when is_atom(A) ->
    atom;
typeof_elem(B) when is_binary(B) ->
    binary;
typeof_elem([N | _]) when is_integer(N) andalso N > 0 ->
    string;
typeof_elem(_) ->
    undefined.

-spec normalize(term(), elem_type()) -> term().
normalize(K, atom) when is_atom(K) ->
    K;
normalize(K, atom) when is_binary(K) ->
    try binary_to_existing_atom(K, utf8)
    catch error:badarg ->
            K
    end;
normalize(K, atom) when is_list(K) ->
    try list_to_existing_atom(K)
    catch error:badarg ->
            K
    end;
normalize(K, binary) when is_binary(K) ->
    K;
normalize(K, binary) when is_atom(K) ->
    atom_to_binary(K, utf8);
normalize(K, binary) when is_list(K) ->
    iolist_to_binary(K);
normalize(K, string) when is_list(K) ->
    K;
normalize(K, string) when is_binary(K) ->
    binary_to_list(K);
normalize(K, string) when is_atom(K) ->
    atom_to_list(K);
normalize(K, undefined) ->
    K.
