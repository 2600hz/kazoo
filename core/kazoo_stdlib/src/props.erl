%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Mostly a drop-in replacement and extension of the proplists module,
%%% but using the lists module to implement
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(props).

-export([get_value/2, get_value/3
        ,delete/2, delete_keys/2
        ,is_defined/2
        ,get_integer_value/2, get_integer_value/3
        ,get_atom_value/2, get_atom_value/3
        ,get_binary_value/2, get_binary_value/3
        ,get_ne_binary_value/2, get_ne_binary_value/3
        ,get_is_true/2, get_is_true/3, is_true/2, is_true/3
        ,get_is_false/2, get_is_false/3, is_false/2, is_false/3
        ,get_keys/1
        ,get_first_defined/2, get_first_defined/3
        ,get_all_values/2
        ,get_values_and_keys/1
        ,set_values/2
        ,set_value/2, set_value/3
        ,insert_value/2, insert_value/3, insert_values/2
        ,replace_value/3
        ,unique/1
        ,filter/2
        ,filter_empty/1
        ,filter_undefined/1
        ,to_log/1, to_log/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

%% don't import the get_keys/1 that fetches keys from the process dictionary
-compile({'no_auto_import', [get_keys/1]}).

-spec set_values(kz_proplist(), kz_proplist()) -> kz_proplist().
set_values([], Props) -> Props;
set_values([{K, V}|KVs], Props) ->
    set_values(KVs, set_value(K, V, Props));
set_values([K|KVs], Props) ->
    set_values(KVs, set_value(K, 'true', Props)).

-spec set_value(kz_proplist_property(), kz_proplist()) ->
                       kz_proplist().
-spec set_value(kz_proplist_key(), kz_proplist_value(), kz_proplist()) ->
                       kz_proplist().
set_value({K, V}, Props) ->
    set_value(K, V, Props);
set_value(K, Props) ->
    set_value(K, 'true', Props).

set_value([K], V, Props) ->
    [{K, V} | [KV || KV <- Props, not do_keys_match(KV, K)]];
set_value([K|Ks], V, Props) ->
    SubProps = get_value(Ks, Props, []),
    [{K, set_value(Ks, V, SubProps)} | [KV || KV <- Props, not do_keys_match(KV, K)]];
set_value(K, V, Props) ->
    [{K, V} | [KV || KV <- Props, not do_keys_match(KV, K)]].

-spec do_keys_match(kz_proplist_property(), kz_proplist_key()) ->
                           boolean().
do_keys_match({Key, _}, Key) -> 'true';
do_keys_match(Key, Key) -> 'true';
do_keys_match(_K1, _K2) -> 'false'.

-spec insert_value(kz_proplist_property(), kz_proplist()) ->
                          kz_proplist().
-spec insert_value(kz_proplist_key(), kz_proplist_value(), kz_proplist()) ->
                          kz_proplist().
insert_value({K, V}, Props) ->
    insert_value(K, V, Props);
insert_value(K, Props) ->
    insert_value(K, 'true', Props).

insert_value(K, V, Props) ->
    case get_value(K, Props) of
        'undefined' when V =/= 'undefined' -> [{K, V} | Props];
        _Value -> Props
    end.

-spec insert_values(kz_proplist(), kz_proplist()) -> kz_proplist().
insert_values(KVs, Props) ->
    lists:foldl(fun insert_value/2, Props, KVs).

%% replaces value of Key with Value if Key exists; otherwise Props is unchanged
-spec replace_value(any(), any(), kz_proplist()) -> kz_proplist().
replace_value(Key, Value, Props) ->
    lists:keyreplace(Key, 1, Props, {Key, Value}).

-type filter_fun() :: fun((kz_proplist_property()) -> boolean()).
-spec filter(filter_fun(), kz_proplist()) -> kz_proplist().
filter(Fun, Props) when is_function(Fun, 1),
                        is_list(Props) ->
    [P || P <- Props, Fun(P)].

-spec filter_empty(kz_proplist()) -> kz_proplist().
filter_empty(Props) ->
    [KV || KV <- Props,
           case KV of
               {_, V} -> not kz_term:is_empty(V);
               _V -> 'true'
           end
    ].

-spec filter_undefined(kz_proplist()) -> kz_proplist().
filter_undefined(Props) ->
    [KV || KV <- Props,
           case KV of
               {_, 'undefined'} -> 'false';
               _ -> 'true'
           end
    ].

-spec get_value(kz_proplist_key() | [kz_proplist_key()], kz_proplist()) -> any().
-spec get_value(kz_proplist_key() | [kz_proplist_key()], kz_proplist(), Default) ->
                       Default | any().
get_value(Key, Props) ->
    get_value(Key, Props, 'undefined').

get_value(_Key, [], Default) -> Default;
get_value([Key], Props, Default) when is_binary(Key)
                                      orelse is_atom(Key) ->
    get_value(Key, Props, Default);
get_value([Key|Keys], Props, Default) when is_binary(Key)
                                           orelse is_atom(Key) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        SubProps -> get_value(Keys, SubProps, Default)
    end;
get_value(Key, Props, Default) when is_list(Props) ->
    case lists:keyfind(Key, 1, Props) of
        'false' ->
            case lists:member(Key, Props) of
                'true' -> 'true';
                'false' -> Default
            end;
        {Key, V} -> V; % only return V if a two-tuple is found
        Other when is_tuple(Other) -> Default % otherwise return the default
    end.

%% Given a list of keys, find the first one defined
-spec get_first_defined([kz_proplist_key()], kz_proplist()) -> 'undefined' | any().
-spec get_first_defined([kz_proplist_key()], kz_proplist(), Default) -> Default | any().
get_first_defined(Keys, Props) -> get_first_defined(Keys, Props, 'undefined').

get_first_defined([], _Props, Default) -> Default;
get_first_defined([H|T], Props, Default) ->
    case get_value(H, Props) of
        'undefined' -> get_first_defined(T, Props, Default);
        V -> V
    end.

-spec get_is_true(kz_proplist_key(), kz_proplist()) -> api_boolean().
-spec get_is_true(kz_proplist_key(), kz_proplist(), Default) -> Default | boolean().
get_is_true(Key, Props) -> is_true(Key, Props).
get_is_true(Key, Props, Default) -> is_true(Key, Props, Default).

-spec is_true(kz_proplist_key(), kz_proplist()) -> api_boolean().
-spec is_true(kz_proplist_key(), kz_proplist(), Default) -> Default | boolean().
is_true(Key, Props) ->
    is_true(Key, Props, 'undefined').
is_true(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        V -> kz_term:is_true(V)
    end.

-spec get_is_false(kz_proplist_key(), kz_proplist()) -> api_boolean().
-spec get_is_false(kz_proplist_key(), kz_proplist(), Default) -> Default | boolean().
get_is_false(Key, Props) -> is_false(Key, Props).
get_is_false(Key, Props, Default) -> is_false(Key, Props, Default).

-spec is_false(kz_proplist_key(), kz_proplist()) -> api_boolean().
-spec is_false(kz_proplist_key(), kz_proplist(), Default) -> boolean() | Default.
is_false(Key, Props) ->
    is_false(Key, Props, 'undefined').
is_false(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        V -> kz_term:is_false(V)
    end.

-spec get_integer_value(kz_proplist_key(), kz_proplist()) ->
                               api_integer().
-spec get_integer_value(kz_proplist_key(), kz_proplist(), Default) ->
                               integer() | Default.
get_integer_value(Key, Props) ->
    get_integer_value(Key, Props, 'undefined').
get_integer_value(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        Val -> kz_term:to_integer(Val)
    end.

-spec get_atom_value(kz_proplist_key(), kz_proplist()) ->
                            atom().
-spec get_atom_value(kz_proplist_key(), kz_proplist(), Default) ->
                            atom() | Default.
get_atom_value(Key, Props) ->
    get_atom_value(Key, Props, 'undefined').
get_atom_value(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        Val -> kz_term:to_atom(Val)
    end.

-spec get_binary_value(kz_proplist_key() | [kz_proplist_key()], kz_proplist()) -> api_binary().
-spec get_binary_value(kz_proplist_key() | [kz_proplist_key()], kz_proplist(), Default) ->
                              ne_binary() | Default.
get_binary_value(Key, Props) ->
    get_binary_value(Key, Props, 'undefined').
get_binary_value(Keys, Props, Default) when is_list(Keys) ->
    case get_first_defined(Keys, Props) of
        'undefined' -> Default;
        V -> kz_term:to_binary(V)
    end;
get_binary_value(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        V -> kz_term:to_binary(V)
    end.

-spec get_ne_binary_value(kz_proplist_key(), kz_proplist()) -> api_binary().
-spec get_ne_binary_value(kz_proplist_key(), kz_proplist(), Default) ->
                                 ne_binary() | Default.
get_ne_binary_value(Key, Props) ->
    get_ne_binary_value(Key, Props, 'undefined').
get_ne_binary_value(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        <<>> -> Default;
        V -> kz_term:to_binary(V)
    end.

-spec get_keys(kz_proplist()) -> [kz_proplist_key()] | [].
get_keys([]) -> [];
get_keys(Props) -> [K || {K,_} <- Props].

-spec get_all_values(kz_proplist_key(), kz_proplist()) -> [kz_proplist_value()].
get_all_values(Key, Props) -> [V || {K, V} <- Props, K =:= Key].

-spec get_values_and_keys(kz_proplist()) -> {[kz_proplist_value()], [kz_proplist_key()]}.
get_values_and_keys(Props) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, Props)|Vs], [Key|Ks]}
                end
               ,{[], []}
               ,get_keys(Props)
               ).

-spec delete(kz_proplist_key(), kz_proplist()) -> kz_proplist().
delete(K, Props) ->
    case lists:keyfind(K, 1, Props) of
        {K, _} -> lists:keydelete(K, 1, Props);
        'false' -> lists:delete(K, Props)
    end.

-spec delete_keys([kz_proplist_key()], kz_proplist()) -> kz_proplist().
delete_keys([], Props) -> Props;
delete_keys([_|_]=Ks, Props) -> lists:foldl(fun delete/2, Props, Ks).

-spec is_defined(kz_proplist_property(), kz_proplist()) -> boolean().
is_defined({Key, _}, Props) -> get_value(Key, Props) =/= 'undefined';
is_defined(Key, Props) -> get_value(Key, Props) =/= 'undefined'.

-spec unique(kz_proplist()) -> kz_proplist().
unique(List) ->
    unique(List, []).

-spec unique(kz_proplist(), kz_proplist()) -> kz_proplist().
unique([], Uniques) -> lists:reverse(Uniques);
unique([{Key, _}=H|T], Uniques) ->
    unique([X || X <- T, ufun(X, Key)], [H|Uniques]);
unique([Key|T], Uniques) ->
    unique([X || X <- T, ufun(X, Key)], [Key|Uniques]).

ufun({K, _}, Key) -> K =/= Key;
ufun(K, Key) -> K =/= Key.

-spec to_log(kz_proplist()) -> 'ok'.
to_log(Props) ->
    to_log(Props, <<"Props">>).

-spec to_log(kz_proplist(), ne_binary()) -> 'ok'.
to_log(Props, Header) ->
    Id = kz_binary:rand_hex(4),
    lager:debug("===== Start ~s - ~s ====", [Header, Id]),
    F = fun(K) -> lager:info("~s - ~p = ~p", [Id, K, get_value(K, Props)]) end,
    lists:foreach(F, ?MODULE:get_keys(Props)),
    lager:debug("===== End ~s - ~s ====", [Header, Id]).
