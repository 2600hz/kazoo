%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
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
         ,get_is_true/2, get_is_true/3, is_true/2, is_true/3
         ,get_is_false/2, get_is_false/3, is_false/2, is_false/3
         ,get_keys/1
         ,get_all_values/2, get_values/2
         ,set_value/3
         ,unique/1
         ,filter/2
         ,filter_empty/1
         ,filter_undefined/1
        ]).

-include_lib("whistle/include/wh_types.hrl").

-spec set_value(wh_proplist_key(), wh_proplist_value(), wh_proplist()) ->
                             wh_proplist().
set_value(K, V, Prop) ->
    [{K, V} | [KV || {Key, _}=KV <- Prop, K =/= Key]].

-type filter_fun() :: fun(({wh_proplist_key(), wh_proplist_value()}) -> boolean()).
-spec filter(filter_fun(), wh_proplist()) -> wh_proplist().
filter(Fun, Prop) when is_function(Fun, 1), is_list(Prop) ->
    lists:filter(Fun, Prop).

-spec filter_empty(wh_proplist()) -> wh_proplist().
filter_empty(Prop) ->
    [KV || {_, V}=KV <- Prop, (not wh_util:is_empty(V))].

-spec filter_undefined(wh_proplist()) -> wh_proplist().
filter_undefined(Prop) ->
    [KV || {_, V}=KV <- Prop, V =/= 'undefined'].

-spec get_value(wh_proplist_key(), wh_proplist()) -> term().
-spec get_value(wh_proplist_key(), wh_proplist(), Default) -> Default | term().
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

get_value(_Key, [], Def) -> Def;
get_value(Key, Prop, Default) when is_list(Prop) ->
    case lists:keyfind(Key, 1, Prop) of
        false ->
            case lists:member(Key, Prop) of
                true -> true;
                false -> Default
            end;
        {Key, V} -> V; % only return V if a two-tuple is found
        Other when is_tuple(Other) -> Default % otherwise return the default
    end.

-spec get_is_true(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec get_is_true(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
get_is_true(Key, Prop) -> is_true(Key, Prop).
get_is_true(Key, Prop, Default) -> is_true(Key, Prop, Default).

-spec is_true(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec is_true(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
is_true(Key, Prop) ->
    is_true(Key, Prop, 'undefined').
is_true(Key, Prop, Default) ->
    case get_value(Key, Prop) of
        'undefined' -> Default;
        V -> wh_util:is_true(V)
    end.

-spec get_is_false(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec get_is_false(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
get_is_false(Key, Prop) -> is_false(Key, Prop).
get_is_false(Key, Prop, Default) -> is_false(Key, Prop, Default).

is_false(Key, Prop) ->
    is_false(Key, Prop, 'undefined').
is_false(Key, Prop, Default) ->
    case get_value(Key, Prop) of
        'undefined' -> Default;
        V -> wh_util:is_false(V)
    end.

-spec get_integer_value(wh_proplist_key(), wh_proplist()) ->
                                     api_integer().
-spec get_integer_value(wh_proplist_key(), wh_proplist(), Default) ->
                                     integer() | Default.
get_integer_value(Key, Prop) ->
    get_integer_value(Key, Prop, 'undefined').
get_integer_value(Key, Prop, Default) ->
    case ?MODULE:get_value(Key, Prop) of
        'undefined' -> Default;
        Val -> wh_util:to_integer(Val)
    end.

-spec get_atom_value(wh_proplist_key(), wh_proplist()) ->
                                  atom().
-spec get_atom_value(wh_proplist_key(), wh_proplist(), Default) ->
                                  atom() | Default.
get_atom_value(Key, Prop) ->
    get_atom_value(Key, Prop, 'undefined').
get_atom_value(Key, Prop, Default) ->
    case ?MODULE:get_value(Key, Prop) of
        'undefined' -> Default;
        Val -> wh_util:to_atom(Val)
    end.

-spec get_binary_value(wh_proplist_key(), wh_proplist()) -> api_binary().
-spec get_binary_value(wh_proplist_key(), wh_proplist(), Default) ->
                                    ne_binary() | Default.
get_binary_value(Key, Prop) ->
    get_binary_value(Key, Prop, 'undefined').
get_binary_value(Key, Prop, Default) ->
    case ?MODULE:get_value(Key, Prop) of
        'undefined' -> Default;
        V -> wh_util:to_binary(V)
    end.

-spec get_keys(wh_proplist()) -> [wh_proplist_key(),...] | [].
get_keys(Prop) -> [ K || {K,_} <- Prop].

-spec get_all_values(wh_proplist_key(), wh_proplist()) -> [wh_proplist_value(),...] | [].
get_all_values(Key, Prop) -> get_values(Key, Prop).
get_values(Key, Prop) -> [V || {K, V} <- Prop, K =:= Key].

-spec delete(ne_binary() | atom(), wh_proplist()) -> wh_proplist().
delete(K, Prop) ->
    case lists:keyfind(K, 1, Prop) of
        {K, _} -> lists:keydelete(K, 1, Prop);
        'false' -> lists:delete(K, Prop)
    end.

delete_keys([_|_]=Ks, Prop) -> lists:foldl(fun ?MODULE:delete/2, Prop, Ks).

-spec is_defined(wh_proplist_key(), wh_proplist()) -> boolean().
is_defined(Key, Prop) ->
    case lists:keyfind(Key, 1, Prop) of
        {Key,_} -> 'true';
        _ -> 'false'
    end.

-spec unique(wh_proplist()) -> wh_proplist().
unique(List) ->
    unique(List, []).

-spec unique(wh_proplist(), wh_proplist()) -> wh_proplist().
unique([], Uniques) -> lists:reverse(Uniques);
unique([{Key, _}=H|T], Uniques) ->
    unique(lists:filter(fun({K, _}) -> not (K =:= Key) end, T)
           ,[H|Uniques]
          ).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

filter_test() ->
    Fun = fun({_, V}) -> V < 5 end,
    ?assertEqual([], filter(Fun, [])),
    ?assertEqual([], filter(Fun, [{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([{z, 1}], filter(Fun, [{a, 10}, {b, 8}, {c, 6}, {z, 1}])).

filter_empty_test() ->
    ?assertEqual([], filter_empty([])),
    ?assertEqual([{a, 10}, {b, 8}, {c, 6}], filter_empty([{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([], filter_empty([{a, 0}, {b, []}, {c, <<>>}, {z, undefined}])).

filter_undefined_test() ->
    ?assertEqual([], filter_undefined([])),
    ?assertEqual([{a, 10}, {b, 8}, {c, 6}], filter_undefined([{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([{a, 0}, {b, []}, {c, <<>>}], filter_undefined([{a, 0}, {b, []}, {c, <<>>}, {z, undefined}])).

unique_test() ->
    L = [{a, b}, {a, b}, {a, c}, {b,c}, {b,d}],
    ?assertEqual([{a, b}, {b, c}], unique(L)).

delete_test() ->
    L = [{a, 1}, {b, 2}, c, {d, 3}],
    ?assertEqual(L, delete(foo, L)),
    ?assertEqual([{a, 1}, {b, 2}, {d, 3}]
                 ,delete(c, L)),
    ?assertEqual([{a, 1}, c, {d, 3}]
                 ,delete(b, L)).

-endif.
