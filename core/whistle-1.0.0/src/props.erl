%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
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
         ,get_first_defined/2, get_first_defined/3
         ,get_all_values/2, get_values/2
         ,set_value/3, insert_value/3
         ,unique/1
         ,filter/2
         ,filter_empty/1
         ,filter_undefined/1
         ,to_querystring/1
        ]).

-include_lib("whistle/include/wh_types.hrl").

%% don't import the get_keys/1 that fetches keys from the process dictionary
-compile({'no_auto_import', [get_keys/1]}).

-type wh_proplist_keys() :: [wh_proplist_key(),...] | [].
-type wh_proplist_values() :: [wh_proplist_value(),...] | [].

-spec set_value(wh_proplist_key(), wh_proplist_value(), wh_proplist()) ->
                       wh_proplist().
set_value(K, V, Props) ->
    [{K, V} | [KV || {Key, _}=KV <- Props, K =/= Key]].

-spec insert_value(wh_proplist_key(), wh_proplist_value(), wh_proplist()) ->
                       wh_proplist().
insert_value(K, V, Props) ->
    case get_value(K, Props) of
        'undefined' -> [{K, V} | Props];
        _Value -> Props
    end.

-type filter_fun() :: fun(({wh_proplist_key(), wh_proplist_value()}) -> boolean()).
-spec filter(filter_fun(), wh_proplist()) -> wh_proplist().
filter(Fun, Props) when is_function(Fun, 1), is_list(Props) ->
    lists:filter(Fun, Props).

-spec filter_empty(wh_proplist()) -> wh_proplist().
filter_empty(Props) ->
    [KV || {_, V}=KV <- Props, (not wh_util:is_empty(V))].

-spec filter_undefined(wh_proplist()) -> wh_proplist().
filter_undefined(Props) ->
    [KV || {_, V}=KV <- Props, V =/= 'undefined'].

-spec get_value(wh_proplist_key(), wh_proplist()) -> term().
-spec get_value(wh_proplist_key(), wh_proplist(), Default) -> Default | term().
get_value(Key, Props) ->
    get_value(Key, Props, 'undefined').

get_value(_Key, [], Def) -> Def;
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
-spec get_first_defined(wh_proplist_keys(), wh_proplist()) -> 'undefined' | term().
-spec get_first_defined(wh_proplist_keys(), wh_proplist(), Default) -> Default | term().
get_first_defined(Keys, Props) -> get_first_defined(Keys, Props, 'undefined').

get_first_defined([], _Props, Default) -> Default;
get_first_defined([H|T], Props, Default) ->
    case get_value(H, Props) of
        'undefined' -> get_first_defined(T, Props, Default);
        V -> V
    end.

-spec get_is_true(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec get_is_true(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
get_is_true(Key, Props) -> is_true(Key, Props).
get_is_true(Key, Props, Default) -> is_true(Key, Props, Default).

-spec is_true(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec is_true(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
is_true(Key, Props) ->
    is_true(Key, Props, 'undefined').
is_true(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        V -> wh_util:is_true(V)
    end.

-spec get_is_false(wh_proplist_key(), wh_proplist()) -> api_boolean().
-spec get_is_false(wh_proplist_key(), wh_proplist(), Default) -> Default | boolean().
get_is_false(Key, Props) -> is_false(Key, Props).
get_is_false(Key, Props, Default) -> is_false(Key, Props, Default).

is_false(Key, Props) ->
    is_false(Key, Props, 'undefined').
is_false(Key, Props, Default) ->
    case get_value(Key, Props) of
        'undefined' -> Default;
        V -> wh_util:is_false(V)
    end.

-spec get_integer_value(wh_proplist_key(), wh_proplist()) ->
                               api_integer().
-spec get_integer_value(wh_proplist_key(), wh_proplist(), Default) ->
                               integer() | Default.
get_integer_value(Key, Props) ->
    get_integer_value(Key, Props, 'undefined').
get_integer_value(Key, Props, Default) ->
    case ?MODULE:get_value(Key, Props) of
        'undefined' -> Default;
        Val -> wh_util:to_integer(Val)
    end.

-spec get_atom_value(wh_proplist_key(), wh_proplist()) ->
                            atom().
-spec get_atom_value(wh_proplist_key(), wh_proplist(), Default) ->
                            atom() | Default.
get_atom_value(Key, Props) ->
    get_atom_value(Key, Props, 'undefined').
get_atom_value(Key, Props, Default) ->
    case ?MODULE:get_value(Key, Props) of
        'undefined' -> Default;
        Val -> wh_util:to_atom(Val)
    end.

-spec get_binary_value(wh_proplist_key(), wh_proplist()) -> api_binary().
-spec get_binary_value(wh_proplist_key(), wh_proplist(), Default) ->
                              ne_binary() | Default.
get_binary_value(Key, Props) ->
    get_binary_value(Key, Props, 'undefined').
get_binary_value(Key, Props, Default) ->
    case ?MODULE:get_value(Key, Props) of
        'undefined' -> Default;
        V -> wh_util:to_binary(V)
    end.

-spec get_keys(wh_proplist()) -> wh_proplist_keys().
get_keys(Props) -> [K || {K,_} <- Props].

-spec get_all_values(wh_proplist_key(), wh_proplist()) -> wh_proplist_values().
get_all_values(Key, Props) -> get_values(Key, Props).
get_values(Key, Props) -> [V || {K, V} <- Props, K =:= Key].

-spec delete(ne_binary() | atom(), wh_proplist()) -> wh_proplist().
delete(K, Props) ->
    case lists:keyfind(K, 1, Props) of
        {K, _} -> lists:keydelete(K, 1, Props);
        'false' -> lists:delete(K, Props)
    end.

-spec delete_keys(ne_binaries(), wh_proplist()) -> wh_proplist().
delete_keys([], Props) -> Props;
delete_keys([_|_]=Ks, Props) -> lists:foldl(fun ?MODULE:delete/2, Props, Ks).

-spec is_defined(wh_proplist_key(), wh_proplist()) -> boolean().
is_defined(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
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

get_values_and_keys(Props) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, Props)|Vs], [Key|Ks]}
                end, {[], []}, get_keys(Props)).

to_querystring(Props) ->
    to_querystring(Props, <<>>).

to_querystring(Props, Prefix) ->
    {Vs, Ks} = get_values_and_keys(Props),
    fold_kvs([wh_util:to_binary(K) || K <- Ks], Vs, Prefix, []).

%% foreach key/value pair, encode the key/value with the prefix and prepend the &
%% if the last key/value pair, encode the key/value with the prefix, prepend to accumulator
%% and reverse the list (putting the key/value at the end of the list)
-spec fold_kvs(ne_binaries(), term(), binary() | iolist(), iolist()) -> iolist().
fold_kvs([], [], _, Acc) -> Acc;
fold_kvs([K], [V], Prefix, Acc) -> lists:reverse([encode_kv(Prefix, K, V) | Acc]);
fold_kvs([K|Ks], [V|Vs], Prefix, Acc) ->
    fold_kvs(Ks, Vs, Prefix, [<<"&">>, encode_kv(Prefix, K, V) | Acc]).

-spec encode_kv(iolist() | binary(), ne_binary(), term()) -> iolist().
%% If a list of values, use the []= as a separator between the key and each value
encode_kv(Prefix, K, Vs) when is_list(Vs) ->
    encode_kv(Prefix, wh_util:to_binary(K), Vs, <<"[]=">>, []);
%% if the value is a "simple" value, just encode it (url-encoded)
encode_kv(Prefix, K, V) when is_binary(V) orelse is_number(V) ->
    encode_kv(Prefix, K, <<"=">>, mochiweb_util:quote_plus(V));

% key:{k1:v1, k2:v2} => key[k1]=v1&key[k2]=v2
%% if no prefix is present, use just key to prefix the key/value pairs in the jobj
encode_kv(<<>>, K, [_|_]=Props) -> to_querystring(Props, [wh_util:to_binary(K)]);
%% if a prefix is defined, nest the key in square brackets
encode_kv(Prefix, K, [_|_]=Props) -> to_querystring(Props, [Prefix, <<"[">>, wh_util:to_binary(K), <<"]">>]).

-spec encode_kv(iolist() | binary(), ne_binary(), ne_binary(), string() | binary()) -> iolist().
encode_kv(<<>>, K, Sep, V) -> [wh_util:to_binary(K), Sep, wh_util:to_binary(V)];
encode_kv(Prefix, K, Sep, V) -> [Prefix, <<"[">>, wh_util:to_binary(K), <<"]">>, Sep, wh_util:to_binary(V)].

-spec encode_kv(iolist() | binary(), ne_binary(), [string(),...] | [], ne_binary(), iolist()) -> iolist().
encode_kv(Prefix, K, [V], Sep, Acc) ->
    lists:reverse([ encode_kv(Prefix, K, Sep, mochiweb_util:quote_plus(V)) | Acc]);
encode_kv(Prefix, K, [V|Vs], Sep, Acc) ->
    encode_kv(Prefix, K, Vs, Sep, [ <<"&">>, encode_kv(Prefix, K, Sep, mochiweb_util:quote_plus(V)) | Acc]);
encode_kv(_, _, [], _, Acc) -> lists:reverse(Acc).

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

to_querystring_test() ->
    Tests = [{[], <<>>}
             ,{[{<<"foo">>, <<"bar">>}], <<"foo=bar">>}
             ,{[{<<"foo">>, <<"bar">>}, {<<"fizz">>, <<"buzz">>}], <<"foo=bar&fizz=buzz">>}
             ,{[{'foo', <<"bar">>}
                ,{<<"fizz">>, <<"buzz">>}
                ,{<<"arr">>, [1,3,5]}
               ], <<"foo=bar&fizz=buzz&arr[]=1&arr[]=3&arr[]=5">>}
             ,{[{<<"Msg-ID">>, <<"123-abc">>}], <<"Msg-ID=123-abc">>}
             ,{[{<<"url">>, <<"http://user:pass@host:port/">>}], <<"url=http%3A%2F%2Fuser%3Apass%40host%3Aport%2F">>}
            ],
    lists:foreach(fun({Props, QS}) ->
                          QS1 = wh_util:to_binary(to_querystring(Props)),
                          ?assertEqual(QS, QS1)
                  end, Tests).

-endif.
