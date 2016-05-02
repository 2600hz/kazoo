%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%% Mostly a drop-in replacement and extension of the proplists module,
%%% but using the lists module to implement
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(props_test).

-include_lib("eunit/include/eunit.hrl").

filter_test() ->
    Fun = fun({_, V}) -> V < 5 end,
    ?assertEqual([], props:filter(Fun, [])),
    ?assertEqual([], props:filter(Fun, [{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([{z, 1}], props:filter(Fun, [{a, 10}, {b, 8}, {c, 6}, {z, 1}])).

filter_empty_test() ->
    ?assertEqual([], props:filter_empty([])),
    ?assertEqual([{a, 10}, {b, 8}, {c, 6}], props:filter_empty([{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([], props:filter_empty([{a, 0}, {b, []}, {c, <<>>}, {z, undefined}])),
    ?assertEqual(['a'], props:filter_empty(['a'])),
    ?assertEqual(['a'], props:filter_empty(['a', {'b', 0}])).

filter_undefined_test() ->
    ?assertEqual(['a'], props:filter_undefined(['a'])),

    ?assertEqual([], props:filter_undefined([])),
    ?assertEqual([{a, 10}, {b, 8}, {c, 6}], props:filter_undefined([{a, 10}, {b, 8}, {c, 6}])),
    ?assertEqual([{a, 0}, {b, []}, {c, <<>>}], props:filter_undefined([{a, 0}, {b, []}, {c, <<>>}, {z, undefined}])).

unique_test() ->
    L = [{a, b}, {a, b}, {a, c}, {b,c}, {b,d}],
    ?assertEqual([{a, b}, {b, c}], props:unique(L)).

delete_test() ->
    L = [{a, 1}, {b, 2}, c, {d, 3}],
    ?assertEqual(L, props:delete(foo, L)),
    ?assertEqual([{a, 1}, {b, 2}, {d, 3}]
                 ,props:delete(c, L)),
    ?assertEqual([{a, 1}, c, {d, 3}]
                 ,props:delete(b, L)).

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
                          QS1 = kz_util:to_binary(props:to_querystring(Props)),
                          ?assertEqual(QS, QS1)
                  end, Tests).

insert_value_test() ->
    P = [{a, 1}, {b, 2}],
    P1 = props:insert_value(a, 2, P),
    P2 = props:insert_value({b, 3}, P),
    P3 = props:insert_value(c, 3, P),
    P4 = props:insert_value(d, P),
    ?assertEqual(1, props:get_value(a, P1)),
    ?assertEqual(2, props:get_value(b, P2)),
    ?assertEqual(3, props:get_value(c, P3)),
    ?assertEqual('true', props:get_value(d, P4)).

insert_values_test() ->
    P = [{a, 1}, {b, 2}],
    KVs = [{a, 2}, {b, 3}, {c, 3}, d],
    P1 = props:insert_values(KVs, P),

    ?assertEqual(1, props:get_value(a, P1)),
    ?assertEqual(2, props:get_value(b, P1)),
    ?assertEqual(3, props:get_value(c, P1)),
    ?assertEqual('true', props:get_value(d, P1)).

is_defined_test() ->
    Tests = [{[], 'foo', 'false'}
             ,{['foo'], 'foo', 'true'}
             ,{['foo'], 'bar', 'false'}
             ,{[{'foo', 'bar'}], 'foo', 'true'}
             ,{[{'foo', 'bar'}], 'bar', 'false'}
            ],
    lists:foreach(fun({Props, Key, Expected}) ->
                          ?assertEqual(Expected, props:is_defined(Key, Props))
                  end, Tests).
