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
-module(props_tests).

-include_lib("eunit/include/eunit.hrl").

filter_test_() ->
    Fun = fun({_, V}) -> V < 5 end,
    [?_assertEqual([], props:filter(Fun, []))
    ,?_assertEqual([], props:filter(Fun, [{'a', 10}, {'b', 8}, {'c', 6}]))
    ,?_assertEqual([{'z', 1}], props:filter(Fun, [{'a', 10}, {'b', 8}, {'c', 6}, {'z', 1}]))
    ].

filter_empty_test_() ->
    [?_assertEqual([], props:filter_empty([]))
    ,?_assertEqual([{'a', 10}, {'b', 8}, {'c', 6}], props:filter_empty([{'a', 10}, {'b', 8}, {'c', 6}]))
    ,?_assertEqual([], props:filter_empty([{'a', 0}, {'b', []}, {'c', <<>>}, {'z', 'undefined'}]))
    ,?_assertEqual(['a'], props:filter_empty(['a']))
    ,?_assertEqual(['a'], props:filter_empty(['a', {'b', 0}]))
    ,?_assertEqual([], props:filter_empty([{<<"a">>, undefined}]))
    ,?_assertEqual([{<<"a">>, false}], props:filter_empty([{<<"a">>, false}]))
    ,?_assertEqual([{<<"a">>, true}], props:filter_empty([{<<"a">>, true}]))
    ].

filter_undefined_test_() ->
    [?_assertEqual(['a'], props:filter_undefined(['a']))

    ,?_assertEqual([], props:filter_undefined([]))
    ,?_assertEqual([{'a', 10}, {'b', 8}, {'c', 6}], props:filter_undefined([{'a', 10}, {'b', 8}, {'c', 6}]))
    ,?_assertEqual([{'a', 0}, {'b', []}, {'c', <<>>}], props:filter_undefined([{'a', 0}, {'b', []}, {'c', <<>>}, {'z', 'undefined'}]))
    ,?_assertEqual([{<<"pouet">>, null}], props:filter_undefined([{<<"pouet">>, null}]))
    ].

unique_test() ->
    L = [{'a', 'b'}, {'a', 'b'}, {'a', 'c'}, {'b','c'}, {'b','d'}],
    ?assertEqual([{'a', 'b'}, {'b', 'c'}], props:unique(L)).

delete_test_() ->
    L = [{'a', 1}, {'b', 2}, 'c', {'d', 3}],
    [?_assertEqual(L, props:delete('foo', L))
    ,?_assertEqual([{'a', 1}, {'b', 2}, {'d', 3}], props:delete('c', L))
    ,?_assertEqual([{'a', 1}, 'c', {'d', 3}], props:delete('b', L))
    ].

insert_value_test_() ->
    P = [{'a', 1}, {'b', 2}],
    P1 = props:insert_value('a', 2, P),
    P2 = props:insert_value({'b', '3'}, P),
    P3 = props:insert_value('c', 3, P),
    P4 = props:insert_value('d', P),
    [?_assertEqual(1, props:get_value('a', P1))
    ,?_assertEqual(2, props:get_value('b', P2))
    ,?_assertEqual(3, props:get_value('c', P3))
    ,?_assertEqual('true', props:get_value('d', P4))
    ].

insert_values_test_() ->
    P = [{'a', 1}, {'b', 2}],
    KVs = [{'a', 2}, {'b', 3}, {'c', 3}, 'd'],
    P1 = props:insert_values(KVs, P),

    [?_assertEqual(1, props:get_value('a', P1))
    ,?_assertEqual(2, props:get_value('b', P1))
    ,?_assertEqual(3, props:get_value('c', P1))
    ,?_assertEqual('true', props:get_value('d', P1))
    ].

is_defined_test_() ->
    Tests = [{[], 'foo', 'false'}
            ,{['foo'], 'foo', 'true'}
            ,{['foo'], 'bar', 'false'}
            ,{[{'foo', 'bar'}], 'foo', 'true'}
            ,{[{'foo', 'bar'}], 'bar', 'false'}
            ],
    [?_assertEqual(Expected, props:is_defined(Key, Props))
     || {Props, Key, Expected} <- Tests
    ].

bools_test_() ->
    Props1 = [{key, undefined}],
    Props2 = [{key, <<"undefined">>}],
    Props3 = [{key, <<"false">>}],
    Props4 = [{key, false}],
    Props5 = [{key, <<"true">>}],
    Props6 = [{key, true}],
    [?_assertEqual(undefined, props:is_true(key, []))
    ,?_assertEqual(undefined, props:is_true(key, Props1))
    ,?_assertEqual(false, props:is_true(key, Props2))
    ,?_assertEqual(false, props:is_true(key, Props3))
    ,?_assertEqual(false, props:is_true(key, Props4))
    ,?_assertEqual(true, props:is_true(key, Props5))
    ,?_assertEqual(true, props:is_true(key, Props6))
    ,?_assertEqual(undefined, props:is_false(key, []))
    ,?_assertEqual(undefined, props:is_false(key, Props1))
    ,?_assertEqual(false, props:is_false(key, Props2))
    ,?_assertEqual(true, props:is_false(key, Props3))
    ,?_assertEqual(true, props:is_false(key, Props4))
    ,?_assertEqual(false, props:is_false(key, Props5))
    ,?_assertEqual(false, props:is_false(key, Props6))
    ].
