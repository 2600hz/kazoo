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
-module(props_test).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-endif.

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

-ifdef(PROPER).

run_proper_test_() ->
    {"Runs props PropEr tests"
    ,{'timeout'
     ,10000
     ,[?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}
                                                ,{'numtests', 500}
                                                ]
                                      ))
      ]
     }
    }.

prop_set_value() ->
    ?FORALL({KV, Props}
           ,{kz_proplist_property(), kz_proplist()}
           ,?WHENFAIL(io:format("failed to set value ~p in ~p~n", [KV, Props])
                     ,is_defined(KV, props:set_value(KV, Props))
                     )
           ).

prop_set_values() ->
    ?FORALL({KVs, Props}
           ,{[kz_proplist_property()], kz_proplist()}
           ,?WHENFAIL(io:format("failed to set values ~p in ~p~n", [KVs, Props])
                     ,begin
                          NewProps = props:set_values(KVs, Props),
                          lists:all(fun(KV) -> is_defined(KV, NewProps) end
                                   ,KVs
                                   )
                      end
                     )
           ).

is_defined({K, _V}, Props) ->
    'false' =/= lists:keyfind(K, 1, Props);
is_defined(K, Props) ->
    'false' =/= lists:keyfind(K, 1, Props).

-endif.
