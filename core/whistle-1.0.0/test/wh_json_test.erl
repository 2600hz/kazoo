%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% proplists-like interface to json objects
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_json_test).

-ifdef(PROPER).
- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

-include_lib("whistle/src/wh_json.hrl").


%% PropEr Testing
-ifdef(PROPER).

prop_is_object() ->
    ?FORALL(JObj
            ,wh_json:object()
            ,?WHENFAIL(io:format("Failed prop_is_json_object ~p~n", [JObj])
                       ,wh_json:is_json_object(JObj)
                      )
           ).

prop_from_list() ->
    ?FORALL(Prop
            ,wh_json:json_proplist()
            ,?WHENFAIL(io:format("Failed prop_from_list with ~p~n", [Prop])
                       ,wh_json:is_json_object(wh_json:from_list(Prop))
                      )
           ).

prop_get_value() ->
    ?FORALL(Prop
            ,wh_json:json_proplist()
            ,?WHENFAIL(io:format("Failed prop_get_value with ~p~n", [Prop])
                       ,begin
                            JObj = wh_json:from_list(Prop),
                            case length(Prop) > 0 andalso hd(Prop) of
                                {K,V} ->
                                    V =:= wh_json:get_value([K], JObj);
                                'false' -> wh_json:new() =:= JObj
                            end
                        end)
           ).

prop_set_value() ->
    ?FORALL({JObj, Key, Value}
            ,{wh_json:object(), wh_json:keys(), wh_json:json_term()}
            ,?WHENFAIL(io:format("Failed prop_set_value with ~p:~p -> ~p~n", [Key, Value, JObj]),
                       begin
                           JObj1 = wh_json:set_value(Key, Value, JObj),
                           Value =:= wh_json:get_value(Key, JObj1)
                       end)
           ).

prop_to_proplist() ->
    ?FORALL(Prop, json_proplist(),
      ?WHENFAIL(io:format("Failed prop_to_proplist ~p~n", [Prop]),
                begin
                    JObj = wh_json:from_list(Prop),
                    lists:all(fun(K) -> props:get_value(K, Prop) =/= 'undefined' end, wh_json:get_keys(JObj))
                end)
           ).

-endif.

-define(D1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}
                           ,{<<"d1k2">>, 'd1v2'}
                           ,{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}
                          ])).
-define(D2, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                           ,{<<"d2k2">>, 3.14}
                           ,{<<"sub_d1">>, ?D1}
                          ])).
-define(D3, ?JSON_WRAPPER([{<<"d3k1">>, <<"d3v1">>}
                           ,{<<"d3k2">>, []}
                           ,{<<"sub_docs">>, [?D1, ?D2]}
                          ])).
-define(D4, [?D1, ?D2, ?D3]).

-define(D6, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                           ,{<<"d2k2">>, 3.14}
                           ,{<<"sub_d1">>, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])}
                          ]
                         )).
-define(D7, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])).

-define(SP, wh_json:decode(<<"{\"plan\":{\"phone_numbers\":{\"did_us\":{\"discounts\":{\"cumulative\":{\"rate\":1}}}}}}">>)).
-define(O, wh_json:decode(<<"{\"phone_numbers\":{\"did_us\":{\"discounts\":{\"cumulative\":{\"rate\":20}}}}}">>)).

merge_overrides_test() ->
    AP = wh_json:merge_recursive(?SP, wh_json:from_list([{<<"plan">>, ?O}])),

    Key = [<<"plan">>, <<"phone_numbers">>, <<"did_us">>, <<"discounts">>, <<"cumulative">>, <<"rate">>],

    ?assertEqual(1, wh_json:get_value(Key, ?SP)),
    ?assertEqual(20, wh_json:get_value(Key, AP)).

-ifdef(PROPER).
is_json_object_proper_test_() ->
    {"Runs wh_json PropEr tests for is_json_object/1",
     {'timeout', 10000, [?_assertEqual([], proper:module(?MODULE))]}}.
-endif.

is_empty_test() ->
    ?assertEqual('true', wh_json:is_empty(wh_json:new())),
    ?assertEqual('false', wh_json:is_empty(?D1)),
    ?assertEqual('false', wh_json:is_empty(?D6)),
    ?assertEqual('false', wh_json:is_empty(123)),
    ?assertEqual('false', wh_json:is_empty(<<"foobar">>)),
    ?assertEqual('false', wh_json:is_empty([{'bar', 'bas'}])).

merge_jobjs_test() ->
    JObj = wh_json:merge_jobjs(?D1, ?D2),
    ?assertEqual('true', 'undefined' =/= wh_json:get_value(<<"d1k1">>, JObj)),
    ?assertEqual('true', 'undefined' =/= wh_json:get_value(<<"d2k1">>, JObj)),
    ?assertEqual('true', 'undefined' =/= wh_json:get_value(<<"sub_d1">>, JObj)),
    ?assertEqual('true', 'undefined' =:= wh_json:get_value(<<"missing_k">>, JObj)).

merge_recursive_test() ->
    Base = ?D2,
    New = wh_json:set_value([<<"sub_d1">>, <<"d1k1">>], 'd2k2', ?D2),
    JObj = wh_json:merge_recursive(Base, New),
    JObj1 = wh_json:merge_recursive([Base, New]),

    lists:foreach(fun(J) ->
                          ?assertEqual('true', wh_json:is_json_object(J)),
                          ?assertEqual('undefined', wh_json:get_value(<<"d1k1">>, J)),
                          ?assertEqual(1, wh_json:get_value(<<"d2k1">>, J)),

                          ?assertEqual('true', 'undefined' =/= wh_json:get_value(<<"sub_d1">>, J)),

                          %% second JObj takes precedence
                          ?assertEqual('d2k2',  wh_json:get_value([<<"sub_d1">>, <<"d1k1">>], J)),
                          ?assertEqual('undefined', wh_json:get_value(<<"missing_k">>, J))
                  end
                  ,[JObj, JObj1]
                 ).

get_binary_value_test() ->
    ?assertEqual('true', is_binary(wh_json:get_binary_value(<<"d1k1">>, ?D1))),
    ?assertEqual('undefined', wh_json:get_binary_value(<<"d2k1">>, ?D1)),
    ?assertEqual('true', is_binary(wh_json:get_binary_value(<<"d1k1">>, ?D1, <<"something">>))),
    ?assertEqual(<<"something">>, wh_json:get_binary_value(<<"d2k1">>, ?D1, <<"something">>)).

get_integer_value_test() ->
    ?assertEqual(1, wh_json:get_integer_value(<<"d2k1">>, ?D2)),
    ?assertEqual('undefined', wh_json:get_integer_value(<<"d1k1">>, ?D2)),
    ?assertEqual(1, wh_json:get_integer_value(<<"d2k1">>, ?D2, 0)),
    ?assertEqual(0, wh_json:get_integer_value(<<"d1k1">>, ?D2, 0)).

get_float_value_test() ->
    ?assertEqual('true', is_float(wh_json:get_float_value(<<"d2k2">>, ?D2))),
    ?assertEqual('undefined', wh_json:get_float_value(<<"d1k1">>, ?D2)),
    ?assertEqual(3.14, wh_json:get_float_value(<<"d2k2">>, ?D2, 0.0)),
    ?assertEqual(0.0, wh_json:get_float_value(<<"d1k1">>, ?D2, 0.0)).

get_binary_boolean_test() ->
    ?assertEqual('undefined', wh_json:get_binary_boolean(<<"d1k1">>, ?D2)),
    ?assertEqual(<<"false">>, wh_json:get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}]))),
    ?assertEqual(<<"true">>, wh_json:get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}]))).

is_false_test() ->
    ?assertEqual('false', wh_json:is_false(<<"d1k1">>, ?D1)),
    ?assertEqual('true', wh_json:is_false(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}]))).

is_true_test() ->
    ?assertEqual('false', wh_json:is_true(<<"d1k1">>, ?D1)),
    ?assertEqual('true', wh_json:is_true(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}]))).

-define(D1_FILTERED, ?JSON_WRAPPER([{<<"d1k2">>, 'd1v2'}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D2_FILTERED, ?JSON_WRAPPER([{<<"sub_d1">>, ?D1}])).
-define(D3_FILTERED, ?JSON_WRAPPER([{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?D1, ?D2_FILTERED]}])).
filter_test() ->
    ?assertEqual(?D1_FILTERED, wh_json:filter(fun({<<"d1k1">>, _}) -> 'false'; (_) -> 'true' end, ?D1)),
    ?assertEqual(?D2_FILTERED, wh_json:filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D2)),
    ?assertEqual(?D3_FILTERED, wh_json:filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D3, [<<"sub_docs">>, 2])).

new_test() ->
    ?EMPTY_JSON_OBJECT =:= wh_json:new().

-spec is_json_object_test() -> any().
is_json_object_test() ->
    ?assertEqual('false', wh_json:is_json_object('foo')),
    ?assertEqual('false', wh_json:is_json_object(123)),
    ?assertEqual('false', wh_json:is_json_object(['boo', 'yah'])),
    ?assertEqual('false', wh_json:is_json_object(<<"bin">>)),

    ?assertEqual('true', wh_json:is_json_object(?D1)),
    ?assertEqual('true', wh_json:is_json_object(?D2)),
    ?assertEqual('true', wh_json:is_json_object(?D3)),
    ?assertEqual('true', lists:all(fun wh_json:is_json_object/1, ?D4)),
    ?assertEqual('true', wh_json:is_json_object(?D6)),
    ?assertEqual('true', wh_json:is_json_object(?D7)).

%% delete results
-define(D1_AFTER_K1, ?JSON_WRAPPER([{<<"d1k2">>, 'd1v2'}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D1_AFTER_K3_V2, ?JSON_WRAPPER([{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.3">>]}, {<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, 'd1v2'}])).

-define(D6_AFTER_SUB, ?JSON_WRAPPER([{<<"sub_d1">>, ?EMPTY_JSON_OBJECT}
                                     ,{<<"d2k1">>, 1}
                                     ,{<<"d2k2">>, 3.14}
                                    ]
                                   )).
-define(D6_AFTER_SUB_PRUNE, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                                           ,{<<"d2k2">>, 3.14}
                                          ]
                                         )).

-define(P1, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P2, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?JSON_WRAPPER(?P1)}]).
-define(P3, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?JSON_WRAPPER(?P1), ?JSON_WRAPPER(?P2)]}]).
-define(P4, [?P1, ?P2, ?P3]).
-define(P6, [{<<"d2k1">>, 1},{<<"d2k2">>, 3.14},{<<"sub_d1">>, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])}]).
-define(P7, [{<<"d1k1">>, <<"d1v1">>}]).

-define(P8, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, 'd1v2'}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P9, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?P1}]).
-define(P10, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?P8, ?P9]}]).
-define(P11, [?P8, ?P9, ?P10]).
-define(P12, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14},{<<"sub_d1">>, [{<<"d1k1">>, <<"d1v1">>}]}]).
-define(P13, [{<<"d1k1">>, <<"d1v1">>}]).

%% deleting [k1, 1] should return empty json object
-define(D_ARR, ?JSON_WRAPPER([{<<"k1">>, [1]}])).
-define(P_ARR, ?JSON_WRAPPER([{<<"k1">>, []}])).

-spec get_keys_test() -> any().
get_keys_test() ->
    Keys = [<<"d1k1">>, <<"d1k2">>, <<"d1k3">>],
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, wh_json:get_keys([], ?D1))),
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, wh_json:get_keys([<<"sub_docs">>, 1], ?D3))),
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, [1,2,3]) end, wh_json:get_keys([<<"sub_docs">>], ?D3))).

-spec to_proplist_test() -> any().
to_proplist_test() ->
    ?assertEqual(?P1, wh_json:to_proplist(?D1)),
    ?assertEqual(?P2, wh_json:to_proplist(?D2)),
    ?assertEqual(?P3, wh_json:to_proplist(?D3)),
    ?assertEqual(?P4, lists:map(fun wh_json:to_proplist/1, ?D4)),
    ?assertEqual(?P6, wh_json:to_proplist(?D6)),
    ?assertEqual(?P7, wh_json:to_proplist(?D7)).

-spec recursive_to_proplist_test() -> any().
recursive_to_proplist_test() ->
    ?assertEqual(?P8, wh_json:recursive_to_proplist(?D1)),
    ?assertEqual(?P9, wh_json:recursive_to_proplist(?D2)),
    ?assertEqual(?P10, wh_json:recursive_to_proplist(?D3)),
    ?assertEqual(?P11, lists:map(fun wh_json:recursive_to_proplist/1, ?D4)),
    ?assertEqual(?P12, wh_json:recursive_to_proplist(?D6)),
    ?assertEqual(?P13, wh_json:recursive_to_proplist(?D7)).

-spec delete_key_test() -> any().
delete_key_test() ->
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"d1k1">>], ?D7)),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"d1k1">>], ?D7, 'prune')),
    ?assertEqual(?D1_AFTER_K1, wh_json:delete_key([<<"d1k1">>], ?D1)),
    ?assertEqual(?D1_AFTER_K1, wh_json:delete_key([<<"d1k1">>], ?D1, 'prune')),
    ?assertEqual(?D1_AFTER_K3_V2, wh_json:delete_key([<<"d1k3">>, 2], ?D1)),
    ?assertEqual(?D1_AFTER_K3_V2, wh_json:delete_key([<<"d1k3">>, 2], ?D1, 'prune')),
    ?assertEqual(?D6_AFTER_SUB, wh_json:delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6)),
    ?assertEqual(?D6_AFTER_SUB_PRUNE, wh_json:delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6, 'prune')),
    ?assertEqual(?P_ARR, wh_json:delete_key([<<"k1">>, 1], ?D_ARR)),
    ?assertEqual(?EMPTY_JSON_OBJECT, wh_json:delete_key([<<"k1">>, 1], ?D_ARR, 'prune')).

-spec get_value_test() -> any().
get_value_test() ->
    %% Basic first level key
    ?assertEqual('undefined', wh_json:get_value([<<"d1k1">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(<<"d1v1">>, wh_json:get_value([<<"d1k1">>], ?D1)),
    ?assertEqual('undefined', wh_json:get_value([<<"d1k1">>], ?D2)),
    ?assertEqual('undefined', wh_json:get_value([<<"d1k1">>], ?D3)),
    ?assertEqual('undefined', wh_json:get_value([<<"d1k1">>], ?D4)),
    %% Basic nested key
    ?assertEqual('undefined', wh_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual('undefined', wh_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D1)),
    ?assertEqual('d1v2',      wh_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D2)),
    ?assertEqual('undefined', wh_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D3)),
    ?assertEqual('undefined', wh_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects
    ?assertEqual('undefined', wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual('undefined', wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D1)),
    ?assertEqual('undefined', wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D2)),
    ?assertEqual('undefined', wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D3)),
    ?assertEqual(3.14,      wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects, but change the default return if it is not present.
    %% Also tests the ability to have indexs represented as strings
    ?assertEqual(<<"not">>, wh_json:get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], [], <<"not">>)),
    ?assertEqual(<<"not">>, wh_json:get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D1, <<"not">>)),
    ?assertEqual(<<"not">>, wh_json:get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D2, <<"not">>)),
    ?assertEqual(<<"not">>, wh_json:get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D3, <<"not">>)),
    ?assertEqual(3.14,      wh_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4, <<"not">>)).

-define(T2R1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"update">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R2, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, 'new_value'}])).
-define(T2R3, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, ?JSON_WRAPPER([{<<"new_key">>, 'added_value'}])}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R4, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, ?JSON_WRAPPER([{<<"new_key">>, 'added_value'}])}])).

set_value_object_test() ->
    %% Test setting an existing key
    ?assertEqual(?T2R1, wh_json:set_value([<<"d1k2">>], <<"update">>, ?D1)),
    %% Test setting a non-existing key
    ?assertEqual(?T2R2, wh_json:set_value([<<"d1k4">>], 'new_value', ?D1)),
    %% Test setting an existing key followed by a non-existant key
    ?assertEqual(?T2R3, wh_json:set_value([<<"d1k2">>, <<"new_key">>], 'added_value', ?D1)),
    %% Test setting a non-existing key followed by another non-existant key
    ?assertEqual(?T2R4, wh_json:set_value([<<"d1k4">>, <<"new_key">>], 'added_value', ?D1)).

-define(D5,   [?JSON_WRAPPER([{<<"k1">>, 'v1'}]), ?JSON_WRAPPER([{<<"k2">>, 'v2'}])]).
-define(T3R1, [?JSON_WRAPPER([{<<"k1">>,'test'}]),?JSON_WRAPPER([{<<"k2">>,'v2'}])]).
-define(T3R2, [?JSON_WRAPPER([{<<"k1">>,'v1'},{<<"pi">>, 3.14}]),?JSON_WRAPPER([{<<"k2">>,'v2'}])]).
-define(T3R3, [?JSON_WRAPPER([{<<"k1">>,'v1'},{<<"callerid">>,?JSON_WRAPPER([{<<"name">>,<<"2600hz">>}])}]),?JSON_WRAPPER([{<<"k2">>,'v2'}])]).
-define(T3R4, [?JSON_WRAPPER([{<<"k1">>,'v1'}]),?JSON_WRAPPER([{<<"k2">>,<<"updated">>}])]).
-define(T3R5, [?JSON_WRAPPER([{<<"k1">>,'v1'}]),?JSON_WRAPPER([{<<"k2">>,'v2'}]),?JSON_WRAPPER([{<<"new_key">>,<<"added">>}])]).

set_value_multiple_object_test() ->
    %% Set an existing key in the first wh_json:object()
    ?assertEqual(?T3R1, wh_json:set_value([1, <<"k1">>], 'test', ?D5)),
    %% Set a non-existing key in the first wh_json:object()
    ?assertEqual(?T3R2, wh_json:set_value([1, <<"pi">>], 3.14, ?D5)),
    %% Set a non-existing key followed by another non-existant key in the first wh_json:object()
    ?assertEqual(?T3R3, wh_json:set_value([1, <<"callerid">>, <<"name">>], <<"2600hz">>, ?D5)),
    %% Set an existing key in the second wh_json:object()
    ?assertEqual(?T3R4, wh_json:set_value([2, <<"k2">>], <<"updated">>, ?D5)),
    %% Set a non-existing key in a non-existing wh_json:object()
    ?assertEqual(?T3R5, wh_json:set_value([3, <<"new_key">>], <<"added">>, ?D5)).

%% <<"ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
-define(T4R1,  ?JSON_WRAPPER([{<<"Caller-ID">>, 1234},{list_to_binary(lists:seq(16#C0, 16#D6)), <<"Smith">>} ])).
%% <<"àáâãäåæçèéêëìíîïðñòóôõö"
-define(T4R1V, ?JSON_WRAPPER([{<<"caller_id">>, 1234},{list_to_binary(lists:seq(16#E0, 16#F6)), <<"Smith">>} ])).
%% <<"ØÙÚÛÜÝÞ"
-define(T5R1,  ?JSON_WRAPPER([{<<"Caller-ID">>, 1234},{list_to_binary(lists:seq(16#D8, 16#DE)), <<"Smith">>} ])).
%% <<"øùúûüýþ"
-define(T5R1V, ?JSON_WRAPPER([{<<"caller_id">>, 1234},{list_to_binary(lists:seq(16#F8, 16#FE)), <<"Smith">>} ])).

-define(T4R2,  ?JSON_WRAPPER([{<<"Account-ID">>, <<"45AHGJDF8DFDS2130S">>}, {<<"TRUNK">>, 'false'}, {<<"Node1">>, ?T4R1 }, {<<"Node2">>, ?T4R1 }])).
-define(T4R2V, ?JSON_WRAPPER([{<<"account_id">>, <<"45AHGJDF8DFDS2130S">>}, {<<"trunk">>, 'false'}, {<<"node1">>, ?T4R1V}, {<<"node2">>, ?T4R1V}])).
-define(T4R3,  ?JSON_WRAPPER([{<<"Node-1">>, ?JSON_WRAPPER([{<<"Node-2">>, ?T4R2  }])}, {<<"Another-Node">>, ?T4R1 }] )).
-define(T4R3V, ?JSON_WRAPPER([{<<"node_1">>, ?JSON_WRAPPER([{<<"node_2">>, ?T4R2V }])}, {<<"another_node">>, ?T4R1V}] )).

set_value_normalizer_test() ->
    %% Normalize a flat JSON object
    ?assertEqual(wh_json:normalize_jobj(?T4R1), ?T4R1V),
    %% Normalize a single nested JSON object
    ?assertEqual(wh_json:normalize_jobj(?T4R2), ?T4R2V),
    %% Normalize multiple nested JSON object
    ?assertEqual(wh_json:normalize_jobj(?T4R3), ?T4R3V),

    ?assertEqual(wh_json:normalize_jobj(?T5R1), ?T5R1V).

to_querystring_test() ->
    Tests = [{<<"{}">>, <<>>}
             ,{<<"{\"foo\":\"bar\"}">>, <<"foo=bar">>}
             ,{<<"{\"foo\":\"bar\",\"fizz\":\"buzz\"}">>, <<"foo=bar&fizz=buzz">>}
             ,{<<"{\"foo\":\"bar\",\"fizz\":\"buzz\",\"arr\":[1,3,5]}">>, <<"foo=bar&fizz=buzz&arr[]=1&arr[]=3&arr[]=5">>}
             ,{<<"{\"Msg-ID\":\"123-abc\"}">>, <<"Msg-ID=123-abc">>}
             ,{<<"{\"url\":\"http://user:pass@host:port/\"}">>, <<"url=http%3A%2F%2Fuser%3Apass%40host%3Aport%2F">>}
             ,{<<"{\"topkey\":{\"subkey1\":\"v1\",\"subkey2\":\"v2\",\"subkey3\":[\"v31\",\"v32\"]}}">>
                   ,<<"topkey[subkey1]=v1&topkey[subkey2]=v2&topkey[subkey3][]=v31&topkey[subkey3][]=v32">>}
             ,{<<"{\"topkey\":{\"subkey1\":\"v1\",\"subkey2\":{\"k3\":\"v3\"}}}">>
                   ,<<"topkey[subkey1]=v1&topkey[subkey2][k3]=v3">>}
            ],
    lists:foreach(fun({JSON, QS}) ->
                          QS1 = wh_util:to_binary(
                                  wh_json:to_querystring(
                                    wh_json:decode(JSON)
                                   )
                                 ),
                          ?assertEqual(QS, QS1)
                  end, Tests).

get_values_test() ->
    ?assertEqual('true', are_all_there(?D1, [<<"d1v1">>, 'd1v2', [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]], [<<"d1k1">>, <<"d1k2">>, <<"d1k3">>])).

-define(K3_JOBJ, ?JSON_WRAPPER([{<<"k3.1">>, <<"v3.1">>}])).
-define(CODEC_JOBJ, ?JSON_WRAPPER([{<<"k1">>, <<"v1">>}
                                   ,{<<"k2">>, ?EMPTY_JSON_OBJECT}
                                   ,{<<"k3">>, ?K3_JOBJ}
                                   ,{<<"k4">>, [1,2,3]}
                                  ])).
codec_test() ->
    ?assertEqual(?CODEC_JOBJ, wh_json:decode(wh_json:encode(?CODEC_JOBJ))).

are_all_there(JObj, Vs, Ks) ->
    {Values, Keys} = wh_json:get_values(JObj),
    lists:all(fun(K) -> lists:member(K, Keys) end, Ks)
        andalso lists:all(fun(V) -> lists:member(V, Values) end, Vs).

find_value_test() ->
    JObjs = wh_json:decode(<<"[{\"k1\":\"v1\"},{\"k1\":\"v2\"}]">>),
    ?assertEqual(<<"{\"k1\":\"v1\"}">>, wh_json:encode(wh_json:find_value(<<"k1">>, <<"v1">>, JObjs))),
    ?assertEqual(<<"{\"k1\":\"v2\"}">>, wh_json:encode(wh_json:find_value(<<"k1">>, <<"v2">>, JObjs))),
    ?assertEqual('undefined', wh_json:find_value(<<"k1">>, <<"v3">>, JObjs)).

insert_value_test() ->
    JObj = wh_json:decode(<<"{\"k1\":\"v1\",\"k2\":\"v2\"}">>),
    NonInsert = wh_json:insert_value(<<"k1">>, <<"v3">>, JObj),
    Insert = wh_json:insert_value(<<"k3">>, <<"v3">>, JObj),
    ?assertEqual(<<"v1">>, wh_json:get_value(<<"k1">>, NonInsert)),
    ?assertEqual(<<"v3">>, wh_json:get_value(<<"k3">>, Insert)).
