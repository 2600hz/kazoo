%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc proplists-like interface to json objects
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_tests).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% PropEr Testing
-ifdef(PROPER).

-define(MAX_OBJECT_DEPTH, 10).

%% Lifted from Erlang ML
proper_test_() ->
    {"Runs kz_json PropEr tests"
    ,[{'timeout'
      ,10 * ?MILLISECONDS_IN_SECOND
      ,{atom_to_list(F)
       ,fun () ->
                ?assert(proper:quickcheck(?MODULE:F(), [{'to_file', 'user'}
                                                       ,{'numtests', 100}
                                                       ]))
        end
       }
      }
      || {F, 0} <- ?MODULE:module_info('exports'),
         F > 'prop_',
         F < 'prop`'
     ]
    }.

%% Checks the depth of keys in generated JSON objects
%% proper:quickcheck(kz_json_tests:prop_test_object_gen(), 10000).
%% 67% 1
%% 19% 0
%% 11% 2
%% 1% 3
%% 0% 4
%% This means 67% of generated objects in that run had a max depth of 1 (single key/values in the JObj)
%% 19% were the empty JObj (not helpful)
%% 11% has 1 level of nested keys (length 2)
%% and 1% has 2 levels of nested keys and almost none had 3 levels of keys
%% Changing to the deep_object() generator with the resize/2 to control depth:
%% | Depth | Runs  | Percentages (ascending range)
%% |  10   |   100 | 34, 59, 7
%% |  10   |  1000 | 37, 56, 6, 0
%% |  10   | 10000 | 38, 54, 6, 0

prop_test_object_gen() ->
    ?FORALL(JObj
           ,kz_json_generators:test_object()
           ,collect(to_range(2, kz_json_generators:max_depth(JObj))
                   ,kz_json:is_valid_json_object(JObj)
                   )
           ).

prop_deep_object_gen() ->
    ?FORALL(JObj
           ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
           ,collect(to_range(2, kz_json_generators:max_depth(JObj))
                   ,kz_json:is_valid_json_object(JObj)
                   )
           ).

to_range(M, N) ->
    Base = N div M,
    {Base * M, (Base+1) * M}.

%% Test lifting common properties out of a list of objects
prop_lift_common() ->
    ?FORALL({JObj, UniqueJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                CommonJObj = remove_unique_from_common(UniqueJObj, JObj),
                ?WHENFAIL(?debugFmt("~nfailed to lift out common ~p~nunique: ~p~n"
                                   ,[CommonJObj, UniqueJObj]
                                   )
                         ,begin
                              FlatUnique = kz_json:to_proplist(kz_json:flatten(UniqueJObj)),
                              Merged = kz_json:set_values(FlatUnique, CommonJObj),
                              %% Merged = kz_json:merge_recursive(CommonJObj, UniqueJObj),
                              {CommonProperties, [UpCommon, UpUnique, UpCommon, UpUnique]} = kz_json:lift_common_properties([CommonJObj, Merged, CommonJObj, Merged]),
                              kz_json:are_equal(CommonJObj, CommonProperties)
                                  andalso kz_json:are_equal(kz_json:new(), UpCommon)
                                  andalso kz_json:are_equal(UpUnique, UniqueJObj)
                          end
                         )
            end
           ).

prop_nothing_in_common() ->
    ?FORALL({JObj, UniqueJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                CommonJObj = remove_unique_from_common(UniqueJObj, JObj),
                ?WHENFAIL(?debugFmt("~nfailed nothing_in_common:~ncommon ~p~nunique: ~p~n"
                                   ,[CommonJObj, UniqueJObj]
                                   )
                         ,begin
                              {CommonProperties, [UpCommon, UpUnique]} = kz_json:lift_common_properties([CommonJObj, UniqueJObj]),
                              kz_json:are_equal(kz_json:new(), CommonProperties)
                                  andalso kz_json:are_equal(CommonJObj, UpCommon)
                                  andalso kz_json:are_equal(UpUnique, UniqueJObj)
                          end
                         )
            end
           ).

prop_lift_common_blacklist() ->
    ?FORALL({JObj, UniqueJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                CommonJObj = remove_unique_from_common(UniqueJObj, JObj),
                ?WHENFAIL(?debugFmt("~nfailed to lift out common ~p~nunique: ~p~nmerged: ~p~nblacklisted: ~p~n"
                                   ,[CommonJObj
                                    ,UniqueJObj
                                    ,kz_json:merge(UniqueJObj, CommonJObj)
                                    ,make_blacklist(kz_json:get_keys(CommonJObj))
                                    ]
                                   )
                         ,begin
                              Blacklist = make_blacklist(kz_json:get_keys(CommonJObj)),
                              Merged = kz_json:merge(UniqueJObj, CommonJObj),
                              {CommonProperties
                              ,[UpCommon, UpUnique]
                              } = kz_json:lift_common_properties([CommonJObj, Merged], Blacklist),

                              kz_json:are_equal(kz_json:delete_keys(Blacklist, CommonJObj), CommonProperties)
                                  andalso kz_json:get_value(Blacklist, UpCommon) =:= kz_json:get_value(Blacklist, CommonJObj)
                                  andalso kz_json:get_value(Blacklist, UpUnique) =:= kz_json:get_value(Blacklist, Merged)
                          end
                         )
            end
           ).

make_blacklist([]) -> [];
make_blacklist([H|_]) -> [H].

remove_unique_from_common(Common, Unique) ->
    kz_json:foldl(fun(K,_, U) -> kz_json:delete_key(K, U) end, Unique, Common).

prop_is_object() ->
    ?FORALL(JObj
           ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
           ,?WHENFAIL(?debugFmt("Failed is_valid_json_object with ~p~n", [JObj])
                     ,kz_json:is_valid_json_object(JObj)
                     )
           ).

prop_from_list() ->
    ?FORALL(Prop
           ,flat_proplist()
           ,?WHENFAIL(?debugFmt("Failed prop_from_list with ~p~n", [Prop])
                     ,kz_json:is_valid_json_object(kz_json:expand(kz_json:from_list(Prop)))
                     )
           ).

prop_get_value() ->
    ?FORALL(JObj
           ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
           ,?WHENFAIL(?debugFmt("Failed prop_get_value with ~p~n", [JObj])
                     ,begin
                          Prop = kz_json:to_proplist(JObj),
                          case Prop =/= []
                              andalso hd(Prop)
                          of
                              {K,V} ->
                                  V =:= kz_json:get_value([K], JObj, V);
                              'false' -> kz_json:new() =:= JObj
                          end
                      end)
           ).

prop_set_value() ->
    ?FORALL({JObj, Key, Value}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object()), path(), non_object_json_term()}
           ,?TRAPEXIT(
               ?WHENFAIL(?debugFmt("Failed prop_set_value with ~w:~w -> ~p~n"
                                  ,[Key, Value, JObj]
                                  ),
                         begin
                             Normalized = kz_json:check_value_term(Value),
                             JObj1 = kz_json:set_value(Key, Normalized, JObj),

                             'true' =:= kz_json:is_defined(Key, JObj1)
                                 andalso (Normalized =:= kz_json:get_value(Key, JObj1))
                         end)
              )
           ).

prop_delete_key() ->
    ?FORALL({JObj, Key, Value}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object()), key(), non_object_json_term()}
           ,?TRAPEXIT(
               ?WHENFAIL(?debugFmt("Failed kz_json:delete_key(~w, ~w) after setting ~w~n"
                                  ,[Key, JObj, Value]
                                  ),
                         begin
                             JObj1 = kz_json:set_value(Key, Value, JObj),
                             JObj2 = kz_json:delete_key(Key, JObj1),

                             'true' =:= kz_json:is_defined(Key, JObj1)
                                 andalso 'false' =:= kz_json:is_defined(Key, JObj2)
                         end)
              )
           ).

prop_flatten_expand() ->
    ?FORALL(JObj
           ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
           ,?WHENFAIL(?debugFmt("Failed to flatten/expand: ~p~n", [JObj])
                     ,kz_json:are_equal(JObj, kz_json:expand(kz_json:flatten(JObj)))
                     )
           ).

prop_merge_right() ->
    ?FORALL({LeftJObj, RightJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                MergedJObj = kz_json:merge(fun kz_json:merge_right/2, LeftJObj, RightJObj),

                ?WHENFAIL(?debugFmt("Failed to merge_right (~p, ~p)~nmerge-r/2: ~p~n"
                                   ,[LeftJObj, RightJObj, MergedJObj]
                                   )
                         ,are_all_properties_found(MergedJObj, RightJObj)
                         )
            end
           ).

prop_merge_left() ->
    ?FORALL({LeftJObj, RightJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                MergedJObj = kz_json:merge(fun kz_json:merge_left/2, LeftJObj, RightJObj),

                ?WHENFAIL(?debugFmt("Failed to merge_left (~p, ~p)~nmerge-l/2: ~p~n"
                                   ,[LeftJObj, RightJObj, MergedJObj]
                                   )
                         ,are_all_properties_found(MergedJObj, LeftJObj)
                         )
            end
           ).

prop_key_with_null() ->
    ?FORALL({JObj, Path}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,kz_json_generators:path(?MAX_OBJECT_DEPTH)
            }
           ,begin
                WithNull = kz_json:set_value(Path, 'null', JObj, #{'keep_null' => 'true'}),
                WithoutNull = kz_json:set_value(Path, 'null', JObj),

                ?WHENFAIL(?debugFmt("setting nulls failed on ~p:~nnull: ~p~nnot: ~p~n"
                                   ,[Path, WithNull, WithoutNull]
                                   )
                         ,kz_json:get_value(Path, WithNull) =:= 'null'
                          andalso kz_json:get_value(Path, WithoutNull) =:= 'undefined'
                         )
            end
           ).

prop_merge_with_null() ->
    ?FORALL({LeftJObj, Path, RightJObj}
           ,{resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            ,kz_json_generators:path(?MAX_OBJECT_DEPTH)
            ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
            }
           ,begin
                WithNull = kz_json:set_value(Path, 'null', LeftJObj, #{'keep_null' => 'true'}),
                Merged = kz_json:merge(fun kz_json:merge_left/3, WithNull, RightJObj, #{'keep_null' => 'true'}),


                ?WHENFAIL(begin
                              ?debugFmt("merging nulls failed on ~p:~nnull: ~p~nright: ~p~nmerged: ~p~n"
                                       ,[Path, WithNull, RightJObj, Merged]
                                       ),
                              start_debug(kz_json),
                              kz_json:merge(fun kz_json:merge_left/3, WithNull, RightJObj, #{'keep_null' => 'true'}),
                              stop_debug()
                          end
                         ,kz_json:get_value(Path, WithNull) =:= 'null'
                          andalso kz_json:get_value(Path, Merged) =:= 'null'
                         )
            end
           ).

start_debug(M) ->
    dbg:start(),
    dbg:tracer(),

    dbg:tpl(M, [{'_', [], [$_]}]),
    dbg:p(all, c).

stop_debug() ->
    dbg:stop_clear(),
    dbg:stop().

%% Once-failing tests found by PropEr
merge_left_test_() ->
    JObjs = [{kz_json:from_list([{<<"foo">>, kz_json:new()}])
             ,kz_json:set_value([<<"foo">>, <<"bar">>], <<"baz">>, kz_json:new())
             }
            ,{kz_json:from_list([{<<164,157,198,86>>,<<>>}
                                ,{<<141,91,80,224,4,15,58>>,<<123,90,22,250,127,8>>}
                                ,{<<89,32,154,252,169,163,159>>,<<12,20,213,203>>}
                                ,{<<"ï¿½ï¿½F">>,false}
                                ,{<<158,113,31,162,148>>,kz_json:new()}
                                ,{<<"ï¿½">>,kz_json:new()}
                                ]
                               )
             ,kz_json:from_list([{<<"ï¿½">>, kz_json:from_list([{<<143,81,222,182,5>>,<<>>}
                                                               ,{<<181,191,27,138,73,202>>,<<>>}
                                                               ,{<<52,84,188,214,154,82>>,kz_json:new()}
                                                               ])
                                 }
                                ])
             }
            ],
    [[?_assert(are_all_properties_found(kz_json:merge(fun kz_json:merge_left/2, Left, Right)
                                       ,Left
                                       )
              )
     ,?_assert(are_all_properties_found(kz_json:merge(fun kz_json:merge_left/2, Right, Left)
                                       ,Right
                                       )
              )
     ]
     || {Left, Right} <- JObjs
    ].

prop_to_map() ->
    ?FORALL(JObj
           ,resize(?MAX_OBJECT_DEPTH, kz_json_generators:deep_object())
           ,?WHENFAIL(?debugFmt("failed json->map->json on ~p~n", [JObj])
                     ,kz_json:are_equal(JObj, kz_json:from_map(kz_json:to_map(JObj)))
                     )
           ).

are_all_properties_found(Merged, Favored) ->
    kz_json:all(fun({K,V}) -> is_property_found(K, V, Merged) end, Favored).

is_property_found(Key, ?JSON_WRAPPER(_)=Value, Merged) ->
    case kz_json:get_value(Key, Merged) of
        ?JSON_WRAPPER(_)=MergedV -> are_all_properties_found(Value, MergedV);
        Missing ->
            log_failure(Key, Value, Missing),
            'false'
    end;
is_property_found(Key, Value, Merged) ->
    case kz_json:get_value(Key, Merged) of
        Value -> 'true';
        Missing ->
            log_failure(Key, Value, Missing),
            'false'
    end.

log_failure(Key, Value, Missing) ->
    ?debugFmt("failed to find ~p~nexpected: ~p~nfound: ~p~n", [Key, Value, Missing]).

-endif.

lift_empty_test_() ->
    Empty = kz_json:new(),
    ?_assertEqual({Empty, [Empty, Empty]}
                 ,kz_json:lift_common_properties([Empty, Empty])
                 ).

lift_common_properties_test_() ->
    CommonProperties = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                         ,{<<"key">>, <<"value">>}
                                         ]),
    UniqueProperties = kz_json:from_list([{<<"bing">>, <<"bang">>}]),

    Merged = kz_json:merge(CommonProperties, UniqueProperties),

    ?_assertEqual({CommonProperties, [kz_json:new(), UniqueProperties]}
                 ,kz_json:lift_common_properties([CommonProperties, Merged])
                 ).

lift_common_properties_blacklist_test_() ->
    CommonProperties = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                         ,{<<"key">>, <<"value">>}
                                         ]),
    UniqueProperties = kz_json:from_list([{<<"bing">>, <<"bang">>}]),
    Blacklist = [<<"foo">>],

    Merged = kz_json:merge(CommonProperties, UniqueProperties),

    {Common, Endpoints} = kz_json:lift_common_properties([CommonProperties, Merged], Blacklist),

    [?_assert(kz_json:are_equal(Common, kz_json:delete_key(Blacklist, CommonProperties)))
    ,?_assertEqual(Endpoints
                  ,[kz_json:from_list([{<<"foo">>, <<"bar">>}])
                   ,kz_json:from_list([{<<"foo">>, <<"bar">>}
                                      ,{<<"bing">>, <<"bang">>}
                                      ])
                   ]
                  )
    ].

lift_common_properties_nested_blacklist_test_() ->
    CommonProperties = kz_json:set_values([{[<<"foo">>, <<"fang">>], <<"bar">>}
                                          ,{[<<"foo">>, <<"fong">>], <<"bor">>}
                                          ]
                                         ,kz_json:new()
                                         ),

    Blacklist = [ [<<"foo">>, <<"fang">>] ],

    {Common, Endpoints} = kz_json:lift_common_properties([CommonProperties, CommonProperties]
                                                        ,Blacklist
                                                        ),

    [?_assert(kz_json:are_equal(Common, kz_json:set_values([{[<<"foo">>, <<"fong">>], <<"bor">>}], kz_json:new())))
    ,?_assertEqual(Endpoints
                  ,[kz_json:set_values([{[<<"foo">>, <<"fang">>], <<"bar">>}], kz_json:new())
                   ,kz_json:set_values([{[<<"foo">>, <<"fang">>], <<"bar">>}], kz_json:new())
                   ]
                  )
    ].


proper_findings_lift_1_test_() ->
    JObj = kz_json:from_list([{<<0>>,'false'}]),
    {C, Es} = kz_json:lift_common_properties([JObj, JObj], [<<0>>]),
    [?_assert(kz_json:are_equal(kz_json:new(), C))
     | [?_assert(kz_json:are_equal(E, JObj)) || E <- Es]
    ].

-define(D1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}
                          ,{<<"d1k2">>, <<"d1v2">>}
                          ,{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}
                          ])).
-define(D1_MERGE, ?JSON_WRAPPER([{<<"d1k1">>, <<"d2k2">>}
                                ,{<<"d1k2">>, <<"d1v2">>}
                                ,{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}
                                ])).
-define(D2, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                          ,{<<"d2k2">>, 3.14}
                          ,{<<"sub_d1">>, ?D1}
                          ])).
-define(D2_MERGE, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                                ,{<<"d2k2">>, 3.14}
                                ,{<<"sub_d1">>, ?D1_MERGE}
                                ,{<<"blip">>, ?JSON_WRAPPER([{<<"blop">>, null}])}
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

-define(SP, kz_json:decode(<<"{\"plan\":{\"phone_numbers\":{\"did_us\":{\"discounts\":{\"cumulative\":{\"rate\":1}}}}}}">>)).
-define(O, kz_json:decode(<<"{\"phone_numbers\":{\"did_us\":{\"discounts\":{\"cumulative\":{\"rate\":20}}}}}">>)).

get_first_defined_test_() ->
    Paths = [{<<"d1v1">>, [<<"d1k1">>, <<"d1k2">>]}
            ,{<<"d1v2">>, [<<"d1k2">>, <<"d1k1">>]}
            ,{'undefined', [<<"nope">>, <<"definitely_nope">>]}
            ,{'undefined', []}
            ,{<<"d1v1">>, [<<"nope">>, <<"d1k1">>]}
            ],
    [?_assertEqual('undefined', kz_json:get_first_defined([<<"foo">>, <<"bar">>], kz_json:new()))
     |
     [?_assertEqual(V, kz_json:get_first_defined(Path, ?D1))
      || {V, Path} <- Paths
     ]
    ].

find_test_() ->
    KVs = [{<<"d1v1">>, <<"d1k1">>}
          ,{1, <<"d2k1">>}
          ,{<<"d3v1">>, <<"d3k1">>}
          ,{'undefined', <<"foo">>}
          ,{'undefined', [<<"d3k1">>, <<"foo">>]}
          ],
    [?_assertEqual(V, kz_json:find(K, ?D4)) || {V, K} <- KVs].

merge_recursive_overrides_test_() ->
    Key = [<<"plan">>, <<"phone_numbers">>, <<"did_us">>, <<"discounts">>, <<"cumulative">>, <<"rate">>],
    DefaultPred = fun(_, _) -> 'true' end,
    CustomPred = fun(1, 20) -> 'false';
                    (_, _) -> 'true'
                 end,

    JObj1 = ?SP,
    JObj2 = kz_json:from_list([{<<"plan">>, ?O}]),

    Merge1Arg = kz_json:merge_recursive([JObj1, JObj2]),

    MergeTrue2Args = kz_json:merge_recursive([JObj1, JObj2], DefaultPred),
    MergeCustom2Args = kz_json:merge_recursive([JObj1, JObj2], CustomPred),
    MergeDefault2Args = kz_json:merge_recursive(JObj1, JObj2),

    MergeTrue3Args = kz_json:merge_recursive(JObj1, JObj2, DefaultPred),
    MergeCustom3Args = kz_json:merge_recursive(JObj1, JObj2, CustomPred),

    [?_assertEqual(1, kz_json:get_value(Key, JObj1))
    ,?_assertEqual(20, kz_json:get_value(Key, JObj2))
    ,?_assertEqual(20, kz_json:get_value(Key, Merge1Arg))
    ,?_assertEqual(20, kz_json:get_value(Key, MergeTrue2Args))
    ,?_assertEqual(20, kz_json:get_value(Key, MergeDefault2Args))
    ,?_assertEqual(20, kz_json:get_value(Key, MergeTrue3Args))
    ,?_assertEqual(20, kz_json:get_value(Key, kz_json:merge(JObj1, JObj2)))
    ,?_assertEqual(1, kz_json:get_value(Key, MergeCustom2Args))
    ,?_assertEqual(1, kz_json:get_value(Key, MergeCustom3Args))
    ].

merge_recursive_options_test_() ->
    Key = [<<"plan">>, <<"phone_numbers">>, <<"did_us">>, <<"discounts">>, <<"cumulative">>, <<"rate">>],
    Options = #{'keep_null' => 'true'},
    CustomPred = fun(1, 'null') -> 'false';
                    (_, _) -> 'true'
                 end,

    JObj1 = ?SP,
    JObj2 = kz_json:set_value(Key, 'null', kz_json:from_list([{<<"plan">>, ?O}]), Options),

    Merge1Arg = kz_json:merge_recursive([JObj1, JObj2]),

    Merge2Args = kz_json:merge_recursive([JObj1, JObj2], Options),
    MergeDefault2Args = kz_json:merge_recursive(JObj1, JObj2),

    Merge3Args = kz_json:merge_recursive(JObj1, JObj2, Options),
    MergeCustom3Args = kz_json:merge_recursive([JObj1, JObj2], CustomPred, Options),

    [?_assertEqual(1, kz_json:get_value(Key, JObj1))
    ,?_assertEqual('null', kz_json:get_value(Key, JObj2))
    ,?_assertEqual('undefined', kz_json:get_value(Key, Merge1Arg))
    ,?_assertEqual('null', kz_json:get_value(Key, Merge2Args))
    ,?_assertEqual('undefined', kz_json:get_value(Key, MergeDefault2Args))
    ,?_assertEqual('null', kz_json:get_value(Key, Merge3Args))
    ,?_assertEqual(1, kz_json:get_value(Key, MergeCustom3Args))
    ].

merge_overrides_test_() ->
    %% default merges left onto right
    Left = kz_json:merge(fun kz_json:merge_left/2, ?SP, kz_json:from_list([{<<"plan">>, ?O}])),
    Right = kz_json:merge(fun kz_json:merge_right/2, ?SP, kz_json:from_list([{<<"plan">>, ?O}])),

    Key = [<<"plan">>, <<"phone_numbers">>, <<"did_us">>, <<"discounts">>, <<"cumulative">>, <<"rate">>],

    [?_assertEqual(1, kz_json:get_value(Key, ?SP))
    ,?_assertEqual(20, kz_json:get_value(Key, Right))
    ,?_assertEqual(1, kz_json:get_value(Key, Left))
    ].

is_empty_test_() ->
    [?_assertEqual('true', kz_json:is_empty(kz_json:new()))
    ,?_assertEqual('false', kz_json:is_empty(?D1))
    ,?_assertEqual('false', kz_json:is_empty(?D6))
    ,?_assertEqual('false', kz_json:is_empty(123))
    ,?_assertEqual('false', kz_json:is_empty(<<"foobar">>))
    ,?_assertEqual('false', kz_json:is_empty([{'bar', 'bas'}]))
    ].

merge_jobjs_test_() ->
    JObj = kz_json:merge_jobjs(?D1, ?D2),
    [?_assertEqual('true', 'undefined' =/= kz_json:get_value(<<"d1k1">>, JObj))
    ,?_assertEqual('true', 'undefined' =/= kz_json:get_value(<<"d2k1">>, JObj))
    ,?_assertEqual('true', 'undefined' =/= kz_json:get_value(<<"sub_d1">>, JObj))
    ,?_assertEqual('true', 'undefined' =:= kz_json:get_value(<<"missing_k">>, JObj))
    ].

merge_with_merge_fun_test_() ->
    JObj1 =
        kz_json:from_list(
          [{<<"key_a">>, <<"value_1">>}
          ,{<<"key_b">>, <<"value_2">>}
          ,{<<"key_c">>, kz_json:new()}
          ,{<<"key_d">>, <<"value_4">>}
          ]
         ),
    JObj2 =
        kz_json:from_list(
          [{<<"key_a">>, <<"value_5">>}
          ,{<<"key_b">>, <<"value_6">>}
          ,{<<"key_c">>
           ,kz_json:from_list(
              [{<<"sub_key_a">>, <<"sub_value_1">>}
              ,{<<"sub_key_b">>, <<"sub_value_2">>}
              ]
             )
           }
          ,{<<"key_e">>, <<"value_7">>}
          ,{<<"key_f">>, <<"value_8">>}
          ]
         ),
    JObj3 =
        kz_json:from_list(
          [{<<"key_a">>, <<"value_9">>}
          ,{<<"key_b">>, <<"value_10">>}
          ,{<<"key_c">>
           ,kz_json:from_list(
              [{<<"sub_key_b">>, <<"sub_value_3">>}
              ,{<<"sub_key_c">>, <<"sub_value_4">>}
              ]
             )
           }
          ,{<<"key_h">>, <<"value_11">>}
          ,{<<"key_j">>, <<"value_12">>}
          ]
         ),
    RightMerge =
        kz_json:from_list(
          [{<<"key_f">>, <<"value_8">>}
          ,{<<"key_e">>, <<"value_7">>}
          ,{<<"key_d">>, <<"value_4">>}
          ,{<<"key_c">>
           ,kz_json:from_list(
              [{<<"sub_key_b">>, <<"sub_value_2">>}
              ,{<<"sub_key_a">>, <<"sub_value_1">>}
              ]
             )
           }
          ,{<<"key_b">>, <<"value_6">>}
          ,{<<"key_a">>, <<"value_5">>}
          ]
         ),
    LeftMerge =
        kz_json:from_list(
          [{<<"key_f">>, <<"value_8">>}
          ,{<<"key_e">>, <<"value_7">>}
          ,{<<"key_d">>, <<"value_4">>}
          ,{<<"key_c">>, kz_json:new()}
          ,{<<"key_b">>, <<"value_2">>}
          ,{<<"key_a">>, <<"value_1">>}
          ]
         ),
    MultipleRightMerge =
        kz_json:from_list(
          [{<<"key_j">>, <<"value_12">>}
          ,{<<"key_h">>, <<"value_11">>}
          ,{<<"key_f">>, <<"value_8">>}
          ,{<<"key_e">>, <<"value_7">>}
          ,{<<"key_d">>, <<"value_4">>}
          ,{<<"key_c">>
           ,kz_json:from_list(
              [{<<"sub_key_c">>, <<"sub_value_4">>}
              ,{<<"sub_key_b">>, <<"sub_value_3">>}
              ,{<<"sub_key_a">>, <<"sub_value_1">>}
              ]
             )
           }
          ,{<<"key_b">>, <<"value_10">>}
          ,{<<"key_a">>, <<"value_9">>}
          ]
         ),
    [?_assertEqual(RightMerge
                  ,kz_json:merge([JObj1, JObj2])
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge([JObj2, JObj1])
                  )
    ,?_assertEqual(MultipleRightMerge
                  ,kz_json:merge([JObj1, JObj2, JObj3])
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(JObj1, JObj2)
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(JObj2, JObj1)
                  )
    ,?_assertEqual(MultipleRightMerge
                  ,kz_json:merge(fun kz_json:merge_right/2, [JObj1, JObj2, JObj3])
                  )
    ,?_assertEqual(MultipleRightMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, [JObj1, JObj2, JObj3])
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/2, JObj1, JObj2)
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, JObj1, JObj2)
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_right/2, JObj2, JObj1)
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, JObj2, JObj1)
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_left/2, JObj1, JObj2)
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_left/3, JObj1, JObj2)
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/2, JObj1, JObj2)
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, JObj1, JObj2)
                  )
    ,?_assertEqual(MultipleRightMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, [JObj1, JObj2, JObj3], kz_json:merge_options())
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/2, JObj1, JObj2, kz_json:merge_options())
                  )
    ,?_assertEqual(RightMerge
                  ,kz_json:merge(fun kz_json:merge_right/3, JObj1, JObj2, kz_json:merge_options())
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_left/2, JObj1, JObj2, kz_json:merge_options())
                  )
    ,?_assertEqual(LeftMerge
                  ,kz_json:merge(fun kz_json:merge_left/3, JObj1, JObj2, kz_json:merge_options())
                  )
    ].

merge_with_null_test_() ->
    NullKeys = [[<<"foo">>, <<"bar">>]
               ,[<<"foobar">>]
               ],
    Options = #{'keep_null' => 'true'},
    NullD2 = kz_json:set_values([{NullKey, 'null'}
                                 || NullKey <- NullKeys
                                ]
                               ,?D2
                               ,Options
                               ),
    [?_assertEqual('true'
                  ,lists:all(fun(NullKey) ->
                                     kz_json:get_value(NullKey, NullD2) =:= 'null'
                             end
                            ,NullKeys
                            )
                  )
    ,?_assertEqual('true'
                  ,kz_json:all(fun({Key, Value}) ->
                                       (not lists:member(Key, NullKeys))
                                           orelse
                                           Value =:= 'null'
                               end
                              ,kz_json:merge([?D1, NullD2], Options)
                              )
                  )
    ,?_assertEqual('true'
                  ,kz_json:all(fun({Key, Value}) ->
                                       (not lists:member(Key, NullKeys))
                                           orelse
                                           Value =:= 'null'
                               end
                              ,kz_json:merge(?D1, NullD2, Options)
                              )
                  )
    ,?_assertEqual('true'
                  ,kz_json:all(fun({Key, Value}) ->
                                       (not lists:member(Key, NullKeys))
                                           orelse
                                           Value =:= 'null'
                               end
                              ,kz_json:merge(fun kz_json:merge_right/3, ?D1, NullD2, Options)
                              )
                  )
    ].

merge_recursive_test_() ->
    Base = kz_json:set_value([<<"blip">>, <<"blop">>], 42, ?D2),
    New = ?D2_MERGE,
    JObj = kz_json:merge_recursive(Base, New),
    JObj1 = kz_json:merge_recursive([Base, New]),
    lists:flatmap(fun do_merge_recursive/1, [JObj, JObj1]).

merge_test_() ->
    Base = kz_json:set_value([<<"blip">>, <<"blop">>], 42, ?D2),
    New = ?D2_MERGE,
    JObj = kz_json:merge(Base, New),
    JObj1 = kz_json:merge([Base, New]),
    lists:flatmap(fun do_merge_recursive/1, [JObj, JObj1]).

-spec do_merge_recursive(kz_json:object()) -> list().
do_merge_recursive(J) ->
    [?_assert(kz_json:is_valid_json_object(J))
    ,?_assertEqual('undefined', kz_json:get_value(<<"d1k1">>, J))
    ,?_assertEqual(1, kz_json:get_value(<<"d2k1">>, J))

    ,?_assert('undefined' =/= kz_json:get_value(<<"sub_d1">>, J))

     %% second JObj takes precedence
    ,?_assertEqual(<<"d2k2">>,  kz_json:get_value([<<"sub_d1">>, <<"d1k1">>], J))
    ,?_assertEqual('undefined', kz_json:get_value(<<"missing_k">>, J))

    ,?_assert(kz_json:is_empty(kz_json:get_value(<<"blip">>, J)))
    ].

get_binary_value_test_() ->
    [?_assert(is_binary(kz_json:get_binary_value(<<"d1k1">>, ?D1)))
    ,?_assertEqual('undefined', kz_json:get_binary_value(<<"d2k1">>, ?D1))
    ,?_assert(is_binary(kz_json:get_binary_value(<<"d1k1">>, ?D1, <<"something">>)))
    ,?_assertEqual(<<"something">>, kz_json:get_binary_value(<<"d2k1">>, ?D1, <<"something">>))
    ].

get_integer_value_test_() ->
    [?_assertEqual(1, kz_json:get_integer_value(<<"d2k1">>, ?D2))
    ,?_assertEqual('undefined', kz_json:get_integer_value(<<"d1k1">>, ?D2))
    ,?_assertEqual(1, kz_json:get_integer_value(<<"d2k1">>, ?D2, 0))
    ,?_assertEqual(0, kz_json:get_integer_value(<<"d1k1">>, ?D2, 0))
    ].

get_float_value_test_() ->
    [?_assert(is_float(kz_json:get_float_value(<<"d2k2">>, ?D2)))
    ,?_assertEqual('undefined', kz_json:get_float_value(<<"d1k1">>, ?D2))
    ,?_assertEqual(3.14, kz_json:get_float_value(<<"d2k2">>, ?D2, 0.0))
    ,?_assertEqual(0.0, kz_json:get_float_value(<<"d1k1">>, ?D2, 0.0))
    ].

get_binary_boolean_test_() ->
    [?_assertEqual('undefined', kz_json:get_binary_boolean(<<"d1k1">>, ?D2))
    ,?_assertEqual(<<"false">>, kz_json:get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}])))
    ,?_assertEqual(<<"true">>, kz_json:get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}])))
    ].

is_false_test_() ->
    [?_assertEqual('false', kz_json:is_false(<<"d1k1">>, ?D1))
    ,?_assertEqual('true', kz_json:is_false(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}])))
    ].

is_true_test_() ->
    [?_assertEqual('false', kz_json:is_true(<<"d1k1">>, ?D1))
    ,?_assertEqual('true', kz_json:is_true(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}])))
    ].

-define(D1_FILTERED, ?JSON_WRAPPER([{<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D2_FILTERED, ?JSON_WRAPPER([{<<"sub_d1">>, ?D1}])).
-define(D3_FILTERED, ?JSON_WRAPPER([{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?D1, ?D2_FILTERED]}])).
filter_test_() ->
    [?_assertEqual(?D1_FILTERED, kz_json:filter(fun({<<"d1k1">>, _}) -> 'false'; (_) -> 'true' end, ?D1))
    ,?_assertEqual(?D2_FILTERED, kz_json:filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D2))
    ,?_assertEqual(?D3_FILTERED, kz_json:filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D3, [<<"sub_docs">>, 2]))
    ].

new_test_() ->
    [?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:new())].

is_valid_json_object_test_() ->
    [?_assertEqual('false', kz_json:is_valid_json_object('foo'))
    ,?_assertEqual('false', kz_json:is_valid_json_object(123))
    ,?_assertEqual('false', kz_json:is_valid_json_object(['boo', 'yah']))
    ,?_assertEqual('false', kz_json:is_valid_json_object(<<"bin">>))

    ,?_assertEqual('true', kz_json:is_valid_json_object(?D1))
    ,?_assertEqual('true', kz_json:is_valid_json_object(?D2))
    ,?_assertEqual('true', kz_json:is_valid_json_object(?D3))
    ,?_assertEqual('true', lists:all(fun kz_json:is_valid_json_object/1, ?D4))
    ,?_assertEqual('true', kz_json:is_valid_json_object(?D6))
    ,?_assertEqual('true', kz_json:is_valid_json_object(?D7))
    ].

%% delete results
-define(D1_AFTER_K1, ?JSON_WRAPPER([{<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D1_AFTER_K3_V2, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.3">>]}])).
-define(D1_AFTER_K3_V2_PRUNE, ?JSON_WRAPPER([{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.3">>]}, {<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}])).

-define(D6_AFTER_SUB, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                                    ,{<<"d2k2">>, 3.14}
                                    ,{<<"sub_d1">>, ?EMPTY_JSON_OBJECT}
                                    ]
                                   )).
-define(D6_AFTER_SUB_PRUNE, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                                          ,{<<"d2k2">>, 3.14}
                                          ]
                                         )).

-define(P1, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P2, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?JSON_WRAPPER(?P1)}]).
-define(P3, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?JSON_WRAPPER(?P1), ?JSON_WRAPPER(?P2)]}]).
-define(P4, [?P1, ?P2, ?P3]).
-define(P6, [{<<"d2k1">>, 1},{<<"d2k2">>, 3.14},{<<"sub_d1">>, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])}]).
-define(P7, [{<<"d1k1">>, <<"d1v1">>}]).

-define(P8, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P9, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?P1}]).
-define(P10, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?P8, ?P9]}]).
-define(P11, [?P8, ?P9, ?P10]).
-define(P12, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14},{<<"sub_d1">>, [{<<"d1k1">>, <<"d1v1">>}]}]).
-define(P13, [{<<"d1k1">>, <<"d1v1">>}]).

%% deleting [k1, 1] should return empty json object
-define(D_ARR, ?JSON_WRAPPER([{<<"k1">>, [1]}])).
-define(P_ARR, ?JSON_WRAPPER([{<<"k1">>, []}])).

get_keys_test_() ->
    Keys = [<<"d1k1">>, <<"d1k2">>, <<"d1k3">>],
    [?_assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, kz_json:get_keys([], ?D1)))
    ,?_assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, kz_json:get_keys([<<"sub_docs">>, 1], ?D3)))
    ,?_assertEqual('true', lists:all(fun(K) -> lists:member(K, [1,2,3]) end, kz_json:get_keys([<<"sub_docs">>], ?D3)))
    ].

to_proplist_test_() ->
    [?_assertEqual(?P1, kz_json:to_proplist(?D1))
    ,?_assertEqual(?P2, kz_json:to_proplist(?D2))
    ,?_assertEqual(?P3, kz_json:to_proplist(?D3))
    ,?_assertEqual(?P4, lists:map(fun kz_json:to_proplist/1, ?D4))
    ,?_assertEqual(?P6, kz_json:to_proplist(?D6))
    ,?_assertEqual(?P7, kz_json:to_proplist(?D7))
    ].

recursive_to_proplist_test_() ->
    [?_assertEqual(?P8, kz_json:recursive_to_proplist(?D1))
    ,?_assertEqual(?P9, kz_json:recursive_to_proplist(?D2))
    ,?_assertEqual(?P10, kz_json:recursive_to_proplist(?D3))
    ,?_assertEqual(?P11, lists:map(fun kz_json:recursive_to_proplist/1, ?D4))
    ,?_assertEqual(?P12, kz_json:recursive_to_proplist(?D6))
    ,?_assertEqual(?P13, kz_json:recursive_to_proplist(?D7))
    ].

delete_key_test_() ->
    [?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT, 'prune'))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT, 'prune'))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT, 'prune'))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"d1k1">>], ?D7))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"d1k1">>], ?D7, 'prune'))
    ,?_assertEqual(?D1_AFTER_K1, kz_json:delete_key([<<"d1k1">>], ?D1))
    ,?_assertEqual(?D1_AFTER_K1, kz_json:delete_key([<<"d1k1">>], ?D1, 'prune'))
    ,?_assertEqual(?D1_AFTER_K3_V2, kz_json:delete_key([<<"d1k3">>, 2], ?D1))
    ,?_assertEqual(?D1_AFTER_K3_V2_PRUNE, kz_json:delete_key([<<"d1k3">>, 2], ?D1, 'prune'))
    ,?_assertEqual(?D6_AFTER_SUB, kz_json:delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6))
    ,?_assertEqual(?D6_AFTER_SUB_PRUNE, kz_json:delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6, 'prune'))
    ,?_assertEqual(?P_ARR, kz_json:delete_key([<<"k1">>, 1], ?D_ARR))
    ,?_assertEqual(?EMPTY_JSON_OBJECT, kz_json:delete_key([<<"k1">>, 1], ?D_ARR, 'prune'))

    ,?_assertError('badarg', kz_json:delete_key([<<"foo">>, <<"bar">>], kz_json:from_list([{<<"foo">>, 5}])))
    ,?_assertError('badarg', kz_json:delete_key([<<"foo">>, <<"bar">>], kz_json:from_list([{<<"foo">>, 5}]), 'prune'))
    ].

get_value_test_() ->
    %% Basic first level key
    [?_assertEqual('undefined', kz_json:get_value([<<"d1k1">>], ?EMPTY_JSON_OBJECT))
    ,?_assertEqual(<<"d1v1">>, kz_json:get_value([<<"d1k1">>], ?D1))
    ,?_assertEqual('undefined', kz_json:get_value([<<"d1k1">>], ?D2))
    ,?_assertEqual('undefined', kz_json:get_value([<<"d1k1">>], ?D3))
    ,?_assertEqual('undefined', kz_json:get_value([<<"d1k1">>], ?D4))
     %% Basic nested key
    ,?_assertEqual('undefined', kz_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?EMPTY_JSON_OBJECT))
    ,?_assertEqual('undefined', kz_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D1))
    ,?_assertEqual(<<"d1v2">>,      kz_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D2))
    ,?_assertEqual('undefined', kz_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D3))
    ,?_assertEqual('undefined', kz_json:get_value([<<"sub_d1">>, <<"d1k2">>], ?D4))
     %% Get the value in an object in an array in another object that is part of
     %% an array of objects
    ,?_assertEqual('undefined', kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?EMPTY_JSON_OBJECT))
    ,?_assertEqual('undefined', kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D1))
    ,?_assertEqual('undefined', kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D2))
    ,?_assertEqual('undefined', kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D3))
    ,?_assertEqual(3.14,      kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4))
     %% Get the value in an object in an array in another object that is part of
     %% an array of objects, but change the default return if it is not present.
     %% Also tests the ability to have indexes represented as strings
    ,?_assertEqual(<<"not">>, kz_json:get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], [], <<"not">>))
    ,?_assertEqual(3.14,      kz_json:get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4, <<"not">>))
     %% Reading from non JObj should not be okay
    ,?_assertError(badarg, kz_json:get_value(<<"a">>, not_json))

    ,?_assertEqual('undefined'
                  ,kz_json:get_value([<<"en-us">>, <<"label">>], ?D1)
                  )
    ,?_assertEqual(<<"boom">>
                  ,kz_json:get_value([<<"en-us">>, <<"label">>], ?D1, <<"boom">>)
                  )
    ].

take_value_test_() ->
    Tests = [{'false', <<"foo">>, kz_json:new()}
            ,{{'value', <<"bar">>, kz_json:new()}
             ,<<"foo">>
             ,kz_json:from_list([{<<"foo">>, <<"bar">>}])
             }
            ,{'false'
             ,<<"fuu">>
             ,kz_json:from_list([{<<"foo">>, <<"bar">>}])
             }
            ,{'false'
             ,[<<"foo">>, <<"fuu">>]
             ,kz_json:from_list([{<<"foo">>, kz_json:from_list([{<<"fii">>, <<"bar">>}])}])
             }
            ,{{'value', <<"bar">>, kz_json:from_list([{<<"foo">>, kz_json:new()}])}
             ,[<<"foo">>, <<"fuu">>]
             ,kz_json:from_list([{<<"foo">>, kz_json:from_list([{<<"fuu">>, <<"bar">>}])}])
             }
            ],
    [?_assertEqual(Result, kz_json:take_value(Key, JObj))
     || {Result, Key, JObj} <- Tests
    ].

-define(T2R1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"update">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R2, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, <<"new_value">>}])).
-define(T2R3, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, ?JSON_WRAPPER([{<<"new_key">>, <<"added_value">>}])}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R4, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"d1v2">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, ?JSON_WRAPPER([{<<"new_key">>, <<"added_value">>}])}])).

set_value_object_test_() ->
    %% Test setting an existing key
    [?_assertEqual(?T2R1, kz_json:set_value([<<"d1k2">>], <<"update">>, ?D1))
     %% Test setting a non-existing key
    ,?_assertEqual(?T2R2, kz_json:set_value([<<"d1k4">>], <<"new_value">>, ?D1))
     %% Test setting an existing key followed by a nonexistent key
    ,?_assertEqual(?T2R3, kz_json:set_value([<<"d1k2">>, <<"new_key">>], <<"added_value">>, ?D1))
     %% Test setting a non-existing key followed by another nonexistent key
    ,?_assertEqual(?T2R4, kz_json:set_value([<<"d1k4">>, <<"new_key">>], <<"added_value">>, ?D1))
    ].

-define(D5,   [?JSON_WRAPPER([{<<"k1">>, <<"v1">>}]), ?JSON_WRAPPER([{<<"k2">>, <<"v2">>}])]).
-define(T3R1, [?JSON_WRAPPER([{<<"k1">>, <<"test">>}]),?JSON_WRAPPER([{<<"k2">>,<<"v2">>}])]).
-define(T3R2, [?JSON_WRAPPER([{<<"k1">>, <<"v1">>},{<<"pi">>, 3.14}]),?JSON_WRAPPER([{<<"k2">>,<<"v2">>}])]).
-define(T3R3, [?JSON_WRAPPER([{<<"k1">>,<<"v1">>},{<<"callerid">>,?JSON_WRAPPER([{<<"name">>,<<"2600Hz">>}])}]),?JSON_WRAPPER([{<<"k2">>,<<"v2">>}])]).
-define(T3R4, [?JSON_WRAPPER([{<<"k1">>,<<"v1">>}]),?JSON_WRAPPER([{<<"k2">>,<<"updated">>}])]).
-define(T3R5, [?JSON_WRAPPER([{<<"k1">>,<<"v1">>}]),?JSON_WRAPPER([{<<"k2">>,<<"v2">>}]),?JSON_WRAPPER([{<<"new_key">>,<<"added">>}])]).

set_value_multiple_object_test_() ->
    %% Set an existing key in the first kz_json:object()
    [?_assertEqual(?T3R1, kz_json:set_value([1, <<"k1">>], <<"test">>, ?D5))
     %% Set a non-existing key in the first kz_json:object()
    ,?_assertEqual(?T3R2, kz_json:set_value([1, <<"pi">>], 3.14, ?D5))
     %% Set a non-existing key followed by another nonexistent key in the first kz_json:object()
    ,?_assertEqual(?T3R3, kz_json:set_value([1, <<"callerid">>, <<"name">>], <<"2600Hz">>, ?D5))
     %% Set an existing key in the second kz_json:object()
    ,?_assertEqual(?T3R4, kz_json:set_value([2, <<"k2">>], <<"updated">>, ?D5))
     %% Set a non-existing key in a non-existing kz_json:object()
    ,?_assertEqual(?T3R5, kz_json:set_value([3, <<"new_key">>], <<"added">>, ?D5))
    ].

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
-define(T5R3,  ?JSON_WRAPPER([{<<"Node-1">>, ?JSON_WRAPPER([{<<"Node-2">>, null}])}, {<<"Another-Node">>, ?T4R1 }] )).
-define(T5R3V, ?JSON_WRAPPER([{<<"node_1">>, ?JSON_WRAPPER([                    ])}, {<<"another_node">>, ?T4R1V}] )).

set_value_normalizer_test_() ->
    %% Normalize a flat JSON object
    [?_assertEqual(kz_json:normalize_jobj(?T4R1), ?T4R1V)
     %% Normalize a single nested JSON objects
    ,?_assertEqual(kz_json:normalize_jobj(?T4R2), ?T4R2V)
     %% Normalize multiple nested JSON objects
    ,?_assertEqual(kz_json:normalize_jobj(?T4R3), ?T4R3V)
    ,?_assertEqual(kz_json:normalize_jobj(?T5R3), ?T5R3V)

    ,?_assertEqual(kz_json:normalize_jobj(?T5R1), ?T5R1V)
    ].

-define(D1_values, [<<"d1v1">>
                   ,<<"d1v2">>
                   ,[<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]
                   ]).

get_values_test_() ->
    {Values, Keys} = kz_json:get_values(?D1),
    are_all_there(Values
                 ,Keys
                 ,?D1_values
                 ,kz_json:get_keys(?D1)
                 ).

values_test_() ->
    Values = kz_json:values(?D1),
    are_all_there(Values, [], ?D1_values, []).

are_all_there(Values, Keys, Vs, Ks) ->
    [?_assert(lists:member(K, Keys)) || K <- Ks]
        ++ [?_assert(lists:member(V, Values)) || V <- Vs].

-define(K3_JOBJ, ?JSON_WRAPPER([{<<"k3.1">>, <<"v3.1">>}])).
-define(CODEC_JOBJ, ?JSON_WRAPPER([{<<"k1">>, <<"v1">>}
                                  ,{<<"k2">>, ?EMPTY_JSON_OBJECT}
                                  ,{<<"k3">>, ?K3_JOBJ}
                                  ,{<<"k4">>, [1,2,3]}
                                  ])).
-define(PROPS_WITH_UNDEFINED, [{<<"a">>, 42}
                              ,{<<"b">>, undefined}
                              ]).

codec_test_() ->
    [?_assertEqual(?CODEC_JOBJ, kz_json:decode(kz_json:encode(?CODEC_JOBJ)))
    ,?_assertException(error,{invalid_ejson,undefined}, kz_json:encode(undefined))
    ,?_assertException(error,{invalid_ejson,undefined}, kz_json:encode(?JSON_WRAPPER(?PROPS_WITH_UNDEFINED)))
    ,?_assert(kz_json:are_equal(kz_json:from_list(?PROPS_WITH_UNDEFINED)
                               ,kz_json:decode(kz_json:encode(kz_json:from_list(?PROPS_WITH_UNDEFINED)))
                               ))
    ].

find_value_test_() ->
    JObjs = kz_json:decode(<<"[{\"k1\":\"v1\"},{\"k1\":\"v2\"}]">>),
    [?_assertEqual(<<"{\"k1\":\"v1\"}">>, kz_json:encode(kz_json:find_value(<<"k1">>, <<"v1">>, JObjs)))
    ,?_assertEqual(<<"{\"k1\":\"v2\"}">>, kz_json:encode(kz_json:find_value(<<"k1">>, <<"v2">>, JObjs)))
    ,?_assertEqual('undefined', kz_json:find_value(<<"k1">>, <<"v3">>, JObjs))
    ,?_assertEqual('undefined', kz_json:find_value([<<"k1">>, <<"k9">>], <<"v1">>, JObjs))
    ].

insert_value_test_() ->
    JObj = kz_json:decode(<<"{\"k1\":\"v1\",\"k2\":\"v2\"}">>),
    NonInsert = kz_json:insert_value(<<"k1">>, <<"v3">>, JObj),
    Insert = kz_json:insert_value(<<"k3">>, <<"v3">>, JObj),
    [?_assertEqual(<<"v1">>, kz_json:get_value(<<"k1">>, NonInsert))
    ,?_assertEqual(<<"v3">>, kz_json:get_value(<<"k3">>, Insert))
    ].

insert_values_test_() ->
    JObj = kz_json:decode(<<"{\"k1\":\"v1\",\"k2\":\"v2\"}">>),
    NonInsert = kz_json:insert_values([{<<"k1">>, <<"v3">>}], JObj),
    Insert = kz_json:insert_values([{<<"k3">>, <<"v3">>}], JObj),
    [?_assertEqual(<<"v1">>, kz_json:get_value(<<"k1">>, NonInsert))
    ,?_assertEqual(<<"v3">>, kz_json:get_value(<<"k3">>, Insert))
    ].

get_ne_json_object_test_() ->
    JObj = kz_json:decode(<<"{\"k1\":\"v1\",\"k2\":\"v2\",\"o1\":{\"k3\":\"v3\"},\"o2\":{}}">>),
    [?_assertEqual('undefined'
                  ,kz_json:get_ne_json_value(<<"k1">>, JObj)
                  )
    ,?_assertEqual(?JSON_WRAPPER([{<<"k3">>, <<"v3">>}])
                  ,kz_json:get_ne_json_value(<<"o1">>, JObj)
                  )
    ,?_assertEqual('undefined'
                  ,kz_json:get_ne_json_value(<<"o2">>, JObj)
                  )
    ].

-define(MAP_JSON,
        kz_json:from_list([{<<"a">>, <<"zero">>}
                          ,{<<"B">>, true}
                          ,{<<"c">>, [1
                                     ,2
                                     ,kz_json:from_list([{42, 24}
                                                        ,{<<"42">>, 24}
                                                        ])
                                     ]}
                          ])).

-define(JSON_MAP, #{<<"a">> => <<"zero">>
                   ,<<"B">> => true
                   ,<<"c">> => [1
                               ,2
                               ,#{<<"42">> => 24
                                 ,42 => 24
                                 }
                               ]
                   }).

to_map_test_() ->
    JObjs = [kz_json:from_list([{<<"A">>, 1}, {<<"B">>, 2}])
            ,kz_json:from_list([{<<"A">>, 8}, {<<"B">>, 9}])
            ],

    [?_assertEqual(?JSON_MAP, kz_json:to_map(?MAP_JSON))
    ,?_assert(kz_json:are_equal(?MAP_JSON, kz_json:from_map(?JSON_MAP)))

    ,?_assertEqual(?JSON_MAP, kz_json:to_map(kz_json:from_map(?JSON_MAP)))
    ,?_assert(kz_json:are_equal(?MAP_JSON, kz_json:from_map(kz_json:to_map(?MAP_JSON))))

    ,?_assertEqual([#{<<"A">> => 1,<<"B">> => 2}
                   ,#{<<"A">> => 8,<<"B">> => 9}
                   ]
                  ,kz_json:to_map(JObjs)
                  )
    ].

are_equal_test_() ->
    JObj = kz_json:from_map(kz_json:to_map(?MAP_JSON)),
    [?_assertEqual(true, kz_json:are_equal(?MAP_JSON, JObj))
    ,?_assertEqual(true, kz_json:are_equal(JObj, ?MAP_JSON))
    ,?_assertEqual(true, kz_json:are_equal(undefined, undefined))
    ,?_assertEqual(false, kz_json:are_equal(undefined, kz_json:new()))
    ,?_assertEqual(false, kz_json:are_equal(kz_json:new(), undefined))
    ].

-define(CHARGES_SIMPLE,
        kz_json:decode(<<"{\"phone_numbers\":{\"did_us\":{\"category\":\"phone_numbers\",\"item\":\"did_us\",\"name\":\"US DID\",\"quantity\":1,\"rate\":1.0,\"single_discount\":true,\"single_discount_rate\":0.0,\"cumulative_discount\":1,\"cumulative_discount_rate\":0.5,\"activation_charge\":2.0,\"minimum\":0,\"exceptions\":[],\"activation_charges\":2.0}},\"activation_charges\":2.0}">>)).
-define(CHARGES_DOUBLE,
        kz_json:decode(<<"{\"phone_numbers\":{\"did_us\":{\"category\":\"phone_numbers\",\"item\":\"did_us\",\"name\":\"US DID\",\"quantity\":2,\"rate\":2.0,\"single_discount\":true,\"single_discount_rate\":0.0,\"cumulative_discount\":2,\"cumulative_discount_rate\":1.0,\"activation_charge\":4.0,\"minimum\":0,\"exceptions\":[],\"activation_charges\":4.0}},\"activation_charges\":4.0}">>)).

-define(AMOUNTS,
        kz_json:decode(<<"[{\"mobile_data\":{\"amount\":-10.5,\"usage\":{\"type\":\"debit\",\"unit\":\"MB\",\"quantity\":9482}}},{\"per_minute_voip\":{\"amount\":-155,\"usage\":{\"type\":\"voice\",\"unit\":\"sec\",\"quantity\":2120}}},{\"per_minute_voip\":{\"amount\":37,\"usage\":{\"type\":\"voice\",\"unit\":\"sec\",\"quantity\":480}}}]">>)).

-define(AMOUNT,
        kz_json:decode(<<"{\"mobile_data\":{\"amount\":-10.5,\"usage\":{\"type\":\"debit\",\"unit\":\"MB\",\"quantity\":9482}},\"per_minute_voip\":{\"amount\":-118,\"usage\":{\"type\":\"voice\",\"unit\":\"sec\",\"quantity\":2600}}}">>)).

sum_test_() ->
    E = kz_json:new(),
    A42 = kz_json:from_list([{<<"a">>, 42}]),
    A40 = kz_json:from_list([{<<"a">>, 40}]),
    A2 = kz_json:from_list([{<<"a">>, 2}]),
    A42Bhi = kz_json:from_list([{<<"a">>, 42}, {<<"b">>, <<"hi">>}]),
    A2Bhi = kz_json:from_list([{<<"a">>, 2}, {<<"b">>, <<"hi">>}]),
    [?_assertEqual(E, kz_json:sum(E, E))
    ,?_assertEqual(A42, kz_json:sum(A42, E))
    ,?_assertEqual(A42, kz_json:sum(E, A42))
    ,?_assertEqual(A42Bhi, kz_json:sum(A42Bhi, E))
    ,?_assertEqual(A42Bhi, kz_json:sum(E, A42Bhi))
    ,?_assertEqual(A42, kz_json:sum(A40, A2))
    ,?_assertEqual(A42, kz_json:sum(A2, A40))
    ,?_assertEqual(A42Bhi, kz_json:sum(A40, A2Bhi))
    ,?_assertEqual(A42Bhi, kz_json:sum(A2Bhi, A40))
    ,?_assertEqual(?CHARGES_DOUBLE, kz_json:sum(?CHARGES_SIMPLE, ?CHARGES_SIMPLE))
    ,?_assertEqual(?CHARGES_DOUBLE, kz_json:sum_jobjs([?CHARGES_SIMPLE, ?CHARGES_SIMPLE]))
    ,?_assertEqual(E, kz_json:sum_jobjs([E, E]))
    ,?_assertEqual(kz_json:new(), kz_json:sum_jobjs([]))
    ,?_assertEqual(A42Bhi, kz_json:sum_jobjs([A40, A2Bhi]))
    ,?_assertEqual(A42Bhi, kz_json:sum_jobjs([A2Bhi, A40]))
    ,?_assert(kz_json:are_equal(?AMOUNT, kz_json:sum_jobjs(?AMOUNTS)))
    ].

order_by_test_() ->
    Unordered = [H1, H2|T] =
        [kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"3">>}]}])
        ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"5">>}]}])
        ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"1">>}]}])
        ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"2">>}]}])
        ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"4">>}]}])
        ],
    InOrder = [kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"1">>}]}])
              ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"2">>}]}])
              ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"3">>}]}])
              ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"4">>}]}])
              ,kz_json:from_list_recursive([{<<"a">>, [{<<"k">>, <<"5">>}]}])
              ],
    Ids = [<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>],
    [?_assertEqual([], kz_json:order_by([<<"a">>, <<"k">>], [], []))
    ,?_assertEqual([], kz_json:order_by([<<"a">>, <<"k">>], [], [[]]))
    ,?_assertEqual(InOrder, kz_json:order_by([<<"a">>, <<"k">>], Ids, [Unordered]))
    ,?_assertEqual(InOrder, kz_json:order_by([<<"a">>, <<"k">>], Ids, [[H1,H2], T]))
    ].

from_list_recursive_test_() ->
    Obj1 = kz_json:from_list([{<<"send_to">>, [<<"someone@somedomain.com">>]}]),
    Obj2 = kz_json:from_list([{<<"email">>, Obj1}]),
    L1 = [{<<"fax_hangup_codes">>, [200, 201, 202]}
         ,{<<"fax_hangup_cause">>, <<"NORMAL_CLEARING">>}
         ],
    L3 = [{<<"fax">>, [{<<"id">>, "201702-1b6cfec4e3ab0b7bb97ced78bf431f39"}
                      ,{<<"info">>, L1}
                      ,{<<"notifications">>, Obj2}
                      ]}],
    Obj3 = kz_json:from_list([{<<"id">>,<<"201702-1b6cfec4e3ab0b7bb97ced78bf431f39">>}
                             ,{<<"info">>, kz_json:from_list(L1)}
                             ,{<<"notifications">>, Obj2}
                             ]),
    JObj1 = kz_json:from_list([{<<"fax">>, Obj3}]),
    JObj2 = kz_json:from_list_recursive(L3),

    Key1 = [<<"fax">>, <<"info">>, <<"fax_hangup_codes">>],
    Key2 = [<<"fax">>, <<"notifications">>, <<"email">>, <<"send_to">>],

    [?_assertEqual(kz_json:get_list_value(Key1, JObj1), kz_json:get_list_value(Key1, JObj2))
    ,?_assertEqual(kz_json:get_ne_binary_value(Key2, JObj1), kz_json:get_ne_binary_value(Key2, JObj2))
    ,?_assertEqual('true', kz_json:are_equal(JObj1, JObj2))
    ].

flatten_expand_diff_test_() ->
    Add = [{<<"k20">>, <<"v20">>}],
    X = kz_json:set_value([<<"k10">>, <<"k11">>, <<"k12">>], <<"v10">>, kz_json:new()),
    X2 = kz_json:set_values(Add, X),

    A1 = kz_json:set_value([<<"a">>, <<"b">>]
                          ,[kz_json:from_list([{<<"c1">>, 1}, {<<"c2">>, 2}])]
                          ,kz_json:new()),
    A2 = kz_json:set_value([<<"a">>, <<"b">>]
                          ,[kz_json:from_list([{<<"c1">>, 1}, {<<"c2">>, 2}, {<<"c3">>, 3}])]
                          ,kz_json:new()),
    FLATTEN_LEGACY = kz_json:from_list([{[<<"a">>,<<"b">>],[kz_json:from_list([{<<"c1">>,1},{<<"c2">>,2}])]}]),
    FLATTEN_DEEP = kz_json:from_list([{[<<"a">>,<<"b">>,1,<<"c1">>],1}
                                     ,{[<<"a">>,<<"b">>,1,<<"c2">>],2}
                                     ]),

    DIFF_LEGACY_VALUE = kz_json:from_list([{<<"c1">>,1}, {<<"c2">>,2}, {<<"c3">>,3}]),
    DIFF_LEGACY = kz_json:set_value([<<"a">>, <<"b">>], [DIFF_LEGACY_VALUE], kz_json:new()),
    DIFF_DEEP_VALUE = kz_json:from_list([{<<"c3">>, 3}]),
    DIFF_DEEP = kz_json:set_value([<<"a">>, <<"b">>], [DIFF_DEEP_VALUE], kz_json:new()),

    Delta = kz_json:from_list(Add),
    Empty = kz_json:new(),

    [?_assertEqual(X2, kz_json:expand(kz_json:flatten(X2)))
    ,?_assertEqual(Delta, kz_json:diff(X2, X))
    ,?_assertEqual(Empty, kz_json:diff(X, X2))
    ,?_assertEqual(FLATTEN_LEGACY, kz_json:flatten(A1))
    ,?_assertEqual(FLATTEN_DEEP, kz_json:flatten_deep(A1))
    ,?_assertEqual(A1, kz_json:expand(kz_json:flatten(A1)))
    ,?_assertEqual(A1, kz_json:expand_deep(kz_json:flatten_deep(A1)))
    ,?_assertEqual(DIFF_LEGACY, kz_json:diff(A2, A1))
    ,?_assertEqual(DIFF_DEEP, kz_json:diff_deep(A2, A1))
    ].

diff_test_() ->
    Empty = kz_json:new(),

    AJObj = kz_json:from_list([{<<"a">>, <<"1">>}]),
    BJObj = kz_json:from_list([{<<"b">>, <<"2">>}]),
    B0JObj = kz_json:from_list([{<<"b">>, Empty}]),

    CzJObj = kz_json:from_list([{<<"c">>, kz_json:from_list([{<<"z">>, <<"26">>}])}]),
    CyJObj = kz_json:from_list([{<<"c">>, kz_json:from_list([{<<"y">>, <<"25">>}])}]),
    C0JObj = kz_json:from_list([{<<"c">>, Empty}]),

    BothAB = kz_json:merge(AJObj, BJObj),

    [?_assertEqual(Empty, kz_json:diff(Empty, Empty))

     %% top-level keys
    ,?_assertEqual(Empty, kz_json:diff(AJObj, AJObj))
    ,?_assertEqual(AJObj, kz_json:diff(AJObj, BJObj))
    ,?_assertEqual(BJObj, kz_json:diff(BJObj, AJObj))
    ,?_assertEqual(AJObj, kz_json:diff(BothAB, BJObj))
    ,?_assertEqual(BJObj, kz_json:diff(BothAB, AJObj))

    ,?_assertEqual(B0JObj, kz_json:diff(B0JObj, BJObj))
    ,?_assertEqual(BJObj, kz_json:diff(BJObj, B0JObj))

     %% nested keys
    ,?_assertEqual(Empty, kz_json:diff(CzJObj, CzJObj))
    ,?_assertEqual(CzJObj, kz_json:diff(CzJObj, CyJObj))
    ,?_assertEqual(CyJObj, kz_json:diff(CyJObj, CzJObj))

    ,?_assertEqual(C0JObj, kz_json:diff(C0JObj, CzJObj))
    ,?_assertEqual(CzJObj, kz_json:diff(CzJObj, C0JObj))
    ].

-define(REQUEST, kz_json:from_list([{<<"a">>, 1}
                                   ,{<<"b">>, [2, 3, 4]}
                                   ,{<<"c">>, kz_json:from_list([{<<"c.a">>, 1.1}])}
                                   ])).
-define(DOCUMENT, kz_json:from_list([{<<"a">>, 99}
                                    ,{<<"b">>, [98, 97, 96]}
                                    ,{<<"c">>, kz_json:from_list([{<<"c.a">>, 99.99}])}
                                    ])).

merge_vs_merge_recursive_test_() ->
    M1 = kz_json:merge(fun kz_json:merge_left/2, ?DOCUMENT, ?REQUEST),
    M2 = kz_json:merge_recursive(?DOCUMENT, ?REQUEST),
    M3 = kz_json:merge(fun kz_json:merge_right/2, ?DOCUMENT, ?REQUEST),

    [?_assert(kz_json:are_equal(M1, ?DOCUMENT))
    ,?_assert(kz_json:are_equal(M2, ?REQUEST))
    ,?_assert(kz_json:are_equal(M3, ?REQUEST))
    ].

set_value_test_() ->
    JObj = kz_json:from_list([{<<189>>,[]}]),
    Key = [<<189>>,<<0>>],
    Value = null,
    JObj1 = kz_json:set_value(Key, Value, JObj),
    [{"prop_set_value error", ?_assertEqual(Value, kz_json:get_value(Key, JObj1, Value))}].

-define(FROM_MAP_JSON_1,
        kz_json:from_list([{options, kz_json:from_list([{opt1,<<"my-opt1">>}
                                                       ,{opt2,<<"my-opt2">>}
                                                       ])}])).
-define(FROM_MAP_JSON_2,
        kz_json:from_list([{outer_key, kz_json:from_list([{inner_key,<<"inner_value">>}])}])).

-define(FROM_MAP_JSON_3,
        kz_json:from_list([{outer_key, kz_json:from_list([{inner_key,<<"inner_value">>}
                                                         ,{inner_options, kz_json:from_list([{opt1,<<"my-opt1">>}
                                                                                            ,{opt2,<<"my-opt2">>}
                                                                                            ])}
                                                         ])}])).
-define(FROM_MAP_JSON_4_WRONG,
        kz_json:from_list([{outer_key, kz_json:from_list([{inner_key,<<"inner_value">>}
                                                         ,{inner_options, [{opt1,<<"my-opt1">>}
                                                                          ,<<"anykey">>
                                                                          ,{opt1,<<"my-opt1">>}
                                                                          ]}
                                                         ])}])).
-define(FROM_MAP_JSON_4_RIGHT,
        kz_json:from_list([{outer_key, kz_json:from_list([{inner_key,<<"inner_value">>}
                                                         ,{inner_options, [kz_json:from_list([{opt1,<<"my-opt1">>}])
                                                                          ,<<"anykey">>
                                                                          ,kz_json:from_list([{opt1,<<"my-opt1">>}])
                                                                          ]}
                                                         ])}])).

-define(FROM_MAP_MAP_1, #{options => [{opt1,<<"my-opt1">>},{opt2,<<"my-opt2">>}]}).
-define(FROM_MAP_MAP_2, #{outer_key => #{inner_key => <<"inner_value">>}}).
-define(FROM_MAP_MAP_3, #{outer_key => #{inner_key => <<"inner_value">>
                                        ,inner_options => [{opt1,<<"my-opt1">>}
                                                          ,{opt2,<<"my-opt2">>}
                                                          ]}}).
-define(FROM_MAP_MAP_4, #{outer_key => #{inner_key => <<"inner_value">>
                                        ,inner_options => [{opt1,<<"my-opt1">>}
                                                          ,<<"anykey">>
                                                          ,{opt1,<<"my-opt1">>}
                                                          ]}}).

from_map_test_() ->
    [{"Map with a proplist as a key's value"
     ,?_assertEqual(?FROM_MAP_JSON_1, kz_json:from_map(?FROM_MAP_MAP_1))
     }
    ,{"Normal map", ?_assertEqual(?FROM_MAP_JSON_2, kz_json:from_map(?FROM_MAP_MAP_2))}
    ,{"Submap with a proplist as a key's value"
     ,?_assertEqual(?FROM_MAP_JSON_3, kz_json:from_map(?FROM_MAP_MAP_3))
     }
    ,{"wrong from_map with a mixed proplist inside map"
     ,?_assertNotEqual(?FROM_MAP_JSON_4_WRONG, kz_json:from_map(?FROM_MAP_MAP_4))
     }
    ,{"correct from_map with a mixed proplist inside map"
     ,?_assertEqual(?FROM_MAP_JSON_4_RIGHT, kz_json:from_map(?FROM_MAP_MAP_4))
     }
    ,{"error encoding the invalid json generated in from_map with a mixed proplist inside map"
     ,?_assertException(error, {invalid_ejson,{opt1,<<"my-opt1">>}}, kz_json:encode(?FROM_MAP_JSON_4_WRONG))
     }
    ,{"encoding the invalid json generated in from_map with a mixed proplist inside map"
     ,?_assertEqual(kz_json:encode(?FROM_MAP_JSON_4_RIGHT), kz_json:encode(kz_json:from_map(?FROM_MAP_MAP_4)))
     }
    ].

utf8_binary_values_test_() ->
    K = <<"key">>,
    V = <<"Bör">>,
    VUTF8 = <<"Bör"/utf8>>,

    %% "Proper" object
    Props = [{K, V}],
    UTF8Props = [{K, VUTF8}],
    UTF8JObj = ?JSON_WRAPPER(UTF8Props),
    UTF8EncJObj = <<"{\"key\":\"Bör\"}"/utf8>>,

    %% "flat" object
    Flat = [{[<<"foo">>,<<"fong">>],V}],
    UTF8Flat = [{[<<"foo">>,<<"fong">>],VUTF8}],

    %% Recursive "proper" object
    Recursive = [{K, Props}],
    UTF8Recursive = ?JSON_WRAPPER([{K, UTF8JObj}]),
    UTF8EncRecursive = <<"{\"key\":{\"key\":\"Bör\"}}"/utf8>>,

    %% File's content
    {'ok', MP3} = file:read_file(filename:join([code:lib_dir('kazoo_stdlib'), "test/mp3.mp3"])),
    MP3Props = [{<<"contents">>, MP3}],

    [{"When setting a binary value it should be encoded in utf8 format"
     ,?_assertEqual(UTF8JObj, kz_json:set_values(Props, kz_json:new()))
     }
    ,{"When building a JObj using from_list/1 it should encode binary values in utf8 format"
     ,?_assertEqual(UTF8JObj, kz_json:from_list(Props))
     }
    ,{"When building a JObj using from_list_recursive/1 it should encode binary values in utf8 format"
     ,?_assertEqual(UTF8Recursive, kz_json:from_list_recursive(Recursive))
     }
    ,{"When encoding a NOT utf8 ready object it should fail"
     ,?_assertException(error, {invalid_string,V}, kz_json:encode(?JSON_WRAPPER(Props)))
     }
    ,{"When encoding a NOT utf8 ready object it should fail"
     ,?_assertException(error, {invalid_string,V}, kz_json:encode(?JSON_WRAPPER([{K, ?JSON_WRAPPER(Props)}])))
     }
    ,{"When encoding a utf8 ready object it should work"
     ,?_assertEqual(UTF8EncJObj, kz_json:encode(UTF8JObj))
     }
    ,{"When encoding a utf8 ready object it should work"
     ,?_assertEqual(UTF8EncJObj, kz_json:encode(kz_json:from_list(Props)))
     }
    ,{"When encoding a utf8 ready object it should work"
     ,?_assertEqual(UTF8EncRecursive, kz_json:encode(kz_json:from_list_recursive(Recursive)))
     }
    ,{"When creating a flat object its binary value(s) should also be encoded in utf8 format"
     ,?_assertEqual(?JSON_WRAPPER(UTF8Flat), kz_json:from_list(Flat))
     }
    ,{"When creating a mixed object (Props + Flat) its binary value(s) should also be encoded in utf8 format"
     ,?_assertEqual(?JSON_WRAPPER(UTF8Props ++ UTF8Flat), kz_json:from_list(Props ++ Flat))
     }
    ,{"When trying to encode a non printable_unicode_list of chars the value should not be encoded"
     ,?_assertEqual(?JSON_WRAPPER(MP3Props), kz_json:from_list(MP3Props))
     }
    ].

-ifdef(PERF).
-define(REPEAT, 100000).

horse_merge() ->
    horse:repeat(?REPEAT
                ,kz_json:merge(?D1, ?D2)
                ).

horse_merge_recursive() ->
    horse:repeat(?REPEAT
                ,kz_json:merge_recursive(?D1, ?D2)
                ).

-endif.
