%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoint_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))].

lift_empty_test_() ->
    Empty = kz_json:new(),
    ?_assertEqual({Empty, [Empty, Empty]}
                 ,kz_endpoints:lift_common_properties([Empty, Empty])
                 ).

lift_common_properties_test_() ->
    CommonProperties = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                         ,{<<"key">>, <<"value">>}
                                         ]),
    UniqueProperties = kz_json:from_list([{<<"bing">>, <<"bang">>}]),

    Merged = kz_json:merge(CommonProperties, UniqueProperties),

    ?_assertEqual({CommonProperties, [kz_json:new(), UniqueProperties]}
                 ,kz_endpoints:lift_common_properties([CommonProperties, Merged])
                 ).

lift_common_properties_blacklist_test_() ->
    CommonProperties = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                         ,{<<"key">>, <<"value">>}
                                         ]),
    UniqueProperties = kz_json:from_list([{<<"bing">>, <<"bang">>}]),
    Blacklist = [<<"foo">>],

    Merged = kz_json:merge(CommonProperties, UniqueProperties),

    {Common, Endpoints} = kz_endpoints:lift_common_properties([CommonProperties, Merged], Blacklist),

    [?_assert(kz_json:are_equal(Common, kz_json:delete_key(Blacklist, CommonProperties)))
    ,?_assertEqual(Endpoints
                  ,[kz_json:from_list([{<<"foo">>, <<"bar">>}])
                   ,kz_json:from_list([{<<"foo">>, <<"bar">>}
                                      ,{<<"bing">>, <<"bang">>}
                                      ])
                   ]
                  )
    ].

proper_findings_lift_1_test_() ->
    JObj = {[{<<0>>,'false'}]},
    {C, Es} = kz_endpoints:lift_common_properties([JObj, JObj], [<<0>>]),
    [?_assert(kz_json:are_equal(kz_json:new(), C))
     | [?_assert(kz_json:are_equal(E, JObj)) || E <- Es]
    ].

-ifdef(PROPER).

%% Lifted from Erlang ML
proper_test_() ->
    {"Runs kz_endpoint PropEr tests"
    ,{'timeout'
     ,10000
     ,[{atom_to_list(F)
       ,fun () ->
                ?assert(proper:quickcheck(?MODULE:F(), [{'to_file', 'user'}
                                                       ,{'numtests', 500}
                                                       ]))
        end
       }
       || {F, 0} <- ?MODULE:module_info('exports'),
          F > 'prop_',
          F < 'prop`'
      ]
     }
    }.

prop_lift_common() ->
    ?FORALL({CommonJObj, UniqueJObj}
           ,common_and_unique()
           ,?WHENFAIL(?debugFmt("~nfailed to lift out common ~p~nunique: ~p~n"
                               ,[CommonJObj, UniqueJObj]
                               )
                     ,begin
                          Merged = kz_json:merge(UniqueJObj, CommonJObj),
                          {CommonProperties, [UpCommon, UpUnique, UpCommon, UpUnique]} = kz_endpoints:lift_common_properties([CommonJObj, Merged, CommonJObj, Merged]),
                          kz_json:are_equal(CommonJObj, CommonProperties)
                              andalso kz_json:are_equal(kz_json:new(), UpCommon)
                              andalso kz_json:are_equal(UpUnique, UniqueJObj)
                      end
                     )
           ).

prop_nothing_in_common() ->
    ?FORALL({CommonJObj, UniqueJObj}
           ,common_and_unique()
           ,?WHENFAIL(?debugFmt("~nfailed nothing_in_common:~ncommon ~p~nunique: ~p~n"
                               ,[CommonJObj, UniqueJObj]
                               )
                     ,begin
                          {CommonProperties, [UpCommon, UpUnique]} = kz_endpoints:lift_common_properties([CommonJObj, UniqueJObj]),
                          kz_json:are_equal(kz_json:new(), CommonProperties)
                              andalso kz_json:are_equal(CommonJObj, UpCommon)
                              andalso kz_json:are_equal(UpUnique, UniqueJObj)
                      end
                     )
           ).

prop_lift_common_blacklist() ->
    ?FORALL({CommonJObj, UniqueJObj}
           ,common_and_unique()
           ,?WHENFAIL(?debugFmt("~nfailed to lift out common ~p~nunique: ~p~nmerged: ~p~nblacklisted: ~p~n"
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
                          } = kz_endpoints:lift_common_properties([CommonJObj, Merged], Blacklist),

                          kz_json:are_equal(kz_json:delete_keys(Blacklist, CommonJObj), CommonProperties)
                              andalso kz_json:get_value(Blacklist, UpCommon) =:= kz_json:get_value(Blacklist, CommonJObj)
                              andalso kz_json:get_value(Blacklist, UpUnique) =:= kz_json:get_value(Blacklist, Merged)
                      end
                     )
           ).

make_blacklist([]) -> [];
make_blacklist([H|_]) -> [H].

common_and_unique() ->
    ?LET(Common
        ,kz_json_generators:test_object()
        ,?LET(Unique
             ,kz_json_generators:test_object()
             ,{Common, kz_json:foldl(fun(K,_, U) -> kz_json:delete_key(K, U) end, Unique, Common)}
             )
        ).


-endif.
