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

-ifdef(PROPER).

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


common_and_unique() ->
    ?LET(Common
        ,kz_json_generators:test_object()
        ,?LET(Unique
             ,kz_json_generators:test_object()
             ,{Common, kz_json:foldl(fun(K,_, U) -> kz_json:delete_key(K, U) end, Unique, Common)}
             )
        ).


-endif.
