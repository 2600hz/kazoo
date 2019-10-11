-module(ecallmgr_util_tests).

-include_lib("eunit/include/eunit.hrl").

maybe_sanitize_fs_value_test_() ->
    UTF8Bin = <<"Bör1-Goes2$%^ To4 Škofja Loka"/utf8>>,
    Expected = <<"Bör1-Goes2 To4 Škofja Loka"/utf8>>,

    Keys = [<<"Outbound-Caller-ID-Name">>
           ,<<"Outbound-Callee-ID-Name">>
           ,<<"Caller-ID-Name">>
           ,<<"Callee-ID-Name">>
           ],
    TestMsg = fun(KeyStr) ->
                      lists:flatten(["When sanitizing "
                                    ,kz_term:to_list(KeyStr)
                                    ,"'s value it should allow utf8 characters"
                                    ])
              end,
    [{TestMsg(Key)
     ,?_assertEqual(Expected, ecallmgr_util:maybe_sanitize_fs_value(Key, UTF8Bin))
     }
     || Key <- Keys
    ].
