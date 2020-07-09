-module(teletype_account_zone_change_tests).

-include_lib("eunit/include/eunit.hrl").

zones_data_test_() ->
    Mod = 'teletype_account_zone_change',
    Test = fun Mod:build_zones_data/1,
    Macros = kz_json:get_keys(Mod:macros()),
    JObj = kz_json:from_list_recursive([{<<"zones">>, [{<<"zone1">>, kz_binary:rand_hex(4)}
                                                      ,{<<"zone2">>, kz_binary:rand_hex(4)}
                                                      ]}
                                       ]),

    [{"Assert *" ++ kz_term:to_list(Macro) ++ "* macro is listed when "
      ++ kz_term:to_list(Mod) ++ ":macros/0 is called"
     ,?_assert(lists:member(Macro, Macros))
     } || Macro <- props:get_keys(Test(JObj))
    ].
