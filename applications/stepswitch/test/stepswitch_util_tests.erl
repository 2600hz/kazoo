%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2021, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("stepswitch.hrl").

maybe_constrain_shortdial_correction_test_() ->
    Test = fun(Length) -> stepswitch_util:maybe_constrain_shortdial_correction(Length, 2, 4) end,

    [{"If calculated length is within bounds , return length"
     ,[?_assertEqual(4, Test(4))
      ,?_assertEqual(3, Test(3))
      ,?_assertEqual(2, Test(2))
      ]
     }
    ,{"If calculated length is out of bounds, return 0"
     ,[?_assertEqual(0, Test(10))
      ,?_assertEqual(0, Test(5))
      ,?_assertEqual(0, Test(1))
      ]
     }
    ].

shortdial_correction_length_test_() ->
    Test = fun stepswitch_util:shortdial_correction_length/3,
    FixedLength = 5,
    Fixed = fun(CalleeNum, CallerNum) -> Test(CalleeNum, CallerNum, FixedLength) end,
    Undefined = fun(CalleeNum, CallerNum) -> Test(CalleeNum, CallerNum, 'undefined') end,

    %% By default min=2, max=5
    [{"If fixed is undefined, return calculated length if it is within bounds, 0 otherwise"
     ,[?_assertEqual(2, Undefined(<<"12345">>, <<"1234567">>))
      ,?_assertEqual(4, Undefined(<<"123">>, <<"1234567">>))
      ,?_assertEqual(0, Undefined(<<"123">>, <<"123456789">>)) %% Difference = 6
      ,?_assertEqual(0, Undefined(<<"123">>, <<"1234">>)) %% Difference = 1
      ]
     }
    ,{"If fixed is set, return its value"
     ,[?_assertEqual(FixedLength, Fixed(<<"12345">>, <<"1234567">>))
      ,?_assertEqual(FixedLength, Fixed(<<"123">>, <<"1234567">>))
      ]
     }
    ].

maybe_deny_reclassified_number_test_() ->
    Test = fun stepswitch_util:maybe_deny_reclassified_number/3,
    CorrectedNumber = <<"1234567">>,
    CallRestrictions = kz_json:from_list_recursive([{<<"unknown">>, [{<<"action">>, <<"deny">>}]}]),

    meck:new('knm_converters', ['passthrough']),
    meck:expect('knm_converters', 'classify', fun(_) -> <<"unknown">> end), %% Just to be sure ;)
    Resp1 = Test(CorrectedNumber, <<"1234">>, CallRestrictions),
    Resp2 = Test(CorrectedNumber, <<"7654321">>, CallRestrictions),
    meck:unload('knm_converters'),

    [{"If not Call Restrictions, return the corrected number"
     ,[?_assertEqual(CorrectedNumber, Test(CorrectedNumber, <<"1234">>, kz_json:new()))
      ,?_assertEqual(CorrectedNumber, Test(CorrectedNumber, <<"7654321">>, kz_json:new()))
      ]
     }
    ,{"If Call Restrictions apply, return undefined"
     ,[?_assertEqual('undefined', Resp1)
      ,?_assertEqual('undefined', Resp2)
      ]
     }
    ].

do_correct_shortdial_test_() ->
    Test = fun stepswitch_util:do_correct_shortdial/3,
    CallerNumber = <<"12223334444">>,
    CalleeNumber = <<"3335555">>,

    [{"Take up to Length digits from CallerNumber and prefix CalleeNumber with those digits"
     ,[?_assertEqual(<<"3335555">>, Test(CalleeNumber, CallerNumber, 0))
      ,?_assertEqual(<<"123335555">>, Test(CalleeNumber, CallerNumber, 2))
      ,?_assertEqual(<<"+12223335555">>, Test(CalleeNumber, CallerNumber, 4))
      ,?_assertEqual(<<CallerNumber/binary, CalleeNumber/binary>>
                    ,Test(CalleeNumber, CallerNumber, byte_size(CallerNumber) + 1)
                    )
      ]
     }
    ].

should_correct_shortdial_test_() ->
    Test = fun stepswitch_util:should_correct_shortdial/2,
    Number = <<"3335555">>,
    Length1 = byte_size(Number) + 1,
    Length2 = byte_size(Number),

    [{"If min_shortdial_destination is disabled, return {true, 0}"
     ,?_assertEqual({'true', 0}, Test(Number, 'undefined'))
     }
    ,{"If min_shortdial_destination is set and doesn't match dialed number's length, return {false, Length}"
     ,[?_assertEqual({'false', Length1}, Test(Number, Length1))
      ,?_assertEqual({'false', Length1}, Test(kz_binary:truncate(Number, 5), Length1))
      ]
     }
    ,{"If min_shortdial_destination is set and matches dialed number's length, return {true, Length}"
     ,?_assertEqual({'true', Length2}, Test(Number, Length2))
     }
    ].

correct_shortdial_test_() ->
    Test = fun stepswitch_util:correct_shortdial/4,
    %% automatically-calculated = length(CallerNumber) - length(CalleeNumber) = 4
    CallerNumber = <<"12223334444">>,
    CalleeNumber = <<"3335555">>,
    CallRestrictions = kz_json:from_list_recursive([{<<"unknown">>, [{<<"action">>, <<"deny">>}]}]),

    meck:new('kapps_config', ['unstick', 'passthrough']),
    meck:new('knm_converters', ['passthrough']),

    Fixed = fun(L) -> fun(?SS_CONFIG_CAT, <<"fixed_length_shortdial_correction">>) -> L end end,

    %% By default min=2, max=5, fixed=undefined
    %% fixed=undefined, length=automatically-calculated
    Resp1 = Test(CalleeNumber, CallerNumber, kz_json:new(), {'true', 0}),

    meck:expect('kapps_config', 'get_integer', Fixed(6)),
    %% fixed=6, length=fixed
    Resp2 = Test(CalleeNumber, CallerNumber, kz_json:new(), {'true', 0}),

    meck:expect('kapps_config', 'get_integer', Fixed(3)),
    %% fixed=3, length=fixed
    Resp3 = Test(CalleeNumber, CallerNumber, kz_json:new(), {'true', 0}),

    meck:expect('kapps_config', 'get_integer', Fixed('undefined')),
    %% fixed=undefined length=automatically-calculated+2 = 6
    Resp4 = Test(CalleeNumber, <<CallerNumber/binary, "12">>, kz_json:new(), {'true', 0}),

    %% fixed=undefined, length=automatically-calculated-3 = 1
    Resp5 = Test(<<CalleeNumber/binary, "123">>, CallerNumber, kz_json:new(), {'true', 0}),

    meck:expect('knm_converters', 'classify', fun(_) -> <<"unknown">> end), %% Just to be sure ;)
    %% fixed=undefined, length=automatically-calculated
    Resp6 = Test(CalleeNumber, CallerNumber, CallRestrictions, {'true', 0}),

    meck:expect('kapps_config', 'get_integer', Fixed(1)),
    %% fixed=1, length=fixed
    Resp7 = Test(CalleeNumber, CallerNumber, CallRestrictions, {'true', 0}),

    meck:expect('kapps_config', 'get_integer', Fixed(0)),
    %% fixed=0, length=fixed
    Resp8 = Test(CalleeNumber, CallerNumber, kz_json:new(), {'true', 0}),

    Tests =
        [{"If min_shortdial_destination is 'disabled', no call restrictions, and calculated length is between bounds or fixed is defined, return corrected number"
         ,[?_assertEqual(<<"+12223335555">>, Resp1)
          ,?_assertEqual(<<"1222333335555">>, Resp2)
          ,?_assertEqual(<<"1223335555">>, Resp3)
          ]
         }
        ,{"If fixed and min_shortdial_destination are 'disabled', no call restrictions, and calculated length is outside bounds, return undefined"
         ,[?_assertEqual('undefined', Resp4)
          ,?_assertEqual('undefined', Resp5)
          ]
         }
        ,{"If min_shortdial_destination is disabled, calculated length is between bounds or fixed is defined, but call restrictions apply, return undefined"
         ,[?_assertEqual('undefined', Resp6)
          ,?_assertEqual('undefined', Resp7)
          ]
         }
        ,{"If min_shortdial_destination is disabled, no call restrictions, and fixed value is 0 (zero), return undefined"
         ,?_assertEqual('undefined', Resp8)
         }
        ,{"If min_shortdial_destination is defined and CalleeNumber's length doesn't match it, return undefined"
         ,[?_assertEqual('undefined'
                        ,Test(CalleeNumber, CallerNumber, kz_json:new(), {'false', byte_size(CalleeNumber) + 1})
                        )
          ,?_assertEqual('undefined'
                        ,Test(CalleeNumber, CallerNumber, kz_json:new(), {'false', byte_size(CalleeNumber) - 1})
                        )
          ]
         }
        ],

    meck:unload('knm_converters'),
    meck:unload('kapps_config'),
    Tests.
