%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2021, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_attributes_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

-define(NEW_CF_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_RESELLER_ACCOUNT_ID, C) end
                      ,fun(C) -> kapps_call:set_authorizing_id(<<"device00000000000000000000000002">>, C) end
                      ,fun(C) -> kapps_call:set_owner_id(<<"user0000000000000000000000000002">>, C) end
                      ]).

-define(MIXED_CF_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_PARENT_ACCOUNT_ID, C) end
                        ,fun(C) -> kapps_call:set_authorizing_id(<<"device00000000000000000000000003">>, C) end
                        ,fun(C) -> kapps_call:set_owner_id(<<"user0000000000000000000000000003">>, C) end
                        ]).

-define(NEW_TS_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_RESELLER_ACCOUNT_ID, C) end
                      ,fun(C) -> kapps_call:set_authorizing_id(<<"trunkstore0000000000000000000002">>, C) end
                      ]).

-define(DISA_CID_CALL, [fun(C) -> kapps_call:set_account_id(?FIXTURE_PARENT_ACCOUNT_ID, C) end
                       ,fun(C) -> kapps_call:set_caller_id_name(<<"disa">>, C) end
                       ,fun(C) -> kapps_call:set_caller_id_number(<<"+12225552600">>, C) end
                       ]).

kz_attributes_test_() ->
    {'setup'
    ,fun kzd_test_fixtures:setup/0
    ,fun kzd_test_fixtures:cleanup/1
    ,fun(_ReturnOfSetup) ->
             [test_get_flags_callflow()
             ,test_get_flags_trunkstore()
             ,test_process_dynamic_flags()
             ,test_account_cid()
             ]
     end
    }.

test_get_flags_callflow() ->
    Call = kapps_call_tests:create_callflow_call(),
    ExpectedOld = [<<"user_old_static_flag">>
                  ,<<"device_old_static_flag">>
                  ,<<"account_old_static_flag">>
                  ],
    ExpectedNew = [<<"user0000000000000000000000000002">>
                  ,<<"user_new_static_flag">>
                  ,<<"local">>
                  ,<<"device_new_static_flag">>
                  ,<<"4a6863.sip.2600hz.local">>
                  ,<<"account_new_static_flag">>
                  ],
    ExpectedMixed = [<<"user_new_static_flag">>
                    ,<<"local">>
                    ,<<"device_old_static_flag">>
                    ,<<"user0000000000000000000000000003">>
                    ,<<"account_new_static_flag">>
                    ],
    [{"verify that get flags will pull the static and dynamic flags from all sources with old formats"
     ,?_assertEqual(ExpectedOld, kz_attributes:get_flags(<<"callflows">>, Call))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with new formats"
     ,?_assertEqual(ExpectedNew, kz_attributes:get_flags(<<"callflows">>, kapps_call:exec(?NEW_CF_FLAGS, Call)))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with mixed formats"
     ,?_assertEqual(ExpectedMixed, kz_attributes:get_flags(<<"callflows">>, kapps_call:exec(?MIXED_CF_FLAGS, Call)))
     }
    ].

test_get_flags_trunkstore() ->
    Call = kapps_call_tests:create_trunkstore_call(),
    ExpectedOld = [<<"account_old_static_flag">>],
    ExpectedNew = [<<"local">>
                  ,<<"account_new_static_flag">>
                  ],
    [{"verify that get flags will pull the static and dynamic flags from all sources with old formats"
     ,?_assertEqual(ExpectedOld, kz_attributes:get_flags(<<"trunkstore">>, Call))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with new formats"
     ,?_assertEqual(ExpectedNew, kz_attributes:get_flags(<<"trunkstore">>, kapps_call:exec(?NEW_TS_FLAGS, Call)))
     }
    ].

test_process_dynamic_flags() ->
    Call = kapps_call_tests:create_callflow_call(),
    [{"verify that dynamic CCVs can be fetched and are converted to binary"
     ,?_assertEqual([<<"device">>], kz_attributes:process_dynamic_flags([<<"custom_channel_vars.authorizing_type">>], Call))
     }
    ,{"verify that exported kapps_call functions can be used"
     ,?_assertEqual([<<"20255520140">>], kz_attributes:process_dynamic_flags([<<"to_user">>], Call))
     }
    ,{"verify that non-exported kapps_call functions dont crash"
     ,?_assertEqual([], kz_attributes:process_dynamic_flags([<<"not_exported">>], Call))
     }
    ,{"verify that the zone name can be resolved"
     ,?_assertEqual([<<"local">>], kz_attributes:process_dynamic_flags([<<"zone">>], Call))
     }
    ,{"verify that dynamic flags are added to a provided list of static flags"
     ,?_assertEqual([<<"local">>, <<"static">>], kz_attributes:process_dynamic_flags([<<"zone">>], [<<"static">>], Call))
     }
    ].

test_account_cid() ->
    Call = kapps_call_tests:create_callflow_call(),
    DISACall = kapps_call:exec(?DISA_CID_CALL, Call),
    {CIDNumber, CIDName} = kz_attributes:get_account_external_cid(DISACall),

    [{"account external caller id number chosen"
     ,?_assertEqual(<<"+19995552600">>, CIDNumber)
     }
    ,{"account external caller id name chosen"
     ,?_assertEqual(<<"account-external-name">>, CIDName)
     }
    ].
