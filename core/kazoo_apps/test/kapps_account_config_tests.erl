%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_account_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include("kazoo_apps.hrl").
-include("kazoo_apps_test.hrl").

kapps_accuont_config_test_() ->
    {'setup'
    ,fun kazoo_apps_test_util:setup/0
    ,fun kazoo_apps_test_util:cleanup/1
    ,fun(Map) ->
             [test_get_ne_binary(Map)
             ,test_get(Map)
             ,test_get_global(Map)
             ,test_get_from_reseller(Map)
             ,test_get_with_strategy(Map)
             ]
     end
    }.

test_get_ne_binary(_) ->
    [{"Testing get_ne_binary account config"
     ,[{"get a key with binary value"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>])))
       }
      ,{"get a non cast-able to binary value should crash"
       ,?_assertError(badarg, kapps_account_config:get_ne_binary(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"list_of_json">>]))
       }
      ,{"get a non-empty value which is cast-able to binary should return it as a non-empty binary"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"i_key">>])))
       }
      ,{"get an empty value should return default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binary(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, <<"not_exists">>))
       }
      ,{"get a list of binary value should return list of binary"
       ,?_assertEqual(true, is_ne_binaries(kapps_account_config:get_ne_binaries(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"b_keys">>])))
       }
      ,{"get not a list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ,{"get an empty list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ]
     }
    ].

is_ne_binary(Value) -> kz_term:is_ne_binary(Value).
is_ne_binaries(Value) -> kz_term:is_ne_binaries(Value).

test_get(_) ->
    [{"Testing account get config"
     ,[{"exists config doc in account should result in account"
       ,?_assertEqual(<<"sub_account">>, kapps_account_config:get(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ,{"undefined property in account should result in Default"
       ,?_assertEqual(<<"me_don_you">>, kapps_account_config:get(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, <<"udon_me">>, <<"me_don_you">>))
       }
      ]
     }
    ].

test_get_global(#{sub_config := SubAccountConfig
                 ,reseller_only_config := ResellerOnly
                 ,system_config := SystemConfig
                 ,system_only := SystemOnly
                 }) ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_global(AccountId, Category, Key)
                end,
    [{"Testing get global account config"
     ,[{"exists config doc in sub-account on get_global/2 should result in account's config doc"
       ,?_assertEqual(SubAccountConfig, kapps_account_config:get_global(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT))
       }
      ,{"undefined account_id on get_global/2 should result in system_config config doc"
       ,?_assertEqual(SystemConfig, kapps_account_config:get_global(undefined, ?TEST_CAT))
       }
      ,{"not exists sub-account config doc and exists config doc for reseller on get_global/2 should result in reseller config doc"
       ,?_assertEqual(ResellerOnly, kapps_account_config:get_global(?FIXTURE_CHILD_ACCOUNT_ID, ?RESELLER_ONLY))
       }
      ,{"not exists config doc for sub-account and reseller on get_global/2 should result in system_config default value object"
       ,?_assertEqual(SystemOnly, kapps_account_config:get_global(?FIXTURE_CHILD_ACCOUNT_ID, ?SYSTEM_ONLY))
       }
      ,{"not exists config doc for sub-account and reseller and an empty default system_config on get_global/2 should result in empty doc"
       ,?_assertEqual(kz_json:new(), kapps_account_config:get_global(?FIXTURE_PARENT_ACCOUNT_ID, ?SYSTEM_EMPTY))
       }
      ,{"not exists config doc for category on get_global/2 should result in an empty object with category as id"
       ,?_assertEqual(kz_doc:set_id(kz_json:new(), <<"no_cat_please">>), kapps_account_config:get_global(?FIXTURE_PARENT_ACCOUNT_ID, <<"no_cat_please">>))
       }
      ,common_tests_for_get_global(FunToTest)
      ]
     }
    ].

common_tests_for_get_global(Fun) ->
    [{"Common get global account config"
     ,[{"undefined account id should result in system_config value"
       ,?_assertEqual(<<"system_only">>, Fun(undefined, ?TEST_CAT, [<<"root_obj_key">>, <<"system_key">>]))
       }
      ,{"not exists config doc for sub-account and reseller should result in system_config value"
       ,?_assertEqual(<<"system_only">>, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?SYSTEM_ONLY, <<"key">>))
       }
      ,{"not exists sub-account config doc and defined and exists config doc in reseller should result in reseller value"
       ,?_assertEqual(<<"reseller_only">>, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?RESELLER_ONLY, <<"reseller_only">>))
       }
      ,{"not exists sub-account config doc and undefined property in reseller config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?RESELLER_SYSTEM, [<<"root_obj_key">>, <<"system_key">>]))
       }
       %% ,{"empty customized sub-account and customized reseller should result in reseller"       <---- look at me
       %%  ,?_assertEqual(SysValue, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?RESELLER_CUSTOMIZED, <<"root_obj_key">>))
       %%  }
      ,{"undefined property in sub-account config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"system_key">>]))
       }
      ,{"defined key in sub-account config doc should result in account value"
       ,?_assertEqual(<<"sub_account">>, Fun(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, <<"one_root_key">>))
       }
      ]
     }
    ].

test_get_from_reseller(_) ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_from_reseller/3, Args);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_from_reseller/4, Args)
                end,
    [{"Testing get config from reseller"
     ,common_tests_for_get_from_reseller(FunToTest)
     }
    ].

common_tests_for_get_from_reseller(FunToTest) ->
    [{"Common get config from reseller tests"
     ,[{"undefined account id should result in system_config value"
       ,?_assertEqual(<<"from_system">>, FunToTest([undefined, ?RESELLER_SYSTEM, [<<"root_obj_key">>, <<"reseller_system_only">>]]))
       }
      ,{"not exists reseller config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>, FunToTest([?FIXTURE_PARENT_ACCOUNT_ID, ?SYSTEM_ONLY, <<"key">>]))
       }
      ,{"not exists reseller config doc and undefined property in system_config doc should result in set Default on system_config"
       ,?_assertEqual(<<"default">>, FunToTest([?FIXTURE_PARENT_ACCOUNT_ID, ?SYSTEM_ONLY, <<"new_key">>, <<"default">>]))
       }
      ,{"undefined property in reseller config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>, FunToTest([?FIXTURE_PARENT_ACCOUNT_ID, ?RESELLER_SYSTEM, [<<"root_obj_key">>, <<"system_key">>], <<"this_should_not_set">>]))
       }
      ,{"defined config in reseller should result in reseller's value"
       ,?_assertEqual(<<"from_reseller">>
                     ,FunToTest([?FIXTURE_PARENT_ACCOUNT_ID, ?RESELLER_SYSTEM, [<<"root_obj_key">>, <<"reseller_system_only">>], <<"this_should_not_set">>])
                     )
       }
      ,{"only get from direct reseller"
       ,?_assertEqual(<<"from_system">>
                     ,FunToTest([?FIXTURE_MASTER_ACCOUNT_ID, ?RESELLER_SYSTEM, [<<"root_obj_key">>, <<"reseller_system_only">>], <<"this_should_not_set">>])
                     )
       }
      ]
     }
    ].

test_get_with_strategy(_) ->
    [{"Testing getting account config with strategy"
     ,[get_with_strategy_general()
      ,get_strategy_global()
      ,get_strategy_reseller()
      ,get_strategy_hierarchy_merge()
      ]
     }
    ].

get_with_strategy_general() ->
    Db = kz_util:format_account_db(?FIXTURE_PARENT_ACCOUNT_ID),

    [{"Testing strategy with no account id"
     ,[{"undefined account id should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, undefined, ?TEST_CAT, [<<"root_obj_key">>, <<"system_key">>])
                     )
       }
      ,{"empty call object (no account id) should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, kapps_call:new(), ?TEST_CAT, [<<"root_obj_key">>, <<"system_key">>])
                     )
       }
      ,{"empty jobj object (no account id) should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, kz_json:new(), ?SYSTEM_ONLY, <<"key">>)
                     )
       }
      ,{"unknown object (no account id) should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, maps:new(), ?SYSTEM_ONLY, <<"key">>)
                     )
       }
      ,{"passing non raw account id where account does not have config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, Db, ?SYSTEM_ONLY, <<"key">>)
                     )
       }
      ,{"passing non raw account id in jobj where account & reseller do not have config doc should result in system_config value"
       ,?_assertEqual(<<"system_only">>
                     ,kapps_account_config:get_with_strategy(<<"global">>, kz_json:from_list([{<<"account_id">>, Db}]), ?SYSTEM_ONLY, <<"key">>)
                     )
       }
      ]
     }
    ,{"Testing some general situation"
     ,[{"not exists config doc for account and reseller and undefined property system_config should result in set Default on system_config"
       ,?_assertEqual(<<"dummy_me">>, kapps_account_config:get_with_strategy(<<"global">>, ?FIXTURE_PARENT_ACCOUNT_ID, ?SYSTEM_ONLY, <<"new_key">>, <<"dummy_me">>))
       }
      ]
     }
    ].

get_strategy_global() ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_with_strategy(<<"global">>, AccountId, Category, Key)
                end,
    [{"Testing get config global strategy"
     ,common_tests_for_get_global(FunToTest)
     }
    ].

get_strategy_reseller() ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_with_strategy/4, [<<"reseller">>|Args]);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_with_strategy/5, [<<"reseller">>|Args])
                end,
    [{"Testing get config reseller strategy"
     ,common_tests_for_get_from_reseller(FunToTest)
     }
    ].

get_strategy_hierarchy_merge() ->
    SysRootObjKey = get_fixture_value([<<"default">>, <<"root_obj_key">>], ?KZ_CONFIG_DB, ?TEST_CAT),
    ResellerRootJObjKey = get_fixture_value(<<"root_obj_key">>, ?FIXTURE_RESELLER_ACCOUNT_ID, ?TEST_CAT),
    ParentRootObjKey = get_fixture_value(<<"root_obj_key">>, ?FIXTURE_PARENT_ACCOUNT_ID, ?TEST_CAT),
    SubRootObjKey = get_fixture_value(<<"root_obj_key">>, ?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT),

    SometimesEmptyValue_System = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], SysRootObjKey),
    SometimesEmptyValue_Reseller = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], ResellerRootJObjKey),
    SometimesEmptyValue_Sub1 = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], ParentRootObjKey),

    [{"Testing get config hierarchy_merge strategy"
     ,[{"defined value in account where account is reseller itself should result in merged value of reseller and system"
       ,?_assertEqual(kz_json:merge_recursive([SysRootObjKey, ResellerRootJObjKey])
                     ,kapps_account_config:get_with_strategy(<<"hierarchy_merge">>, ?FIXTURE_RESELLER_ACCOUNT_ID, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"defined value in parent-account and reseller should result in merged value of parent-account, reseller and system"
       ,?_assertEqual(kz_json:merge_recursive([SysRootObjKey, ResellerRootJObjKey, ParentRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_PARENT_ACCOUNT_ID, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"defined value in account & system and undefined property in parent & reseller should result in merged value of account and system"
       ,?_assertEqual(get_key_and_merge_configs(<<"sub_account+system">>, [SysRootObjKey, SubRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"sub_account+system">>])
                     )
       }
      ,{"customized account, reseller, system and empty/not_exists parent should result in merged value of all customized and system"
       ,?_assertEqual(get_key_and_merge_configs(<<"sub+reseller+system">>, [SysRootObjKey, ResellerRootJObjKey, SubRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"sub+reseller+system">>])
                     )
       }
      ,{"customized account, parent, system and empty reseller should result in merged value of all customized and system"
       ,?_assertEqual(get_key_and_merge_configs(<<"sub+parent+empty_reseller+system">>, [SysRootObjKey, ParentRootObjKey, SubRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"sub+parent+empty_reseller+system">>])
                     )
       }
      ,{"customized account, parent, system and not customized reseller should result in merged value of all customized and system"
       ,?_assertEqual(get_key_and_merge_configs(<<"sub+parent+system">>, [SysRootObjKey, ParentRootObjKey, SubRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"sub+parent+system">>])
                     )
       }
      ,{"customized account, parents and system should result in merged value of all"
       ,?_assertEqual(kz_json:merge_recursive([SysRootObjKey, ResellerRootJObjKey, ParentRootObjKey, SubRootObjKey])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"not customized account with undefined parent account id and no reseller should result in system_config"
       ,?_assertEqual(SysRootObjKey, kapps_account_config:get_hierarchy(kz_binary:rand_hex(16), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"check if the account set an empty jobj, the content of the hierarchy is merged into the jobj properly"
       ,?_assertEqual(kz_json:merge_recursive([SometimesEmptyValue_System, SometimesEmptyValue_Reseller, SometimesEmptyValue_Sub1])
                     ,kapps_account_config:get_hierarchy(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT, [<<"root_obj_key">>, <<"obj_empty_test">>, <<"obj_empty_sometimes">>])
                     )
       }
      ]
     }
    ].

get_key_and_merge_configs(Key, Configs) ->
    kz_json:merge_recursive(
      [kz_json:get_value(Key, Conf, kz_json:new())
       || Conf <- Configs
      ]
     ).
