%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapps_account_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_config.hrl").

get_ne_binary_test_() ->
    %% SysDefaultValue = get_fixture_value(<<"default">>, "test_cat_system"),
    [{"Testing get_ne_binary account config"
     ,[{"get a key with binary value"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>])))
       }
       %% ,{"get a non cast-able to binary value should crash"
       %%  ,?_assertEqual(undefined, is_ne_binary(kapps_account_config:get_ne_binary(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"obj_key">>])))
       %%  }
      ,{"get a non-empty value which is cast-able to binary should return it as non-empty binary"
       ,?_assertEqual(true, is_ne_binary(kapps_account_config:get_ne_binary(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"i_key">>])))
       }
      ,{"get an empty value should return default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binary(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, <<"not_exists">>))
       }
      ,{"get a list of binary value should return list of binary"
       ,?_assertEqual(true, is_ne_binaries(kapps_account_config:get_ne_binaries(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"b_keys">>])))
       }
      ,{"get not a list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ,{"get an empty list of binary value should return Default"
       ,?_assertEqual(undefined, kapps_account_config:get_ne_binaries(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, [<<"root_obj_key">>, <<"b_key">>]))
       }
      ]
     }
    ].

is_ne_binary(Value) -> kz_term:is_ne_binary(Value).
is_ne_binaries(Value) -> kz_term:is_ne_binaries(Value).

get_test_() ->
    SubAccountValue = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_1"),
    Default = kz_json:from_list([{<<"new_key">>, <<"new_val">>}]),

    [{"Testing account get config"
     ,[{"customized account should result in account"
       ,?_assertEqual(SubAccountValue, kapps_account_config:get(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized account should result in Default"
       ,?_assertEqual(Default, kapps_account_config:get(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"udon_me">>, Default))
       }
      ]
     }
    ].

get_global_test_() ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_global(AccountId, Category, Key)
                end,
    SysDefaultValue = kz_doc:set_account_db(get_fixture_value(<<"default">>, "test_cat_system"), ?KZ_CONFIG_DB),
    ResellerFixture = kapps_config_util:fixture("test_cat_reseller"),
    SubAccountFixture = kapps_config_util:fixture("test_cat_subaccount_1"),
    EmptySysDoc = kz_doc:set_account_db(kz_json:new(), ?KZ_CONFIG_DB),

    [{"Testing get global account config"
     ,[{"customized sub-account on get_global/2 should result in account"
       ,?_assertEqual(SubAccountFixture, kapps_account_config:get_global(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT))
       }
      ,{"undefined sub-account on get_global/2 should result in system_config"
       ,?_assertEqual(kz_doc:set_id(SysDefaultValue, ?TEST_CAT), kapps_account_config:get_global(undefined, ?TEST_CAT))
       }
      ,{"not customized sub-account and customized reseller on get_global/2 should result in reseller"
       ,?_assertEqual(ResellerFixture, kapps_account_config:get_global(?CUSTOMIZED_RESELLER, ?TEST_CAT))
       }
      ,{"not customized sub-account and reseller on get_global/2 should result in system_config"
       ,?_assertEqual(kz_doc:set_id(SysDefaultValue, ?TEST_CAT), kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT))
       }
      ,{"not customized sub-account and reseller and empty system_config on get_global/2 should result in empty"
       ,?_assertEqual(kz_doc:set_id(EmptySysDoc, ?TEST_CAT_EMPTY), kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT_EMPTY))
       }
      ,{"non existing category on get_global/2 should result in empty"
       ,?_assertEqual(kz_doc:set_id(EmptySysDoc, <<"no_cat">>), kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, <<"no_cat">>))
       }
      ,common_get_global_tests(FunToTest)
      ]
     }
    ].

common_get_global_tests(Fun) ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, "test_cat_reseller"),
    SubAccountValue = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_1"),

    [{"Common get global account config"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, Fun(undefined, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and reseller should result in system_config"
       ,?_assertEqual(SysValue, Fun(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, Fun(?CUSTOMIZED_RESELLER, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and empty customized reseller should result in system_config"
       ,?_assertEqual(SysValue, Fun(?CUSTOMIZED_RESELLER_UNDEFINED, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"empty customized sub-account should result in system_config"
       ,?_assertEqual(SysValue, Fun(?CUSTOMIZED_SUBACCOUNT_1_UNDEFINED, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"customized sub-account should result in account"
       ,?_assertEqual(SubAccountValue, Fun(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, <<"root_obj_key">>))
       }
      ]
     }
    ].

get_from_reseller_test_() ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_from_reseller/3, Args);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_from_reseller/4, Args)
                end,
    [{"Testing get config from reseller"
     ,common_get_from_reseller_tests(FunToTest)
     }
    ].

common_get_from_reseller_tests(FunToTest) ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),
    Default = kz_json:from_list([{<<"new_key">>, <<"new_val">>}]),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, "test_cat_reseller"),

    [{"Common get config from reseller tests"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([undefined, ?TEST_CAT, <<"root_obj_key">>]))
       }
      ,{"not customized reseller should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"root_obj_key">>]))
       }
      ,{"not customized reseller and empty system_config should result in set Default on system_config"
       ,?_assertEqual(Default, FunToTest([?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT_EMPTY, <<"new_key">>, Default]))
       }
      ,{"empty customized reseller should result in system_config"
       ,?_assertEqual(SysValue, FunToTest([?CUSTOMIZED_RESELLER_UNDEFINED, ?TEST_CAT, <<"root_obj_key">>, Default]))
       }
      ,{"customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, FunToTest([?CUSTOMIZED_RESELLER, ?TEST_CAT, <<"root_obj_key">>, Default]))
       }
      ,{"only get from direct reseller"
       ,?_assertEqual(SysValue, FunToTest([?SELF_RESELLER, ?TEST_CAT, <<"root_obj_key">>, Default]))
       }
      ]
     }
    ].

get_with_strategy_test_() ->
    [{"Testing getting account config with strategy"
     ,[get_with_strategy_general()
      ,get_startegy_global()
      ,get_startegy_reseller()
      ,get_startegy_hierarchy_merge()
      ]
     }
    ].

get_with_strategy_general() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),

    Default = kz_json:from_list([{<<"new_key">>, <<"new_val">>}]),
    Db = kz_util:format_account_db(?NOT_CUSTOMIZED_ALL_ACCOUNTS),

    [{"Testing strategy with no account id"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, undefined, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"empty call object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kapps_call:new(), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"empty jobj object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kz_json:new(), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"unknown object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, maps:new(), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"passing non raw account id where account is not customized should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, Db, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"passing non raw account id in jobj where account is not customized should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kz_json:from_list([{<<"account_id">>, Db}]), ?TEST_CAT, <<"root_obj_key">>))
       }
      ]
     }
    ,{"Testing some general situation"
     ,[{"not customized account and reseller and empty system_config should result in set Default on system_config"
       ,?_assertEqual(Default, kapps_account_config:get_with_strategy(<<"global">>, ?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT_EMPTY, <<"new_key">>, Default))
       }
      ,{"not customized account and reseller and not exists system_config should result in set Default on system_config"
       ,?_assertEqual(Default, kapps_account_config:get_with_strategy(<<"global">>, ?NO_CONFIG, <<"no_cat">>, <<"new_key">>, Default))
       }
      ]
     }
    ].

get_startegy_global() ->
    FunToTest = fun(AccountId, Category, Key) ->
                        kapps_account_config:get_with_strategy(<<"global">>, AccountId, Category, Key)
                end,
    [{"Testing get config global strategy"
     ,common_get_global_tests(FunToTest)
     }
    ].

get_startegy_reseller() ->
    FunToTest = fun(Args) when length(Args) =:= 3 ->
                        apply(fun kapps_account_config:get_with_strategy/4, [<<"reseller">>|Args]);
                   (Args) when length(Args) =:= 4 ->
                        apply(fun kapps_account_config:get_with_strategy/5, [<<"reseller">>|Args])
                end,
    [{"Testing get config reseller strategy"
     ,common_get_from_reseller_tests(FunToTest)
     }
    ].

%%
%% Test Customized Account scenario
%%
%%  A
%%  |
%%  `-> P -> R ==> APR
%%  |   `--> E(R) ==> AP
%%  |   `--> N(R) ==> AP
%%  |
%%  `-> E(P) -> R ==> AR
%%  |   `-----> E(R) ==> A
%%  |   `-----> N(R) ==> A
%%  |
%%  `-> N(P) -> R ==> AR
%%      `-----> E(R) ==> A
%%      `-----> N(R) ==> A
%% Legend: A=Account P=ParentAccount R=ResellerAccount
%%         E(X)=Account X has the document but it's empty
%%         N(X)=Account X does not have the document
%%
get_startegy_hierarchy_merge() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),
    SubAccount1Value = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_1"),
    SubAccount2Value = get_fixture_value(<<"root_obj_key">>, "test_cat_subaccount_2"),
    CusResellerHierValue = get_fixture_value(<<"root_obj_key">>, "test_cat_reseller"),

    SometimesEmptyValue_System = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], SysValue),
    SometimesEmptyValue_Reseller = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], CusResellerHierValue),
    SometimesEmptyValue_Sub1 = kz_json:get_value([<<"obj_empty_test">>, <<"obj_empty_sometimes">>], SubAccount1Value),

    [{"Testing get config hierarchy_merge strategy"
     ,[{"customized account where account is reseller itself should result in merged value of account and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, CusResellerHierValue])
                     ,kapps_account_config:get_with_strategy(<<"hierarchy_merge">>, ?SELF_RESELLER, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account 1 should result in merged value of account, reseller and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, CusResellerHierValue, SubAccount1Value])
                     ,kapps_account_config:get_hierarchy(?CUSTOMIZED_SUBACCOUNT_1, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, system and empty/not_exists parent, reseller should result in merged value of account and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?CUST_A_404_P_404_R, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, reseller, system and empty/not_exists parent should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, CusResellerHierValue, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?CUST_A_404_P_CUST_R, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parent, system and empty reseller should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?CUST_A_CUST_P_EMPTY_R, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parent, system and not customized reseller should result in merged value of all customized and system"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?CUST_A_CUST_P_404_R, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"customized account, parents and system should result in merged value of all"
       ,?_assertEqual(kz_json:merge_recursive([SysValue, CusResellerHierValue, SubAccount1Value, SubAccount2Value])
                     ,kapps_account_config:get_hierarchy(?CUST_A_CUST_P_CUST_R, ?TEST_CAT, <<"root_obj_key">>)
                     )
       }
      ,{"not customized account with undefined parent account id and no reseller should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_hierarchy(?AN_ACCOUNT_ID, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"check if the account set an empty jobj, the content of the hierarchy is merged into the jobj properly"
       ,?_assertEqual(kz_json:merge_recursive([SometimesEmptyValue_System, SometimesEmptyValue_Reseller, SometimesEmptyValue_Sub1])
                     ,kapps_account_config:get_hierarchy(?CUSTOMIZED_SUBACCOUNT_2, ?TEST_CAT, [<<"root_obj_key">>, <<"obj_empty_test">>, <<"obj_empty_sometimes">>])
                     )
       }
      ]
     }
    ].


get_fixture_value(Key, Fixture) ->
    kz_json:get_value(Key, kapps_config_util:fixture(Fixture)).
