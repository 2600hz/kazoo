%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapps_account_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_config.hrl").

get_global_test_() ->
    SysDefaultValue = get_fixture_value(<<"default">>, "test_cat_system"),
    SysValue = kz_json:get_value(<<"root_obj_key">>, SysDefaultValue),
    ResellerFixture = kapps_config_util:fixture("test_cat_reseller"),
    SubAccountFixture = kapps_config_util:fixture("test_cat_subaccount2"),
    ResellerValue = kz_json:get_value(<<"root_obj_key">>, ResellerFixture),
    SubAccountValue = kz_json:get_value(<<"root_obj_key">>, SubAccountFixture),

    [{"Testing get global account config"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_global(undefined, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and reseller should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, kapps_account_config:get_global(?CUSTOMIZED_RESELLER, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and customized reseller with undefined key should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_global(?CUSTOMIZED_RESELLER_UNDEFINED, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"customized sub-account with undefined key should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_global(?CUSTOMIZED_SUBACCOUNT_UNDEFINED, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"customized sub-account should result in account"
       ,?_assertEqual(SubAccountValue, kapps_account_config:get_global(?CUSTOMIZED_SUBACCOUNT, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"customized sub-account on get_global/2 should result in account"
       ,?_assertEqual(SubAccountFixture, kapps_account_config:get_global(?CUSTOMIZED_SUBACCOUNT, ?TEST_CAT))
       }
      ,{"undefined sub-account on get_global/2 should result in system_config"
       ,?_assertEqual(SysDefaultValue, kapps_account_config:get_global(undefined, ?TEST_CAT))
       }
      ,{"not customized sub-account and customized reseller on get_global/2 should result in reseller"
       ,?_assertEqual(ResellerFixture, kapps_account_config:get_global(?CUSTOMIZED_RESELLER, ?TEST_CAT))
       }
      ,{"not customized sub-account and reseller on get_global/2 should result in system_config"
       ,?_assertEqual(SysDefaultValue, kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT))
       }
      ,{"not customized sub-account and reseller and empty system_config on get_global/2 should result in empty"
       ,?_assertEqual(kz_json:new(), kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT_EMPTY))
       }
      ,{"non exisiting category on get_global/2 should result in empty"
       ,?_assertEqual(kz_json:new(), kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, <<"no_cat">>))
       }
      ]
     }
    ].

get_from_reseller_test_() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),

    [{"Testing get config from reseller"
     ,[{"not customized reseller should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_global(?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"root_obj_key">>))
       }
      ]
     }
    ].

get_with_strategy_test_() ->
    [{"Testing getting account config with strategy"
     ,[strategy_no_account_id()
      ,get_startegy_global_from_sub_account()
      ]
     }
    ].

strategy_no_account_id() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),
    Db = kz_util:format_account_db(?NOT_CUSTOMIZED_ALL_ACCOUNTS),
    [{"Testing strategy with no account id"
     ,[{"undefined account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, 'undefined', ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"empty call object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kapps_call:new(), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"empty jobj object account id should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, kz_json:new(), ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"unkown object account id should result in system_config"
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
    ].

get_startegy_global_from_sub_account() ->
    SysValue = get_fixture_value([<<"default">>, <<"root_obj_key">>], "test_cat_system"),
    ResellerValue = get_fixture_value(<<"root_obj_key">>, "test_cat_reseller"),
    [{"Testing global strategy for sub-account"
     ,[{"not customized sub-account and reseller should result in system_config"
       ,?_assertEqual(SysValue, kapps_account_config:get_with_strategy(<<"global">>, ?NOT_CUSTOMIZED_ALL_ACCOUNTS, ?TEST_CAT, <<"root_obj_key">>))
       }
      ,{"not customized sub-account and customized reseller should result in reseller"
       ,?_assertEqual(ResellerValue, kapps_account_config:get_with_strategy(<<"global">>, ?CUSTOMIZED_RESELLER, ?TEST_CAT, <<"root_obj_key">>))
       }
      ]
     }
    ].

get_fixture_value(Key, Fixture) ->
    kz_json:get_value(Key, kapps_config_util:fixture(Fixture)).
