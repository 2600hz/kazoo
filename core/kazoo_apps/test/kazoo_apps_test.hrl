-ifndef(KAZOO_APPS_TEST_HRL).

-define(TEST_CAT, <<"test_account_config">>).

-define(SUB_EMPTY, <<"test_account_config_sub_empty">>).
-define(RESELLER_ONLY, <<"test_account_config_reseller_only">>).
-define(RESELLER_SYSTEM, <<"test_account_config_reseller_system">>).
-define(SYSTEM_EMPTY, <<"test_account_config_system_empty">>).
-define(SYSTEM_ONLY, <<"test_account_config_system_only">>).

get_fixture_value(Key, DbName, Category) ->
    {'ok', JObj} = get_fixture(DbName, Category),
    kz_json:get_value(Key, JObj).

get_fixture(?KZ_CONFIG_DB, Category) ->
    Path = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, Category),
    kz_json:fixture(Path);
get_fixture(AccountId, Category) ->
    Path = kz_fixturedb_util:get_doc_path(kzs_util:format_account_db(AccountId), <<"configs_", Category/binary>>),
    kz_json:fixture(Path).


-define(KAZOO_APPS_TEST_HRL, 'true').
-endif.
