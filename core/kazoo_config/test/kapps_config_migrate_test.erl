-module(kapps_config_migrate_test).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_config.hrl").

kz_account_test_() ->
    {'setup'
    ,fun setup/0
    ,fun cleanup/1
    ,fun('ok') ->
             [test_whapps_controller_migrate()]
     end
    }.

setup() ->
    {'ok', _} = application:ensure_all_started('kazoo_config'),
    {'ok', _CachesPid} = kazoo_caches_sup:start_link(),
    {'ok', _LinkPid} = kazoo_data_link_sup:start_link(),

    'ok'.

cleanup('ok') ->
    application:stop('kazoo_config').

test_whapps_controller_migrate() ->
    WhappsPath = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, <<"whapps_controller">>),
    {'ok', WhappsController} = kz_json:fixture(WhappsPath),

    KappsPath = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, <<"kapps_controller">>),
    {'ok', KappsController} = kz_json:fixture(KappsPath),

    Migrated = kapps_config:migrate_from_doc(WhappsController, kz_json:from_list([{<<"_id">>, <<"kapps_controller">>}])),
    [?_assert(kz_json:are_equal(KappsController, Migrated))].
