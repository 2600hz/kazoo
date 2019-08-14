-module(kapps_config_migrate_tests).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_apps.hrl").

migrate_test_() ->
    {'setup'
    ,fun kazoo_apps_test_util:setup/0
    ,fun kazoo_apps_test_util:cleanup/1
    ,fun test_whapps_controller_migrate/1
    }.

test_whapps_controller_migrate(_Setup) ->
    WhappsPath = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, <<"whapps_controller">>),
    {'ok', WhappsController} = kz_json:fixture(WhappsPath),

    KappsPath = kz_fixturedb_util:get_doc_path(?KZ_CONFIG_DB, <<"kapps_controller">>),
    {'ok', KappsController} = kz_json:fixture(KappsPath),

    Migrated = kapps_config:migrate_from_doc(WhappsController, kz_json:from_list([{<<"_id">>, <<"kapps_controller">>}])),

    [{"migrated whapps_controller to kapps_controller"
     ,?_assert(kz_json:are_equal(kz_doc:public_fields(KappsController), kz_doc:public_fields(Migrated)))
     }
    ].
