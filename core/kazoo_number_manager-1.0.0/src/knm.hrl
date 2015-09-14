-ifndef(KNM_HRL).
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"kazoo_number_manager">>).

-define(KNM_CACHE, 'knm_cache').
-define(KNM_CONFIG_CAT, <<"number_manager">>).

-define(FEATURE_OUTBOUND_CNAM, <<"outbound_cnam">>).
-define(FEATURE_INBOUND_CNAM, <<"inbound_cnam">>).
-define(FEATURE_CNAM, <<"cnam">>).

-define(KEY_DISPLAY_NAME, <<"display_name">>).
-define(KEY_INBOUND_LOOKUP, <<"inbound_lookup">>).

-define(KNM_USER_AGENT, "Kazoo Number Manager 1.0.0").

-ifdef(TEST).
-define(TEST_CREATE_NUM, <<"5559871234">>).
-define(TEST_EXISTING_NUM, <<"+15551239876">>).

-define(MASTER_ACCOUNT_ID, <<"master_account_6992af0e9504d0b27">>).
-define(RESELLER_ACCOUNT_ID, <<"reseller_account_b113394f16cb76d">>).

-define(PVT_TREE, [?MASTER_ACCOUNT_ID
                   ,?RESELLER_ACCOUNT_ID
                  ]).

-define(RESELLER_ACCOUNT_DOC
        ,wh_json:from_list(
           [{<<"_id">>, ?RESELLER_ACCOUNT_ID}]
          )
       ).

-define(EXISTING_NUMBER
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_EXISTING_NUM}
            ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
            ,{<<"pvt_modified">>, 63565934349}
            ,{<<"pvt_features">>, wh_json:new()}
            ,{<<"pvt_assigned_to">>, ?RESELLER_ACCOUNT_ID}
            ,{<<"pvt_reserve_history">>, [?RESELLER_ACCOUNT_ID]}
            ,{<<"pvt_module_name">>, ?CARRIER_LOCAL}
            ,{<<"pvt_number_state">>, ?NUMBER_STATE_IN_SERVICE}
            ,{<<"pvt_db_name">>, <<"numbers%2F%2B1555">>}
            ,{<<"pvt_created">>, 63565934344}
            ,{<<"pvt_authorizing_account">>, ?MASTER_ACCOUNT_ID}
            ,{<<"used_by">>, <<"callflow">>}
           ]
          )
       ).
-endif.

-define(KNM_HRL, 'true').
-endif.
