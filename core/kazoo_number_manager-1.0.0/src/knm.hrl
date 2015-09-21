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
            ,{?PVT_MODIFIED, 63565934349}
            ,{?PVT_FEATURES, wh_json:new()}
            ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
            ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
            ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
            ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
            ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
            ,{?PVT_CREATED, 63565934344}
            ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
            ,{?PVT_USED_BY, <<"callflow">>}
           ]
          )
       ).

-define(BLOCK_PHONEBOOK_URL, <<"http://blocks.tld">>).

-define(START_BLOCK, <<"+14158867900">>).
-define(END_BLOCK, <<"+14158897909">>).
-define(BLOCK_RESP
        ,wh_json:from_list([{<<"start_number">>, ?START_BLOCK}
                            ,{<<"end_number">>, ?END_BLOCK}
                           ])
       ).

-define(BLOCKS_RESP
        ,wh_json:from_list([{<<"status">>, <<"success">>}
                            ,{<<"data">>, [?BLOCK_RESP]}
                           ])
       ).

-define(NUMBER_PHONEBOOK_URL_L, "http://numbers.tld").
-define(NUMBER_PHONEBOOK_URL, <<?NUMBER_PHONEBOOK_URL_L>>).

-define(NUMBERS_DATA
        ,wh_json:from_list(
           [{<<"+1415886790", (D + $0)>>
             ,wh_json:from_list([{<<"extension">>, D}])
            }
            || D <- lists:seq(0,9)
           ]
          )
       ).

-define(NUMBERS_RESPONSE
        ,wh_json:from_list([{<<"status">>, <<"success">>}
                            ,{<<"data">>, ?NUMBERS_DATA}
                           ])
       ).

-endif.

-define(KNM_HRL, 'true').
-endif.
