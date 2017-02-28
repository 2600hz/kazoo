-ifndef(KNM_HRL).
-include_lib("kazoo/include/kz_databases.hrl").
-include("knm_phone_number.hrl").

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"kazoo_number_manager">>).
-define(APP, 'kazoo_number_manager').

-define(CACHE_NAME, 'knm_cache').
-define(KNM_CONFIG_CAT, <<"number_manager">>).

-define(KNM_USER_AGENT, "Kazoo Number Manager " ++ binary_to_list(?APP_VERSION)).

-define(IS_US_TOLLFREE(Prefix)
       ,Prefix == <<"800">>
            orelse Prefix == <<"822">>
            orelse Prefix == <<"833">>
            orelse Prefix == <<"844">>
            orelse Prefix == <<"855">>
            orelse Prefix == <<"866">>
            orelse Prefix == <<"877">>
            orelse Prefix == <<"880">>
            orelse Prefix == <<"881">>
            orelse Prefix == <<"882">>
            orelse Prefix == <<"883">>
            orelse Prefix == <<"884">>
            orelse Prefix == <<"885">>
            orelse Prefix == <<"886">>
            orelse Prefix == <<"887">>
            orelse Prefix == <<"888">>
            orelse Prefix == <<"889">>
       ).

-define(IS_US_TOLLFREE_WILDCARD(Prefix)
       ,Prefix == <<"80*">>
            orelse Prefix == <<"84*">>
            orelse Prefix == <<"85*">>
            orelse Prefix == <<"86*">>
            orelse Prefix == <<"87*">>
            orelse Prefix == <<"88*">>
       ).


-ifdef(TEST).
-define(START_BLOCK, <<"+14158867900">>).
-define(END_BLOCK, <<"+14158897909">>).

-define(BLOCK_PHONEBOOK_URL, <<"http://blocks.tld">>).
-define(NUMBER_PHONEBOOK_URL_L, "http://numbers.tld").
-define(NUMBER_PHONEBOOK_URL, <<?NUMBER_PHONEBOOK_URL_L>>).

-define(TEST_CREATE_NUM, <<"+15559871234">>).
-define(TEST_AVAILABLE_NUM, <<"+15551239876">>).
-define(TEST_IN_SERVICE_BAD_CARRIER_NUM, <<"+15551233337">>).
-define(TEST_IN_SERVICE_NUM, <<"+15551233322">>).
-define(TEST_IN_SERVICE_MDN, <<"+15551233324">>).
-define(TEST_IN_SERVICE_WITH_HISTORY_NUM, <<"+15551255693">>).
-define(TEST_CREATE_TOLL, <<"+18887771111">>).
-define(TEST_EXISTING_TOLL, <<"+18005551212">>).
-define(TEST_OLD_NUM, <<"+15045551226">>).
-define(TEST_OLD2_NUM, <<"+12014370855">>).
-define(TEST_OLD3_NUM, <<"+14082141750">>).
-define(TEST_OLD4_NUM, <<"+14242424247">>).
-define(TEST_OLD5_NUM, <<"+19377038880">>).
-define(TEST_OLD6_NUM, <<"+12156774700">>).
-define(TEST_TELNYX_NUM, <<"+14352154006">>).
-define(BW_EXISTING_DID, <<"+14122065197">>).

-define(MASTER_ACCOUNT_ID,   <<"master_account_6992af0e9504d0b27">>).
-define(RESELLER_ACCOUNT_ID, <<"reseller_account_b113394f16cb76d">>).
-define(CHILD_ACCOUNT_ID,    <<"child_account_670a04df0014d0b27a">>).

-define(PVT_TREE, [?MASTER_ACCOUNT_ID
                  ,?RESELLER_ACCOUNT_ID
                  ]).

-define(RESELLER_ACCOUNT_DOC, kz_json:from_list(
                                [{<<"_id">>, ?RESELLER_ACCOUNT_ID}]
                               )).

-define(AVAILABLE_NUMBER
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_AVAILABLE_NUM}
          ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{<<"my_key">>, <<"my string">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
          ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(IN_SERVICE_BAD_CARRIER_NUMBER
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_IN_SERVICE_BAD_CARRIER_NUM}
          ,{<<"_rev">>, <<"3-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, <<"wnm_pacwest">>}
          ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(IN_SERVICE_NUMBER
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_IN_SERVICE_NUM}
          ,{<<"_rev">>, <<"3-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
          ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(IN_SERVICE_MDN
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_IN_SERVICE_MDN}
          ,{<<"_rev">>, <<"4-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, ?CARRIER_MDN}
          ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(IN_SERVICE_WITH_HISTORY_NUMBER
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_IN_SERVICE_WITH_HISTORY_NUM}
          ,{<<"_rev">>, <<"3-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID, ?MASTER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
          ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(EXISTING_TOLL
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_EXISTING_TOLL}
          ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
          ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1800">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ,{?PVT_USED_BY, <<"callflow">>}
          ])).

-define(TELNYX_NUMBER
       ,kz_json:from_list(
          [{<<"_id">>, ?TEST_TELNYX_NUM}
          ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
          ,{?PVT_MODIFIED, 63565934349}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
          ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
          ,{?PVT_MODULE_NAME, <<"knm_telnyx">>}
          ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1435">>}
          ,{?PVT_CREATED, 63565934344}
          ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
          ])).

-define(BW_EXISTING_JSON
       ,kz_json:from_list(
          [{<<"_id">>, <<"+14122065197">>}
          ,{<<"_rev">>, <<"4-96e8a3de0419862c3e4a18e3251a6dd7">>}
          ,{?PVT_DB_NAME, <<"numbers%2F%2B1412">>}
          ,{?PVT_FEATURES, kz_json:new()}
          ,{?PVT_STATE, ?NUMBER_STATE_DISCOVERY}
          ,{?PVT_RESERVE_HISTORY, []}
          ,{?PVT_PORTED_IN, 'false'}
          ,{?PVT_MODULE_NAME, <<"knm_bandwidth">>}
          ,{?PVT_CARRIER_DATA
           ,kz_json:from_list(
              [{<<"number_id">>, <<"0C107941-CDDA-42FE-823C-042EADBD3719">>}
              ,{<<"ten_digit">>, <<"4122065197">>}
              ,{<<"formatted_number">>, <<"1-412-206-5197">>}
              ,{<<"e164">>, <<"+14122065197">>}
              ,{<<"npa_nxx">>, <<"412206">>}
              ,{<<"status">>, <<"Available">>}
              ,{<<"rate_center">>, kz_json:from_list(
                                     [{<<"name">>, <<"PITTSBURGH SUBURBAN ZONE 13">>}
                                     ,{<<"lata">>, <<"234">>}
                                     ,{<<"state">>, <<"PA">>}
                                     ])
               }
              ])
           }
          ,{?PVT_MODIFIED, 63610268576}
          ,{?PVT_CREATED, 63610268576}
          ,{?PVT_TYPE, <<"number">>}
          ])).

-define(LOG_WARN(F,A), io:format(user, F++ "\n", A)).
-define(LOG_DEBUG(F,A), io:format(user, F++"\n", A)).
-define(LOG_DEBUG(F), io:format(user, F++ "\n", [])).
-else.
-define(LOG_WARN(F,A), lager:warning(F,A)).
-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).
-endif.

-define(KNM_HRL, 'true').
-endif.
