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
-include_lib("eunit/include/eunit.hrl").
-define(LOG_WARN(F,A), ?debugFmt(F ++ "\n",A)).
-define(LOG_DEBUG(F,A), ?debugFmt(F ++ "\n",A)).
-define(LOG_DEBUG(F),?debugFmt(F ++ "\n",[])).

-define(TEST_CREATE_NUM, <<"+15559871234">>).
-define(TEST_AVAILABLE_NUM, <<"+15551239876">>).
-define(TEST_IN_SERVICE_NUM, <<"+15551233322">>).
-define(TEST_IN_SERVICE_WITH_HISTORY_NUM, <<"+15551255693">>).
-define(TEST_CREATE_TOLL, <<"+18887771111">>).
-define(TEST_EXISTING_TOLL, <<"+18005551212">>).

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

-define(AVAILABLE_NUMBER
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_AVAILABLE_NUM}
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

-define(IN_SERVICE_NUMBER
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_IN_SERVICE_NUM}
            ,{<<"_rev">>, <<"3-7dd6a1523e81a4e3c2689140ed3a8e69">>}
            ,{?PVT_MODIFIED, 63565934349}
            ,{?PVT_FEATURES, wh_json:new()}
            ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
            ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
            ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
            ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
            ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
            ,{?PVT_CREATED, 63565934344}
            ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
            ,{?PVT_USED_BY, <<"callflow">>}
           ]
          )
       ).

-define(IN_SERVICE_WITH_HISTORY_NUMBER
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_IN_SERVICE_WITH_HISTORY_NUM}
            ,{<<"_rev">>, <<"3-7dd6a1523e81a4e3c2689140ed3a8e69">>}
            ,{?PVT_MODIFIED, 63565934349}
            ,{?PVT_FEATURES, wh_json:new()}
            ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
            ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID, ?MASTER_ACCOUNT_ID]}
            ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
            ,{?PVT_STATE, ?NUMBER_STATE_IN_SERVICE}
            ,{?PVT_DB_NAME, <<"numbers%2F%2B1555">>}
            ,{?PVT_CREATED, 63565934344}
            ,{?PVT_AUTH_BY, ?MASTER_ACCOUNT_ID}
            ,{?PVT_USED_BY, <<"callflow">>}
           ]
          )
       ).


-define(EXISTING_TOLL
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_EXISTING_TOLL}
            ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
            ,{?PVT_MODIFIED, 63565934349}
            ,{?PVT_FEATURES, wh_json:new()}
            ,{?PVT_ASSIGNED_TO, ?RESELLER_ACCOUNT_ID}
            ,{?PVT_RESERVE_HISTORY, [?RESELLER_ACCOUNT_ID]}
            ,{?PVT_MODULE_NAME, ?CARRIER_LOCAL}
            ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
            ,{?PVT_DB_NAME, <<"numbers%2F%2B1800">>}
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

-define(BANDWIDTH_NPAN_RESPONSE
        ,knm_util:fixture("bandwidth_numbersearch_response.xml")
       ).

-define(BANDWIDTH_AREACODE_RESPONSE
        ,knm_util:fixture("bandwidth_areacode_response.xml")
       ).

-define(BW_EXISTING_DID, <<"+14122065197">>).
-define(BW_EXISTING_JSON
        ,wh_json:from_list(
           [{<<"_id">>,<<"+14122065197">>},
            {<<"pvt_db_name">>,<<"numbers%2F%2B1412">>},
            {<<"pvt_features">>,wh_json:new()},
            {<<"pvt_state">>,<<"discovery">>},
            {<<"pvt_reserve_history">>,[]},
            {<<"pvt_ported_in">>,'false'},
            {<<"pvt_module_name">>,<<"knm_bandwidth">>},
            {<<"pvt_carrier_data">>
             ,wh_json:from_list(
                [{<<"number_id">>, <<"0C107941-CDDA-42FE-823C-042EADBD3719">>},
                 {<<"ten_digit">>,<<"4122065197">>},
                 {<<"formatted_number">>,<<"1-412-206-5197">>},
                 {<<"e164">>,<<"+14122065197">>},
                 {<<"npa_nxx">>,<<"412206">>},
                 {<<"status">>,<<"Available">>},
                 {<<"rate_center">>
                  ,wh_json:from_list(
                     [{<<"name">>,<<"PITTSBURGH SUBURBAN ZONE 13">>},
                      {<<"lata">>,<<"234">>},
                      {<<"state">>,<<"PA">>}
                     ])
                 }
                ])
            },
            {<<"pvt_modified">>,63610268576},
            {<<"pvt_created">>,63610268576},
            {<<"pvt_type">>,<<"number">>}]
          )
       ).
-else.

-define(LOG_WARN(F,A), lager:warning(F,A)).
-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).

-endif.

-define(KNM_HRL, 'true').
-endif.
