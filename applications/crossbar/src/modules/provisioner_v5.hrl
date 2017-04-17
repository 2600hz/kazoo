-ifndef(PROVISIONER_V5_HRL).

-define(MOD_CONFIG_CAT, <<"provisioner">>).
-define(SCHEMA, <<"provisioner_v5">>).
-define(FEATURE_KEYS, kapps_config:get(?MOD_CONFIG_CAT, <<"feature_keys">>, ?LOCAL_FEATURE_KEYS)).

-define(FEATURE_KEYS_FUN(A, B, C, D), kz_json:from_list(
                                        [{<<"presence">>, A}
                                        ,{<<"speed_dial">>, B}
                                        ,{<<"parking">>, C}
                                        ,{<<"personal_parking">>, D}
                                        ])).

-define(LOCAL_FEATURE_KEYS, kz_json:from_list(
                              [{<<"polycom">>, ?POLYCOM_FEATURE_KEYS}
                              ,{<<"yealink">>, ?YEALINK_FEATURE_KEYS}
                              ,{<<"cisco">>, ?CISCO_FEATURE_KEYS}
                              ])).

-define(POLYCOM_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?FEATURE_KEYS_FUN(<<"normal">>, <<"normal">>, <<"automata">>, <<"automata">>)}]
         )).

-define(YEALINK_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?FEATURE_KEYS_FUN(<<"16">>, <<"13">>, <<"10">>, <<"10">>)}]
         )).

-define(CISCO_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?FEATURE_KEYS_FUN(<<"">>, <<"">>, <<"">>, <<"">>)}]
         )).

-define(PROVISIONER_V5_HRL, 'true').
-endif.
