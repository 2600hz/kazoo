-ifndef(PROVISIONER_V5_HRL).

-define(MOD_CONFIG_CAT, <<"provisioner">>).
-define(SCHEMA, <<"provisioner_v5">>).
-define(FEATURE_KEYS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"feature_keys">>, ?LOCAL_FEATURE_KEYS)).
-define(COMBO_KEYS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"combo_keys">>, ?LOCAL_COMBO_KEYS)).

-define(KEYS_FUN(A, B, C, D), kz_json:from_list(
                                [{<<"presence">>, A}
                                ,{<<"speed_dial">>, B}
                                ,{<<"parking">>, C}
                                ,{<<"personal_parking">>, D}
                                ])).

-define(LOCAL_FEATURE_KEYS, kz_json:from_list(
                              [{<<"polycom">>, ?POLYCOM_FEATURE_KEYS}
                              ,{<<"yealink">>, ?YEALINK_FEATURE_KEYS}
                              ,{<<"cisco">>, ?CISCO_FEATURE_KEYS}
                              ,{<<"grandstream">>, ?GRANDSTREAM_FEATURE_KEYS}
                              ,{<<"obihai">>, ?OBIHAI_FEATURE_KEYS}
                              ])).

-define(POLYCOM_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"normal">>, <<"normal">>, <<"automata">>, <<"automata">>)}]
         )).

-define(YEALINK_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"16">>, <<"13">>, <<"10">>, <<"10">>)}]
         )).

-define(CISCO_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<>>, <<>>, <<>>, <<>>)}]
         )).

-define(GRANDSTREAM_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"1">>, <<"0">>, <<"9">>, <<"1">>)}]
         )).

-define(OBIHAI_FEATURE_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"Busy Lamp Field">>, <<"Speed Dial">>, <<"Call Park Monitor">>, <<"Busy Lamp Field">>)}]
         )).


-define(LOCAL_COMBO_KEYS, kz_json:from_list(
                            [{<<"polycom">>, ?POLYCOM_COMBO_KEYS}
                            ,{<<"yealink">>, ?YEALINK_COMBO_KEYS}
                            ,{<<"cisco">>, ?CISCO_COMBO_KEYS}
                            ,{<<"grandstream">>, ?GRANDSTREAM_COMBO_KEYS}
                            ,{<<"obihai">>, ?OBIHAI_COMBO_KEYS}
                            ])).

-define(POLYCOM_COMBO_KEYS, ?POLYCOM_FEATURE_KEYS).

-define(YEALINK_COMBO_KEYS, ?YEALINK_FEATURE_KEYS).

-define(CISCO_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<>>, <<>>, <<>>, <<>>)}]
         )).

-define(GRANDSTREAM_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"11">>, <<"10">>, <<"19">>, <<"11">>)}]
         )).

-define(OBIHAI_COMBO_KEYS, ?OBIHAI_FEATURE_KEYS).

-define(PROVISIONER_V5_HRL, 'true').
-endif.
