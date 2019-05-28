-ifndef(PROVISIONER_V5_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(MOD_CONFIG_CAT, <<"provisioner">>).
-define(MOD_CONFIG_CLUSTER, <<"cluster">>).
-define(FEATURE_KEYS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"feature_keys">>, ?LOCAL_FEATURE_KEYS)).
-define(COMBO_KEYS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"combo_keys">>, ?LOCAL_COMBO_KEYS)).

-define(KEYS_FUN(A, B, C, D), ?KEYS_FUN(A, B, C, D, undefined)).
-define(KEYS_FUN(A, B, C, D, E), kz_json:from_list(
                                   [{<<"presence">>, A}
                                   ,{<<"speed_dial">>, B}
                                   ,{<<"parking">>, C}
                                   ,{<<"personal_parking">>, D}
                                   ,{<<"line">>, E}
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
                            [{<<"yealink">>, ?YEALINK_COMBO_KEYS}
                            ,{<<"cisco">>, ?CISCO_COMBO_KEYS}
                            ,{<<"grandstream">>, ?GRANDSTREAM_COMBO_KEYS}
                            ,{<<"obihai">>, ?OBIHAI_COMBO_KEYS}
                            ,{<<"vtech">>, ?VTECH_COMBO_KEYS}
                            ])).

-define(YEALINK_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"16">>, <<"13">>, <<"10">>, <<"10">>, <<"15">>)}]
         )).

-define(CISCO_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<>>, <<>>, <<>>, <<>>, <<>>)}]
         )).

-define(GRANDSTREAM_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"11">>, <<"10">>, <<"19">>, <<"11">>, <<"0">>)}]
         )).

-define(OBIHAI_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"Busy Lamp Field">>, <<"Speed Dial">>, <<"Call Park Monitor">>, <<"Busy Lamp Field">>, <<"Call Appearance">>)}]
         )).

-define(VTECH_COMBO_KEYS,
        kz_json:from_list(
          [{<<"_">>, ?KEYS_FUN(<<"presense">>, <<"speed">>, <<"blf">>, <<"blf">>, <<"line">>)}]
         )).

-define(PROVISIONER_V5_HRL, 'true').
-endif.
