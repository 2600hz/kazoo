-ifndef(KAPPS_CONFERENCE_HRL).

-define(CONFERENCE_CONFIG_CAT, <<"conferences">>).

-define(DEFAULT_PROFILE_NAME, <<"default">>).
-define(PAGE_PROFILE_NAME, <<"page">>).

-define(DEFAULT_ENTRY_TONE, <<"tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)">>).
-define(ENTRY_TONE(AccountId), kapps_account_config:get_global(AccountId, ?CONFERENCE_CONFIG_CAT, <<"entry_tone">>, ?DEFAULT_ENTRY_TONE)).
-define(MOD_ENTRY_TONE(AccountId), kapps_account_config:get_global(AccountId, ?CONFERENCE_CONFIG_CAT, <<"moderator_entry_tone">>, ?DEFAULT_ENTRY_TONE)).

-define(DEFAULT_EXIT_TONE, <<"tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)">>).
-define(EXIT_TONE(AccountId), kapps_account_config:get_global(AccountId, ?CONFERENCE_CONFIG_CAT, <<"exit_tone">>, ?DEFAULT_EXIT_TONE)).
-define(MOD_EXIT_TONE(AccountId), kapps_account_config:get_global(AccountId, ?CONFERENCE_CONFIG_CAT, <<"moderator_exit_tone">>, ?DEFAULT_EXIT_TONE)).

-define(SUPPORT_NAME_ANNOUNCEMENT(AccountId), kz_term:is_true(kapps_account_config:get_global(AccountId, ?CONFERENCE_CONFIG_CAT, <<"support_name_announcement">>, 'true'))).

-define(DEFAULT_PROFILE_CONFIG, [{<<"rate">>, 16000}
                                ,{<<"caller-controls">>, <<"default">>}
                                ,{<<"moderator-controls">>, <<"default">>}
                                ,{<<"interval">>, 20}
                                ,{<<"energy-level">>, 20}
                                ,{<<"comfort-noise">>, 1000}
                                ,{<<"moh-sound">>, <<"$${hold_music}">>}
                                ,{<<"enter-sound">>, ?DEFAULT_ENTRY_TONE}
                                ,{<<"exit-sound">>, ?DEFAULT_EXIT_TONE}
                                ,{<<"max-members-sound">>, <<"prompt://system_media/conf-max_participants">>}
                                ,{<<"locked-sound">>, <<"prompt://system_media/conf-max_participants">>}
                                ,{<<"muted-sound">>, <<"prompt://system_media/conf-muted">>}
                                ,{<<"unmuted-sound">>, <<"prompt://system_media/conf-unmuted">>}
                                ,{<<"deaf-sound">>, <<"prompt://system_media/conf-deaf">>}
                                ,{<<"undeaf-sound">>, <<"prompt://system_media/conf-undeaf">>}
                                ,{<<"member-enter-sound">>, <<"prompt://system_media/conf-joining_conference">>}
                                ,{<<"alone-sound">>, <<"prompt://system_media/conf-alone">>}
                                ]).

-define(PAGE_PROFILE_CONFIG, [{<<"rate">>, 8000}
                             ,{<<"caller-controls">>, <<"paging">>}
                             ,{<<"interval">>, 20}
                             ,{<<"energy-level">>, 20}
                             ,{<<"comfort-noise">>, 1000}
                             ,{<<"moh-sound">>, <<>>}
                             ,{<<"enter-sound">>, <<>>}
                             ]).

-define(DEFAULT_CONTROLS, [kz_json:from_list([{<<"action">>, <<"mute">>},     {<<"digits">>, <<"*1">>}])
                          ,kz_json:from_list([{<<"action">>, <<"mute on">>},  {<<"digits">>, <<"*2">>}])
                          ,kz_json:from_list([{<<"action">>, <<"mute off">>}, {<<"digits">>, <<"*3">>}])
                          ,kz_json:from_list([{<<"action">>, <<"deaf">>},     {<<"digits">>, <<"*4">>}])
                          ,kz_json:from_list([{<<"action">>, <<"deaf on">>},  {<<"digits">>, <<"*5">>}])
                          ,kz_json:from_list([{<<"action">>, <<"deaf off">>}, {<<"digits">>, <<"*6">>}])
                          ,kz_json:from_list([{<<"action">>, <<"hangup">>},   {<<"digits">>, <<"#">>}])
                          ]).

-define(DEFAULT_ADVERTISE_CONFIG, 'undefined').
-define(PAGE_ADVERTISE_CONFIG, 'undefined').
-define(ADVERTISE(ConfigName, Default)
       ,kapps_config:get_json(?CONFIG_CAT, [<<"advertise">>, ConfigName], Default)
       ).
-define(ADVERTISE(ConfigName), ?ADVERTISE(ConfigName, 'undefined')).

-define(DEFAULT_CHAT_CONFIG, 'undefined').
-define(PAGE_CHAT_CONFIG, 'undefined').
-define(CHAT_PERMISSIONS(ConfigName, Default)
       ,kapps_config:get_json(?CONFIG_CAT, [<<"chat-permissions">>, ConfigName], Default)
       ).
-define(CHAT_PERMISSIONS(ConfigName), ?CHAT_PERMISSIONS(ConfigName, 'undefined')).

-define(KAPPS_CONFERENCE_HRL, 'true').
-endif.
