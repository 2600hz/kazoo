-ifndef(CONFERENCE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_api.hrl").

-define(APP_NAME, <<"conference">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"conferences">>).

-define(CONFERENCE_CACHE, 'conference_cache').

-define(DEFAULT_PROFILE_NAME, <<"default">>).

-define(DEFAULT_ENTRY_TONE, <<"tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)">>).
-define(ENTRY_TONE, whapps_config:get(?CONFIG_CAT, <<"entry_tone">>, ?DEFAULT_ENTRY_TONE)).
-define(MOD_ENTRY_TONE, whapps_config:get(?CONFIG_CAT, <<"moderator_entry_tone">>, ?DEFAULT_ENTRY_TONE)).

-define(DEFAULT_EXIT_TONE, <<"tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)">>).
-define(EXIT_TONE, whapps_config:get(?CONFIG_CAT, <<"exit_tone">>, ?DEFAULT_EXIT_TONE)).

-define(DEFAULT_PROFILE_CONFIG, [{<<"rate">>, 8000}
                                 ,{<<"caller-controls">>, <<"default">>}
                                 ,{<<"interval">>, 20}
                                 ,{<<"energy-level">>, 20}
                                 ,{<<"comfort-noise">>, 1000}
                                 ,{<<"moh-sound">>, <<"$${hold_music}">>}
                                 ,{<<"enter-sound">>, ?ENTRY_TONE}
                                ]).

-define(CALLER_CONTROLS(ConfigName, Default)
        ,whapps_config:get(?CONFIG_CAT, [<<"caller-controls">>, ConfigName], [])
       ).
-define(CALLER_CONTROLS(ConfigName), ?CALLER_CONTROLS(ConfigName, 'undefined')).

-define(ADVERTISE(ConfigName, Default)
        ,whapps_config:get(?CONFIG_CAT, [<<"advertise">>, ConfigName], Default)
       ).
-define(ADVERTISE(ConfigName), ?ADVERTISE(ConfigName, 'undefined')).

-define(CHAT_PERMISSIONS(ConfigName, Default)
        ,whapps_config:get(?CONFIG_CAT, [<<"chat-permissions">>, ConfigName], Default)
       ).
-define(CHAT_PERMISSIONS(ConfigName), ?CHAT_PERMISSIONS(ConfigName, 'undefined')).

-define(DEFAULT_ADVERTISE_CONFIG, 'undefined').
-define(DEFAULT_CHAT_CONFIG, 'undefined').

-define(DEFAULT_MAX_MEMBERS_MEDIA, <<"conf-max_participants">>).

-define(CONFERENCE_HRL, 'true').
-endif.
