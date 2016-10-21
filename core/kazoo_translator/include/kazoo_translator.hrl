-ifndef(KAZOO_TRANSLATOR_HRL).

-define(DEFAULT_TTS_ENGINE,
        kapps_config:get_binary(<<"speech">>, <<"tts_provider">>, <<"flite">>)).
-define(DEFAULT_TTS_ENGINE(Call),
        kapps_account_config:get_global(Call, <<"speech">>, <<"tts_provider">>, <<"flite">>)).

-define(KAZOO_TRANSLATOR_HRL, 'true').
-endif.
