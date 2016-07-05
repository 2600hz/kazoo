%% -*- coding: utf-8 -*-
-ifndef(KAPPS_SPEECH_HRL).

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(ISPEECH_VOICE_MAPPINGS
        ,[{<<"female/en-us">>, <<"usenglishfemale">>}
          ,{<<"male/en-us">>, <<"usenglishmale">>}
          ,{<<"female/en-ca">>, <<"caenglishfemale">>}
          ,{<<"female/en-au">>, <<"auenglishfemale">>}
          ,{<<"female/en-gb">>, <<"ukenglishfemale">>}
          ,{<<"male/en-gb">>, <<"ukenglishmale">>}
          ,{<<"female/es-us">>, <<"usspanishfemale">>}
          ,{<<"male/es-us">>, <<"usspanishmale">>}
          ,{<<"female/us-us">>, <<"usspanishfemale">>}
          ,{<<"female/zh-cn">>, <<"chchinesefemale">>}
          ,{<<"male/zh-cn">>, <<"chchinesemale">>}
          ,{<<"female/zh-hk">>, <<"hkchinesefemale">>}
          ,{<<"female/zh-tw">>, <<"twchinesefemale">>}
          ,{<<"female/ja-jp">>, <<"jpjapanesefemale">>}
          ,{<<"male/ja-jp">>, <<"jpjapanesemale">>}
          ,{<<"female/ko-kr">>, <<"krkoreanfemale">>}
          ,{<<"male/ko-kr">>, <<"krkoreanmale">>}
          ,{<<"female/da-dk">>, <<"eurdanishfemale">>}
          ,{<<"female/de-de">>, <<"eurgermanfemale">>}
          ,{<<"male/de-de">>, <<"eurgermanmale">>}
          ,{<<"female/ca-es">>, <<"eurcatalanfemale">>}
          ,{<<"female/es-es">>, <<"eurspanishfemale">>}
          ,{<<"male/es-es">>, <<"eurspanishmale">>}
          ,{<<"female/fi-fi">>, <<"eurfinnishfemale">>}
          ,{<<"female/fr-ca">>, <<"cafrenchfemale">>}
          ,{<<"male/fr-ca">>, <<"cafrenchmale">>}
          ,{<<"female/fr-fr">>, <<"eurfrenchfemale">>}
          ,{<<"male/fr-fr">>, <<"eurfrenchmale">>}
          ,{<<"female/it-it">>, <<"euritalianfemale">>}
          ,{<<"male/it-it">>, <<"euritalianmale">>}
          ,{<<"female/nb-no">>, <<"eurnorwegianfemale">>}
          ,{<<"female/nl-nl">>, <<"eurdutchfemale">>}
          ,{<<"female/pl-pl">>, <<"eurpolishfemale">>}
          ,{<<"female/pt-br">>, <<"brportuguesefemale">>}
          ,{<<"female/pt-pt">>, <<"eurportuguesefemale">>}
          ,{<<"male/pt-pt">>, <<"eurportuguesemale">>}
          ,{<<"female/ru-ru">>, <<"rurussianfemale">>}
          ,{<<"male/ru-ru">>, <<"rurussianmale">>}
          ,{<<"female/sv-se">>, <<"swswedishfemale">>}
          ,{<<"female/hu-hu">>, <<"huhungarianfemale">>}
          ,{<<"female/cs-cz">>, <<"eurczechfemale">>}
          ,{<<"female/tr-tr">>, <<"eurturkishfemale">>}
          ,{<<"male/tr-tr">>, <<"eurturkishmale">>}
         ]
       ).
-define(ISPEECH_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url">>, <<"http://api.ispeech.org/api/json">>)).

-define(VOICEFABRIC_VOICE_MAPPINGS
       ,[{<<"male/ru-ru">>, <<"Владимир8000"/utf8>>}
        ,{<<"female/ru-ru">>, <<"Юлия8000"/utf8>>}
        ,{<<"female/en-us">>, <<"Carol8000">>}
        ,{<<"male/ru-ru/vladimir">>, <<"Владимир8000"/utf8>>}
        ,{<<"female/ru-ru/julia">>, <<"Юлия8000"/utf8>>}
        ,{<<"female/ru-ru/anna">>, <<"Анна8000"/utf8>>}
        ,{<<"female/ru-ru/viktoria">>, <<"Виктория8000"/utf8>>}
        ,{<<"male/ru-ru/alexander">>, <<"Александр8000"/utf8>>}
        ,{<<"female/ru-ru/maria">>, <<"Мария8000"/utf8>>}
        ,{<<"female/ru-ru/lidia">>, <<"Лидия8000"/utf8>>}
        ,{<<"female/en-US/carol">>, <<"Carol8000">>}
        ,{<<"female/en-US/asel">>, <<"Asel8000">>}
        ,{<<"male/ru-ru/vladimir/22050">>, <<"Владимир"/utf8>>}
        ,{<<"female/ru-ru/julia/22050">>, <<"Юлия"/utf8>>}
        ,{<<"female/ru-ru/anna/22050">>, <<"Анна"/utf8>>}
        ,{<<"female/ru-ru/viktoria/22050">>, <<"Виктория"/utf8>>}
        ,{<<"male/ru-ru/alexander/22050">>, <<"Александр"/utf8>>}
        ,{<<"female/ru-ru/maria/22050">>, <<"Мария"/utf8>>}
        ,{<<"female/ru-ru/lidia/22050">>, <<"Лидия"/utf8>>}
        ,{<<"female/en-US/carol/22050">>, <<"Carol">>}
        ,{<<"female/en-US/asel/22050">>, <<"Asel">>}
        ]
       ).

-define(VOICEFABRIC_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url">>, <<"https://voicefabric.ru/WSServer/ws/tts">>)).

-define(TMP_PATH, kapps_config:get(?MOD_CONFIG_CAT, <<"temporary_storage_path">>, <<"/tmp">>)).

-define(KAPPS_SPEECH_HRL, 'true').
-endif.
