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
        ,[%% Владимир8000
          {<<"male/ru-ru">>, <<208,146,208,187,208,176,208,180,208,184,208,188,208,184,209,128,56,48,48,48>>}
          %% Юлия8000
          ,{<<"female/ru-ru">>, <<208,174,208,187,208,184,209,143,56,48,48,48>>}
          %% Carol8000
          ,{<<"female/en-us">>, <<"Carol8000">>}
          %% Владимир8000
          ,{<<"male/ru-ru/vladimir">>, <<208,146,208,187,208,176,208,180,208,184,208,188,208,184,209,128,56,48,48,48>>}
          %% Юлия8000
          ,{<<"female/ru-ru/julia">>, <<208,174,208,187,208,184,209,143,56,48,48,48>>}
          %% Анна8000
          ,{<<"female/ru-ru/anna">>, <<208,144,208,189,208,189,208,176,56,48,48,48>>}
          %% Виктория8000
          ,{<<"female/ru-ru/viktoria">>, <<208,146,208,184,208,186,209,130,208,190,209,128,208,184,209,143,56,48,48,48>>}
          %% Александр8000
          ,{<<"male/ru-ru/alexander">>, <<208,144,208,187,208,181,208,186,209,129,208,176,208,189,208,180,209,128,56,48,48,48>>}
          %% Мария8000
          ,{<<"female/ru-ru/maria">>, <<208,156,208,176,209,128,208,184,209,143,56,48,48,48>>}
          %% Лидия8000
          ,{<<"female/ru-ru/lidia">>, <<208,155,208,184,208,180,208,184,209,143,56,48,48,48>>}
          %% Carol8000
          ,{<<"female/en-US/carol">>, <<"Carol8000">>}
          %% Asel8000
          ,{<<"female/en-US/asel">>, <<"Asel8000">>}
          %% Владимир
          ,{<<"male/ru-ru/vladimir/22050">>, <<208,146,208,187,208,176,208,180,208,184,208,188,208,184,209,128>>}
          %% Юлия
          ,{<<"female/ru-ru/julia/22050">>, <<208,174,208,187,208,184,209,143>>}
          %% Анна
          ,{<<"female/ru-ru/anna/22050">>, <<208,144,208,189,208,189,208,176>>}
          %% Виктория
          ,{<<"female/ru-ru/viktoria/22050">>, <<208,146,208,184,208,186,209,130,208,190,209,128,208,184,209,143>>}
          %% Александр
          ,{<<"male/ru-ru/alexander/22050">>, <<208,144,208,187,208,181,208,186,209,129,208,176,208,189,208,180,209,128>>}
          %% Мария
          ,{<<"female/ru-ru/maria/22050">>, <<208,156,208,176,209,128,208,184,209,143>>}
          %% Лидия
          ,{<<"female/ru-ru/lidia/22050">>, <<208,155,208,184,208,180,208,184,209,143>>}
          %% Carol
          ,{<<"female/en-US/carol/22050">>, <<"Carol">>}
          %% Asel
          ,{<<"female/en-US/asel/22050">>, <<"Asel">>}
         ]
       ).

-define(VOICEFABRIC_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url">>, <<"https://voicefabric.ru/WSServer/ws/tts">>)).

-define(TMP_PATH, kapps_config:get(?MOD_CONFIG_CAT, <<"temporary_storage_path">>, <<"/tmp">>)).

-define(KAPPS_SPEECH_HRL, 'true').
-endif.
