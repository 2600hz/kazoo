-ifndef(KAZOO_SPEECH_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(MOD_CONFIG_CAT, <<"speech">>).
-define(GOOGLE_CONFIG_CAT, <<(?MOD_CONFIG_CAT)/binary, ".google">>).
-define(GOOGLE_ASR_URL, kapps_config:get_string(?GOOGLE_CONFIG_CAT, <<"asr_url">>, <<"https://speech.googleapis.com/v1/speech:recognize">>)).
-define(GOOGLE_ASR_KEY, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_api_key">>, <<"">>)).
-define(GOOGLE_ASR_PROFANITY_FILTER, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_profanity_filter">>)).
-define(GOOGLE_ASR_ENABLE_WORD_TIME_OFFSETS, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_enable_word_time_offsets">>)).

-define(TTS_API_KEY, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)).
-define(TMP_PATH, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"temporary_storage_path">>, <<"/tmp">>)).

-type conversion_return() :: {binary(), kz_term:ne_binary()} |
                             {'error', 'unsupported_content_type'}.

-type provider_error() :: 'invalid_voice' | 'unknown_provider' | 'unsupported_content_type'.
-type provider_return() :: {'error', provider_error()} |
                           {'error', 'asr_provider_failure', kz_term:ne_binary()} |
                           kz_http:ret().

-type create_resp() :: provider_return() |
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', 'tts_provider_failure', binary()}.

-type asr_resp() :: kz_http:req_id() |
                    {'ok', kz_json:object()} | %% {'ok', JObj}
                    {'error', provider_error()} |
                    {'error',  'asr_provider_failure', kz_json:object()}.

-define(KAZOO_SPEECH_HRL, 'true').
-endif.
