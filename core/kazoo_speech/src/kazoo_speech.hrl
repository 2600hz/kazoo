-ifndef(KAZOO_SPEECH_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(TTS_API_KEY, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)).
-define(TMP_PATH, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"temporary_storage_path">>, <<"/tmp">>)).

-type conversion_return() :: {binary(), kz_term:ne_binary()} |
                             {'error', 'unsupported_content_type'}.

-type provider_errors() :: 'invalid_voice' | 'unknown_provider' | 'unsupported_content_type'.
-type provider_return() :: {'error', provider_errors()} |
                           kz_http:ret().

-type create_resp() :: provider_return() |
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', 'tts_provider_failure', binary()}.

-type asr_resp() :: kz_http:req_id() |
                    {'ok', kz_json:object()} | %% {'ok', JObj}
                    {'error', provider_errors()} |
                    {'error',  'asr_provider_failure', kz_json:object()}.

-define(KAZOO_SPEECH_HRL, 'true').
-endif.
