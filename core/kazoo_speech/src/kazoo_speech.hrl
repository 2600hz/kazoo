-ifndef(KAZOO_SPEECH_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(TTS_API_KEY, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)).
-define(TMP_PATH, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"temporary_storage_path">>, <<"/tmp">>)).

-record(voice_desc, {voice_name :: kz_term:ne_binary()
                    ,language_code :: kz_term:ne_binary()
                    ,gender :: voice_gender()
                    }).

-type voice_desc() :: #voice_desc{}.
-type voice_gender() :: 'male'   |
                        'female' |
                        'neutral'.

-type conversion_return() :: {binary(), kz_term:ne_binary()} |
                             {'error', 'unsupported_content_type'}.

-type provider_error() :: 'invalid_voice' | 'unknown_provider' | 'unsupported_content_type' | 'invalid_format'.
-type provider_return() :: {'error', provider_error()} |
                           {'error', 'asr_provider_failure', kz_term:ne_binary()} |
                           kz_http:ret().

-type create_resp() :: provider_return() |
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'async', reference(), any()} | %% {'async', Reference, EngineData}
                       {'error', 'tts_provider_failure', binary()}.

-type decode_resp() :: {kz_term:ne_binary(), any()}.

-type asr_resp() :: kz_http:req_id() |
                    {'ok', kz_json:object()} | %% {'ok', JObj}
                    {'error', provider_error()} |
                    {'error',  'asr_provider_failure', kz_json:object()}.

-define(KAZOO_SPEECH_HRL, 'true').
-endif.
