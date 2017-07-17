-ifndef(KZT_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include("kazoo_translator.hrl").

-type ok_return() :: {'ok', kapps_call:call()}.
-type stop_return() :: {'stop', kapps_call:call()}.
-type usurp_return() :: {'usurp', kapps_call:call()}.
-type request_return() :: {'request', kapps_call:call()}.
-type error_return() :: {'error', kapps_call:call()} |
                        {'error', kapps_call:call(), list()}.

-type exec_element_return() ::
        ok_return() |
        stop_return() |
        usurp_return() |
        error_return() |
        request_return().

-type exec_return() :: exec_element_return().

-define(KZT_USER_VARS, <<"user_vars">>).

-define(CONFIG_CAT, <<"pivot">>).

-define(TTS_SIZE_LIMIT, 4000).

-define(STATUS_QUEUED, <<"queued">>).
-define(STATUS_RINGING, <<"ringing">>).
-define(STATUS_ANSWERED, <<"in-progress">>).
-define(STATUS_COMPLETED, <<"completed">>).
-define(STATUS_BUSY, <<"busy">>).
-define(STATUS_FAILED, <<"failed">>).
-define(STATUS_NOANSWER, <<"no-answer">>).
-define(STATUS_CANCELED, <<"canceled">>).

-define(APP_NAME, <<"translator">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(DEFAULT_TTS_LANG, kazoo_tts:default_language()).
-define(DEFAULT_TTS_VOICE, kazoo_tts:default_voice()).

-define(KZT_HRL, 'true').
-endif.
