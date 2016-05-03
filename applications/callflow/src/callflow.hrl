-ifndef(CALLFLOW_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_api.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_apps/src/kapps_call_command_types.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-type cf_exe_response() :: {'stop'} |
                           {'continue'} |
                           {'continue', integer()} |
                           {'heartbeat'}.
-type cf_api_error() :: {'error'
                         ,'channel_hungup' |
                         'channel_unbridge' |
                         'timeout' |
                         'invalid_endpoint_id' |
                         'not_found' |
                         kz_json:object()
                        }.
-type cf_api_std_return() :: cf_api_error() | {'ok', kz_json:object()}.
-type cf_api_bridge_return() :: {'error', 'timeout' | kz_json:object()} |
                                {'fail', kz_json:object()} |
                                {'ok', kz_json:object()}.
-type cf_api(binary()) :: api(binary()).

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(RECORDED_NAME_KEY, [<<"media">>, <<"name">>]).
-define(CF_RECORDING_ID_KEY, <<"Recording-ID">>).

-define(CONFIRM_FILE(Call), kz_media_util:get_prompt(<<"ivr-group_confirm">>, Call)).

-define(DIALPLAN_MAP, [{<<"tone">>, <<"tones">>}]).

-define(LIST_BY_NUMBER, <<"callflow/listing_by_number">>).
-define(LIST_BY_PATTERN, <<"callflow/listing_by_pattern">>).

-define(NO_MATCH_CF, <<"no_match">>).

-define(DEFAULT_TIMEOUT_S, 20).

-define(CF_CONFIG_CAT, <<"callflow">>).

-define(MANUAL_PRESENCE_DOC, <<"manual_presence">>).

-define(CACHE_NAME, 'callflow_cache').

-define(CF_ATTR_LOWER_KEY, <<109,108,112,112>>).
-define(CF_ATTR_UPPER_KEY, <<109,097,120,095,112,114,101,099,101,100,101,110,099,101>>).

-define(DEFAULT_TIMEZONE
        ,kapps_config:get(<<"accounts">>, <<"default_timezone">>, <<"America/Los_Angeles">>)
       ).

-define(RESTRICTED_ENDPOINT_KEY, <<"Restricted-Endpoint-ID">>).

-define(CALLFLOW_HRL, 'true').
-endif.
