-ifndef(CALLFLOW_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_call/include/kapps_call_command_types.hrl").
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
-type cf_api_bridge_return() :: {'error', 'invalid_endpoint' | 'timeout' | kz_json:object()} |
                                {'fail', kz_json:object()} |
                                {'ok', kz_json:object()}.

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"4.0.0">> ).
-define(CF_CONFIG_CAT, ?APP_NAME).

-define(DEFAULT_CHILD_KEY, <<"_">>).

-define(RECORDED_NAME_KEY, [<<"media">>, <<"name">>]).
-define(CF_RECORDING_ID_KEY, <<"Recording-ID">>).

-define(DIALPLAN_MAP, [{<<"tone">>, <<"tones">>}]).

-define(LIST_BY_NUMBER, <<"callflows/listing_by_number">>).
-define(LIST_BY_PATTERN, <<"callflows/listing_by_pattern">>).

-define(NO_MATCH_CF, <<"no_match">>).

-define(DEFAULT_TIMEOUT_S, ?BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S).

-define(MANUAL_PRESENCE_DOC, <<"manual_presence">>).

-define(CACHE_NAME, 'callflow_cache').

-define(RESTRICTED_ENDPOINT_KEY, <<"Restricted-Endpoint-ID">>).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).
-define(CF_FLOW_CACHE_KEY(Number, AccountId), {'cf_flow', Number, AccountId}).
-define(CF_PATTERN_CACHE_KEY(AccountId), {'cf_patterns', AccountId}).

-define(CALLFLOW_HRL, 'true').
-endif.
