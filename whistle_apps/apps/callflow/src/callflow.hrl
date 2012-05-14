-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-type cf_exe_response() :: {'stop'} | {'continue'} | {'continue', integer()} | {'heartbeat'}.
-type cf_api_error() :: {'error', 'channel_hungup' | 'channel_unbridge' | 'timeout' | wh_json:json_object()}.
-type cf_api_std_return() :: cf_api_error() | {'ok', wh_json:json_object()}.
-type cf_api_bridge_return() :: {'error', 'timeout' | wh_json:json_object()} | {'fail', wh_json:json_object()} | {'ok', wh_json:json_object()}.
-type cf_api_binary() :: binary() | 'undefined'.

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"0.8.2">> ).

-define(CONFIRM_FILE, <<"/opt/freeswitch/sounds/en/us/callie/ivr/8000/ivr-accept_reject_voicemail.wav">>).

-define(DIALPLAN_MAP, [{<<"tone">>, <<"tones">>}]).

-define(LIST_BY_NUMBER, {<<"callflow">>, <<"listing_by_number">>}).
-define(LIST_BY_PATTERN, {<<"callflow">>, <<"listing_by_pattern">>}).

-define(NO_MATCH_CF, <<"no_match">>).

-define(DEFAULT_TIMEOUT, <<"20">>).

-define(CF_CONFIG_CAT, <<"callflow">>).

-define(MANUAL_PRESENCE_DOC, <<"manual_presence">>).
