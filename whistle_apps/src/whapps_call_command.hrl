-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(DEFAULT_TIMEOUT, <<"20">>).
-define(ANY_DIGIT, [<<"1">>, <<"2">>, <<"3">>
                    ,<<"4">>, <<"5">>, <<"6">>
                    ,<<"7">>, <<"8">>, <<"9">>
                    ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-define(APP_NAME, <<"whapps_call_command">>).
-define(APP_VERSION, <<"1.0.0">>).

-type whapps_custom_publish() :: fun((proplist(), whapps_call:call()) -> 'ok').
-type whapps_api_error() :: {'error', 'channel_hungup' | 'channel_unbridge' | 'timeout' | wh_json:json_object()}.
-type whapps_api_std_return() :: whapps_api_error() | {'ok', wh_json:json_object()}.
-type whapps_api_bridge_return() :: {'error', 'timeout' | wh_json:json_object()} | {'fail', wh_json:json_object()} | {'ok', wh_json:json_object()}.
-type whapps_api_binary() :: binary() | 'undefined'.
