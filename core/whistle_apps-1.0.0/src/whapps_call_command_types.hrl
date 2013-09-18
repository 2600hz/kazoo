-ifndef(WHAPPS_CALL_COMMAND_TYPES_HRL).

-type whapps_custom_publish() :: fun((wh_proplist(), whapps_call:call()) -> 'ok').
-type whapps_api_error() :: {'error', 'channel_hungup' |
                             'channel_unbridge' |
                             'channel_destroy' |
                             'timeout' |
                             wh_json:object()
                            }.
-type whapps_api_std_return() :: whapps_api_error() |
                                 {'ok', wh_json:object() |
                                  ne_binary()
                                 } |
                                 'ok'.
-type whapps_api_bridge_return() :: {'error', 'timeout' |
                                     wh_json:object()
                                    } |
                                    {'fail', wh_json:object()} |
                                    {'ok', wh_json:object()}.

-define(WHAPPS_CALL_COMMAND_TYPES_HRL, 'true').
-endif.
