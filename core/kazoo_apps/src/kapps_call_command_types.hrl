-ifndef(KAPPS_CALL_COMMAND_TYPES_HRL).

-type kapps_custom_publish() :: fun((kz_proplist(), kapps_call:call()) -> 'ok').
-type kapps_api_error() :: {'error', 'channel_hungup' |
                             'channel_unbridge' |
                             'channel_disconnected' |
                             'timeout' |
                             kz_json:object()
                            }.
-type kapps_api_std_return() :: kapps_api_error() |
                                 {'ok', kz_json:object() |
                                  ne_binary()
                                 } |
                                 'ok'.
-type kapps_api_bridge_return() :: {'error', 'timeout' |
                                     kz_json:object()
                                    } |
                                    {'fail', kz_json:object()} |
                                    {'ok', kz_json:object()}.

-define(KAPPS_CALL_COMMAND_TYPES_HRL, 'true').
-endif.
