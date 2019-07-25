-ifndef(KAPPS_CALL_COMMAND_TYPES_HRL).

-type kapps_custom_publish() :: fun((kz_term:proplist(), kapps_call:call()) -> 'ok').

-type api_error() :: 'channel_hungup' |
                     'channel_unbridge' |
                     'channel_disconnected' |
                     'timeout' |
                     'no_channel_id' |
                     'not_found' |
                     kz_json:object().
-type kapps_api_error() :: {'error', api_error()}.
-type kapps_api_std_return() :: kapps_api_error() |
                                {'ok', kz_json:object() | kz_term:ne_binary()} |
                                'ok'.
-type kapps_api_bridge_return() :: {'error', 'timeout' | kz_json:object()} |
                                   {'fail', kz_json:object()} |
                                   {'ok', kz_json:object()}.

-define(DEFAULT_CCV_KEYS
       ,[<<"Account-ID">>
        ,<<"Account-Name">>
        ,<<"Account-Realm">>
        ,<<"Authorizing-ID">>
        ,<<"Authorizing-Type">>
        ,<<"Auto-Answer">>
        ,<<"Call-Forward">>
        ,<<"Call-Interaction-ID">>
        ,<<"Ecallmgr-Node">>
        ,<<"Call-Waiting-Disabled">>
        ,<<"Confirm-Cancel-Timeout">>
        ,<<"Confirm-File">>
        ,<<"Confirm-Key">>
        ,<<"Fax-Enabled">>
        ,<<"Fetch-ID">>
        ,<<"Loopback-Bowout">>
        ,<<"Media-Encryption-Enforce-Security">>
        ,<<"Media-Webrtc">>
        ,<<"Owner-ID">>
        ,<<"Presence-ID">>
        ,<<"RTCP-MUX">>
        ,<<"Realm">>
        ,<<"Require-Ignore-Early-Media">>
        ,<<"Retain-CID">>
        ,<<"SIP-Invite-Domain">>
        ,<<"Simplify-Loopback">>
        ,<<"Username">>
        ]).

-define(KAPPS_CALL_COMMAND_TYPES_HRL, 'true').
-endif.
