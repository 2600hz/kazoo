-ifndef(REG_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"0.4.2">>).

-define(CONFIG_CAT, <<"registrar">>).

-define(REG_CACHE, 'registrar_cache').

-record(auth_user, {realm :: ne_binary()
                    ,username :: ne_binary()
                    ,password :: api_binary()
                    ,account_id :: api_binary()
                    ,account_db :: api_binary()
                    ,authorizing_type :: api_binary()
                    ,authorizing_id :: api_binary()
                    ,method :: api_binary()
                    ,owner_id :: api_binary()
                    ,suppress_unregister_notifications = 'false' :: boolean()
                    ,register_overwrite_notify = 'false' :: boolean()
                    ,account_realm :: api_binary()
                    ,account_name :: api_binary()
                    ,nonce :: api_binary()
                    ,a3a8_key :: api_binary()
                    ,a3a8_kc :: api_binary()
                    ,a3a8_sres :: api_binary()
                    ,msisdn :: api_binary()
                    ,doc :: api_object()
                    ,request :: api_object()
                   }).
-type auth_user() :: #auth_user{}.


-define(GSM_CACHED_METHOD, <<"gsm_cached">>).
-define(GSM_A3A8_METHOD, <<"gsm_a3a8">>).
-define(GSM_ROAMING_METHOD, <<"gsm_roaming">>).
-define(GSM_ANY_METHOD, <<"gsm_", _/binary>>).
-define(ANY_AUTH_METHOD, _ ).

-define(REG_HRL, 'true').
-endif.
