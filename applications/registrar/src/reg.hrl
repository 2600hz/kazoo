-ifndef(REG_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"registrar">>).

-define(CACHE_NAME, 'registrar_cache').

-record(auth_user, {realm :: ne_binary()
                    ,username :: ne_binary()
                    ,password :: api(binary())
                    ,account_id :: api(binary())
                    ,account_db :: api(binary())
                    ,authorizing_type :: api(binary())
                    ,authorizing_id :: api(binary())
                    ,method :: api(binary())
                    ,owner_id :: api(binary())
                    ,suppress_unregister_notifications = 'false' :: boolean()
                    ,register_overwrite_notify = 'false' :: boolean()
                    ,account_realm :: api(binary())
                    ,account_normalized_realm :: api(binary())
                    ,account_name :: api(binary())
                    ,nonce :: api(binary())
                    ,a3a8_key :: api(binary())
                    ,a3a8_kc :: api(binary())
                    ,a3a8_sres :: api(binary())
                    ,msisdn :: api(binary())
                    ,doc :: api(kz_json:object())
                    ,request :: api(kz_json:object())
                   }).
-type auth_user() :: #auth_user{}.


-define(GSM_CACHED_METHOD, <<"gsm_cached">>).
-define(GSM_A3A8_METHOD, <<"gsm_a3a8">>).
-define(GSM_ROAMING_METHOD, <<"gsm_roaming">>).
-define(GSM_ANY_METHOD, <<"gsm_", _/binary>>).
-define(ANY_AUTH_METHOD, _ ).

-define(REG_HRL, 'true').
-endif.
