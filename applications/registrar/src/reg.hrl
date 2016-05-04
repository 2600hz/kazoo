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
                    ,password :: maybe(binary())
                    ,account_id :: maybe(binary())
                    ,account_db :: maybe(binary())
                    ,authorizing_type :: maybe(binary())
                    ,authorizing_id :: maybe(binary())
                    ,method :: maybe(binary())
                    ,owner_id :: maybe(binary())
                    ,suppress_unregister_notifications = 'false' :: boolean()
                    ,register_overwrite_notify = 'false' :: boolean()
                    ,account_realm :: maybe(binary())
                    ,account_normalized_realm :: maybe(binary())
                    ,account_name :: maybe(binary())
                    ,nonce :: maybe(binary())
                    ,a3a8_key :: maybe(binary())
                    ,a3a8_kc :: maybe(binary())
                    ,a3a8_sres :: maybe(binary())
                    ,msisdn :: maybe(binary())
                    ,doc :: maybe(kz_json:object())
                    ,request :: maybe(kz_json:object())
                   }).
-type auth_user() :: #auth_user{}.


-define(GSM_CACHED_METHOD, <<"gsm_cached">>).
-define(GSM_A3A8_METHOD, <<"gsm_a3a8">>).
-define(GSM_ROAMING_METHOD, <<"gsm_roaming">>).
-define(GSM_ANY_METHOD, <<"gsm_", _/binary>>).
-define(ANY_AUTH_METHOD, _ ).

-define(REG_HRL, 'true').
-endif.
