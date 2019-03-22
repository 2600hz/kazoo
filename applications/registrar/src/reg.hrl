-ifndef(REG_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'registrar_cache').

-record(auth_user, {realm :: kz_term:ne_binary()
                   ,username :: kz_term:ne_binary()
                   ,password :: kz_term:api_binary()
                   ,account_id :: kz_term:api_binary()
                   ,account_db :: kz_term:api_binary()
                   ,authorizing_type :: kz_term:api_binary()
                   ,authorizing_id :: kz_term:api_binary()
                   ,method :: kz_term:api_binary()
                   ,owner_id :: kz_term:api_binary()
                   ,register_overwrite_notify = 'false' :: boolean()
                   ,account_realm :: kz_term:api_binary()
                   ,account_normalized_realm :: kz_term:api_binary()
                   ,account_name :: kz_term:api_binary()
                   ,nonce :: kz_term:api_binary()
                   ,a3a8_key :: kz_term:api_binary()
                   ,a3a8_kc :: kz_term:api_binary()
                   ,a3a8_sres :: kz_term:api_binary()
                   ,msisdn :: kz_term:api_binary()
                   ,doc :: kz_term:api_object()
                   ,request :: kz_term:api_object()
                   }).
-type auth_user() :: #auth_user{}.

-define(GSM_CACHED_METHOD, <<"gsm_cached">>).
-define(GSM_A3A8_METHOD, <<"gsm_a3a8">>).
-define(GSM_ROAMING_METHOD, <<"gsm_roaming">>).
-define(GSM_ANY_METHOD, <<"gsm_", _/binary>>).
-define(ANY_AUTH_METHOD, _ ).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>, <<"sms">>]).

-define(REG_HRL, 'true').
-endif.
