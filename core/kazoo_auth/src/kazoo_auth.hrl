-ifndef(KAZOO_AUTH_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(CONFIG_CAT, <<"auth">>).

-define(APP_NAME, <<"auth">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(OAUTH_GRANT_TYPE,<<"urn:ietf:params:oauth:grant-type:jwt-bearer">>).
-define(OAUTH_GRANT_TYPE2,<<"http://oauth.net/grant_type/jwt/1.0/bearer">>).


-define(PK_CACHE, 'kazoo_auth_public_keys_cache').
-define(TOKENS_CACHE, 'kazoo_auth_tokens_cache').
-define(PROFILE_CACHE, 'kazoo_auth_profile_cache').

-define(APPID_KEYS, [<<"app_id">>
                    ,<<"clientId">>
                    ,<<"client_id">>
                    ]).

-define(REDIRECT_URI_KEYS, [<<"redirectUri">>
                           ,<<"redirect_uri">>
                           ]).

-define(KAZOO_GEN_SIGNATURE_SECRET, kz_binary:rand_hex(16)).
-define(KAZOO_SIGNATURE_ID, <<"secret_for_user_signature">>).
-define(KAZOO_SIGNATURE_SECRET, kapps_config:get_ne_binary(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID, ?KAZOO_GEN_SIGNATURE_SECRET)).

-define(RSA_KEY_SIZE, 2048).

-define(SYSTEM_KEY_ID, kapps_config:get_ne_binary(?CONFIG_CAT, <<"system_key">>, kz_binary:rand_hex(16))).
-define(SYSTEM_KEY_ATTACHMENT_NAME, <<"private_key.pem">>).
-define(SYSTEM_KEY_ATTACHMENT_CTYPE, <<"application/x-pem-file">>).

-define(JWT_CLAIMS, [{<<"pvt_account_id">>, <<"account_id">>}
                    ,{<<"pvt_owner_id">>, <<"owner_id">>}
                    ,{<<"_id">>, <<"auth_id">>}
                    ,{<<"pvt_app_id">>, <<"auth_app_id">>}
                    ,{<<"pvt_app_provider_id">>, <<"auth_provider">>}
                    ,{<<"email">>, <<"email">>}
                    ,{<<"pvt_accounts">>, <<"as">>}
                    ,{<<"scope">>, <<"scope">>}
                    ,{<<"display_name">>, <<"displayName">>}
                    ,{<<"photo_url">>, <<"photoUrl">>}
                    ]).

-define(JWT_MAP_CLAIMS, [{access_token, <<"auth_app_token">>}
                        ,{token_type, <<"auth_app_token_type">>}
                         %%                         ,{id_token, <<"auth_app_id_token">>}
                        ]).

-type mfa_error() :: 'no_provider' |
                     'unauthorized' |
                     {'configuration', kz_term:ne_binary()} |
                     kz_term:ne_binary().

-type mfa_result() :: {'ok', 'authenticated'} |
                      {'error', mfa_error()} |
                      {'error', non_neg_integer(), kz_json:object()}.

-define(TEST_DUO_SIGN_EXPIRE, 1234).

-define(KAZOO_AUTH_HRL, 'true').
-endif.
