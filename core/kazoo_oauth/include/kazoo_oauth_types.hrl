-ifndef(KAZOO_OAUTH_TYPES_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(oauth_token, {token :: kz_term:ne_binary()
                     ,type :: kz_term:ne_binary()
                     ,expires :: non_neg_integer()
                     ,issued  :: kz_time:gregorian_seconds()
                     }).
-type oauth_token() :: #oauth_token{}.

-record(oauth_refresh_token, {token :: kz_term:api_binary()}).
-type oauth_refresh_token() :: #oauth_refresh_token{}.

-record(oauth_provider, {name :: kz_term:api_binary()
                        ,auth_url :: kz_term:api_binary()
                        ,tokeninfo_url :: kz_term:api_binary()
                        ,profile_url :: kz_term:api_binary()
                        ,servers :: kz_term:api_object()
                        ,scopes :: kz_term:api_object()
                        }).
-type oauth_provider() :: #oauth_provider{}.

-record(oauth_app, {name :: kz_term:api_binary()
                   ,account_id :: kz_term:api_binary()
                   ,secret :: kz_term:api_binary()
                   ,user_prefix :: kz_term:api_binary()
                   ,provider :: oauth_provider()
                   }).
-type oauth_app() :: #oauth_app{}.

-record(oauth_service_app, {name :: kz_term:ne_binary()
                           ,account_id :: kz_term:ne_binary()
                           ,email :: kz_term:ne_binary()
                           ,public_key_fingerprints :: kz_term:ne_binary()
                           ,public_key :: any()
                           ,private_key :: public_key:rsa_private_key() | public_key:dsa_private_key()
                           ,provider :: oauth_provider()
                           }).
-type oauth_service_app() :: #oauth_service_app{}.

-record(oauth_user, {full_id :: kz_term:api_binary()
                    ,app_id  :: kz_term:api_binary()
                    ,oauth_id  :: kz_term:api_binary()
                    ,account_id :: kz_term:api_binary()
                    ,owner_id :: kz_term:api_binary()
                    ,email :: kz_term:api_binary()
                    ,verified_email :: kz_term:api_boolean()
                    ,access_type :: kz_term:api_binary()
                    ,scope   :: kz_term:api_binary()
                    ,scopes  :: kz_term:api_binaries()
                    ,refresh_token :: kz_term:api_binary()
                    ,app :: oauth_app()
                    }).
-type oauth_user() :: #oauth_user{}.

-define(KAZOO_OAUTH_TYPES_HRL, 'true').
-endif.
