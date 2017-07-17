-ifndef(KAZOO_OAUTH_TYPES_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(oauth_token, {token :: ne_binary()
                     ,type :: ne_binary()
                     ,expires :: non_neg_integer()
                     ,issued  :: gregorian_seconds()
                     }).
-type oauth_token() :: #oauth_token{}.

-record(oauth_refresh_token, {token :: api_binary()}).
-type oauth_refresh_token() :: #oauth_refresh_token{}.

-record(oauth_provider, {name :: api_binary()
                        ,auth_url :: api_binary()
                        ,tokeninfo_url :: api_binary()
                        ,profile_url :: api_binary()
                        ,servers :: api_object()
                        ,scopes :: api_object()
                        }).
-type oauth_provider() :: #oauth_provider{}.

-record(oauth_app, {name :: api_binary()
                   ,account_id :: api_binary()
                   ,secret :: api_binary()
                   ,user_prefix :: api_binary()
                   ,provider :: oauth_provider()
                   }).
-type oauth_app() :: #oauth_app{}.

-record(oauth_service_app, {name :: ne_binary()
                           ,account_id :: ne_binary()
                           ,email :: ne_binary()
                           ,public_key_fingerprints :: ne_binary()
                           ,public_key :: any()
                           ,private_key :: public_key:rsa_private_key() | public_key:dsa_private_key()
                           ,provider :: oauth_provider()
                           }).
-type oauth_service_app() :: #oauth_service_app{}.

-record(oauth_user, {full_id :: api_binary()
                    ,app_id  :: api_binary()
                    ,oauth_id  :: api_binary()
                    ,account_id :: api_binary()
                    ,owner_id :: api_binary()
                    ,email :: api_binary()
                    ,verified_email :: api_boolean()
                    ,access_type :: api_binary()
                    ,scope   :: api_binary()
                    ,scopes  :: api_binaries()
                    ,refresh_token :: api_binary()
                    ,app :: oauth_app()
                    }).
-type oauth_user() :: #oauth_user{}.

-define(KAZOO_OAUTH_TYPES_HRL, 'true').
-endif.
