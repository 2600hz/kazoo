-ifndef(KAZOO_OAUTH_TYPES_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(oauth_token, {token :: ne_binary()
                      ,type :: ne_binary()
                      ,expires :: non_neg_integer()
                      ,issued  :: gregorian_seconds()
                     }).
-type oauth_token() :: #oauth_token{}.

-record(oauth_refresh_token, {token :: api(binary())}).
-type oauth_refresh_token() :: #oauth_refresh_token{}.

-record(oauth_provider, {name :: api(binary())
                         ,auth_url :: api(binary())
                         ,tokeninfo_url :: api(binary())
                         ,profile_url :: api(binary())
                         ,servers :: api(kz_json:object())
                         ,scopes :: api(kz_json:object())
                        }).
-type oauth_provider() :: #oauth_provider{}.

-record(oauth_app, {name :: api(binary())
                    ,account_id :: api(binary())
                    ,secret :: api(binary())
                    ,user_prefix :: api(binary())
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

-record(oauth_user, {full_id :: api(binary())
                     ,app_id  :: api(binary())
                     ,oauth_id  :: api(binary())
                     ,account_id :: api(binary())
                     ,owner_id :: api(binary())
                     ,email :: api(binary())
                     ,verified_email :: api(boolean())
                     ,access_type :: api(binary())
                     ,scope   :: api(binary())
                     ,scopes  :: api([api(binary())])
                     ,refresh_token :: api(binary())
                     ,app :: oauth_app()
                    }).
-type oauth_user() :: #oauth_user{}.

-define(KAZOO_OAUTH_TYPES_HRL, 'true').
-endif.
