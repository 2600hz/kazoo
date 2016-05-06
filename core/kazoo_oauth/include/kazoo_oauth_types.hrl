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

-record(oauth_refresh_token, {token :: maybe(binary())}).
-type oauth_refresh_token() :: #oauth_refresh_token{}.

-record(oauth_provider, {name :: maybe(binary())
                         ,auth_url :: maybe(binary())
                         ,tokeninfo_url :: maybe(binary())
                         ,profile_url :: maybe(binary())
                         ,servers :: maybe(kz_json:object())
                         ,scopes :: maybe(kz_json:object())
                        }).
-type oauth_provider() :: #oauth_provider{}.

-record(oauth_app, {name :: maybe(binary())
                    ,account_id :: maybe(binary())
                    ,secret :: maybe(binary())
                    ,user_prefix :: maybe(binary())
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

-record(oauth_user, {full_id :: maybe(binary())
                     ,app_id  :: maybe(binary())
                     ,oauth_id  :: maybe(binary())
                     ,account_id :: maybe(binary())
                     ,owner_id :: maybe(binary())
                     ,email :: maybe(binary())
                     ,verified_email :: maybe(boolean())
                     ,access_type :: maybe(binary())
                     ,scope   :: maybe(binary())
                     ,scopes  :: maybe([maybe(binary())])
                     ,refresh_token :: maybe(binary())
                     ,app :: oauth_app()
                    }).
-type oauth_user() :: #oauth_user{}.

-define(KAZOO_OAUTH_TYPES_HRL, 'true').
-endif.
