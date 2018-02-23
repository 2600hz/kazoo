-ifndef(KAZOO_OAUTH_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(CONFIG_CAT, <<"oauth">>).

-define(APP_NAME, <<"oauth">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(OAUTH_GRANT_TYPE,<<"urn:ietf:params:oauth:grant-type:jwt-bearer">>).
-define(OAUTH_GRANT_TYPE2,<<"http://oauth.net/grant_type/jwt/1.0/bearer">>).

-include_lib("kazoo_oauth/include/kazoo_oauth_types.hrl").

-define(KAZOO_OAUTH_HRL, 'true').
-endif.
