-ifndef(FAX_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"fax">>).

-define(APP_NAME, <<"fax">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(FAX_CACHE, 'fax_cache').
-define(FAX_HRL, 'true').
-endif.
