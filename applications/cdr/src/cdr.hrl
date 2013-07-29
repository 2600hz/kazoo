-ifndef(CDR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"cdr">>).
-define(APP_VERSION, <<"0.4.1">>).
-define(CDR_CACHE, 'cdr_cache').
-define(MAX_RETRIES, 3).

-type account_id() :: ne_binary().
-type account_db() :: ne_binary().

-define(CDR_HRL, 'true').
-endif.
