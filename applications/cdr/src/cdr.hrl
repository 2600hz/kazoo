-ifndef(CDR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"cdr">>).
-define(APP_VERSION, <<"0.4.1">>).
-define(MAX_RETRIES, 3).

-define(CDR_CACHE, 'cdr_cache').

-define(CDR_HRL, 'true').
-endif.
