-ifndef(CDR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"cdr">>).
-define(APP_VERSION, <<"0.4.0">>).

-define(ANONYMOUS_CDR_DB, <<"anonymous_cdrs">>).

-define(CDR_HRL, true).
-endif.
