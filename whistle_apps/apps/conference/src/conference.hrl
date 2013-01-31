-ifndef(CONFERENCE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"conference">>).
-define(APP_VERSION, <<"2.0.0">>).

-define(CONFERENCE_CACHE, conference_cache).

-define(CONFERENCE_HRL, true).
-endif.
