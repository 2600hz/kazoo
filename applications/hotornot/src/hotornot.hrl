-ifndef(HOTORNOT_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"hotornot">>).
-define(APP_VERSION, <<"0.2.0">>).

-type trunking_options() :: ne_binaries().

-define(HOTORNOT_HRL, 'true').
-endif.
