-ifndef(NOTIFY_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include("notify_templates.hrl").

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"notify">>).

-define(NOTIFY_HRL, true).
-endif.
