-ifndef(NOTIFY_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_amqp.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_config.hrl").
-include("notify_templates.hrl").

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"notify">>).

-define(NOTIFY_HRL, true).
-endif.
