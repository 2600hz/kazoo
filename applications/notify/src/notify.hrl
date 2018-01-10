-ifndef(NOTIFY_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo/include/kz_config.hrl").
-include("notify_templates.hrl").

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, <<"notify">>).
-define(APP, 'notify').

-type respond_to() :: {kz_term:api_binary(), kz_term:ne_binary()}.
-type send_email_return() :: 'ok' | {'error', any()} | ['ok' | {'error', any()}].

-define(NOTIFY_HRL, true).
-endif.
