-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"0.4.2">>).

-define(REG_DB, <<"registrations">>).
-define(AUTH_DB, <<"sip_auth">>).
-define(AUTH_VIEW_USERAUTHREALM, {"auth", "userrealm"}).

-define(CLEANUP_RATE, 60000).

-define(JSON_FILES, [{?REG_DB, <<"registrations.json">>}
		     ,{?AUTH_DB, <<"auth.json">>}
		    ]).
