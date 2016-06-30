-ifndef(DOODLE_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_api.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_apps/src/kapps_call_command_types.hrl").

-define(APP_NAME, <<"doodle">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'doodle_cache').

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-record(amqp_listener_connection, {name :: binary()
                                  ,broker :: binary()
                                  ,exchange :: binary()
                                  ,type :: binary()
                                  ,queue :: binary()
                                  ,options :: kz_proplist()
                                  }).

-type amqp_listener_connection() :: #amqp_listener_connection{}.
-type amqp_listener_connections() :: [amqp_listener_connection(),...].

-define(ATOM(X), kz_util:to_atom(X, 'true')).
-define(APP, ?ATOM(?APP_NAME)).

-define(RESOURCE_TYPES_HANDLED,[<<"sms">>]).


-define(DOODLE_HRL, 'true').
-endif.
