-ifndef(KAZOO_IM_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(APP_NAME, <<"kazoo_im">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-record(amqp_listener_connection, {name :: binary()
                                  ,broker :: binary()
                                  ,exchange :: binary()
                                  ,type :: binary()
                                  ,queue :: binary()
                                  ,options :: kz_term:proplist()
                                  }).

-type amqp_listener_connection() :: #amqp_listener_connection{}.
-type amqp_listener_connections() :: [amqp_listener_connection(),...].

-define(ATOM(X), kz_term:to_atom(X, 'true')).
-define(APP, ?ATOM(?APP_NAME)).

-define(RESOURCE_TYPES_HANDLED,[<<"sms">>]).

-define(DEFAULT_EXCHANGE, <<"sms">>).
-define(DEFAULT_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_EXCHANGE_OPTIONS, [{<<"passive">>, 'true'}] ).
-define(DEFAULT_EXCHANGE_OPTIONS_JOBJ, kz_json:from_list(?DEFAULT_EXCHANGE_OPTIONS) ).
-define(DEFAULT_BROKER, kz_amqp_connections:primary_broker()).
-define(DEFAULT_QUEUE_NAME, <<"smsc_inbound_queue_sms">>).

-ifdef(OTP_RELEASE).
%% >= OTP 21
-define(CATCH(Type, Reason, Stacktrace), Type:Reason:Stacktrace).
-define(LOGSTACK(Stacktrace), kz_log:log_stacktrace(Stacktrace)).
-else.
%% =< OTP 20
-define(CATCH(Type, Reason, Stacktrace), Type:Reason).
-define(LOGSTACK(Stacktrace), kz_log:log_stacktrace()).
-endif.


-define(KAZOO_IM_HRL, 'true').
-endif.
