-ifndef(DOODLE_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_call/include/kapps_call_command_types.hrl").

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
                                  ,options :: kz_term:proplist()
                                  }).

-type amqp_listener_connection() :: #amqp_listener_connection{}.
-type amqp_listener_connections() :: [amqp_listener_connection(),...].

-define(ATOM(X), kz_term:to_atom(X, 'true')).
-define(APP, ?ATOM(?APP_NAME)).

-define(RESOURCE_TYPES_HANDLED,[<<"sms">>]).

-define(OUTBOUND_POOL, 'doodle_outbound_amqp_pool').
-define(DOODLE_POOL_NAME_ARGS(Name, Args), ?WORKER_NAME_ARGS('poolboy', Name, Args)).

-define(MSG_LIST_BY_NUMBER, <<"message/listing_by_number">>).
-define(MSG_LIST_BY_PATTERN, <<"message/listing_by_pattern">>).
-define(CF_LIST_BY_NUMBER, <<"callflows/listing_by_number">>).
-define(CF_LIST_BY_PATTERN, <<"callflows/listing_by_pattern">>).
-define(NO_MATCH_FLOW, <<"no_match">>).
-define(MSG_FLOW_CACHE_KEY(Number, AccountId), {'msg_flow', Number, AccountId}).
-define(MSG_PATTERN_CACHE_KEY(AccountId), {'msg_patterns', AccountId}).


-define(DEFAULT_EXCHANGE, <<"sms">>).
-define(DEFAULT_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_EXCHANGE_OPTIONS, [{<<"passive">>, 'true'}] ).
-define(DEFAULT_EXCHANGE_OPTIONS_JOBJ, kz_json:from_list(?DEFAULT_EXCHANGE_OPTIONS) ).
-define(DEFAULT_BROKER, kz_amqp_connections:primary_broker()).
-define(DEFAULT_QUEUE_NAME, <<"smsc_inbound_queue_sms">>).

-define(OUTBOUND_POOL_ARG(K),[<<"default">>, <<"outbound">>, <<"pool">>, K]).
-define(OUTBOUND_EXCHANGE_ARG(K),[<<"default">>, <<"outbound">>, <<"pool">>, <<"exchange">>, K]).

-define(DOODLE_HRL, 'true').
-endif.
