%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% AMQP-specific things for Whistle
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-ifndef(WH_AMQP_HRL).

-include_lib("rabbitmq_client/include/amqp_client.hrl").

-define(KEY_ORGN_RESOURCE_REQ, <<"orginate.resource.req">>). %% corresponds to originate_resource_req/1 api call
-define(RESOURCE_QUEUE_NAME, <<"resource.provider">>).

-define(KEY_OFFNET_RESOURCE_REQ, <<"offnet.resource.req">>). %% corresponds to offnet_resource_req/1 api call

-define(KEY_CALL_MEDIA_REQ, <<"call.media">>). %% corresponds to media_req/1
-define(KEY_CALL_EVENT, <<"call.event.">>). %% corresponds to the call_event/1 api call
-define(KEY_CALL_CDR, <<"call.cdr.">>). %% corresponds to the call_cdr/1 api call
-define(KEY_PUBLISHER_USURP, <<"publisher.usurp.">>).

-define(KEY_CONFERENCE_DISCOVERY, <<"conference.discovery">>).
-define(KEY_CONFERENCE_COMMAND, <<"conference.command.">>).
-define(KEY_CONFERENCE_EVENT, <<"conference.event.">>).
-define(KEY_CONFERENCE_CONFIG, <<"conference.config.">>).

%% To listen for auth requests, bind your queue in the CallMgr Exchange with the <<"auth.req">> routing key.
%% To listen for route requests, bind your queue in the CallMgr Exchange with the <<"route.req">> routing key.

%% For a specific call event stream, bind to <<"call.event.CALLID">> on the CallEvent Exchange
%% For all call events, bind to <<"call.event.*">>

%% For a specific call cdr, bind to <<"call.cdr.CALLID">> on the CallEvent Exchange
%% For all call cdrs, bind to <<"call.cdr.*">>

-define(AMQP_RECONNECT_INIT_TIMEOUT, 500).
-define(AMQP_RECONNECT_MAX_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-define(AMQP_DEBUG, 'false').

%% see http://www.rabbitmq.com/uri-spec.html
-define(DEFAULT_AMQP_URI, "amqp://guest:guest@localhost:5672").

%% Targeted Exchange
%% - Any process that needs a dedicated queue to be reached at creates one on this exchange
%% - One consumer of the queue, many publishers of the queue.
-define(EXCHANGE_TARGETED, <<"targeted">>).
-define(TYPE_TARGETED, <<"direct">>).

%% Call Control Exchange
%% - Specific type of exchange for call control. When a call is spun up, a process is created
%%   to have a queue on this exchange and listen for commands to come to it.
%% - One consumer of the queue, probably one publisher, though may be many, to the queue.
-define(EXCHANGE_CALLCTL, <<"callctl">>).
-define(TYPE_CALLCTL, <<"direct">>).

%% Call Event Exchange
%% - When a call spins up, a process publishes to the callevt exchange at call.event.CALLID
%% - Any app that wants call events can bind to call.event.* for all events, or call.event.CALLID
%%   for a specific call's events
-define(EXCHANGE_CALLEVT, <<"callevt">>).
-define(TYPE_CALLEVT, <<"topic">>).

%% Resource Exchange
%% - Request for resources are published and consumed from this queue.  Topics are used to
%%   distinguish the types of resource
-define(EXCHANGE_RESOURCE, <<"resource">>).
-define(TYPE_RESOURCE, <<"fanout">>).

%% Call Manager Exchange
%% - ecallmgr will publish requests to this exchange using routing keys
%%   apps that want to handle certain messages will create a queue with the appropriate routing key
%%   in the binding to receive the messages.
%% - ecallmgr publishes to the exchange with a routing key; consumers bind their queue with the
%%   routing keys they want messages for.
-define(EXCHANGE_CALLMGR, <<"callmgr">>).
-define(TYPE_CALLMGR, <<"topic">>).

%% Conference Exchange
%% - applications can publish conference discovery request to the appropriate queue in this
%%   exchange to identify the SIP url for a conference focus
%% - conference services use this exchange to share state information
-define(EXCHANGE_CONFERENCE, <<"conference">>).
-define(TYPE_CONFERENCE, <<"topic">>).

%% Monitor Manager Exchange
%% - monitor manager will publish requests to this exchange using routing keys
%%   agents that want to handle certain messages will create a queue with the appropriate routing key
%%   in the binding to receive the messages.
%% - monitor manager publishes to the exchange with a routing key; consumers bind their queue with the
%%   routing keys they want messages for.
-define(EXCHANGE_MONITOR, <<"monitor">>).
-define(TYPE_MONITOR, <<"topic">>).

%% Configuration Exchange
%% - Crossbar will publish events to this exchange using routing keys
%%   apps that want to handle certain messages will create a queue with the appropriate routing key
%%   in the binding to receive the messages.
%% -  publishes to the exchange with a routing key; consumers bind their queue with the
%%   routing keys they want messages for.
%% doc_update.accountaXXXXX.callflow.id
-define(EXCHANGE_CONFIGURATION, <<"configuration">>).
-define(TYPE_CONFIGURATION, <<"topic">>).

%% WhApp Exchange
%% - For inter-whapp communication (amongst themselves)
-define(EXCHANGE_WHAPPS, <<"whapps">>).
-define(TYPE_WHAPPS, <<"topic">>).

%% Presence Exchange
%% - For presence related communication
-define(EXCHANGE_PRESENCE, <<"presence">>).
-define(TYPE_PRESENCE, <<"topic">>).

%% Notification Exchange
%% - For notification events
-define(EXCHANGE_NOTIFICATIONS, <<"notifications">>).
-define(TYPE_NOTIFICATIONS, <<"topic">>).

%% Sysconf Exchange
%% For system configuration events (read/write)
-define(EXCHANGE_SYSCONF, <<"sysconf">>).
-define(TYPE_SYSCONF, <<"topic">>).

%% Nodes Exchange
%% - Used for internode communications
-define(EXCHANGE_NODES, <<"nodes">>).
-define(TYPE_NODES, <<"fanout">>).

%% Registrar Exchange
%% - For registrar related communication
-define(EXCHANGE_REGISTRAR, <<"registrar">>).
-define(TYPE_REGISTRAR, <<"topic">>).

%% Federation Exchange
%% - For federation related communication
-define(EXCHANGE_FEDERATION, <<"federation">>).
-define(TYPE_FEDERATION, <<"topic">>).

-type wh_amqp_command() :: #'queue.declare'{} | #'queue.delete'{} |
                           #'queue.bind'{} | #'queue.unbind'{} |
                           #'basic.consume'{} | #'basic.cancel'{} |
                           #'basic.ack'{} | #'basic.nack'{} |
                           #'basic.qos'{} |
                           #'exchange.declare'{} |
                           #'confirm.select'{} |
                           #'channel.flow'{} | #'channel.flow_ok'{} |
                           '_' | 'undefined'.
-type wh_amqp_commands() :: [wh_amqp_command(),...] | [].

-type wh_amqp_exchange() :: #'exchange.declare'{}.
-type wh_amqp_exchanges() :: [#'exchange.declare'{},...] | [].

-type wh_amqp_queue() :: #'queue.declare'{}.
-type wh_amqp_queues() :: [#'queue.declare'{},...] | [].

-type wh_command_ret_ok() :: #'basic.qos_ok'{} | #'queue.declare_ok'{} |
                             #'exchange.declare_ok'{} | #'queue.delete_ok'{} |
                             #'queue.declare_ok'{} | #'queue.unbind_ok'{} |
                             #'queue.bind_ok'{} | #'basic.consume_ok'{} |
                             #'confirm.select_ok'{} |
                             #'basic.cancel_ok'{}.
-type command_ret() :: 'ok' |
                       {'ok', ne_binary() | wh_command_ret_ok()} |
                       {'error', _}.

-define(WH_AMQP_ETS, 'wh_amqp_ets').

-type wh_amqp_type() :: 'sticky' | 'float'.

-record(wh_amqp_assignment, {timestamp = os:timestamp() :: wh_now() | '_'
                             ,consumer :: api_pid() | '$2' | '_'
                             ,consumer_ref :: api_reference() | '_'
                             ,type = 'float' :: wh_amqp_type() | '_'
                             ,channel :: api_pid() | '$1' | '_'
                             ,channel_ref :: api_reference() | '_'
                             ,connection :: api_pid() | '$1' | '_'
                             ,broker :: api_binary() | '$1' | '_'
                             ,assigned :: wh_timeout() | 'undefined' | '_'
                             ,reconnect = 'false' :: boolean() | '_'
                             ,watchers = sets:new() :: set() | pids() | '_'
                            }).

-type wh_amqp_assignment() :: #wh_amqp_assignment{}.
-type wh_amqp_assignments() :: [wh_amqp_assignment(),...] | [].

-type wh_exchanges() :: [#'exchange.declare'{},...] | [].

-record(wh_amqp_connection, {broker :: ne_binary() | '_'
                             ,params :: #'amqp_params_direct'{} | #'amqp_params_network'{} | '_'
                             ,manager :: pid() | '_'
                             ,connection :: pid() | '_'
                             ,connection_ref :: reference() | '_'
                             ,channel :: api_pid() | '$1' | '_'
                             ,channel_ref :: api_reference() | '$1' | '_'
                             ,reconnect_ref :: api_reference() | '_'
                             ,available = 'false' :: boolean() | '_'
                             ,exchanges_initialized = 'false' :: boolean() | '_'
                             ,prechannels_initialized = 'false' :: boolean() | '_'
                             ,started = os:timestamp() :: wh_now() | '_'
                             ,tags = [] :: list() | '_'
                             ,hidden = 'false' :: boolean() | '_'
                            }).
-type wh_amqp_connection() :: #wh_amqp_connection{}.

-record(wh_amqp_connections, {connection :: api_pid() | '$1' | '_'
                              ,connection_ref :: api_reference() | '_'
                              ,broker :: ne_binary() | '$1' | '$2' | '_'
                              ,available='false' :: boolean() | '$1' | '$2' | '_'
                              ,timestamp=os:timestamp() :: wh_now() | '_'
                              ,zone='local' :: atom() | '$1' | '_'
                              ,manager=self() :: pid() | '_'
                              ,tags = [] :: list() | '_'
                              ,hidden = 'false' :: boolean() | '$3' | '_'
                             }).
-type wh_amqp_connections() :: #wh_amqp_connections{}.
-type wh_amqp_connections_list() :: [wh_amqp_connections(), ...] | [].

-type basic_publish() :: #'basic.publish'{}.
-type basic_deliver() :: #'basic.deliver'{}.
-type basic_info() :: #'P_basic'{}.
-type amqp_msg() :: #'amqp_msg'{}.

-define(AMQP_HIDDEN_TAG, <<"hidden">>).

-define(WH_AMQP_HRL, 'true').
-endif.
