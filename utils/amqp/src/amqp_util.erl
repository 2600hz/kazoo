-module(amqp_util).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../src/whistle_amqp.hrl").

-import(props, [get_value/2, get_value/3]).

-export([targeted_exchange/1, targeted_publish/3, targeted_publish/4]).
-export([callctl_exchange/1, callctl_publish/3, callctl_publish/4]).
-export([callevt_exchange/1, callevt_publish/3, callevt_publish/4]).
-export([broadcast_exchange/1, broadcast_publish/2, broadcast_publish/3]).
-export([resource_exchange/1, resource_publish/2, resource_publish/3]).
-export([callmgr_exchange/1, callmgr_publish/4]).

-export([bind_q_to_targeted/2, bind_q_to_targeted/3]).
-export([bind_q_to_callctl/2, bind_q_to_callctl/3]).
-export([bind_q_to_callevt/3, bind_q_to_callevt/4]).
-export([bind_q_to_broadcast/2, bind_q_to_broadcast/3]).
-export([bind_q_to_resource/2, bind_q_to_resource/3]).
-export([bind_q_to_callmgr/3]).

-export([callctl_consume/2]).

-export([new_targeted_queue/2, new_callevt_queue/2, new_callctl_queue/2, new_broadcast_queue/2, new_callmgr_queue/2]).
-export([delete_callevt_queue/2, delete_callctl_queue/2, delete_callmgr_queue/2]).

-export([new_queue/1, new_queue/2, new_queue/3, delete_queue/2, basic_consume/2, basic_consume/3
	 ,basic_publish/4, basic_publish/5, channel_close/1, channel_close/2
	 ,channel_close/3, queue_delete/2, queue_delete/3]).

-export([access_request/0, access_request/1]).

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

%% Broadcast Exchange
%% - Any consumer can create a queue to get any message published to the exchange
%% - Many publishers to the exchange, one consumer per queue
-define(EXCHANGE_BROADCAST, <<"broadcast">>).
-define(TYPE_BROADCAST, <<"fanout">>).

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

%% Monitor Manager Exchange
%% - monitor manager will publish requests to this exchange using routing keys
%%   agents that want to handle certain messages will create a queue with the appropriate routing key
%%   in the binding to receive the messages.
%% - monitor manager publishes to the exchange with a routing key; consumers bind their queue with the
%%   routing keys they want messages for.
-export([monitor_exchange/1, monitor_publish/4]).
-export([new_monitor_queue/1, delete_monitor_queue/2]).
-export([bind_q_to_monitor/3]).

-define(EXCHANGE_MONITOR, <<"monitor">>).
-define(TYPE_MONITOR, <<"topic">>).

monitor_publish(Host, Payload, ContentType, RoutingKey) ->
    basic_publish(Host, ?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

monitor_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_MONITOR, ?TYPE_MONITOR).

new_monitor_queue(Host) ->
    new_queue(Host, <<"">>, [{exclusive, false}, {auto_delete, true}]).

delete_monitor_queue(Host, Queue) ->
    delete_queue(Host, Queue, []).

bind_q_to_monitor(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_MONITOR).



%% Publish Messages to a given Exchange.Queue
targeted_publish(Host, Queue, Payload) ->
    targeted_publish(Host, Queue, Payload, undefined).

targeted_publish(Host, Queue, Payload, ContentType) ->
    basic_publish(Host, ?EXCHANGE_TARGETED, Queue, Payload, ContentType).

callctl_publish(Host, CallId, Payload) ->
    callctl_publish(Host, CallId, Payload, undefined).

callctl_publish(Host, CallId, Payload, ContentType) when is_binary(CallId) ->
    callctl_publish(Host, binary_to_list(CallId), Payload, ContentType);
callctl_publish(Host, CallId, Payload, ContentType) ->
    Route = case string:str(CallId, binary_to_list(?EXCHANGE_CALLCTL)) of
		0 -> list_to_binary([?EXCHANGE_CALLCTL, ".", CallId]);
		_ -> list_to_binary(CallId)
	    end,
    basic_publish(Host, ?EXCHANGE_CALLCTL, Route, Payload, ContentType).


callevt_publish(Host, CallId, Payload) ->
    callevt_publish(Host, CallId, Payload, event).

callevt_publish(Host, CallId, Payload, event) ->
    basic_publish(Host, ?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, CallId/binary>>, Payload, <<"application/json">>);
callevt_publish(Host, CallId, Payload, cdr) ->
    basic_publish(Host, ?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, CallId/binary>>, Payload, <<"application/json">>).

broadcast_publish(Host, Payload) ->
    broadcast_publish(Host, Payload, undefined).

broadcast_publish(Host, Payload, ContentType) when is_list(ContentType) ->
    broadcast_publish(Host, Payload, list_to_binary(ContentType));
broadcast_publish(Host, Payload, ContentType) when is_list(Payload) ->
    broadcast_publish(Host, list_to_binary(Payload), ContentType);
broadcast_publish(Host, Payload, ContentType) ->
    basic_publish(Host, ?EXCHANGE_BROADCAST, <<"#">>, Payload, ContentType).

resource_publish(Host, Payload) ->
    resource_publish(Host, Payload, undefined).

resource_publish(Host, Payload, ContentType) when is_list(ContentType) ->
    resource_publish(Host, Payload, list_to_binary(ContentType));
resource_publish(Host, Payload, ContentType) ->
    basic_publish(Host, ?EXCHANGE_RESOURCE, <<"#">>, Payload, ContentType).

%% What host to publish to, what to send, what content type, what routing key
callmgr_publish(Host, Payload, ContentType, RoutingKey) ->
    basic_publish(Host, ?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).

%% Create (or access) an Exchange
targeted_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_TARGETED, ?TYPE_TARGETED).

callctl_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

callevt_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

broadcast_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_BROADCAST, ?TYPE_BROADCAST).

resource_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_RESOURCE, ?TYPE_RESOURCE).

callmgr_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_CALLMGR, ?TYPE_CALLMGR).

%% A generic Exchange maker
new_exchange(Host, Exchange, Type) ->
    new_exchange(Host, Exchange, Type, []).
new_exchange(Host, Exchange, Type, _Options) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    ED = #'exchange.declare'{
      ticket = Ticket
      ,exchange = Exchange
      ,type = Type
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ED).

new_targeted_queue(Host, <<>>) ->
    new_queue(Host, <<>>, [{nowait, false}]);
new_targeted_queue(Host, QueueName) ->
    new_queue(Host, list_to_binary([?EXCHANGE_TARGETED, ".", QueueName]), [{nowait, false}]).

new_broadcast_queue(Host, <<>>) ->
    new_queue(Host, <<>>, [{nowait, false}]);
new_broadcast_queue(Host, QueueName) ->
    new_queue(Host, list_to_binary([?EXCHANGE_BROADCAST, ".", QueueName]), [{nowait, false}]).

new_callevt_queue(Host, <<>>) ->
    new_queue(Host, <<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(Host, CallId) ->
    new_queue(Host
	      ,list_to_binary([?EXCHANGE_CALLEVT, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_callctl_queue(Host, <<>>) ->
    new_queue(Host, <<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(Host, CallId) ->
    new_queue(Host
	      ,list_to_binary([?EXCHANGE_CALLCTL, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_callmgr_queue(Host, Queue) ->
    new_queue(Host, Queue, []).

%% Declare a queue and returns the queue Name
new_queue(Host) ->
    new_queue(Host, <<>>). % let's the client lib create a random queue name
new_queue(Host, Queue) ->
    new_queue(Host, Queue, []).
new_queue(Host, Queue, Options) when is_list(Queue) ->
    new_queue(Host, list_to_binary(Queue), Options);
new_queue(Host, Queue, Options) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QD = #'queue.declare'{
      ticket = Ticket
      ,queue = Queue
      ,passive = get_value(passive, Options, false)
      ,durable = get_value(durable, Options, false)
      ,exclusive = get_value(exclusive, Options, true)
      ,auto_delete = get_value(auto_delete, Options, true)
      ,nowait = get_value(nowait, Options, false)
      ,arguments = get_value(arguments, Options, [])
     },
    #'queue.declare_ok'{queue=Q} = amqp_channel:call(Channel, QD),
    Q.

delete_callevt_queue(Host, CallId) ->
    delete_callevt_queue(Host, CallId, []).

delete_callevt_queue(Host, CallId, Prop) ->
    delete_queue(Host, list_to_binary([?EXCHANGE_CALLEVT, ".", CallId]), Prop).

delete_callctl_queue(Host, CallId) ->
    delete_callctl_queue(Host, CallId, []).

delete_callctl_queue(Host, CallId, Prop) ->
    delete_queue(Host, list_to_binary([?EXCHANGE_CALLCTL, ".", CallId]), Prop).

delete_callmgr_queue(Host, Queue) ->
    delete_queue(Host, Queue, []).

delete_queue(Host, Queue) ->
    delete_queue(Host, Queue, []).

delete_queue(Host, Queue, Prop) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QD = #'queue.delete'{
      ticket = Ticket
      ,queue=Queue
      ,if_unused = get_value(if_unused, Prop, false)
      ,if_empty = get_value(if_empty, Prop, false)
      ,nowait = get_value(nowait, Prop, false)
     },
    amqp_channel:call(Channel, QD).

%% Bind a Queue to an Exchange (with optional Routing Key)
bind_q_to_targeted(Host, Queue) ->
    bind_q_to_targeted(Host, Queue, Queue).

bind_q_to_targeted(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_TARGETED).

bind_q_to_callctl(Host, Queue) ->
    bind_q_to_callctl(Host, Queue, Queue).

bind_q_to_callctl(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_CALLCTL).


%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallId
bind_q_to_callevt(Host, Queue, CallId) ->
    bind_q_to_callevt(Host, Queue, CallId, events).

bind_q_to_callevt(Host, Queue, CallId, events) ->
    bind_q_to_exchange(Host, Queue, <<?KEY_CALL_EVENT/binary, CallId/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Host, Queue, CallId, cdr) ->
    bind_q_to_exchange(Host, Queue, <<?KEY_CALL_CDR/binary, CallId/binary>>, ?EXCHANGE_CALLEVT).

bind_q_to_broadcast(Host, Queue) ->
    bind_q_to_broadcast(Host, Queue, <<"#">>).

bind_q_to_broadcast(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_BROADCAST).

bind_q_to_resource(Host, Queue) ->
    bind_q_to_resource(Host, Queue, <<"#">>).

bind_q_to_resource(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_RESOURCE).

bind_q_to_callmgr(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_CALLMGR).

%% generic binder
bind_q_to_exchange(Host, Queue, Routing, Exchange) when is_list(Queue) ->
    bind_q_to_exchange(Host, list_to_binary(Queue), Routing, Exchange);
bind_q_to_exchange(Host, Queue, Routing, Exchange) when is_list(Routing) ->
    bind_q_to_exchange(Host, Queue, list_to_binary(Routing), Exchange);
bind_q_to_exchange(Host, Queue, Routing, Exchange) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QB = #'queue.bind'{
      ticket = Ticket
      ,queue = Queue %% what queue does the binding attach to?
      ,exchange = Exchange %% what exchange does the binding attach to?
      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
      ,nowait = true
      ,arguments = []
     },
    amqp_channel:call(Channel, QB).

%% create a consumer for a Queue
basic_consume(Host, Queue) ->
    basic_consume(Host, Queue, []).

basic_consume(Host, Queue, Options) when is_list(Queue) ->
    basic_consume(Host, list_to_binary(Queue), Options);
basic_consume(Host, Queue, Options) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    BC = #'basic.consume'{
      ticket = Ticket
      ,queue = Queue
      ,consumer_tag = Queue
      ,no_local = get_value(no_local, Options, false)
      ,no_ack = get_value(no_ack, Options, true)
      ,exclusive = get_value(exclusive, Options, true)
      ,nowait = get_value(nowait, Options, false)
     },
    amqp_channel:subscribe(Channel, BC, self()).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
basic_publish(Host, Exchange, Queue, Payload) ->
    basic_publish(Host, Exchange, Queue, Payload, undefined).
basic_publish(Host, Exchange, Queue, Payload, ContentType) ->
    basic_publish(Host, Exchange, Queue, Payload, ContentType, []).

basic_publish(Host, Exchange, Queue, Payload, ContentType, Prop) when is_list(Payload) ->
    basic_publish(Host, Exchange, Queue, list_to_binary(Payload), ContentType, Prop);
basic_publish(Host, Exchange, Queue, Payload, ContentType, Prop) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    BP = #'basic.publish'{
      ticket = Ticket
      ,exchange = Exchange
      ,routing_key = Queue
      ,mandatory = get_value(mandatory, Prop, false)
      ,immediate = get_value(immediate, Prop, false)
     },

    %% Add the message to the publish, converting to binary
    AM = #'amqp_msg'{
      payload = Payload
      ,props=#'P_basic'{content_type=ContentType}
     },
    amqp_channel:cast(Channel, BP, AM).

channel_close(Host) ->
    channel_close(Host, <<"Goodbye">>).

channel_close(Host, Msg) when is_list(Msg) ->
    channel_close(Host, list_to_binary(Msg));
channel_close(Host, Msg) ->
    channel_close(Host, Msg, 200).

channel_close(Host, Msg, Code) when is_list(Host) ->
    {ok, Channel, _Ticket} = amqp_manager:open_channel(self(), Host),
    channel_close(Channel, Msg, Code);
channel_close(Channel, Msg, Code) when is_pid(Channel) ->
    CC = #'channel.close'{
      reply_code = Code,
      reply_text = Msg,
      class_id = 0,
      method_id = 0
     },
    amqp_channel:call(Channel, CC).

queue_delete(Host, Queue) ->
    queue_delete(Host, Queue, []).

queue_delete(Host, Queue, Prop) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QD = #'queue.delete'{
      ticket=Ticket
      ,queue=Queue
      ,if_unused=get_value(if_unused, Prop, false)
      ,if_empty = get_value(if_empty, Prop, false)
      ,nowait = get_value(nowait, Prop, true)
     },
    amqp_channel:call(Channel, QD).

access_request() ->
    access_request([]).
access_request(Options) ->
    #'access.request'{
      realm = get_value(realm, Options, <<"/data">>)
      ,exclusive = get_value(exclusive, Options, false)
      ,passive = get_value(passive, Options, true)
      ,active = get_value(active, Options, true)
      ,write = get_value(write, Options, true)
      ,read = get_value(read, Options, true)
     }.
