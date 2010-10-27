-module(amqp_util).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(props, [get_value/2, get_value/3]).

-export([targeted_exchange/1, targeted_publish/3, targeted_publish/4]).
-export([callctl_exchange/1, callctl_publish/3, callctl_publish/4]).
-export([callevt_exchange/1, callevt_publish/3, callevt_publish/4]).
-export([broadcast_exchange/1, broadcast_publish/2, broadcast_publish/3]).
-export([resource_exchange/1, resource_publish/2, resource_publish/3]).

-export([bind_q_to_targeted/2, bind_q_to_targeted/3]).
-export([bind_q_to_callctl/2, bind_q_to_callctl/3]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3]).
-export([bind_q_to_broadcast/2, bind_q_to_broadcast/3]).
-export([bind_q_to_resource/2, bind_q_to_resource/3]).

-export([callevt_consume/2, callctl_consume/2]).

-export([new_targeted_queue/2, new_callevt_queue/2, new_callctl_queue/2, new_broadcast_queue/2]).
-export([delete_callevt_queue/2, delete_callctl_queue/2]).

-export([new_queue/1, new_queue/2, delete_queue/2, basic_consume/2, basic_consume/3
	 ,basic_publish/4, basic_publish/5, channel_close/1, channel_close/2
	 ,channel_close/3, queue_delete/2,queue_delete/3]).

-export([access_request/0, access_request/1]).

-define(EXCHANGE_TARGETED, <<"targeted">>).
-define(TYPE_TARGETED, <<"direct">>).

-define(EXCHANGE_CALLCTL, <<"callctl">>).
-define(TYPE_CALLCTL, <<"direct">>).

-define(EXCHANGE_CALLEVT, <<"callevt">>).
-define(TYPE_CALLEVT, <<"direct">>).

-define(EXCHANGE_BROADCAST, <<"broadcast">>).
-define(TYPE_BROADCAST, <<"fanout">>).

-define(EXCHANGE_RESOURCE, <<"resource">>).
-define(TYPE_RESOURCE, <<"fanout">>).

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
    callevt_publish(Host, CallId, Payload, undefined).

callevt_publish(Host, CallId, Payload, ContentType) when is_binary(CallId) ->
    callevt_publish(Host, binary_to_list(CallId), Payload, ContentType);
callevt_publish(Host, CallId, Payload, ContentType) ->
    Route = case string:str(CallId, binary_to_list(?EXCHANGE_CALLEVT)) of
		0 -> list_to_binary([?EXCHANGE_CALLEVT, ".", CallId]);
		_ -> list_to_binary(CallId)
	    end,
    basic_publish(Host, ?EXCHANGE_CALLEVT, Route, Payload, ContentType).

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

%% A generic Exchange maker
new_exchange(Host, Exchange, Type) ->
    new_exchange(Host, Exchange, Type, []).
new_exchange(Host, Exchange, Type, Options) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    ED = #'exchange.declare'{
      ticket = Ticket
      ,exchange = Exchange
      ,type = Type
      ,passive = get_value(passive, Options, false)
      ,durable = get_value(durable, Options, false)
      ,auto_delete = get_value(auto_delete, Options, false)
      ,internal = get_value(internal, Options, false)
      ,nowait = get_value(nowait, Options, true)
      ,arguments = get_value(arguments, Options, [])
     },
    amqp_channel:call(Channel, ED).

new_targeted_queue(Host, QueueName) ->
    new_queue(Host, list_to_binary([?EXCHANGE_TARGETED, ".", QueueName])
	      ,[{nowait, false}]).

new_broadcast_queue(Host, QueueName) ->
    new_queue(Host, list_to_binary([?EXCHANGE_BROADCAST, ".", QueueName])
	      ,[{nowait, false}]).

new_callevt_queue(Host, CallId) ->
    new_queue(Host
	      ,list_to_binary([?EXCHANGE_CALLEVT, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_callctl_queue(Host, CallId) ->
    new_queue(Host
	      ,list_to_binary([?EXCHANGE_CALLCTL, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

%% Declare a queue and returns the queue Name
new_queue(Host) ->
    new_queue(Host, <<"">>). % let's the client lib create a random queue name
new_queue(Host, Queue) ->
    new_queue(Host, Queue, []).
new_queue(Host, Queue, Prop) when is_list(Queue) ->
    new_queue(Host, list_to_binary(Queue), Prop);
new_queue(Host, Queue, Prop) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QD = #'queue.declare'{
      ticket = Ticket
      ,queue = Queue
      ,passive = get_value(passive, Prop, false)
      ,durable = get_value(durable, Prop, false)
      ,exclusive = get_value(exclusive, Prop, true)
      ,auto_delete = get_value(auto_delete, Prop, true)
      ,nowait = get_value(nowait, Prop, true)
      ,arguments = get_value(arguments, Prop, [])
     },
    #'queue.declare_ok'{queue = QueueName} = amqp_channel:call(Channel, QD),
    QueueName.

delete_callevt_queue(Host, CallId) ->
    delete_callevt_queue(Host, CallId, []).

delete_callevt_queue(Host, CallId, Prop) ->
    delete_queue(Host, list_to_binary([?EXCHANGE_CALLEVT, ".", CallId]), Prop).

delete_callctl_queue(Host, CallId) ->
    delete_callctl_queue(Host, CallId, []).

delete_callctl_queue(Host, CallId, Prop) ->
    delete_queue(Host, list_to_binary([?EXCHANGE_CALLCTL, ".", CallId]), Prop).

delete_queue(Host, Queue) ->
    delete_queue(Host, Queue, []).

delete_queue(Host, Queue, Prop) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QD = #'queue.delete'{
      ticket = Ticket
      ,queue=Queue
      ,if_unused = get_value(if_unused, Prop, false)
      ,if_empty = get_value(if_empty, Prop, false)
      ,nowait = get_value(nowait, Prop, true)
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

bind_q_to_callevt(Host, Queue) ->
    bind_q_to_callevt(Host, Queue, Queue).

bind_q_to_callevt(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_CALLEVT).

bind_q_to_broadcast(Host, Queue) ->
    bind_q_to_broadcast(Host, Queue, <<"#">>).

bind_q_to_broadcast(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_BROADCAST).

bind_q_to_resource(Host, Queue) ->
    bind_q_to_resource(Host, Queue, <<"#">>).

bind_q_to_resource(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_RESOURCE).

%% generic binder
bind_q_to_exchange(Host, Queue, Routing, Exchange) when is_list(Queue) ->
    bind_q_to_exchange(Host, list_to_binary(Queue), Routing, Exchange);
bind_q_to_exchange(Host, Queue, Routing, Exchange) when is_list(Routing) ->
    bind_q_to_exchange(Host, Queue, list_to_binary(Routing), Exchange);
bind_q_to_exchange(Host, Queue, Routing, Exchange) when is_list(Exchange) ->
    bind_q_to_exchange(Host, Queue, Routing, list_to_binary(Exchange));
bind_q_to_exchange(Host, Queue, Routing, Exchange) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self(), Host),
    QB = #'queue.bind'{
      ticket = Ticket
      ,queue = Queue
      ,exchange = Exchange
      ,routing_key = Routing
      ,nowait = true
      ,arguments = []
     },
    amqp_channel:call(Channel, QB).

callevt_consume(Host, CallId) ->
    basic_consume(Host, list_to_binary([?EXCHANGE_CALLEVT, ".", CallId])).
callctl_consume(Host, CallId) ->
    basic_consume(Host, list_to_binary([?EXCHANGE_CALLCTL, ".", CallId])).

%% create a consumer for a Queue
basic_consume(Host, Queue) when is_list(Queue) ->
    basic_consume(Host, list_to_binary(Queue));
basic_consume(Host, Queue) ->
    basic_consume(Host, Queue, []).

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
