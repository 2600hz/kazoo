-module(amqp_util).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([targeted_exchange/1, targeted_publish/3, targeted_publish/4]).
-export([callctl_exchange/1, callctl_publish/3, callctl_publish/4]).
-export([callevt_exchange/1, callevt_publish/3, callevt_publish/4]).
-export([broadcast_exchange/1, broadcast_publish/2, broadcast_publish/3]).

-export([bind_q_to_targeted/2, bind_q_to_targeted/3]).
-export([bind_q_to_callctl/2, bind_q_to_callctl/3]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3]).
-export([bind_q_to_broadcast/2, bind_q_to_broadcast/3]).

-export([callevt_consume/2, callctl_consume/2]).

-export([new_targeted_queue/2, new_callevt_queue/2, new_callctl_queue/2]).

-export([new_queue/2, basic_consume/2, basic_publish/4, basic_publish/5]).

-define(EXCHANGE_TARGETED, <<"targeted">>).
-define(TYPE_TARGETED, <<"direct">>).

-define(EXCHANGE_CALLCTL, <<"callctl">>).
-define(TYPE_CALLCTL, <<"direct">>).

-define(EXCHANGE_CALLEVT, <<"callevt">>).
-define(TYPE_CALLEVT, <<"direct">>).

-define(EXCHANGE_BROADCAST, <<"broadcast">>).
-define(TYPE_BROADCAST, <<"fanout">>).

%% Publish Messages to a given Exchange.Queue
targeted_publish(Ticket, Queue, Payload) ->
    targeted_publish(Ticket, Queue, Payload, undefined).

targeted_publish(Ticket, Queue, Payload, ContentType) ->
    basic_publish(Ticket, ?EXCHANGE_TARGETED, Queue, Payload, ContentType).

callctl_publish(Ticket, CallId, Payload) ->
    callctl_publish(Ticket, CallId, Payload, undefined).

callctl_publish(Ticket, CallId, Payload, ContentType) when is_binary(CallId) ->
    callctl_publish(Ticket, binary_to_list(CallId), Payload, ContentType);
callctl_publish(Ticket, CallId, Payload, ContentType) ->
    Route = case string:str(CallId, binary_to_list(?EXCHANGE_CALLCTL)) of
		0 -> lists:concat([binary_to_list(?EXCHANGE_CALLCTL), ".", CallId]);
		_ -> CallId
	    end,

    basic_publish(Ticket, ?EXCHANGE_CALLCTL, list_to_binary(Route), Payload, ContentType).

callevt_publish(Ticket, CallId, Payload) ->
    callevt_publish(Ticket, CallId, Payload, undefined).

callevt_publish(Ticket, CallId, Payload, ContentType) when is_binary(CallId) ->
    callevt_publish(Ticket, binary_to_list(CallId), Payload, ContentType);
callevt_publish(Ticket, CallId, Payload, ContentType) ->
    Route = case string:str(CallId, binary_to_list(?EXCHANGE_CALLEVT)) of
		0 -> lists:concat([binary_to_list(?EXCHANGE_CALLEVT), ".", CallId]);
		_ -> CallId
	    end,
    basic_publish(Ticket, ?EXCHANGE_CALLEVT, list_to_binary(Route), Payload, ContentType).

broadcast_publish(Ticket, Payload) ->
    targeted_publish(Ticket, Payload, undefined).

broadcast_publish(Ticket, Payload, ContentType) when is_list(ContentType) ->
    broadcast_publish(Ticket, Payload, list_to_binary(ContentType));
broadcast_publish(Ticket, Payload, ContentType) ->
    basic_publish(Ticket, ?EXCHANGE_BROADCAST, <<"#">>, Payload, ContentType).

%% Create (or access) an Exchange
targeted_exchange(Ticket) ->
    new_exchange(Ticket, ?EXCHANGE_TARGETED, ?TYPE_TARGETED).

callctl_exchange(Ticket) ->
    new_exchange(Ticket, ?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

callevt_exchange(Ticket) ->
    new_exchange(Ticket, ?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

broadcast_exchange(Ticket) ->
    new_exchange(Ticket, ?EXCHANGE_BROADCAST, ?TYPE_BROADCAST).

%% A generic Exchange maker
new_exchange(Ticket, Exchange, Type) ->
    new_exchange(Ticket, Exchange, Type, []).
new_exchange(Ticket, Exchange, Type, Options) ->
    #'exchange.declare'{
	      ticket = Ticket
	      ,exchange = Exchange
	      ,type = Type
	      ,passive = get_value(passive, Options, false)
	      ,durable = get_value(durable, Options, false)
	      ,auto_delete = get_value(auto_delete, Options, false)
	      ,internal = get_value(internal, Options, false)
	      ,nowait = get_value(nowait, Options, false)
	      ,arguments = get_value(arguments, Options, [])
	     }.

new_targeted_queue(Ticket, QueueName) ->
    new_queue(Ticket, lists:concat([binary_to_list(?EXCHANGE_TARGETED), ".", QueueName])).

new_callevt_queue(Ticket, CallId) ->
    new_queue(Ticket
	      ,lists:concat([binary_to_list(?EXCHANGE_CALLEVT), ".", CallId])
	      ,[{exclusive, false}]).

new_callctl_queue(Ticket, CallId) ->
    new_queue(Ticket
	      ,lists:concat([binary_to_list(?EXCHANGE_CALLCTL), ".", CallId])
	      ,[{exclusive, false}, {auto_delete, false}]).

%% Declare a queue
new_queue(Ticket, Queue) ->
    new_queue(Ticket, Queue, []).
new_queue(Ticket, Queue, Prop) when is_list(Queue) ->
    new_queue(Ticket, list_to_binary(Queue), Prop);
new_queue(Ticket, Queue, Prop) ->
    #'queue.declare'{
	   ticket = Ticket
	   ,queue = Queue
	   ,passive = get_value(passive, Prop, false)
	   ,durable = get_value(durable, Prop, false)
	   ,exclusive = get_value(exclusive, Prop, true)
	   ,auto_delete = get_value(auto_delete, Prop, true)
	   ,nowait = get_value(nowait, Prop, false)
	   ,arguments = get_value(arguments, Prop, [])
	  }.

%% Bind a Queue to an Exchange (with optional Routing Key)
bind_q_to_targeted(Ticket, Queue) ->
    bind_q_to_targeted(Ticket, Queue, Queue).

bind_q_to_targeted(Ticket, Queue, Routing) ->
    bind_q_to_exchange(Ticket, Queue, Routing, ?EXCHANGE_TARGETED).

bind_q_to_callctl(Ticket, Queue) ->
    bind_q_to_callctl(Ticket, Queue, Queue).

bind_q_to_callctl(Ticket, Queue, Routing) ->
    bind_q_to_exchange(Ticket, Queue, Routing, ?EXCHANGE_CALLCTL).

bind_q_to_callevt(Ticket, Queue) ->
    bind_q_to_callevt(Ticket, Queue, Queue).

bind_q_to_callevt(Ticket, Queue, Routing) ->
    bind_q_to_exchange(Ticket, Queue, Routing, ?EXCHANGE_CALLEVT).

bind_q_to_broadcast(Ticket, Queue) ->
    bind_q_to_broadcast(Ticket, Queue, <<"#">>).

bind_q_to_broadcast(Ticket, Queue, Routing) ->
    bind_q_to_exchange(Ticket, Queue, Routing, ?EXCHANGE_BROADCAST).

%% generic binder
bind_q_to_exchange(Ticket, Queue, Routing, Exchange) when is_list(Queue) ->
    bind_q_to_exchange(Ticket, list_to_binary(Queue), Routing, Exchange);
bind_q_to_exchange(Ticket, Queue, Routing, Exchange) when is_list(Routing) ->
    bind_q_to_exchange(Ticket, Queue, list_to_binary(Routing), Exchange);
bind_q_to_exchange(Ticket, Queue, Routing, Exchange) when is_list(Exchange) ->
    bind_q_to_exchange(Ticket, Queue, Routing, list_to_binary(Exchange));
bind_q_to_exchange(Ticket, Queue, Routing, Exchange) ->
    #'queue.bind'{
		   ticket = Ticket
		   ,queue = Queue
		   ,exchange = Exchange
		   ,routing_key = Routing
		   ,nowait = false
		   ,arguments = []
		 }.

callevt_consume(Ticket, CallId) ->
    basic_consume(Ticket, lists:concat([binary_to_list(?EXCHANGE_CALLEVT), ".", CallId])).
callctl_consume(Ticket, CallId) ->
    basic_consume(Ticket, lists:concat([binary_to_list(?EXCHANGE_CALLCTL), ".", CallId])).

%% create a consumer for a Queue
basic_consume(Ticket, Queue) when is_list(Queue) ->
    basic_consume(Ticket, list_to_binary(Queue));
basic_consume(Ticket, Queue) ->
    #'basic.consume'{
	       ticket = Ticket
	       ,queue = Queue
	       ,consumer_tag = Queue
	       ,no_local = false
	       ,no_ack = true
	       ,exclusive = true
	       ,nowait = false
	      }.

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
basic_publish(Ticket, Exchange, Queue, Payload) ->
    basic_publish(Ticket, Exchange, Queue, Payload, undefined).
basic_publish(Ticket, Exchange, Queue, Payload, ContentType) ->
    basic_publish(Ticket, Exchange, Queue, Payload, ContentType, []).
basic_publish(Ticket, Exchange, Queue, Payload, ContentType, Prop) ->
    BasicPublish = #'basic.publish'{
      ticket = Ticket
      ,exchange = Exchange
      ,routing_key = Queue
      ,mandatory = get_value(mandatory, Prop, false)
      ,immediate = get_value(immediate, Prop, false)
     },

    %% Add the message to the publish, converting to binary
    AmqpMsg = #'amqp_msg'{payload = Payload
			  ,props=#'P_basic'{content_type=ContentType}
			 },
    {BasicPublish, AmqpMsg}.
