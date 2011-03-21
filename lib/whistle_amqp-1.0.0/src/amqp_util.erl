-module(amqp_util).

-include("amqp_util.hrl").

-import(props, [get_value/2, get_value/3]).

-export([is_host_available/1]).

-export([targeted_exchange/1, targeted_publish/3, targeted_publish/4]).
-export([callctl_exchange/1, callctl_publish/3, callctl_publish/4]).
-export([callevt_exchange/1, callevt_publish/3, callevt_publish/4]).
-export([broadcast_exchange/1, broadcast_publish/2, broadcast_publish/3]).
-export([resource_exchange/1, resource_publish/2, resource_publish/3]).
-export([callmgr_exchange/1, callmgr_publish/4]).

-export([bind_q_to_targeted/2, bind_q_to_targeted/3, unbind_q_from_targeted/2]).
-export([bind_q_to_callctl/2, bind_q_to_callctl/3, unbind_q_from_callctl/2]).
-export([bind_q_to_callevt/3, bind_q_to_callevt/4, unbind_q_from_callevt/3]).
-export([bind_q_to_broadcast/2, bind_q_to_broadcast/3, unbind_q_from_broadcast/3]).
-export([bind_q_to_resource/2, bind_q_to_resource/3, unbind_q_from_resource/3]).
-export([bind_q_to_callmgr/3, unbind_q_from_callmgr/3]).

-export([new_targeted_queue/2, new_callevt_queue/2, new_callctl_queue/2, new_broadcast_queue/2, new_callmgr_queue/2]).
-export([delete_callevt_queue/2, delete_callctl_queue/2, delete_callmgr_queue/2]).

-export([new_queue/1, new_queue/2, new_queue/3, delete_queue/2, basic_consume/2, basic_consume/3
	 ,basic_publish/4, basic_publish/5, channel_close/1, channel_close/2, basic_cancel/2
	 ,channel_close/3, queue_delete/2, queue_delete/3]).

-export([get_msg/2]).

-export([access_request/0, access_request/1]).

-export([get_msg_type/1, is_json/1]).

-export([monitor_exchange/1, monitor_publish/4]).
-export([new_monitor_queue/1, new_monitor_queue/2, delete_monitor_queue/2]).
-export([bind_q_to_monitor/3]).

monitor_publish(Host, Payload, ContentType, RoutingKey) ->
    basic_publish(Host, ?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

monitor_exchange(Host) ->
    new_exchange(Host, ?EXCHANGE_MONITOR, ?TYPE_MONITOR).

new_monitor_queue(Host) ->
    new_queue(Host, <<"">>, [{exclusive, false}, {auto_delete, true}]).

new_monitor_queue(Host, Name) ->
    new_queue(Host, whistle_util:to_binary(Name), [{exclusive, false}, {auto_delete, true}]).

delete_monitor_queue(Host, Queue) ->
    delete_queue(Host, Queue, []).

bind_q_to_monitor(Host, Queue, Routing) ->
    bind_q_to_exchange(Host, Queue, Routing, ?EXCHANGE_MONITOR).

%% Publish Messages to a given Exchange.Queue
targeted_publish(Host, Queue, Payload) ->
    targeted_publish(Host, Queue, Payload, <<"application/json">>).

targeted_publish(Host, Queue, Payload, ContentType) ->
    basic_publish(Host, ?EXCHANGE_TARGETED, Queue, Payload, ContentType).

callctl_publish(Host, CallId, Payload) ->
    callctl_publish(Host, CallId, Payload, <<"application/json">>).

callctl_publish(Host, CallId, Payload, ContentType) ->
    basic_publish(Host, ?EXCHANGE_CALLCTL, CallId, Payload, ContentType).

callevt_publish(Host, Payload, media) ->
    basic_publish(Host, ?EXCHANGE_CALLEVT, ?KEY_CALL_MEDIA_REQ, Payload, <<"application/json">>);
callevt_publish(Host, CallId, Payload) ->
    callevt_publish(Host, CallId, Payload, event).

callevt_publish(Host, CallId, Payload, event) ->
    basic_publish(Host, ?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, CallId/binary>>, Payload, <<"application/json">>);
callevt_publish(Host, CallId, Payload, status_req) ->
    basic_publish(Host, ?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, CallId/binary>>, Payload, <<"application/json">>);
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
    ED = #'exchange.declare'{
      exchange = Exchange
      ,type = Type
     },
    #'exchange.declare_ok'{} = amqp_manager:misc_req(Host, ED).

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
    QD = #'queue.declare'{
      queue = Queue
      ,passive = get_value(passive, Options, false)
      ,durable = get_value(durable, Options, false)
      ,exclusive = get_value(exclusive, Options, true)
      ,auto_delete = get_value(auto_delete, Options, true)
      ,nowait = get_value(nowait, Options, false)
      ,arguments = get_value(arguments, Options, [])
     },
    #'queue.declare_ok'{queue=Q} = amqp_manager:consume(Host, QD),
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

delete_queue(_, <<>>, _) -> no_queue;
delete_queue(Host, Queue, Prop) ->
    queue_delete(Host, Queue, Prop).

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
bind_q_to_callevt(Host, Queue, media_req) ->
    bind_q_to_exchange(Host, Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Host, Queue, CallId) ->
    bind_q_to_callevt(Host, Queue, CallId, events).

bind_q_to_callevt(Host, Queue, CallId, events) ->
    bind_q_to_exchange(Host, Queue, <<?KEY_CALL_EVENT/binary, CallId/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Host, Queue, CallID, status_req) ->
    bind_q_to_exchange(Host, Queue, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
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
    QB = #'queue.bind'{
      queue = Queue %% what queue does the binding attach to?
      ,exchange = Exchange %% what exchange does the binding attach to?
      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
      ,nowait = true
      ,arguments = []
     },
    amqp_manager:consume(Host, QB).

unbind_q_from_callevt(Host, Queue, Routing) ->
    unbind_q_from_exchange(Host, Queue, Routing, ?EXCHANGE_CALLEVT).
unbind_q_from_callctl(Host, Queue) ->
    unbind_q_from_exchange(Host, Queue, Queue, ?EXCHANGE_CALLCTL).
unbind_q_from_resource(Host, Queue, Routing) ->
    unbind_q_from_exchange(Host, Queue, Routing, ?EXCHANGE_RESOURCE).
unbind_q_from_broadcast(Host, Queue, Routing) ->
    unbind_q_from_exchange(Host, Queue, Routing, ?EXCHANGE_BROADCAST).
unbind_q_from_callmgr(Host, Queue, Routing) ->
    unbind_q_from_exchange(Host, Queue, Routing, ?EXCHANGE_CALLMGR).
unbind_q_from_targeted(Host, Queue) ->
    unbind_q_from_exchange(Host, Queue, Queue, ?EXCHANGE_TARGETED).

unbind_q_from_exchange(Host, Queue, Routing, Exchange) ->
    QU = #'queue.unbind'{
      queue = Queue
      ,exchange = Exchange
      ,routing_key = Routing
      ,arguments = []
     },
    amqp_manager:consume(Host, QU).

%% create a consumer for a Queue
basic_consume(Host, Queue) ->
    basic_consume(Host, Queue, []).

basic_consume(Host, Queue, Options) when is_list(Queue) ->
    basic_consume(Host, list_to_binary(Queue), Options);
basic_consume(Host, Queue, Options) ->
    BC = #'basic.consume'{
      queue = Queue
      ,consumer_tag = Queue
      ,no_local = get_value(no_local, Options, false)
      ,no_ack = get_value(no_ack, Options, true)
      ,exclusive = get_value(exclusive, Options, true)
      ,nowait = get_value(nowait, Options, false)
     },
    amqp_manager:consume(Host, BC).

basic_cancel(Host, Queue) ->
    BC = #'basic.cancel'{
      consumer_tag = Queue
     },
    amqp_manager:consume(Host, BC).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
basic_publish(Host, Exchange, Queue, Payload) ->
    basic_publish(Host, Exchange, Queue, Payload, undefined).
basic_publish(Host, Exchange, Queue, Payload, ContentType) ->
    basic_publish(Host, Exchange, Queue, Payload, ContentType, []).

basic_publish(Host, Exchange, Queue, Payload, ContentType, Prop) when is_list(Payload) ->
    basic_publish(Host, Exchange, Queue, list_to_binary(Payload), ContentType, Prop);
basic_publish(Host, Exchange, Queue, Payload, ContentType, Prop) ->
    BP = #'basic.publish'{
      exchange = Exchange
      ,routing_key = Queue
      ,mandatory = get_value(mandatory, Prop, false)
      ,immediate = get_value(immediate, Prop, false)
     },

    %% Add the message to the publish, converting to binary
    AM = #'amqp_msg'{
      payload = Payload
      ,props=#'P_basic'{content_type=ContentType}
     },

    amqp_manager:publish(Host, BP, AM).

channel_close(Host) ->
    channel_close(Host, <<"Goodbye">>).

channel_close(Host, Msg) when is_list(Msg) ->
    channel_close(Host, list_to_binary(Msg));
channel_close(Host, Msg) ->
    channel_close(Host, Msg, 200).

channel_close(Host, Msg, Code) when is_list(Host) ->
    case amqp_manager:open_channel(self(), Host) of
	{ok, Channel, _Ticket} ->
	    channel_close(Channel, Msg, Code);
	{error, _}=E -> E
    end;
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
    QD = #'queue.delete'{
      queue=Queue
      ,if_unused=get_value(if_unused, Prop, false)
      ,if_empty = get_value(if_empty, Prop, false)
      ,nowait = get_value(nowait, Prop, true)
     },
    amqp_manager:consume(Host, QD).

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

-spec(get_msg/2 :: (Host :: string(), Queue :: binary()) -> #amqp_msg{}).
get_msg(Host, Queue) ->
    case amqp_manager:open_channel(self(), Host) of
	{ok, C, _T} ->
	    Get = #'basic.get'{queue=Queue, no_ack=false},
	    case amqp_channel:call(C, Get) of
		{#'basic.get_ok'{}, Content} ->
		    Content;
		#'basic.get_empty'{} ->
		    #amqp_msg{}
	    end;
	{error, _}=E -> E
    end.

get_msg_type(Msg) ->
    { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg) }.

is_json(Props) ->
    Props#'P_basic'.content_type == <<"application/json">>.

-spec(is_host_available/1 :: (Host :: string()) -> boolean()).
is_host_available(Host) ->
    amqp_manager:is_available(Host).
