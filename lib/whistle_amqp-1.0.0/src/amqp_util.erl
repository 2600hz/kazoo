-module(amqp_util).

-include("amqp_util.hrl").

-import(props, [get_value/2, get_value/3]).

-export([targeted_exchange/0, targeted_publish/2, targeted_publish/3]).
-export([callctl_exchange/0, callctl_publish/2, callctl_publish/3]).
-export([callevt_exchange/0, callevt_publish/2, callevt_publish/3]).
-export([broadcast_exchange/0, broadcast_publish/1, broadcast_publish/2]).
-export([resource_exchange/0, resource_publish/1, resource_publish/2]).
-export([callmgr_exchange/0, callmgr_publish/3]).

-export([bind_q_to_targeted/1, bind_q_to_targeted/2, unbind_q_from_targeted/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2, unbind_q_from_callctl/1]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3, unbind_q_from_callevt/2]).
-export([bind_q_to_broadcast/1, bind_q_to_broadcast/2, unbind_q_from_broadcast/2]).
-export([bind_q_to_resource/1, bind_q_to_resource/2, unbind_q_from_resource/2]).
-export([bind_q_to_callmgr/2, unbind_q_from_callmgr/2]).

-export([new_targeted_queue/0, new_targeted_queue/1, new_callevt_queue/1, new_callctl_queue/1, new_broadcast_queue/1, new_callmgr_queue/1, new_callmgr_queue/2]).
-export([delete_callevt_queue/1, delete_callctl_queue/1, delete_callmgr_queue/1]).

-export([new_queue/0, new_queue/1, new_queue/2, basic_consume/1, basic_consume/2
	 ,basic_publish/3, basic_publish/4, basic_cancel/1, queue_delete/1, queue_delete/2]).

-export([access_request/0, access_request/1, basic_ack/0, basic_nack/0]).

-export([is_json/1, is_host_available/0]).

-export([monitor_exchange/0, monitor_publish/3]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).
-export([bind_q_to_monitor/2]).

monitor_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

monitor_exchange() ->
    new_exchange(?EXCHANGE_MONITOR, ?TYPE_MONITOR).

new_monitor_queue() ->
    new_queue(<<"">>, [{exclusive, false}, {auto_delete, true}]).

new_monitor_queue(Name) ->
    new_queue(whistle_util:to_binary(Name), [{exclusive, false}, {auto_delete, true}]).

delete_monitor_queue(Queue) ->
    queue_delete(Queue, []).

bind_q_to_monitor(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_MONITOR).

%% Publish Messages to a given Exchange.Queue
targeted_publish(Queue, Payload) ->
    targeted_publish(Queue, Payload, <<"application/json">>).

targeted_publish(Queue, Payload, ContentType) ->
    basic_publish(?EXCHANGE_TARGETED, Queue, Payload, ContentType).

callctl_publish(CallId, Payload) ->
    callctl_publish(CallId, Payload, <<"application/json">>).

callctl_publish(CallId, Payload, ContentType) ->
    basic_publish(?EXCHANGE_CALLCTL, CallId, Payload, ContentType).

callevt_publish(Payload, media) ->
    basic_publish(?EXCHANGE_CALLEVT, ?KEY_CALL_MEDIA_REQ, Payload, <<"application/json">>);
callevt_publish(CallId, Payload) ->
    callevt_publish(CallId, Payload, event).

callevt_publish(CallId, Payload, event) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, CallId/binary>>, Payload, <<"application/json">>);
callevt_publish(CallId, Payload, status_req) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, CallId/binary>>, Payload, <<"application/json">>);
callevt_publish(CallId, Payload, cdr) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, CallId/binary>>, Payload, <<"application/json">>).

broadcast_publish(Payload) ->
    broadcast_publish(Payload, undefined).

broadcast_publish(Payload, ContentType) when is_list(ContentType) ->
    broadcast_publish(Payload, list_to_binary(ContentType));
broadcast_publish(Payload, ContentType) when is_list(Payload) ->
    broadcast_publish(list_to_binary(Payload), ContentType);
broadcast_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_BROADCAST, <<"#">>, Payload, ContentType).

resource_publish(Payload) ->
    resource_publish(Payload, undefined).

resource_publish(Payload, ContentType) when is_list(ContentType) ->
    resource_publish(Payload, list_to_binary(ContentType));
resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, <<"#">>, Payload, ContentType).

%% What host to publish to, what to send, what content type, what routing key
callmgr_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).

%% Create (or access) an Exchange
targeted_exchange() ->
    new_exchange(?EXCHANGE_TARGETED, ?TYPE_TARGETED).

callctl_exchange() ->
    new_exchange(?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

callevt_exchange() ->
    new_exchange(?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

broadcast_exchange() ->
    new_exchange(?EXCHANGE_BROADCAST, ?TYPE_BROADCAST).

resource_exchange() ->
    new_exchange(?EXCHANGE_RESOURCE, ?TYPE_RESOURCE).

callmgr_exchange() ->
    new_exchange(?EXCHANGE_CALLMGR, ?TYPE_CALLMGR).

%% A generic Exchange maker
new_exchange(Exchange, Type) ->
    new_exchange(Exchange, Type, []).
new_exchange(Exchange, Type, _Options) ->
    ED = #'exchange.declare'{
      exchange = Exchange
      ,type = Type
     },
    #'exchange.declare_ok'{} = amqp_manager:misc_req(ED).

new_targeted_queue() ->
    new_queue(<<>>, [{nowait, false}]).

new_targeted_queue(<<>>) ->
    new_queue(<<>>, [{nowait, false}]);
new_targeted_queue(QueueName) ->
    new_queue(list_to_binary([?EXCHANGE_TARGETED, ".", QueueName]), [{nowait, false}]).

new_broadcast_queue(<<>>) ->
    new_queue(<<>>, [{nowait, false}]);
new_broadcast_queue(QueueName) ->
    new_queue(list_to_binary([?EXCHANGE_BROADCAST, ".", QueueName]), [{nowait, false}]).

new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(CallId) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(CallId) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_callmgr_queue(Queue) ->
    new_queue(Queue, []).
new_callmgr_queue(Queue, Opts) ->
    new_queue(Queue, Opts).

%% Declare a queue and returns the queue Name
new_queue() ->
    new_queue(<<>>). % let's the client lib create a random queue name
new_queue(Queue) ->
    new_queue(Queue, []).
new_queue(Queue, Options) when is_list(Queue) ->
    new_queue(list_to_binary(Queue), Options);
new_queue(Queue, Options) ->
    QD = #'queue.declare'{
      queue = Queue
      ,passive = get_value(passive, Options, false)
      ,durable = get_value(durable, Options, false)
      ,exclusive = get_value(exclusive, Options, false)
      ,auto_delete = get_value(auto_delete, Options, true)
      ,nowait = get_value(nowait, Options, false)
      ,arguments = get_value(arguments, Options, [])
     },
    case amqp_manager:consume(QD) of
	ok -> Queue;
	#'queue.declare_ok'{queue=Q} -> Q;
	_Other ->
	    {error, amqp_error}
    end.

delete_callevt_queue(CallId) ->
    delete_callevt_queue(CallId, []).

delete_callevt_queue(CallId, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLEVT, ".", CallId]), Prop).

delete_callctl_queue(CallId) ->
    delete_callctl_queue(CallId, []).

delete_callctl_queue(CallId, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLCTL, ".", CallId]), Prop).

delete_callmgr_queue(Queue) ->
    queue_delete(Queue, []).

%% Bind a Queue to an Exchange (with optional Routing Key)
bind_q_to_targeted(Queue) ->
    bind_q_to_targeted(Queue, Queue).

bind_q_to_targeted(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TARGETED).

bind_q_to_callctl(Queue) ->
    bind_q_to_callctl(Queue, Queue).

bind_q_to_callctl(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLCTL).


%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallId
bind_q_to_callevt(Queue, media_req) ->
    bind_q_to_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallId) ->
    bind_q_to_callevt(Queue, CallId, events).

bind_q_to_callevt(Queue, CallId, events) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_EVENT/binary, CallId/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, status_req) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallId, cdr) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_CDR/binary, CallId/binary>>, ?EXCHANGE_CALLEVT).

bind_q_to_broadcast(Queue) ->
    bind_q_to_broadcast(Queue, <<"#">>).

bind_q_to_broadcast(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_BROADCAST).

bind_q_to_resource(Queue) ->
    bind_q_to_resource(Queue, <<"#">>).

bind_q_to_resource(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

bind_q_to_callmgr(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

%% generic binder
bind_q_to_exchange(Queue, Routing, Exchange) when is_list(Queue) ->
    bind_q_to_exchange(list_to_binary(Queue), Routing, Exchange);
bind_q_to_exchange(Queue, Routing, Exchange) when is_list(Routing) ->
    bind_q_to_exchange(Queue, list_to_binary(Routing), Exchange);
bind_q_to_exchange(Queue, Routing, Exchange) ->
    QB = #'queue.bind'{
      queue = Queue %% what queue does the binding attach to?
      ,exchange = Exchange %% what exchange does the binding attach to?
      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
      ,nowait = true
      ,arguments = []
     },
    amqp_manager:consume(QB).

unbind_q_from_callevt(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).
unbind_q_from_callctl(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_CALLCTL).
unbind_q_from_resource(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).
unbind_q_from_broadcast(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_BROADCAST).
unbind_q_from_callmgr(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).
unbind_q_from_targeted(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_TARGETED).

unbind_q_from_exchange(Queue, Routing, Exchange) ->
    QU = #'queue.unbind'{
      queue = Queue
      ,exchange = Exchange
      ,routing_key = Routing
      ,arguments = []
     },
    amqp_manager:consume(QU).

%% create a consumer for a Queue
basic_consume(Queue) ->
    basic_consume(Queue, []).

basic_consume(Queue, Options) when is_list(Queue) ->
    basic_consume(list_to_binary(Queue), Options);
basic_consume(Queue, Options) ->
    BC = #'basic.consume'{
      queue = Queue
      ,consumer_tag = Queue
      ,no_local = get_value(no_local, Options, false)
      ,no_ack = get_value(no_ack, Options, true)
      ,exclusive = get_value(exclusive, Options, true)
      ,nowait = get_value(nowait, Options, false)
     },
    amqp_manager:consume(BC).

basic_cancel(Queue) ->
    BC = #'basic.cancel'{
      consumer_tag = Queue
     },
    amqp_manager:consume(BC).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
basic_publish(Exchange, Queue, Payload) ->
    basic_publish(Exchange, Queue, Payload, undefined).
basic_publish(Exchange, Queue, Payload, ContentType) ->
    basic_publish(Exchange, Queue, Payload, ContentType, []).

basic_publish(Exchange, Queue, Payload, ContentType, Prop) when is_list(Payload) ->
    basic_publish(Exchange, Queue, list_to_binary(Payload), ContentType, Prop);
basic_publish(Exchange, Queue, Payload, ContentType, Prop) ->
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

    amqp_manager:publish(BP, AM).

queue_delete(Queue) ->
    queue_delete(Queue, []).

queue_delete(Queue, Prop) ->
    QD = #'queue.delete'{
      queue=Queue
      ,if_unused=get_value(if_unused, Prop, false)
      ,if_empty = get_value(if_empty, Prop, false)
      ,nowait = get_value(nowait, Prop, true)
     },
    amqp_manager:consume(QD).

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

is_json(Props) ->
    Props#'P_basic'.content_type == <<"application/json">>.

basic_ack() ->
    amqp_manager:consume(#'basic.ack'{}).

basic_nack() ->
    amqp_manager:consume(#'basic.nack'{}).

-spec(is_host_available/0 :: () -> boolean()).
is_host_available() ->
    amqp_mgr:is_available().
