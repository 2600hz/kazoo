-module(amqp_util).

-include("amqp_util.hrl").

-export([targeted_exchange/0, targeted_publish/2, targeted_publish/3]).
-export([callctl_exchange/0, callctl_publish/2, callctl_publish/3]).
-export([callevt_exchange/0, callevt_publish/2, callevt_publish/3]).
-export([resource_exchange/0, resource_publish/1, resource_publish/2]).
-export([originate_resource_publish/1, originate_resource_publish/2]).
-export([offnet_resource_publish/1, offnet_resource_publish/2]).
-export([callmgr_exchange/0, callmgr_publish/3]).
-export([monitor_exchange/0, monitor_publish/3]).

-export([bind_q_to_targeted/1, bind_q_to_targeted/2, unbind_q_from_targeted/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2, unbind_q_from_callctl/1]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3, unbind_q_from_callevt/2]).
-export([bind_q_to_resource/1, bind_q_to_resource/2, unbind_q_from_resource/2]).
-export([bind_q_to_callmgr/2, unbind_q_from_callmgr/2]).
-export([bind_q_to_monitor/2]).

-export([new_targeted_queue/0, new_targeted_queue/1]).
-export([new_callctl_queue/1, delete_callctl_queue/1]).
-export([new_callevt_queue/1, delete_callevt_queue/1]).
-export([new_callmgr_queue/1, new_callmgr_queue/2, delete_callmgr_queue/1]).
-export([new_resource_queue/0, new_resource_queue/1]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).

-export([new_queue/0, new_queue/1, new_queue/2, basic_consume/1, basic_consume/2
	 ,basic_publish/3, basic_publish/4, basic_cancel/1, queue_delete/1, queue_delete/2]).

-export([access_request/0, access_request/1, basic_ack/1, basic_nack/1, basic_qos/1]).

-export([is_json/1, is_host_available/0]).

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
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, CallId/binary>>, Payload, <<"application/json">>);
callevt_publish(_CallId, Payload, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, <<"application/json">>).

resource_publish(Payload) ->
    resource_publish(Payload, <<"application/json">>).
resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_RESOURCE_REQ, Payload, ContentType).

originate_resource_publish(Payload) ->
    originate_resource_publish(Payload, <<"application/json">>).
originate_resource_publish(Payload, ContentType) ->
   basic_publish(?EXCHANGE_RESOURCE, ?KEY_ORGN_RESOURCE_REQ, Payload, ContentType).

offnet_resource_publish(Payload) ->
    offnet_resource_publish(Payload, <<"application/json">>).
offnet_resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_OFFNET_RESOURCE_REQ, Payload, ContentType).

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


-spec(new_targeted_queue/0 :: () -> binary() | tuple(error, amqp_error)).
-spec(new_targeted_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
new_targeted_queue() ->
    new_queue(<<>>, [{nowait, false}]).

new_targeted_queue(<<>>) ->
    new_queue(<<>>, [{nowait, false}]);
new_targeted_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec(new_callevt_queue/1 :: (CallID :: binary()) -> binary() | tuple(error, amqp_error)).
new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(CallId) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec(new_callctl_queue/1 :: (CallID :: binary()) -> binary() | tuple(error, amqp_error)).
new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(CallId) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", CallId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec(new_resource_queue/0 :: () -> binary() | tuple(error, amqp_error)).
-spec(new_resource_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
new_resource_queue() ->
    new_resource_queue(?RESOURCE_QUEUE_NAME).
new_resource_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec(new_callmgr_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
-spec(new_callmgr_queue/2 :: (Queue :: binary(), Opts :: proplist()) -> binary() | tuple(error, amqp_error)).
new_callmgr_queue(Queue) ->
    new_queue(Queue, []).
new_callmgr_queue(Queue, Opts) ->
    new_queue(Queue, Opts).

%% Declare a queue and returns the queue Name
-spec(new_queue/0 :: () -> binary() | tuple(error, amqp_error)).
-spec(new_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
-spec(new_queue/2 :: (Queue :: binary(), Opts :: proplist()) -> binary() | tuple(error, amqp_error)).
new_queue() ->
    new_queue(<<>>). % let's the client lib create a random queue name
new_queue(Queue) ->
    new_queue(Queue, []).
new_queue(Queue, Options) when is_binary(Queue) ->
    QD = #'queue.declare'{
      queue = Queue
      ,passive = props:get_value(passive, Options, false)
      ,durable = props:get_value(durable, Options, false)
      ,exclusive = props:get_value(exclusive, Options, false)
      ,auto_delete = props:get_value(auto_delete, Options, true)
      ,nowait = props:get_value(nowait, Options, false)
      ,arguments = props:get_value(arguments, Options, [])
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
-spec(bind_q_to_targeted/1 :: (Queue :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_targeted(Queue) ->
    bind_q_to_exchange(Queue, Queue, ?EXCHANGE_TARGETED).
bind_q_to_targeted(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TARGETED).

-spec(bind_q_to_callctl/1 :: (Queue :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
-spec(bind_q_to_callctl/2 :: (Queue :: binary(), Routing :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_callctl(Queue) ->
    bind_q_to_callctl(Queue, Queue).

bind_q_to_callctl(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLCTL).

%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallId
-spec(bind_q_to_callevt/2 :: (Queue :: binary(), Routing :: media_req | binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
-spec(bind_q_to_callevt/3 :: (Queue :: binary(), Routing :: media_req | binary(), Type :: events | status_req | cdr | other) ->
				  #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_callevt(Queue, media_req) ->
    bind_q_to_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallId) ->
    bind_q_to_callevt(Queue, CallId, events).

bind_q_to_callevt(Queue, CallId, events) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_EVENT/binary, CallId/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, status_req) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallId, cdr) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_CDR/binary, CallId/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, Routing, other) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

-spec(bind_q_to_resource/1 :: (Queue :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
-spec(bind_q_to_resource/2 :: (Queue :: binary(), Routing :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_resource(Queue) ->
    bind_q_to_resource(Queue, <<"#">>).
bind_q_to_resource(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

-spec(bind_q_to_callmgr/2 :: (Queue :: binary(), Routing :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_callmgr(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

%% generic binder
-spec(bind_q_to_exchange/3 :: (Queue :: binary(), Routing :: binary(), Exchange :: binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_exchange(Queue, Routing, Exchange) when is_binary(Queue), is_binary(Routing) ->
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
      ,no_local = props:get_value(no_local, Options, false)
      ,no_ack = props:get_value(no_ack, Options, true)
      ,exclusive = props:get_value(exclusive, Options, true)
      ,nowait = props:get_value(nowait, Options, false)
     },
    amqp_manager:consume(BC).

basic_cancel(Queue) ->
    BC = #'basic.cancel'{
      consumer_tag = Queue
     },
    amqp_manager:consume(BC).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
-spec(basic_publish/3 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist()) -> ok).
-spec(basic_publish/4 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist(), ContentType :: binary()) -> ok).
-spec(basic_publish/5 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist(), ContentType :: binary(), Prop :: proplist()) -> ok).
basic_publish(Exchange, Queue, Payload) ->
    basic_publish(Exchange, Queue, Payload, <<"application/json">>).
basic_publish(Exchange, Queue, Payload, ContentType) ->
    basic_publish(Exchange, Queue, Payload, ContentType, []).

basic_publish(Exchange, Queue, Payload, ContentType, Prop) when not is_binary(Payload) ->
    basic_publish(Exchange, Queue, whistle_util:to_binary(Payload), ContentType, Prop);
basic_publish(Exchange, Queue, Payload, ContentType, Prop) ->
    BP = #'basic.publish'{
      exchange = Exchange
      ,routing_key = Queue
      ,mandatory = props:get_value(mandatory, Prop, false)
      ,immediate = props:get_value(immediate, Prop, false)
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
      ,if_unused=props:get_value(if_unused, Prop, false)
      ,if_empty = props:get_value(if_empty, Prop, false)
      ,nowait = props:get_value(nowait, Prop, true)
     },
    amqp_manager:consume(QD).

access_request() ->
    access_request([]).
access_request(Options) ->
    #'access.request'{
      realm = props:get_value(realm, Options, <<"/data">>)
      ,exclusive = props:get_value(exclusive, Options, false)
      ,passive = props:get_value(passive, Options, true)
      ,active = props:get_value(active, Options, true)
      ,write = props:get_value(write, Options, true)
      ,read = props:get_value(read, Options, true)
     }.

is_json(Props) ->
    Props#'P_basic'.content_type == <<"application/json">>.

basic_ack(DTag) ->
    amqp_manager:consume(#'basic.ack'{delivery_tag=DTag}).

basic_nack(DTag) ->
    amqp_manager:consume(#'basic.nack'{delivery_tag=DTag}).

-spec(is_host_available/0 :: () -> boolean()).
is_host_available() ->
    amqp_mgr:is_available().

-spec(basic_qos/1 :: (PreFetch :: non_neg_integer()) -> ok).
basic_qos(PreFetch) when is_integer(PreFetch) ->
    amqp_manager:consume(#'basic.qos'{prefetch_count = PreFetch}).
