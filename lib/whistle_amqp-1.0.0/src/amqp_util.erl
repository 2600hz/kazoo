-module(amqp_util).

-include("amqp_util.hrl").

-export([targeted_exchange/0, targeted_publish/2, targeted_publish/3]).
-export([callctl_exchange/0, callctl_publish/2, callctl_publish/3]).
-export([callevt_exchange/0, callevt_publish/1, callevt_publish/3]).
-export([resource_exchange/0, resource_publish/1, resource_publish/2]).
-export([originate_resource_publish/1, originate_resource_publish/2]).
-export([offnet_resource_publish/1, offnet_resource_publish/2]).
-export([callmgr_exchange/0, callmgr_publish/3]).
-export([monitor_exchange/0, monitor_publish/3]).
-export([conference_exchange/0, conference_publish/2, conference_publish/3]).

-export([bind_q_to_targeted/1, bind_q_to_targeted/2, unbind_q_from_targeted/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2, unbind_q_from_callctl/1]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3, unbind_q_from_callevt/2, unbind_q_from_callevt/3]).
-export([bind_q_to_resource/1, bind_q_to_resource/2, unbind_q_from_resource/2]).
-export([bind_q_to_callmgr/2, unbind_q_from_callmgr/2]).
-export([bind_q_to_monitor/2]).
-export([bind_q_to_conference/2]).

-export([new_targeted_queue/0, new_targeted_queue/1]).
-export([new_callctl_queue/1, delete_callctl_queue/1]).
-export([new_callevt_queue/1, delete_callevt_queue/1]).
-export([new_callmgr_queue/1, new_callmgr_queue/2, delete_callmgr_queue/1]).
-export([new_resource_queue/0, new_resource_queue/1]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).
-export([new_conference_queue/1, new_conference_queue/2]).

-export([new_queue/0, new_queue/1, new_queue/2, basic_consume/1, basic_consume/2
	 ,basic_publish/3, basic_publish/4, basic_cancel/1, queue_delete/1, queue_delete/2]).

-export([access_request/0, access_request/1, basic_ack/1, basic_nack/1, basic_qos/1]).

-export([is_json/1, is_host_available/0, register_return_handler/0]).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Publish AMQP messages
%% @end
%%------------------------------------------------------------------------------
-spec targeted_publish/2 :: (Queue, Payload) -> ok when
      Queue :: binary(),
      Payload :: iolist().
-spec targeted_publish/3 :: (Queue, Payload, ContentType) -> ok when
      Queue :: binary(),
      Payload :: iolist(),
      ContentType :: binary().
targeted_publish(Queue, Payload) ->
    targeted_publish(Queue, Payload, <<"application/json">>).
targeted_publish(Queue, Payload, ContentType) ->
    basic_publish(?EXCHANGE_TARGETED, Queue, Payload, ContentType).

-spec callmgr_publish/3 :: (Payload, ContentType, RoutingKey) -> ok when
      Payload :: iolist(),
      ContentType :: binary(),
      RoutingKey :: binary().
%% TODO: The routing key on this function should be the first argument for consistency
callmgr_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).

-spec callctl_publish/2 :: (CallID, Payload) -> ok when
      CallID :: binary(),
      Payload :: iolist().
-spec callctl_publish/3 :: (CallID, Payload, ContentType) -> ok when
      CallID :: binary(),
      Payload :: iolist(),
      ContentType :: binary().
callctl_publish(CallID, Payload) ->
    callctl_publish(CallID, Payload, <<"application/json">>).
callctl_publish(CallID, Payload, ContentType) ->
    basic_publish(?EXCHANGE_CALLCTL, CallID, Payload, ContentType).

-spec callevt_publish/1 :: (Payload) -> ok when
      Payload :: iolist().
-spec callevt_publish/3 :: (CallID, Payload, Type) -> ok when
      CallID :: binary(),
      Payload :: iolist(),
      Type :: event | status_req | cdr | binary().
callevt_publish(Payload) ->
    basic_publish(?EXCHANGE_CALLEVT, ?KEY_CALL_MEDIA_REQ, Payload, <<"application/json">>).

callevt_publish(CallID, Payload, event) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, CallID/binary>>, Payload, <<"application/json">>);
callevt_publish(CallID, Payload, status_req) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, Payload, <<"application/json">>);
callevt_publish(CallID, Payload, cdr) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, CallID/binary>>, Payload, <<"application/json">>);
callevt_publish(_CallID, Payload, RoutingKey) when is_binary(RoutingKey) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, <<"application/json">>).

-spec resource_publish/1 :: (Payload) -> ok when
      Payload :: iolist().
-spec resource_publish/2 :: (Payload, ContentType) -> ok when
      Payload :: iolist(),
      ContentType :: binary().
resource_publish(Payload) ->
    resource_publish(Payload, <<"application/json">>).
resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_RESOURCE_REQ, Payload, ContentType).

-spec originate_resource_publish/1 :: (Payload) -> ok when
      Payload :: iolist().
-spec originate_resource_publish/2 :: (Payload, ContentType) -> ok when
      Payload :: iolist(),
      ContentType :: binary().
originate_resource_publish(Payload) ->
    originate_resource_publish(Payload, <<"application/json">>).
originate_resource_publish(Payload, ContentType) ->
   basic_publish(?EXCHANGE_RESOURCE, ?KEY_ORGN_RESOURCE_REQ, Payload, ContentType).

-spec offnet_resource_publish/1 :: (Payload) -> ok when
      Payload :: iolist().
-spec offnet_resource_publish/2 :: (Payload, ContentType) -> ok when
      Payload :: iolist(),
      ContentType :: binary().
offnet_resource_publish(Payload) ->
    offnet_resource_publish(Payload, <<"application/json">>).
offnet_resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_OFFNET_RESOURCE_REQ, Payload, ContentType).

%% monitor
-spec monitor_publish/3 :: (Payload, ContentType, RoutingKey) -> ok when
      Payload :: iolist(),
      ContentType :: binary(),
      RoutingKey :: binary().
monitor_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

%% conference
-spec conference_publish/2 :: (Payload, ConfID) -> ok when
      Payload :: iolist(),
      ConfID :: discovery | binary().
-spec conference_publish/3 :: (Payload, ConfID, Options) -> ok when
      Payload :: iolist(),
      ConfID :: discovery | binary(),
      Options :: proplist().
conference_publish(Payload, discovery) ->
    basic_publish(?EXCHANGE_CONFERENCE, ?KEY_CONF_DISCOVERY_REQ, Payload, <<"application/json">>);
conference_publish(Payload, ConfID) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONF_SERVICE_REQ/binary, ConfID/binary>>, Payload, <<"application/json">>).

conference_publish(Payload, discovery, Options) ->
    basic_publish(?EXCHANGE_CONFERENCE, ?KEY_CONF_DISCOVERY_REQ, Payload, <<"application/json">>, Options);
conference_publish(Payload, ConfID, Options) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONF_SERVICE_REQ/binary, ConfID/binary>>, Payload, <<"application/json">>, Options).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue
-spec(basic_publish/3 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist()) -> ok).
-spec(basic_publish/4 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist(), ContentType :: binary()) -> ok).
-spec(basic_publish/5 :: (Exchange :: binary(), Queue :: binary(), Payload :: iolist()|binary(), ContentType :: binary(), Prop :: proplist()) -> ok).
basic_publish(Exchange, Queue, Payload) ->
    basic_publish(Exchange, Queue, Payload, <<"application/json">>).
basic_publish(Exchange, Queue, Payload, ContentType) ->
    basic_publish(Exchange, Queue, Payload, ContentType, []).

basic_publish(Exchange, Queue, Payload, ContentType, Prop) when is_list(Payload) ->
    basic_publish(Exchange, Queue, iolist_to_binary(Payload), ContentType, Prop);
basic_publish(Exchange, Queue, Payload, ContentType, Prop) when is_binary(Payload) ->
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


%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Create AMQP exchanges
%% @end
%%------------------------------------------------------------------------------
-spec targeted_exchange/0 :: () -> ok.
targeted_exchange() ->
    new_exchange(?EXCHANGE_TARGETED, ?TYPE_TARGETED).

-spec callctl_exchange/0 :: () -> ok.
callctl_exchange() ->
    new_exchange(?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

-spec callevt_exchange/0 :: () -> ok.
callevt_exchange() ->
    new_exchange(?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

-spec resource_exchange/0 :: () -> ok.
resource_exchange() ->
    new_exchange(?EXCHANGE_RESOURCE, ?TYPE_RESOURCE).

-spec callmgr_exchange/0 :: () -> ok.
callmgr_exchange() ->
    new_exchange(?EXCHANGE_CALLMGR, ?TYPE_CALLMGR).

-spec monitor_exchange/0 :: () -> ok.
monitor_exchange() ->
    new_exchange(?EXCHANGE_MONITOR, ?TYPE_MONITOR).

-spec conference_exchange/0 :: () -> ok.
conference_exchange() ->
    new_exchange(?EXCHANGE_CONFERENCE, ?TYPE_CONFERENCE).

%% A generic Exchange maker
-spec new_exchange/2 :: (Exchange, Type) -> ok when
      Exchange :: binary(),
      Type :: binary().
-spec new_exchange/3 :: (Exchange, Type, Options) -> ok when
      Exchange :: binary(),
      Type :: binary(),
      Options :: proplist().
new_exchange(Exchange, Type) ->
    new_exchange(Exchange, Type, []).
new_exchange(Exchange, Type, _Options) ->
    ED = #'exchange.declare'{
      exchange = Exchange
      ,type = Type
     },
    amqp_manager:misc_req(ED).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Create AMQP queues
%% @end
%%------------------------------------------------------------------------------
-spec(new_targeted_queue/0 :: () -> binary() | tuple(error, amqp_error)).
-spec(new_targeted_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
new_targeted_queue() ->
    new_targeted_queue(<<>>).

new_targeted_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec(new_callevt_queue/1 :: (CallID :: binary()) -> binary() | tuple(error, amqp_error)).
new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", CallID])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec(new_callctl_queue/1 :: (CallID :: binary()) -> binary() | tuple(error, amqp_error)).
new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", CallID])
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
    new_callmgr_queue(Queue, []).
new_callmgr_queue(Queue, Opts) ->
    new_queue(Queue, Opts).

-spec(new_monitor_queue/0 :: () -> binary() | tuple(error, amqp_error)).
-spec(new_monitor_queue/1 :: (Queue :: binary()) -> binary() | tuple(error, amqp_error)).
new_monitor_queue() ->
    new_monitor_queue(<<>>).
new_monitor_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}]).

-spec(new_conference_queue/1 :: (Queue :: discovery | binary()) -> binary() | tuple(error, amqp_error)).
-spec(new_conference_queue/2 :: (Queue :: discovery | binary(), Options :: proplist()) -> binary() | tuple(error, amqp_error)).
new_conference_queue(discovery) ->
    new_queue(?CONF_DISCOVERY_QUEUE_NAME, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_conference_queue(ConfId) ->
    new_queue(list_to_binary([?KEY_CONF_SERVICE_REQ, ConfId])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

new_conference_queue(discovery, Options) ->
    new_queue(?CONF_DISCOVERY_QUEUE_NAME, Options);
new_conference_queue(ConfId, Options) ->
    new_queue(list_to_binary([?KEY_CONF_SERVICE_REQ, ConfId]), Options).

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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Delete AMQP queue
%% @end
%%------------------------------------------------------------------------------
delete_callevt_queue(CallID) ->
    delete_callevt_queue(CallID, []).
delete_callevt_queue(CallID, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLEVT, ".", CallID]), Prop).

delete_callctl_queue(CallID) ->
    delete_callctl_queue(CallID, []).
delete_callctl_queue(CallID, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLCTL, ".", CallID]), Prop).

delete_callmgr_queue(Queue) ->
    queue_delete(Queue, []).

delete_monitor_queue(Queue) ->
    queue_delete(Queue, []).

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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
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

%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallID
-spec bind_q_to_callevt/2 :: (Queue, CallID) -> #'basic.consume_ok'{} | tuple(error, term()) when
      Queue :: binary(),
      CallID :: media_req | binary().
-spec bind_q_to_callevt/3 :: (Queue, CallID, Type) -> #'basic.consume_ok'{} | tuple(error, term()) when
      Queue :: binary(),
      CallID :: binary(),
      Type :: events | status_req | cdr | other.
bind_q_to_callevt(Queue, media_req) ->
    bind_q_to_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID) ->
    bind_q_to_callevt(Queue, CallID, events).

bind_q_to_callevt(Queue, CallID, events) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_EVENT/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, status_req) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, cdr) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_CDR/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
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

bind_q_to_monitor(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_MONITOR).

-spec(bind_q_to_conference/2 :: (Queue :: binary(), Routing :: discovery | service | binary()) -> #'basic.consume_ok'{} | tuple(error, term())).
bind_q_to_conference(Queue, service) ->
    bind_q_to_conference(Queue, <<"*">>);
bind_q_to_conference(Queue, discovery) ->
    bind_q_to_exchange(Queue, ?KEY_CONF_DISCOVERY_REQ, ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, ConfId) ->
    bind_q_to_exchange(Queue, <<?KEY_CONF_SERVICE_REQ/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE).

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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Unbind a Queue from an Exchange
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q_from_callevt/2 :: (Queue, CallID) -> #'basic.consume_ok'{} | tuple(error, term()) when
      Queue :: binary(),
      CallID :: media_req | binary().
-spec unbind_q_from_callevt/3 :: (Queue, CallID, Type) -> #'basic.consume_ok'{} | tuple(error, term()) when
      Queue :: binary(),
      CallID :: binary(),
      Type :: events | status_req | cdr | other.
unbind_q_from_callevt(Queue, media_req) ->
    unbind_q_from_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID) ->
    unbind_q_from_callevt(Queue, CallID, events).

unbind_q_from_callevt(Queue, CallID, events) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_EVENT/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, status_req) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, cdr) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_CDR/binary, CallID/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, Routing, other) ->
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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
%% create a consumer for a Queue
-spec basic_consume/1 :: (Queue) -> ok when
      Queue :: binary().
-spec basic_consume/2 :: (Queue, Options) -> ok when
      Queue :: binary(),
      Options :: proplist().
basic_consume(Queue) ->
    basic_consume(Queue, []).

basic_consume(Queue, Options) ->
    BC = #'basic.consume'{
      queue = Queue
      ,consumer_tag = Queue
      ,no_local = props:get_value(no_local, Options, false)
      ,no_ack = props:get_value(no_ack, Options, true)
      ,exclusive = props:get_value(exclusive, Options, true)
      ,nowait = props:get_value(nowait, Options, false)
     },
    {C, Resp} = amqp_manager:consume(BC),
    link(C),
    Resp.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% This method cancels a consumer. This does not affect already delivered messages,
%% but it does mean the server will not send any more messages for that consumer.
%% @end
%%------------------------------------------------------------------------------
basic_cancel(Queue) ->
    BC = #'basic.cancel'{
      consumer_tag = Queue
     },
    amqp_manager:consume(BC).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Determines if the content is flaged as type JSON
%% @end
%%------------------------------------------------------------------------------
is_json(Props) ->
    Props#'P_basic'.content_type == <<"application/json">>.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% When sent by the client, this method acknowledges one or more messages
%% delivered via the Deliver or Get-Ok methods.
%% @end
%%------------------------------------------------------------------------------
basic_ack(DTag) ->
    amqp_manager:consume(#'basic.ack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% NOTE: THIS METHOD IS A RABBITMQ-SPECIFIC EXTENSION OF AMQP
%% Reject one or more incoming messages.
%% @end
%%------------------------------------------------------------------------------
basic_nack(DTag) ->
    amqp_manager:consume(#'basic.nack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the AMQP host is currently reachable
%% @end
%%------------------------------------------------------------------------------
-spec(is_host_available/0 :: () -> boolean()).
is_host_available() ->
    amqp_manager:is_available().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Specify quality of service
%% @end
%%------------------------------------------------------------------------------
-spec(basic_qos/1 :: (PreFetch :: non_neg_integer()) -> ok).
basic_qos(PreFetch) when is_integer(PreFetch) ->
    amqp_manager:consume(#'basic.qos'{prefetch_count = PreFetch}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Register to recieve notifications if ANY messages published with either with
%% the immediate or mandatory flags is returned
%% @end
%%------------------------------------------------------------------------------
-spec(register_return_handler/0 :: () -> ok).
register_return_handler() ->
    amqp_manager:register_return_handler().
