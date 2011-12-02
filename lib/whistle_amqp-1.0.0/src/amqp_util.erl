-module(amqp_util).

-include("amqp_util.hrl").

-export([targeted_exchange/0, targeted_publish/2, targeted_publish/3]).

-export([whapps_exchange/0, whapps_publish/2, whapps_publish/3, whapps_publish/4]).
-export([bind_q_to_whapps/2, bind_q_to_whapps/3]).
-export([unbind_q_from_whapps/2]).

-export([callctl_exchange/0, callctl_publish/2, callctl_publish/3, callctl_publish/4]).
-export([callevt_exchange/0, callevt_publish/1, callevt_publish/3, callevt_publish/4]).
-export([resource_exchange/0, resource_publish/1, resource_publish/2, resource_publish/3]).
-export([originate_resource_publish/1, originate_resource_publish/2]).
-export([offnet_resource_publish/1, offnet_resource_publish/2]).
-export([callmgr_exchange/0, callmgr_publish/3]).
-export([configuration_exchange/0, configuration_publish/2, configuration_publish/3, document_change_publish/5, document_change_publish/6]).
-export([document_routing_key/0, document_routing_key/1, document_routing_key/2, document_routing_key/3, document_routing_key/4]).
-export([monitor_exchange/0, monitor_publish/3]).
-export([conference_exchange/0, conference_publish/2, conference_publish/3, conference_publish/4]).

-export([bind_q_to_targeted/1, bind_q_to_targeted/2, unbind_q_from_targeted/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2, unbind_q_from_callctl/1]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3, unbind_q_from_callevt/2, unbind_q_from_callevt/3]).
-export([bind_q_to_resource/1, bind_q_to_resource/2, unbind_q_from_resource/2]).
-export([bind_q_to_callmgr/2, unbind_q_from_callmgr/2]).
-export([bind_q_to_configuration/2, unbind_q_from_configuration/2]).
-export([bind_q_to_monitor/2]).
-export([bind_q_to_conference/2, bind_q_to_conference/3]).
-export([bind_q_to_exchange/3, bind_q_to_exchange/4]).

-export([new_targeted_queue/0, new_targeted_queue/1]).
-export([new_callctl_queue/1, delete_callctl_queue/1]).
-export([new_callevt_queue/1, delete_callevt_queue/1]).
-export([new_callmgr_queue/1, new_callmgr_queue/2, delete_callmgr_queue/1]).
-export([new_configuration_queue/1, new_configuration_queue/2, delete_configuration_queue/1]).
-export([new_resource_queue/0, new_resource_queue/1]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).
-export([new_conference_queue/1, new_conference_queue/2]).

-export([new_queue/0, new_queue/1, new_queue/2, basic_consume/1, basic_consume/2
	 ,basic_publish/3, basic_publish/4, basic_cancel/1, queue_delete/1, queue_delete/2
	 ,new_exchange/2, new_exchange/3]).

-export([access_request/0, access_request/1, basic_ack/1, basic_nack/1, basic_qos/1]).

-export([is_json/1, is_host_available/0, register_return_handler/0]).
-export([encode/1]).

-define(KEY_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $- orelse C =:= $~ orelse C =:= $_))).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Publish AMQP messages
%% @end
%%------------------------------------------------------------------------------
-spec targeted_publish/2 :: (Queue, Payload) -> 'ok' when
      Queue :: ne_binary(),
      Payload :: iolist().
-spec targeted_publish/3 :: (Queue, Payload, ContentType) -> 'ok' when
      Queue :: ne_binary(),
      Payload :: iolist(),
      ContentType :: ne_binary().
targeted_publish(Queue, Payload) ->
    targeted_publish(Queue, Payload, ?DEFAULT_CONTENT_TYPE).
targeted_publish(Queue, Payload, ContentType) ->
    basic_publish(?EXCHANGE_TARGETED, Queue, Payload, ContentType).

-spec whapps_publish/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec whapps_publish/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
-spec whapps_publish/4 :: (ne_binary(), iolist(), ne_binary(), proplist()) -> 'ok'.
whapps_publish(Routing, Payload) ->
    whapps_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).
whapps_publish(Routing, Payload, ContentType) ->
    whapps_publish(Routing, Payload, ContentType, []).
whapps_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_WHAPPS, Routing, Payload, ContentType, Opts).

-spec callmgr_publish/3 :: (Payload, ContentType, RoutingKey) -> 'ok' when
      Payload :: iolist(),
      ContentType :: ne_binary(),
      RoutingKey :: ne_binary().
%% TODO: The routing key on this function should be the first argument for consistency
callmgr_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).


-spec configuration_publish/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec configuration_publish/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
configuration_publish(RoutingKey, Payload) ->
    configuration_publish(RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).
configuration_publish(RoutingKey, Payload, ContentType) ->
    basic_publish(?EXCHANGE_CONFIGURATION, RoutingKey, Payload, ContentType).

-spec document_change_publish/5 :: (Action, Db, Type, Id, Payload) -> 'ok' when
      Action :: atom(), %% edited | created | deleted
      Db :: ne_binary(),
      Type :: ne_binary(),
      Id :: ne_binary(),
      Payload :: iolist().
document_change_publish(Action, Db, Type, Id, JSON) ->
    document_change_publish(Action, Db, Type, Id, JSON, ?DEFAULT_CONTENT_TYPE).
document_change_publish(Action, Db, Type, Id, Payload, ContentType) ->
    RoutingKey = document_routing_key(Action, Db, Type, Id),
    configuration_publish(RoutingKey, Payload, ContentType).

-spec document_routing_key/0 :: () -> ne_binary().
-spec document_routing_key/1 :: (atom() | ne_binary()) -> ne_binary().
-spec document_routing_key/2 :: (atom() | ne_binary(), ne_binary()) -> ne_binary().
-spec document_routing_key/3 :: (atom() | ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
-spec document_routing_key/4 :: (atom() | ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
document_routing_key() ->
    document_routing_key(<<"*">>).
document_routing_key(Action) ->
    document_routing_key(Action, <<"*">>).
document_routing_key(Action, Db) ->
    document_routing_key(Action, Db, <<"*">>).
document_routing_key(Action, Db, Type) ->
    document_routing_key(Action, Db, Type, <<"*">>).

document_routing_key(<<"*">>, Db, Type, Id) ->
    list_to_binary([<<"*.">>, Db, ".", Type, ".", Id]);
document_routing_key(Action, Db, Type, Id) ->
    list_to_binary(["doc_", wh_util:to_list(Action), ".", Db, ".", Type, ".", Id]).

-spec callctl_publish/2 :: (CtrlQ, Payload) -> 'ok' when
      CtrlQ :: ne_binary(),
      Payload :: iolist().
-spec callctl_publish/3 :: (CtrlQ, Payload, ContentType) -> 'ok' when
      CtrlQ :: ne_binary(),
      Payload :: iolist(),
      ContentType :: ne_binary().
-spec callctl_publish/4 :: (CtrlQ, Payload, ContentType, Props) -> 'ok' when
      CtrlQ :: ne_binary(),
      Payload :: iolist(),
      ContentType :: ne_binary(),
      Props :: proplist().
callctl_publish(CtrlQ, Payload) ->
    callctl_publish(CtrlQ, Payload, ?DEFAULT_CONTENT_TYPE).
callctl_publish(CtrlQ, Payload, ContentType) ->
    callctl_publish(CtrlQ, Payload, ContentType, []).
callctl_publish(CtrlQ, Payload, ContentType, Props) ->
    basic_publish(?EXCHANGE_CALLCTL, CtrlQ, Payload, ContentType, Props).

-spec callevt_publish/1 :: (Payload) -> 'ok' when
      Payload :: iolist().
-spec callevt_publish/3 :: (CallID, Payload, Type) -> 'ok' when
      CallID :: ne_binary() | iolist(),
      Payload :: iolist() | ne_binary(),
      Type :: 'media_req' | 'event' | 'status_req' | 'cdr' | ne_binary().
-spec callevt_publish/4 :: (ne_binary(), iolist(), Type, ne_binary()) -> 'ok' when
      Type :: 'status_req' | 'event'.
callevt_publish(Payload) ->
    callevt_publish(Payload, ?DEFAULT_CONTENT_TYPE, media_req).

callevt_publish(Payload, ContentType, media_req) ->
    basic_publish(?EXCHANGE_CALLEVT, ?KEY_CALL_MEDIA_REQ, Payload, ContentType);

callevt_publish(CallID, Payload, event) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(CallID, Payload, status_req) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(CallID, Payload, cdr) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(_CallID, Payload, RoutingKey) when is_binary(RoutingKey) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).

callevt_publish(CallID, Payload, status_req, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, Payload, ContentType);
callevt_publish(CallID, Payload, event, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, Payload, ContentType);
callevt_publish(CallID, Payload, cdr, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, Payload, ContentType).

-spec resource_publish/1 :: (Payload) -> 'ok' when
      Payload :: iolist().
-spec resource_publish/2 :: (Payload, ContentType) -> 'ok' when
      Payload :: iolist(),
      ContentType :: ne_binary().
-spec resource_publish/3 :: (Payload, RoutingKey, ContentType) -> 'ok' when
      Payload :: iolist(),
      RoutingKey :: ne_binary(),
      ContentType :: ne_binary().
resource_publish(Payload) ->
    resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
resource_publish(Payload, ContentType) ->
    resource_publish(Payload, ?KEY_RESOURCE_REQ, ContentType).
resource_publish(Payload, RoutingKey, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, RoutingKey, Payload, ContentType).

-spec originate_resource_publish/1 :: (Payload) -> 'ok' when
      Payload :: iolist().
-spec originate_resource_publish/2 :: (Payload, ContentType) -> 'ok' when
      Payload :: iolist(),
      ContentType :: ne_binary().
originate_resource_publish(Payload) ->
    originate_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
originate_resource_publish(Payload, ContentType) ->
   basic_publish(?EXCHANGE_RESOURCE, ?KEY_ORGN_RESOURCE_REQ, Payload, ContentType).

-spec offnet_resource_publish/1 :: (Payload) -> 'ok' when
      Payload :: iolist().
-spec offnet_resource_publish/2 :: (Payload, ContentType) -> 'ok' when
      Payload :: iolist(),
      ContentType :: ne_binary().
offnet_resource_publish(Payload) ->
    offnet_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
offnet_resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_OFFNET_RESOURCE_REQ, Payload, ContentType).

%% monitor
-spec monitor_publish/3 :: (Payload, ContentType, RoutingKey) -> 'ok' when
      Payload :: iolist(),
      ContentType :: ne_binary(),
      RoutingKey :: ne_binary().
monitor_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

-spec conference_publish/2 :: (Payload, Queue) -> 'ok' when
      Payload :: iolist(),
      Queue :: 'discovery' | 'events' | 'service'.
-spec conference_publish/3 :: (Payload, Queue, ConfId) -> 'ok' when
      Payload :: iolist(),
      Queue :: 'events' | 'service',
      ConfId :: ne_binary().
-spec conference_publish/4 :: (Payload, Queue, ConfId, Options) -> 'ok' when
      Payload :: iolist(),
      Queue :: 'discovery' | 'events' | 'service',
      ConfId :: 'undefined' | ne_binary(),
      Options :: proplist().
conference_publish(Payload, discovery) ->
    conference_publish(Payload, discovery, undefined, []);
conference_publish(Payload, events) ->
    conference_publish(Payload, events, <<"*">>);
conference_publish(Payload, service) ->
    conference_publish(Payload, service, <<"*">>).

conference_publish(Payload, events, ConfId) ->
    conference_publish(Payload, events, ConfId, []);
conference_publish(Payload, service, ConfId) ->
    conference_publish(Payload, events, ConfId, []).

conference_publish(Payload, discovery, _, Options) ->
    basic_publish(?EXCHANGE_CONFERENCE, ?KEY_CONF_DISCOVERY_REQ, Payload, ?DEFAULT_CONTENT_TYPE, Options);
conference_publish(Payload, events, ConfId, Options) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONF_EVENTS/binary, ConfId/binary>>, Payload, ?DEFAULT_CONTENT_TYPE, Options);
conference_publish(Payload, service, ConfId, Options) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONF_SERVICE_REQ/binary, ConfId/binary>>, Payload, ?DEFAULT_CONTENT_TYPE, Options).

%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue

-spec basic_publish/3 :: (Exchange, Queue, Payload) -> 'ok' when
      Exchange :: ne_binary(),
      Queue :: ne_binary(),
      Payload :: iolist().
-spec basic_publish/4 :: (Exchange, Queue, Payload, ContentType) -> 'ok' when
      Exchange :: ne_binary(),
      Queue :: ne_binary(),
      Payload :: iolist(),
      ContentType :: ne_binary().
-spec basic_publish/5 :: (Exchange, Queue, Payload, ContentType, Prop) -> 'ok' when
      Exchange :: ne_binary(),
      Queue :: ne_binary(),
      Payload :: iolist() | ne_binary(),
      ContentType :: ne_binary(),
      Prop :: proplist().

basic_publish(_Exchange, Queue, _Payload) when not is_binary(Queue) ->
    {error, invalid_queue_name};
basic_publish(Exchange, Queue, Payload) ->
    basic_publish(Exchange, Queue, Payload, ?DEFAULT_CONTENT_TYPE).
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

    ?AMQP_DEBUG andalso ?LOG("publish ~s ~s (~p): ~s", [Exchange, Queue, Prop, Payload]),

    amqp_mgr:publish(BP, AM).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Create AMQP exchanges
%% @end
%%------------------------------------------------------------------------------
-spec targeted_exchange/0 :: () -> 'ok'.
targeted_exchange() ->
    new_exchange(?EXCHANGE_TARGETED, ?TYPE_TARGETED).

-spec whapps_exchange/0 :: () -> 'ok'.
whapps_exchange() ->
    new_exchange(?EXCHANGE_WHAPPS, ?TYPE_WHAPPS).

-spec callctl_exchange/0 :: () -> 'ok'.
callctl_exchange() ->
    new_exchange(?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

-spec callevt_exchange/0 :: () -> 'ok'.
callevt_exchange() ->
    new_exchange(?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

-spec resource_exchange/0 :: () -> 'ok'.
resource_exchange() ->
    new_exchange(?EXCHANGE_RESOURCE, ?TYPE_RESOURCE).

-spec callmgr_exchange/0 :: () -> 'ok'.
callmgr_exchange() ->
    new_exchange(?EXCHANGE_CALLMGR, ?TYPE_CALLMGR).

-spec configuration_exchange/0 :: () -> 'ok'.
configuration_exchange() ->
    new_exchange(?EXCHANGE_CONFIGURATION, ?TYPE_CONFIGURATION).

-spec monitor_exchange/0 :: () -> 'ok'.
monitor_exchange() ->
    new_exchange(?EXCHANGE_MONITOR, ?TYPE_MONITOR).

-spec conference_exchange/0 :: () -> 'ok'.
conference_exchange() ->
    new_exchange(?EXCHANGE_CONFERENCE, ?TYPE_CONFERENCE).

%% A generic Exchange maker
-spec new_exchange/2 :: (Exchange, Type) -> 'ok' when
      Exchange :: ne_binary(),
      Type :: ne_binary().
-spec new_exchange/3 :: (Exchange, Type, Options) -> 'ok' when
      Exchange :: ne_binary(),
      Type :: ne_binary(),
      Options :: proplist().
new_exchange(Exchange, Type) ->
    new_exchange(Exchange, Type, []).
new_exchange(Exchange, Type, _Options) ->
    ED = #'exchange.declare'{
      exchange = Exchange
      ,type = Type
     },
    ?AMQP_DEBUG andalso ?LOG("create new ~s exchange: ~s", [Type, Exchange]),
    amqp_mgr:misc_req(ED).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Create AMQP queues
%% @end
%%------------------------------------------------------------------------------
-spec new_targeted_queue/0 :: () -> binary() | {'error', 'amqp_error'}.
-spec new_targeted_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary().
new_targeted_queue() ->
    new_targeted_queue(<<>>).

new_targeted_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_callevt_queue/1 :: (CallID) -> binary() | {'error', 'amqp_error'} when
      CallID :: binary().
new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", encode(CallID)])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_callctl_queue/1 :: (CallID) -> binary() | {'error', 'amqp_error'} when
      CallID :: binary().
new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", encode(CallID)])
	      ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_resource_queue/0 :: () -> binary() | {'error', 'amqp_error'}.
-spec new_resource_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary().
new_resource_queue() ->
    new_resource_queue(?RESOURCE_QUEUE_NAME).
new_resource_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_callmgr_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary().
-spec new_callmgr_queue/2 :: (Queue, Options) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary(),
      Options :: proplist().
new_callmgr_queue(Queue) ->
    new_callmgr_queue(Queue, []).
new_callmgr_queue(Queue, Opts) ->
    new_queue(Queue, Opts).

-spec new_configuration_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary().
-spec new_configuration_queue/2 :: (Queue, Options) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary(),
      Options :: proplist().
new_configuration_queue(Queue) ->
    new_configuration_queue(Queue, []).
new_configuration_queue(Queue, Options) ->
    new_queue(Queue, Options).

-spec new_monitor_queue/0 :: () -> binary() | {'error', 'amqp_error'}.
-spec new_monitor_queue/1 :: (Queue :: binary()) -> binary() | {'error', 'amqp_error'}.
new_monitor_queue() ->
    new_monitor_queue(<<>>).
new_monitor_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}]).

-spec new_conference_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: discovery | binary().
-spec new_conference_queue/2 :: (Queue, Options) -> binary() | {'error', 'amqp_error'} when
      Queue :: discovery | binary(),
      Options :: proplist().
new_conference_queue(discovery) ->
    new_queue(?CONF_DISCOVERY_QUEUE_NAME, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_conference_queue(ConfId) ->
    new_queue(<<?KEY_CONF_SERVICE_REQ/binary, ConfId/binary>>, [{exclusive, true}, {auto_delete, true}, {nowait, false}]).

new_conference_queue(discovery, Options) ->
    new_queue(?CONF_DISCOVERY_QUEUE_NAME, Options);
new_conference_queue(ConfId, Options) ->
    new_queue(<<?KEY_CONF_SERVICE_REQ/binary, ConfId/binary>>, Options).

%% Declare a queue and returns the queue Name
-spec new_queue/0 :: () -> binary() | {'error', 'amqp_error'}.
-spec new_queue/1 :: (Queue) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary().
-spec new_queue/2 :: (Queue, Options) -> binary() | {'error', 'amqp_error'} when
      Queue :: binary(),
      Options :: proplist().
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
    case amqp_mgr:consume(QD) of
	'ok' ->
            ?AMQP_DEBUG andalso ?LOG("create queue(~p) ~s)", [Options, Queue]),
            Queue;
	#'queue.declare_ok'{queue=Q} ->
            ?AMQP_DEBUG andalso ?LOG("create queue(~p) ~s", [Options, Q]),
            Q;
	_Other ->
            ?AMQP_DEBUG andalso ?LOG("error creating queue(~p): ~p", [Options, _Other]),
	    {'error', 'amqp_error'}
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
    queue_delete(list_to_binary([?EXCHANGE_CALLEVT, ".", encode(CallID)]), Prop).

delete_callctl_queue(CallID) ->
    delete_callctl_queue(CallID, []).
delete_callctl_queue(CallID, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLCTL, ".", encode(CallID)]), Prop).

delete_callmgr_queue(Queue) ->
    queue_delete(Queue, []).

delete_configuration_queue(Queue) ->
    queue_delete(Queue, []).

delete_monitor_queue(Queue) ->
    queue_delete(Queue, []).

queue_delete(Queue) ->
    queue_delete(Queue, []).

queue_delete(Queue, _Prop) when not is_binary(Queue) ->
    {error, invalid_queue_name};
queue_delete(Queue, Prop) ->
    QD = #'queue.delete'{
      queue=Queue
      ,if_unused=props:get_value(if_unused, Prop, false)
      ,if_empty = props:get_value(if_empty, Prop, false)
      ,nowait = props:get_value(nowait, Prop, true)
     },
    ?AMQP_DEBUG andalso ?LOG("delete queue ~s", [Queue]),
    amqp_mgr:consume(QD).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
-spec bind_q_to_targeted/1 :: (Queue) -> 'ok' | {'error', term()} when
      Queue :: binary().
bind_q_to_targeted(Queue) ->
    bind_q_to_exchange(Queue, Queue, ?EXCHANGE_TARGETED).
bind_q_to_targeted(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TARGETED).

-spec bind_q_to_whapps/2 :: (ne_binary(), ne_binary()) -> 'ok' | {'error', term()}.
-spec bind_q_to_whapps/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok' | {'error', term()}.
bind_q_to_whapps(Queue, Routing) ->
    bind_q_to_whapps(Queue, Routing, []).
bind_q_to_whapps(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_WHAPPS, Options).

-spec bind_q_to_callctl/1 :: (Queue) -> 'ok' | {'error', term()} when
      Queue :: binary().
-spec bind_q_to_callctl/2 :: (Queue, Routing) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: binary().
bind_q_to_callctl(Queue) ->
    bind_q_to_callctl(Queue, Queue).
bind_q_to_callctl(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLCTL).

%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallID
-spec bind_q_to_callevt/2 :: (Queue, CallID) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      CallID :: media_req | binary().
-spec bind_q_to_callevt/3 :: (Queue, CallID, Type) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      CallID :: binary(),
      Type :: events | status_req | cdr | other.
bind_q_to_callevt(Queue, media_req) ->
    bind_q_to_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID) ->
    bind_q_to_callevt(Queue, CallID, events).

bind_q_to_callevt(Queue, CallID, events) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, status_req) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, cdr) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, Routing, other) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

-spec bind_q_to_resource/1 :: (Queue) -> 'ok' | {'error', term()} when
      Queue :: binary().
-spec bind_q_to_resource/2 :: (Queue, Routing) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: binary().
bind_q_to_resource(Queue) ->
    bind_q_to_resource(Queue, <<"#">>).
bind_q_to_resource(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

-spec bind_q_to_callmgr/2 :: (Queue, Routing) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: binary().
bind_q_to_callmgr(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

-spec bind_q_to_configuration/2 :: (Queue, Routing) -> ok | {error, term()} when
      Queue :: binary(),
      Routing :: binary().
bind_q_to_configuration(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CONFIGURATION).

-spec bind_q_to_monitor/2 :: (Queue, Routing) -> ok | {error, term()} when
      Queue :: binary(),
      Routing :: binary().
bind_q_to_monitor(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_MONITOR).


-spec bind_q_to_conference/2 :: (Queue, Routing) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: discovery | service | events.
-spec bind_q_to_conference/3 :: (Queue, Routing, ConfId) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: discovery | service | events,
      ConfId :: undefined | binary().


bind_q_to_conference(Queue, discovery) ->
    bind_q_to_conference(Queue, discovery, undefined);
bind_q_to_conference(Queue, service) ->
    bind_q_to_conference(Queue, service, <<"*">>);
bind_q_to_conference(Queue, events) ->
    bind_q_to_conference(Queue, events, <<"*">>).

bind_q_to_conference(Queue, discovery, _) ->
    bind_q_to_exchange(Queue, ?KEY_CONF_DISCOVERY_REQ, ?EXCHANGE_CONFERENCE, [{nowait, false}]);
bind_q_to_conference(Queue, service, ConfId) ->
    bind_q_to_exchange(Queue, <<?KEY_CONF_SERVICE_REQ/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE, [{nowait, false}]);
bind_q_to_conference(Queue, events, ConfId) ->
    bind_q_to_exchange(Queue, <<?KEY_CONF_EVENTS/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE, [{nowait, false}]).

-spec bind_q_to_exchange/3 :: (Queue, Routing, Exchange) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: binary(),
      Exchange :: binary().
-spec bind_q_to_exchange/4 :: (Queue, Routing, Exchange, Options) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      Routing :: binary(),
      Exchange :: binary(),
      Options :: proplist().
bind_q_to_exchange(Queue, _Routing, _Exchange) when not is_binary(Queue) ->
    {error, invalid_queue_name};
bind_q_to_exchange(Queue, Routing, Exchange) ->
    bind_q_to_exchange(Queue, Routing, Exchange, []).
bind_q_to_exchange(Queue, Routing, Exchange, Options) ->
    QB = #'queue.bind'{
      queue = Queue %% what queue does the binding attach to?
      ,exchange = Exchange %% what exchange does the binding attach to?
      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
      ,nowait = props:get_value(nowait, Options, true)
      ,arguments = []
     },
    case amqp_mgr:consume(QB) of
        {'queue.bind_ok'} ->
            ?AMQP_DEBUG andalso ?LOG("bound queue ~s to ~s with key ~s", [Queue, Exchange, Routing]),
            ok;
        ok ->
            ?AMQP_DEBUG andalso ?LOG("bound queue ~s to ~s with key ~s", [Queue, Exchange, Routing]),
            ok;
        Else ->
            ?AMQP_DEBUG andalso ?LOG("failed to bind queue ~s: ~p", [Queue, Else]),
            Else
    end.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Unbind a Queue from an Exchange
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q_from_callevt/2 :: (Queue, CallID) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      CallID :: media_req | binary().
-spec unbind_q_from_callevt/3 :: (Queue, CallID, Type) -> 'ok' | {'error', term()} when
      Queue :: binary(),
      CallID :: binary(),
      Type :: events | status_req | cdr | other.
unbind_q_from_callevt(Queue, media_req) ->
    unbind_q_from_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID) ->
    unbind_q_from_callevt(Queue, CallID, events).

unbind_q_from_callevt(Queue, CallID, events) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, status_req) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, cdr) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, Routing, other) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

unbind_q_from_callctl(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_CALLCTL).

unbind_q_from_resource(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

unbind_q_from_callmgr(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

unbind_q_from_configuration(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CONFIGURATION).

unbind_q_from_targeted(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_TARGETED).

unbind_q_from_whapps(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_WHAPPS).

unbind_q_from_exchange(Queue, _Routing, _Exchange) when not is_binary(Queue) ->
    {error, invalid_queue_name};
unbind_q_from_exchange(Queue, Routing, Exchange) ->
    QU = #'queue.unbind'{
      queue = Queue
      ,exchange = Exchange
      ,routing_key = Routing
      ,arguments = []
     },
    ?AMQP_DEBUG andalso ?LOG("unbound queue ~s to ~s with key ~s", [Queue, Exchange, Routing]),
    amqp_mgr:consume(QU).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
%% create a consumer for a Queue
-spec basic_consume/1 :: (Queue) -> 'ok' when
      Queue :: binary().
-spec basic_consume/2 :: (Queue, Options) -> 'ok' when
      Queue :: binary(),
      Options :: proplist().
basic_consume(Queue) ->
    basic_consume(Queue, []).

basic_consume(Queue, _Options) when not is_binary(Queue) ->
    {error, invalid_queue_name};
basic_consume(Queue, Options) ->
    BC = #'basic.consume'{
      queue = Queue
      ,consumer_tag = Queue
      ,no_local = props:get_value(no_local, Options, false)
      ,no_ack = props:get_value(no_ack, Options, true)
      ,exclusive = props:get_value(exclusive, Options, true)
      ,nowait = props:get_value(nowait, Options, false)
     },
    case amqp_mgr:consume(BC) of
        {_Pid, {'basic.consume_ok', _}} ->
            %% link(C),
            ?AMQP_DEBUG andalso ?LOG("started consume of queue(~p) ~s", [Options, Queue]),
            ok;
        {_, Error} ->
            ?AMQP_DEBUG andalso ?LOG("failed to start consume of queue(~p) ~s: ~p", [Options, Queue, Error]),
            Error;
        Else ->
            ?AMQP_DEBUG andalso ?LOG("failed to start consume of queue(~p) ~s: ~p", [Options, Queue, Else]),
            Else
    end.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% This method cancels a consumer. This does not affect already delivered messages,
%% but it does mean the server will not send any more messages for that consumer.
%% @end
%%------------------------------------------------------------------------------
basic_cancel(Queue) ->
    ?AMQP_DEBUG andalso ?LOG("cancel consume for queue ~s", [Queue]),
    amqp_mgr:consume(#'basic.cancel'{consumer_tag = Queue, nowait = false}).

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
is_json(#'P_basic'{content_type=CT}) ->
    CT =:= ?DEFAULT_CONTENT_TYPE.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% When sent by the client, this method acknowledges one or more messages
%% delivered via the Deliver or Get-'Ok' methods.
%% @end
%%------------------------------------------------------------------------------
basic_ack(DTag) ->
    ?AMQP_DEBUG andalso ?LOG("basic ack of ~s", [DTag]),
    amqp_mgr:consume(#'basic.ack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% NOTE: THIS METHOD IS A RABBITMQ-SPECIFIC EXTENSION OF AMQP
%% Reject one or more incoming messages.
%% @end
%%------------------------------------------------------------------------------
basic_nack(DTag) ->
    ?AMQP_DEBUG andalso ?LOG("basic nack of ~s", [DTag]),
    amqp_mgr:consume(#'basic.nack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the AMQP host is currently reachable
%% @end
%%------------------------------------------------------------------------------
-spec is_host_available/0 :: () -> boolean().
is_host_available() ->
    amqp_mgr:is_available().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Specify quality of service
%% @end
%%------------------------------------------------------------------------------
-spec basic_qos/1 :: (non_neg_integer()) -> 'ok'.
basic_qos(PreFetch) when is_integer(PreFetch) ->
    ?AMQP_DEBUG andalso ?LOG("set basic qos prefetch to ~p", [PreFetch]),
    amqp_mgr:consume(#'basic.qos'{prefetch_count = PreFetch}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Register to recieve notifications if ANY messages published with either with
%% the immediate or mandatory flags is returned
%% @end
%%------------------------------------------------------------------------------
-spec register_return_handler/0 :: () -> 'ok'.
register_return_handler() ->
    ?AMQP_DEBUG andalso ?LOG("registering retunr handler", []),
    amqp_mgr:register_return_handler().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Encode a key so characters like dot won't interfere with routing separator
%% @end
%%------------------------------------------------------------------------------
-spec encode/1 :: (ne_binary()) -> ne_binary().
encode(<<"*">>) ->
    <<"*">>;
encode(<<"#">>) ->
    <<"#">>;
encode(Binary) ->
    do_encode(Binary, <<>>).

-spec do_encode/2 :: (binary(), binary()) -> ne_binary().
do_encode(<<>>, Acc) ->
    Acc;
do_encode(<<C:8, Rest/binary>>, Acc) when ?KEY_SAFE(C) ->
    do_encode(Rest, <<Acc/binary, (<<C>>)/binary>>);
do_encode(<<$\s, Rest/binary>>, Acc) ->
    do_encode(Rest, <<Acc/binary, $+>>);
do_encode(<<$., Rest/binary>>, Acc) ->
    do_encode(Rest, <<Acc/binary, "%2E">>);
do_encode(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    do_encode(Rest, <<Acc/binary, $%, (hexdigit(Hi))/binary, (hexdigit(Lo))/binary>>).

-spec hexdigit/1 :: (byte()) -> binary().
hexdigit(C) when C < 10 ->
    <<($0 + C)>>;
hexdigit(C) when C < 16 ->
    <<($A + (C - 10))>>.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
encode_key_test() ->
    ?assertEqual(<<"key">>, encode(<<"key">>)),
    ?assertEqual(<<"routing%2Ekey">>, encode(<<"routing.key">>)),
    ?assertEqual(<<"long%2Erouting%2Ekey">>, encode(<<"long.routing.key">>)),
    ?assertEqual(<<"test%26%2E192%2E+168%2E+5%2E+5%23">>, encode(<<"test&.192. 168. 5. 5#">>)),
    ok.
-endif.
