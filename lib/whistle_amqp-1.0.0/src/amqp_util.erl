%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Utilities to facilitate AMQP interaction
%%% @end
%%% @contributions
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(amqp_util).

-include("amqp_util.hrl").

-export([targeted_exchange/0]).
-export([new_targeted_queue/0, new_targeted_queue/1]).
-export([delete_targeted_queue/1]).
-export([bind_q_to_targeted/1, bind_q_to_targeted/2]).
-export([unbind_q_from_targeted/1]).
-export([targeted_publish/2, targeted_publish/3]).

-export([whapps_exchange/0]).
-export([new_whapps_queue/0, new_whapps_queue/1]).
-export([delete_whapps_queue/1]).
-export([bind_q_to_whapps/2, bind_q_to_whapps/3]).
-export([unbind_q_from_whapps/2]).
-export([whapps_publish/2, whapps_publish/3, whapps_publish/4]).

-export([notifications_exchange/0]).
-export([new_notifications_queue/0, new_notifications_queue/1]).
-export([delete_notifications_queue/1]).
-export([bind_q_to_notifications/2, bind_q_to_notifications/3]).
-export([unbind_q_from_notifications/2]).
-export([notifications_publish/2, notifications_publish/3, notifications_publish/4]).

-export([sysconf_exchange/0]).
-export([new_sysconf_queue/0, new_sysconf_queue/1]).
-export([delete_sysconf_queue/1]).
-export([bind_q_to_sysconf/2, bind_q_to_sysconf/3]).
-export([unbind_q_from_sysconf/2]).
-export([sysconf_publish/2, sysconf_publish/3, sysconf_publish/4]).

-export([callctl_exchange/0]).
-export([new_callctl_queue/1]).
-export([delete_callctl_queue/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2]).
-export([unbind_q_from_callctl/1]).
-export([callctl_publish/2, callctl_publish/3, callctl_publish/4]).

-export([callevt_exchange/0]).
-export([new_callevt_queue/1]).
-export([delete_callevt_queue/1]).
-export([bind_q_to_callevt/2, bind_q_to_callevt/3]).
-export([unbind_q_from_callevt/2, unbind_q_from_callevt/3]).
-export([callevt_publish/1, callevt_publish/3, callevt_publish/4]).

-export([callmgr_exchange/0]).
-export([new_callmgr_queue/1, new_callmgr_queue/2]).
-export([delete_callmgr_queue/1]).
-export([bind_q_to_callmgr/2]).
-export([unbind_q_from_callmgr/2]).
-export([callmgr_publish/3, callmgr_publish/4]).

-export([resource_exchange/0]).
-export([new_resource_queue/0, new_resource_queue/1]).
-export([delete_resource_queue/1]).
-export([bind_q_to_resource/1, bind_q_to_resource/2]).
-export([unbind_q_from_resource/2]).
-export([resource_publish/1, resource_publish/2, resource_publish/3]).

-export([conference_exchange/0]).
-export([new_conference_queue/0, new_conference_queue/1]).
-export([delete_conference_queue/1]).
-export([bind_q_to_conference/2, bind_q_to_conference/3]).
-export([unbind_q_from_conference/2, unbind_q_from_conference/3]).
-export([conference_publish/2, conference_publish/3, conference_publish/4, conference_publish/5]).

-export([lync_exchange/0]).
-export([new_lync_queue/0, new_lync_queue/1]).
-export([delete_lync_queue/1]).
-export([bind_q_to_lync/2, bind_q_to_lync/3]).
-export([unbind_q_from_lync/2]).
-export([lync_publish/2, lync_publish/3, lync_publish/4]).

-export([originate_resource_publish/1, originate_resource_publish/2]).

-export([offnet_resource_publish/1, offnet_resource_publish/2]).

-export([configuration_exchange/0, configuration_publish/2, configuration_publish/3, document_change_publish/5, document_change_publish/6]).
-export([document_routing_key/0, document_routing_key/1, document_routing_key/2, document_routing_key/3, document_routing_key/4]).
-export([bind_q_to_configuration/2, unbind_q_from_configuration/2]).
-export([new_configuration_queue/1, new_configuration_queue/2, delete_configuration_queue/1]).

-export([monitor_exchange/0, monitor_publish/3]).
-export([bind_q_to_monitor/2]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).

-export([bind_q_to_exchange/3, bind_q_to_exchange/4]).
-export([new_queue/0, new_queue/1, new_queue/2]).
-export([basic_consume/1, basic_consume/2]).
-export([basic_publish/3, basic_publish/4]).
-export([basic_cancel/0]).
-export([queue_delete/1, queue_delete/2]).
-export([new_exchange/2, new_exchange/3]).

-export([access_request/0, access_request/1, basic_ack/1, basic_nack/1, basic_qos/1]).

-export([is_json/1, is_host_available/0, register_return_handler/0]).
-export([encode/1]).

-define(KEY_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $- orelse C =:= $~ orelse C =:= $_))).

-type amqp_payload() :: iolist() | ne_binary().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Publish AMQP messages
%% @end
%%------------------------------------------------------------------------------
-spec targeted_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec targeted_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
targeted_publish(Queue, Payload) ->
    targeted_publish(Queue, Payload, ?DEFAULT_CONTENT_TYPE).
targeted_publish(Queue, Payload, ContentType) ->
    basic_publish(?EXCHANGE_TARGETED, Queue, Payload, ContentType).

-spec whapps_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec whapps_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec whapps_publish/4 :: (ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
whapps_publish(Routing, Payload) ->
    whapps_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).
whapps_publish(Routing, Payload, ContentType) ->
    whapps_publish(Routing, Payload, ContentType, []).
whapps_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_WHAPPS, Routing, Payload, ContentType, Opts).

-spec notifications_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec notifications_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec notifications_publish/4 :: (ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
notifications_publish(Routing, Payload) ->
    notifications_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).
notifications_publish(Routing, Payload, ContentType) ->
    notifications_publish(Routing, Payload, ContentType, []).
notifications_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_NOTIFICATIONS, Routing, Payload, ContentType, Opts).

-spec sysconf_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec sysconf_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec sysconf_publish/4 :: (ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
sysconf_publish(Routing, Payload) ->
    sysconf_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).
sysconf_publish(Routing, Payload, ContentType) ->
    sysconf_publish(Routing, Payload, ContentType, []).
sysconf_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_SYSCONF, Routing, Payload, ContentType, Opts).

-spec lync_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec lync_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec lync_publish/4 :: (ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
lync_publish(Routing, Payload) ->
    lync_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).
lync_publish(Routing, Payload, ContentType) ->
    lync_publish(Routing, Payload, ContentType, []).
lync_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_LYNC, Routing, Payload, ContentType, Opts).


-spec callmgr_publish/3 :: (amqp_payload(), ne_binary(), ne_binary()) -> 'ok'.
-spec callmgr_publish/4 :: (amqp_payload(), ne_binary(), ne_binary(), proplist()) -> 'ok'.
%% TODO: The routing key on this function should be the first argument for consistency
callmgr_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).
callmgr_publish(Payload, ContentType, RoutingKey, Opts) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType, Opts).

-spec configuration_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec configuration_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
configuration_publish(RoutingKey, Payload) ->
    configuration_publish(RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).
configuration_publish(RoutingKey, Payload, ContentType) ->
    basic_publish(?EXCHANGE_CONFIGURATION, RoutingKey, Payload, ContentType).

-spec document_change_publish/5 :: (Action, Db, Type, Id, Payload) -> 'ok' when
      Action :: atom(), %% edited | created | deleted
      Db :: ne_binary(),
      Type :: ne_binary(),
      Id :: ne_binary(),
      Payload :: amqp_payload().
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

-spec callctl_publish/2 :: (ne_binary(), amqp_payload()) -> 'ok'.
-spec callctl_publish/3 :: (ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec callctl_publish/4 :: (ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
callctl_publish(CtrlQ, Payload) ->
    callctl_publish(CtrlQ, Payload, ?DEFAULT_CONTENT_TYPE).
callctl_publish(CtrlQ, Payload, ContentType) ->
    callctl_publish(CtrlQ, Payload, ContentType, []).
callctl_publish(CtrlQ, Payload, ContentType, Props) ->
    basic_publish(?EXCHANGE_CALLCTL, CtrlQ, Payload, ContentType, Props).

-spec callevt_publish/1 :: (amqp_payload()) -> 'ok'.
-spec callevt_publish/3 :: (amqp_payload(), amqp_payload(), Type) -> 'ok' when
      Type :: 'media_req' | 'event' | 'status_req' | 'cdr' | 'publisher_usurp' | ne_binary().
-spec callevt_publish/4 :: (ne_binary(), amqp_payload(), Type, ne_binary()) -> 'ok' when
      Type :: 'status_req' | 'event' | 'cdr' | 'publisher_usurp' | ne_binary().
callevt_publish(Payload) ->
    callevt_publish(Payload, ?DEFAULT_CONTENT_TYPE, media_req).

callevt_publish(Payload, ContentType, media_req) ->
    basic_publish(?EXCHANGE_CALLEVT, ?KEY_CALL_MEDIA_REQ, Payload, ContentType);

callevt_publish(CallID, Payload, event) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(CallID, Payload, status_req) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(CallID, Payload, publisher_usurp) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_PUBLISHER_USURP/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(CallID, Payload, cdr) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, Payload, ?DEFAULT_CONTENT_TYPE);

callevt_publish(_CallID, Payload, RoutingKey) when is_binary(RoutingKey) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).

callevt_publish(_CallID, Payload, RoutingKey, ContentType) when is_binary(RoutingKey) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, ContentType);
callevt_publish(CallID, Payload, status_req, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, Payload, ContentType);
callevt_publish(CallID, Payload, publisher_usurp, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_PUBLISHER_USURP/binary, (encode(CallID))/binary>>, Payload, ContentType);
callevt_publish(CallID, Payload, event, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, Payload, ContentType);
callevt_publish(CallID, Payload, cdr, ContentType) ->
    basic_publish(?EXCHANGE_CALLEVT, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, Payload, ContentType).

-spec resource_publish/1 :: (amqp_payload()) -> 'ok'.
-spec resource_publish/2 :: (amqp_payload(), ne_binary()) -> 'ok'.
-spec resource_publish/3 :: (amqp_payload(), ne_binary(), ne_binary()) -> 'ok'.
resource_publish(Payload) ->
    resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
resource_publish(Payload, ContentType) ->
    resource_publish(Payload, ?KEY_RESOURCE_REQ, ContentType).
resource_publish(Payload, RoutingKey, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, RoutingKey, Payload, ContentType).

-spec originate_resource_publish/1 :: (amqp_payload()) -> 'ok'.
-spec originate_resource_publish/2 :: (amqp_payload(), ne_binary()) -> 'ok'.
originate_resource_publish(Payload) ->
    originate_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
originate_resource_publish(Payload, ContentType) ->
   basic_publish(?EXCHANGE_RESOURCE, ?KEY_ORGN_RESOURCE_REQ, Payload, ContentType).

-spec offnet_resource_publish/1 :: (amqp_payload()) -> 'ok'.
-spec offnet_resource_publish/2 :: (amqp_payload(), ne_binary()) -> 'ok'.
offnet_resource_publish(Payload) ->
    offnet_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).
offnet_resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_OFFNET_RESOURCE_REQ, Payload, ContentType).

%% monitor
-spec monitor_publish/3 :: (amqp_payload(), ne_binary(), ne_binary()) -> 'ok'.
monitor_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

-type conf_routing_type() :: 'discovery' | 'event' | 'command'.
-spec conference_publish/2 :: (amqp_payload(), conf_routing_type()) -> 'ok'.
-spec conference_publish/3 :: (amqp_payload(), conf_routing_type(), undefined | ne_binary()) -> 'ok'.
-spec conference_publish/4 :: (amqp_payload(), conf_routing_type(), undefined | ne_binary(), proplist()) -> 'ok'.
-spec conference_publish/5 :: (amqp_payload(), conf_routing_type(), undefined | ne_binary(), proplist(), ne_binary()) -> 'ok'.

conference_publish(Payload, discovery) ->
    conference_publish(Payload, discovery, <<"*">>);
conference_publish(Payload, event) ->
    conference_publish(Payload, event, <<"*">>);
conference_publish(Payload, command) ->
    conference_publish(Payload, command, <<"*">>).

conference_publish(Payload, discovery, ConfId) ->
    conference_publish(Payload, discovery, ConfId, []);
conference_publish(Payload, event, ConfId) ->
    conference_publish(Payload, event, ConfId, []);
conference_publish(Payload, command, ConfId) ->
    conference_publish(Payload, command, ConfId, []).

conference_publish(Payload, discovery, ConfId, Options) ->
    conference_publish(Payload, discovery, ConfId, Options, ?DEFAULT_CONTENT_TYPE);
conference_publish(Payload, event, ConfId, Options) ->
    conference_publish(Payload, event, ConfId, Options, ?DEFAULT_CONTENT_TYPE);
conference_publish(Payload, command, ConfId, Options) ->
    conference_publish(Payload, command, ConfId, Options, ?DEFAULT_CONTENT_TYPE).

conference_publish(Payload, discovery, _, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, ?KEY_CONFERENCE_DISCOVERY, Payload, ContentType, Options);
conference_publish(Payload, event, ConfId, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary>>, Payload, ContentType, Options);
conference_publish(Payload, command, ConfId, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONFERENCE_COMMAND/binary, ConfId/binary>>, Payload, ContentType, Options).


%% generic publisher for an Exchange.Queue
%% Use <<"#">> for a default Queue

-spec basic_publish/3 :: (ne_binary(), ne_binary(), amqp_payload()) -> 'ok'.
-spec basic_publish/4 :: (ne_binary(), ne_binary(), amqp_payload(), ne_binary()) -> 'ok'.
-spec basic_publish/5 :: (ne_binary(), ne_binary(), amqp_payload(), ne_binary(), proplist()) -> 'ok'.
basic_publish(Exchange, Queue, Payload) ->
    basic_publish(Exchange, Queue, Payload, ?DEFAULT_CONTENT_TYPE).

basic_publish(Exchange, Queue, Payload, ContentType) ->
    basic_publish(Exchange, Queue, Payload, ContentType, []).

basic_publish(Exchange, Queue, Payload, ContentType, Prop) when is_list(Payload) ->
    basic_publish(Exchange, Queue, iolist_to_binary(Payload), ContentType, Prop);
basic_publish(Exchange, Queue, Payload, ContentType, Props) when is_binary(Payload),
                                                                is_binary(Exchange),
                                                                is_binary(Queue),
                                                                is_binary(ContentType),
                                                                is_list(Props) ->
    BP = #'basic.publish'{
      exchange = Exchange
      ,routing_key = Queue
      ,mandatory = props:get_value(mandatory, Props, false)
      ,immediate = props:get_value(immediate, Props, false)
     },

    %% Add the message to the publish, converting to binary
    %% See http://www.rabbitmq.com/amqp-0-9-1-reference.html#class.basic
    MsgProps = #'P_basic'{
      content_type = ContentType % MIME content type
      ,content_encoding = props:get_value(content_encoding, Props) % MIME encoding
      ,headers = props:get_value(headers, Props) % message headers
      ,delivery_mode = props:get_value(delivery_mode, Props) % non-persistent(1) or persistent(2)
      ,priority = props:get_value(priority, Props) % message priority, 0-9
      ,correlation_id = props:get_value(correlation_id, Props) % correlation identifier
      ,reply_to = props:get_value(reply_to, Props) % address to reply to
      ,expiration = wh_util:to_binary(props:get_value(expiration, Props)) % expires time
      ,message_id = props:get_value(message_id, Props) % app message id
      ,timestamp = props:get_value(timestamp, Props) % message timestamp
      ,type = props:get_value(type, Props) % message type
      ,user_id = props:get_value(user_id, Props) % creating user
      ,app_id = props:get_value(app_id, Props) % creating app
      ,cluster_id = props:get_value(cluster_id, Props) % cluster
     },

    AM = #'amqp_msg'{
      payload = Payload
      ,props = MsgProps
     },

    wh_amqp_mgr:publish(BP, AM).

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

-spec notifications_exchange/0 :: () -> 'ok'.
notifications_exchange() ->
    new_exchange(?EXCHANGE_NOTIFICATIONS, ?TYPE_NOTIFICATIONS).

-spec sysconf_exchange/0 :: () -> 'ok'.
sysconf_exchange() ->
    new_exchange(?EXCHANGE_SYSCONF, ?TYPE_SYSCONF).

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

-spec lync_exchange/0 :: () -> 'ok'.
lync_exchange() ->
    new_exchange(?EXCHANGE_LYNC, ?TYPE_LYNC).

%% A generic Exchange maker
-spec new_exchange/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec new_exchange/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok'.
new_exchange(Exchange, Type) ->
    new_exchange(Exchange, Type, []).
new_exchange(Exchange, Type, Options) ->
    ED = #'exchange.declare'{
      exchange = Exchange
      ,type = Type
      ,passive = props:get_value(passive, Options, false)
      ,durable = props:get_value(durable, Options, false)
      ,auto_delete = props:get_value(auto_delete, Options, false)
      ,internal = props:get_value(internal, Options, false)
      ,nowait = props:get_value(nowait, Options, false)
      ,arguments = props:get_value(arguments, Options, [])
     },
    wh_amqp_mgr:misc_req(ED).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Create AMQP queues
%% @end
%%------------------------------------------------------------------------------
-spec new_targeted_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_targeted_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_targeted_queue() ->
    new_targeted_queue(<<>>).

new_targeted_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_whapps_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_whapps_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_whapps_queue() ->
    new_whapps_queue(<<>>).

new_whapps_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_notifications_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_notifications_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_notifications_queue() ->
    new_notifications_queue(<<>>).

new_notifications_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_sysconf_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_sysconf_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_sysconf_queue() ->
    new_sysconf_queue(<<>>).

new_sysconf_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_lync_queue/0 :: () -> ne_binary() | {'error', 'amqp_error'}.
-spec new_lync_queue/1 :: (binary()) -> ne_binary() | {'error', 'amqp_error'}.
new_lync_queue() ->
    new_lync_queue(<<>>).

new_lync_queue(Queue) ->
    new_queue(Queue, [{nowait, false}]).

-spec new_callevt_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callevt_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", encode(CallID)])
              ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_callctl_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{exclusive, false}, {auto_delete, true}, {nowait, false}]);
new_callctl_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", encode(CallID)])
              ,[{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_resource_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_resource_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_resource_queue() ->
    new_resource_queue(?RESOURCE_QUEUE_NAME).
new_resource_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}, {nowait, false}]).

-spec new_callmgr_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
-spec new_callmgr_queue/2 :: (binary(), proplist()) -> ne_binary() | {'error', _}.
new_callmgr_queue(Queue) ->
    new_callmgr_queue(Queue, []).
new_callmgr_queue(Queue, Opts) ->
    new_queue(Queue, Opts).

-spec new_configuration_queue/1 :: (ne_binary()) -> ne_binary() | {'error', _}.
-spec new_configuration_queue/2 :: (ne_binary(), proplist()) -> ne_binary() | {'error', _}.
new_configuration_queue(Queue) ->
    new_configuration_queue(Queue, []).
new_configuration_queue(Queue, Options) ->
    new_queue(Queue, Options).

-spec new_monitor_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_monitor_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_monitor_queue() ->
    new_monitor_queue(<<>>).
new_monitor_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}]).

-spec new_conference_queue/0 :: () -> ne_binary() | {'error', _}.
-spec new_conference_queue/1 :: (binary()) -> ne_binary() | {'error', _}.
new_conference_queue() ->
    new_conference_queue(<<>>).
new_conference_queue(Queue) ->
    new_queue(Queue, [{exclusive, false}, {auto_delete, true}, {nowait, false}]).

%% Declare a queue and returns the queue Name
-type new_queue_ret() :: ne_binary() | integer() | 'undefined' |
                         {ne_binary(), integer(), integer()} |
                         {'error', _}.
-spec new_queue/0 :: () -> new_queue_ret().
-spec new_queue/1 :: (binary()) -> new_queue_ret().
-spec new_queue/2 :: (binary(), wh_proplist()) -> new_queue_ret().
new_queue() ->
    new_queue(<<>>). % lets the client lib create a random queue name
new_queue(Queue) ->
    new_queue(Queue, []).

new_queue(<<"amq.", _/binary>>, Options) ->
    new_queue(<<>>, Options);
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

    %% can be queue | message_count | consumer_count | all
    Return = props:get_value(return_field, Options, queue),

    case wh_amqp_mgr:consume(QD) of
        {ok, #'queue.declare_ok'{queue=Q}} when Return =:= queue -> Q;
        {ok, #'queue.declare_ok'{message_count=Cnt}} when Return =:= message_count -> Cnt;
        {ok, #'queue.declare_ok'{consumer_count=Cnt}} when Return =:= consumer_count -> Cnt;
        {ok, #'queue.declare_ok'{queue=Q
                                 ,message_count=MCnt
                                 ,consumer_count=CCnt
                                }} when Return =:= all -> {Q, MCnt, CCnt};
        {error, _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Delete AMQP queue
%% @end
       %%------------------------------------------------------------------------------
delete_targeted_queue(Queue) ->
    queue_delete(Queue, []).

delete_whapps_queue(Queue) ->
    queue_delete(Queue, []).

delete_notifications_queue(Queue) ->
    queue_delete(Queue, []).

delete_sysconf_queue(Queue) ->
    queue_delete(Queue, []).

delete_lync_queue(Queue) ->
    queue_delete(Queue, []).

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

delete_resource_queue(Queue) ->
    queue_delete(Queue, []).

delete_configuration_queue(Queue) ->
    queue_delete(Queue, []).

delete_conference_queue(Queue) ->
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
    ok = wh_amqp_mgr:consume(QD).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
-spec bind_q_to_targeted/1 :: (ne_binary()) -> 'ok'.
bind_q_to_targeted(Queue) ->
    bind_q_to_exchange(Queue, Queue, ?EXCHANGE_TARGETED).
bind_q_to_targeted(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TARGETED).

-spec bind_q_to_whapps/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec bind_q_to_whapps/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok'.
bind_q_to_whapps(Queue, Routing) ->
    bind_q_to_whapps(Queue, Routing, []).
bind_q_to_whapps(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_WHAPPS, Options).

-spec bind_q_to_notifications/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec bind_q_to_notifications/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok'.
bind_q_to_notifications(Queue, Routing) ->
    bind_q_to_notifications(Queue, Routing, []).
bind_q_to_notifications(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_NOTIFICATIONS, Options).

-spec bind_q_to_sysconf/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec bind_q_to_sysconf/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok'.
bind_q_to_sysconf(Queue, Routing) ->
    bind_q_to_sysconf(Queue, Routing, []).
bind_q_to_sysconf(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_SYSCONF, Options).

-spec bind_q_to_lync/2 :: (ne_binary(), ne_binary()) -> 'ok' | {'error', atom()}.
-spec bind_q_to_lync/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok' | {'error', atom()}.
bind_q_to_lync(Queue, Routing) ->
    bind_q_to_lync(Queue, Routing, []).
bind_q_to_lync(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_LYNC, Options).

-spec bind_q_to_callctl/1 :: (ne_binary()) -> 'ok'.
-spec bind_q_to_callctl/2 :: (ne_binary(), ne_binary()) -> 'ok'.
bind_q_to_callctl(Queue) ->
    bind_q_to_callctl(Queue, Queue).
bind_q_to_callctl(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLCTL).

%% to receive all call events or cdrs, regardless of callid, pass <<"*">> for CallID
-spec bind_q_to_callevt/2 :: (ne_binary(), ne_binary() | 'media_req') -> 'ok'.
-spec bind_q_to_callevt/3 :: (ne_binary(), ne_binary(), Type) -> 'ok' when
      Type :: 'events' | 'status_req' | 'cdr' | 'publisher_usurp' | 'other'.
bind_q_to_callevt(Queue, media_req) ->
    bind_q_to_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID) ->
    bind_q_to_callevt(Queue, CallID, events).

bind_q_to_callevt(Queue, CallID, events) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, status_req) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, publisher_usurp) ->
    bind_q_to_exchange(Queue, <<?KEY_PUBLISHER_USURP/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, CallID, cdr) ->
    bind_q_to_exchange(Queue, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
bind_q_to_callevt(Queue, Routing, other) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

-spec bind_q_to_resource/1 :: (ne_binary()) -> 'ok'.
-spec bind_q_to_resource/2 :: (ne_binary(), ne_binary()) -> 'ok'.
bind_q_to_resource(Queue) ->
    bind_q_to_resource(Queue, <<"#">>).
bind_q_to_resource(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

-spec bind_q_to_callmgr/2 :: (ne_binary(), ne_binary()) -> 'ok'.
bind_q_to_callmgr(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

-spec bind_q_to_configuration/2 :: (ne_binary(), ne_binary()) -> 'ok'.
bind_q_to_configuration(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CONFIGURATION).

-spec bind_q_to_monitor/2 :: (ne_binary(), ne_binary()) -> 'ok'.
bind_q_to_monitor(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_MONITOR).

-spec bind_q_to_conference/2 :: (ne_binary(), conf_routing_type()) -> 'ok'.
-spec bind_q_to_conference/3 :: (ne_binary(), conf_routing_type(), 'undefined' | ne_binary()) -> 'ok'.

bind_q_to_conference(Queue, discovery) ->
    bind_q_to_conference(Queue, discovery, undefined);
bind_q_to_conference(Queue, command) ->
    bind_q_to_conference(Queue, command, <<"*">>);
bind_q_to_conference(Queue, event) ->
    bind_q_to_conference(Queue, event, <<"*">>).

bind_q_to_conference(Queue, discovery, _) ->
    bind_q_to_exchange(Queue, ?KEY_CONFERENCE_DISCOVERY, ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, event, ConfId) ->
    bind_q_to_exchange(Queue, <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, command, ConfId) ->
    bind_q_to_exchange(Queue, <<?KEY_CONFERENCE_COMMAND/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE).

-spec bind_q_to_exchange/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec bind_q_to_exchange/4 :: (ne_binary(), ne_binary(), ne_binary(), proplist()) -> 'ok'.
bind_q_to_exchange(Queue, _Routing, _Exchange) when not is_binary(Queue) ->
    {error, invalid_queue_name};
bind_q_to_exchange(Queue, Routing, Exchange) ->
    bind_q_to_exchange(Queue, Routing, Exchange, []).
bind_q_to_exchange(Queue, Routing, Exchange, Options) ->
    QB = #'queue.bind'{
      queue = Queue %% what queue does the binding attach to?
      ,exchange = Exchange %% what exchange does the binding attach to?
      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
      ,nowait = props:get_value(nowait, Options, false)
      ,arguments = []
     },
    ok = wh_amqp_mgr:consume(QB).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Unbind a Queue from an Exchange
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q_from_callevt/2 :: (ne_binary(), ne_binary() | 'media_req') -> 'ok' | {'error', _}.
-spec unbind_q_from_callevt/3 :: (ne_binary(), ne_binary(), Type) -> 'ok' | {'error', _} when
      Type :: 'events' | 'status_req' | 'cdr' | 'publisher_usurp' | 'other'.
unbind_q_from_callevt(Queue, media_req) ->
    unbind_q_from_exchange(Queue, ?KEY_CALL_MEDIA_REQ, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID) ->
    unbind_q_from_callevt(Queue, CallID, events).

unbind_q_from_callevt(Queue, CallID, events) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_EVENT/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, status_req) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_STATUS_REQ/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, publisher_usurp) ->
    unbind_q_from_exchange(Queue, <<?KEY_PUBLISHER_USURP/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, CallID, cdr) ->
    unbind_q_from_exchange(Queue, <<?KEY_CALL_CDR/binary, (encode(CallID))/binary>>, ?EXCHANGE_CALLEVT);
unbind_q_from_callevt(Queue, Routing, other) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).


-spec unbind_q_from_conference/2 :: (ne_binary(), conf_routing_type()) -> 'ok' | {'error', _}.
-spec unbind_q_from_conference/3 :: (ne_binary(), conf_routing_type(), 'undefined' | ne_binary()) -> 'ok' | {'error', _}.

unbind_q_from_conference(Queue, discovery) ->
    unbind_q_from_conference(Queue, discovery, undefined);
unbind_q_from_conference(Queue, command) ->
    unbind_q_from_conference(Queue, command, <<"*">>);
unbind_q_from_conference(Queue, event) ->
    unbind_q_from_conference(Queue, event, <<"*">>).

unbind_q_from_conference(Queue, discovery, _) ->
    unbind_q_from_exchange(Queue, ?KEY_CONFERENCE_DISCOVERY, ?EXCHANGE_CONFERENCE);
unbind_q_from_conference(Queue, event, ConfId) ->
    unbind_q_from_exchange(Queue, <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE);
unbind_q_from_conference(Queue, command, ConfId) ->
    unbind_q_from_exchange(Queue, <<?KEY_CONFERENCE_COMMAND/binary, ConfId/binary>>, ?EXCHANGE_CONFERENCE).

unbind_q_from_callctl(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_CALLCTL).

unbind_q_from_notifications(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_NOTIFICATIONS).

unbind_q_from_sysconf(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_SYSCONF).

unbind_q_from_lync(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_LYNC).

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

-spec unbind_q_from_exchange/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', _}.
unbind_q_from_exchange(Queue, Routing, Exchange) ->
    QU = #'queue.unbind'{
      queue = Queue
      ,exchange = Exchange
      ,routing_key = Routing
      ,arguments = []
     },
    wh_amqp_mgr:consume(QU).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Bind a Queue to an Exchange (with optional Routing Key)
%% @end
%%------------------------------------------------------------------------------
%% create a consumer for a Queue
-spec basic_consume/1 :: (ne_binary()) -> 'ok' | {'error', _}.
-spec basic_consume/2 :: (ne_binary(), proplist()) -> 'ok' | {'error', _}.
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
    wh_amqp_mgr:consume(BC).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% This method cancels a consumer. This does not affect already delivered messages,
%% but it does mean the server will not send any more messages for that consumer.
%% @end
%%------------------------------------------------------------------------------
-spec basic_cancel/0 :: () -> 'ok'.
basic_cancel() ->
    wh_amqp_mgr:consume(#'basic.cancel'{}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec access_request/0 :: () -> #'access.request'{}.
-spec access_request/1 :: (proplist()) -> #'access.request'{}.
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
-spec is_json/1 :: (#'P_basic'{}) -> boolean().
is_json(#'P_basic'{content_type=CT}) ->
    CT =:= ?DEFAULT_CONTENT_TYPE.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% When sent by the client, this method acknowledges one or more messages
%% delivered via the Deliver or Get-'Ok' methods.
%% @end
%%------------------------------------------------------------------------------
-spec basic_ack/1 :: (integer() | #'basic.deliver'{}) -> 'ok'.
basic_ack(#'basic.deliver'{delivery_tag=DTag}) ->
    basic_ack(DTag);
basic_ack(DTag) ->
    wh_amqp_mgr:consume(#'basic.ack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% NOTE: THIS METHOD IS A RABBITMQ-SPECIFIC EXTENSION OF AMQP
%% Reject one or more incoming messages.
%% @end
%%------------------------------------------------------------------------------
-spec basic_nack/1 :: (integer() | #'basic.deliver'{}) -> 'ok'.
basic_nack(#'basic.deliver'{delivery_tag=DTag}) ->
    basic_nack(DTag);
basic_nack(DTag) ->
    wh_amqp_mgr:consume(#'basic.nack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the AMQP host is currently reachable
%% @end
%%------------------------------------------------------------------------------
-spec is_host_available/0 :: () -> boolean().
is_host_available() ->
    wh_amqp_mgr:is_available().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Specify quality of service
%% @end
%%------------------------------------------------------------------------------
-spec basic_qos/1 :: (non_neg_integer()) -> 'ok'.
basic_qos(PreFetch) when is_integer(PreFetch) ->
    wh_amqp_mgr:consume(#'basic.qos'{prefetch_count = PreFetch}).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Register to recieve notifications if ANY messages published with either with
%% the immediate or mandatory flags is returned
%% @end
%%------------------------------------------------------------------------------
-spec register_return_handler/0 :: () -> 'ok'.
register_return_handler() ->
    wh_amqp_mgr:register_return_handler().

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Encode a key so characters like dot won't interfere with routing separator
%% @end
%%------------------------------------------------------------------------------
-spec encode/1 :: (ne_binary()) -> ne_binary().
encode(<<"*">>) -> <<"*">>;
encode(<<"#">>) -> <<"#">>;
encode(Bin) ->
    << <<(encode_char(B))/binary>> || <<B>> <= Bin >>.

-define(HI4(C), (C band 2#11110000) bsr 4).
-define(LO4(C), (C band 2#00001111)).

encode_char(C) when ?KEY_SAFE(C) ->
    <<C>>;
encode_char($\s) ->
    <<$+>>;
encode_char($.) ->
    <<$%, $2, $E>>;
encode_char(C) ->
    Hi4 = ?HI4(C),
    Lo4 = ?LO4(C),
    <<$%, (hexint(Hi4)), (hexint(Lo4))>>.

hexint(C) when C < 10 ->
    ($0 + C);
hexint(C) when C < 16 ->
    ($A + (C - 10)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_key_test() ->
    ?assertEqual(<<"#">>, encode(<<"#">>)),
    ?assertEqual(<<"*">>, encode(<<"*">>)),
    ?assertEqual(<<"key">>, encode(<<"key">>)),
    ?assertEqual(<<"routing%2Ekey">>, encode(<<"routing.key">>)),
    ?assertEqual(<<"long%2Erouting%2Ekey">>, encode(<<"long.routing.key">>)),
    ?assertEqual(<<"test%26%2E192%2E+168%2E+5%2E+5%23">>, encode(<<"test&.192. 168. 5. 5#">>)),
    ok.
-endif.
