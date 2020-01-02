%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Utilities to facilitate AMQP interaction.
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Edouard Swiac
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_util).

-include("kz_amqp_util.hrl").

-export([targeted_exchange/0]).
-export([new_targeted_queue/0, new_targeted_queue/1]).
-export([delete_targeted_queue/1]).
-export([bind_q_to_targeted/1, bind_q_to_targeted/2]).
-export([unbind_q_from_targeted/1]).
-export([targeted_publish/2, targeted_publish/3, targeted_publish/4]).

-export([nodes_exchange/0]).
-export([new_nodes_queue/0, new_nodes_queue/1]).
-export([delete_nodes_queue/1]).
-export([bind_q_to_nodes/1, bind_q_to_nodes/2]).
-export([unbind_q_from_nodes/1]).
-export([nodes_publish/1, nodes_publish/2]).

-export([kapps_exchange/0]).
-export([new_kapps_queue/0, new_kapps_queue/1]).
-export([delete_kapps_queue/1]).
-export([bind_q_to_kapps/2, bind_q_to_kapps/3]).
-export([unbind_q_from_kapps/2]).
-export([kapps_publish/2, kapps_publish/3, kapps_publish/4]).

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

-export([bookkeepers_exchange/0]).
-export([new_bookkeepers_queue/0, new_bookkeepers_queue/1]).
-export([delete_bookkeepers_queue/1]).
-export([bind_q_to_bookkeepers/2, bind_q_to_bookkeepers/3]).
-export([unbind_q_from_bookkeepers/2]).
-export([bookkeepers_publish/2, bookkeepers_publish/3, bookkeepers_publish/4]).

-export([callctl_exchange/0]).
-export([new_callctl_queue/1]).
-export([delete_callctl_queue/1]).
-export([bind_q_to_callctl/1, bind_q_to_callctl/2]).
-export([unbind_q_from_callctl/1]).
-export([callctl_publish/2, callctl_publish/3, callctl_publish/4]).

-export([callevt_exchange/0]).
-export([new_callevt_queue/1]).
-export([delete_callevt_queue/1]).
-export([bind_q_to_callevt/1, bind_q_to_callevt/2]).
-export([unbind_q_from_callevt/2]).
-export([callevt_publish/2, callevt_publish/3, callevt_publish/4]).

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

-export([conference_exchange/0]).
-export([new_conference_queue/0, new_conference_queue/1]).
-export([delete_conference_queue/1]).
-export([bind_q_to_conference/2, bind_q_to_conference/3]).
-export([unbind_q_from_conference/2, unbind_q_from_conference/3, unbind_q_from_conference/4]).
-export([conference_publish/2, conference_publish/3, conference_publish/4, conference_publish/5, conference_publish/6]).

-export([presence_exchange/0]).
-export([new_presence_queue/0, new_presence_queue/1]).
-export([delete_presence_queue/1]).
-export([bind_q_to_presence/2, bind_q_to_presence/3]).
-export([unbind_q_from_presence/2]).
-export([presence_publish/2, presence_publish/3, presence_publish/4]).

-export([registrar_exchange/0]).
-export([new_registrar_queue/0, new_registrar_queue/1]).
-export([delete_registrar_queue/1]).
-export([bind_q_to_registrar/2, bind_q_to_registrar/3]).
-export([unbind_q_from_registrar/2]).
-export([registrar_publish/2, registrar_publish/3, registrar_publish/4]).

-export([leader_exchange/0]).
-export([bind_q_to_leader/2]).
-export([unbind_q_from_leader/2]).
-export([leader_publish/2, leader_publish/3, leader_publish/4]).

-export([tasks_exchange/0]).
-export([new_tasks_queue/0, new_tasks_queue/1]).
-export([delete_tasks_queue/1]).
-export([bind_q_to_tasks/2, bind_q_to_tasks/3]).
-export([unbind_q_from_tasks/2]).
-export([tasks_publish/2, tasks_publish/3, tasks_publish/4]).

-export([originate_resource_publish/1, originate_resource_publish/2]).

-export([offnet_resource_publish/1, offnet_resource_publish/2, offnet_resource_publish/3]).

-export([configuration_exchange/0
        ,configuration_publish/2, configuration_publish/3, configuration_publish/4
        ,document_change_publish/5, document_change_publish/6
        ]).
-export([document_routing_key/0, document_routing_key/1
        ,document_routing_key/2, document_routing_key/3
        ,document_routing_key/4
        ]).
-export([bind_q_to_configuration/2, unbind_q_from_configuration/2]).
-export([new_configuration_queue/1, new_configuration_queue/2, delete_configuration_queue/1]).

-export([monitor_exchange/0, monitor_publish/3]).
-export([bind_q_to_monitor/2
        ,unbind_q_from_monitor/2
        ]).
-export([new_monitor_queue/0, new_monitor_queue/1, delete_monitor_queue/1]).

-export([bind_q_to_exchange/3, bind_q_to_exchange/4]).
-export([unbind_q_from_exchange/3]).
-export([new_queue/0, new_queue/1, new_queue/2]).
-export([new_queue_name/0, new_queue_name/1]).
-export([basic_consume/1, basic_consume/2]).
-export([basic_publish/3, basic_publish/4, basic_publish/5]).
-export([basic_cancel/0, basic_cancel/1]).
-export([queue_delete/1, queue_delete/2]).
-export([new_exchange/2, new_exchange/3]).
-export([declare_exchange/2, declare_exchange/3]).
-export([confirm_select/0]).
-export([flow_control/0, flow_control/1, flow_control_reply/1]).

-export([access_request/0, access_request/1, basic_ack/1, basic_nack/1, basic_qos/1]).

-export([is_json/1, is_host_available/0]).
-export([encode/1]).
-export([split_routing_key/1]).

-ifdef(TEST).
-export([trim/3]).
-endif.

-define(KEY_SAFE(C), ((C >= $a
                       andalso C =< $z
                      )
                      orelse (C >= $A
                              andalso C =< $Z
                             )
                      orelse (C >= $0
                              andalso C =< $9
                             )
                      orelse (C =:= $-
                              orelse C =:= $~
                              orelse C =:= $_
                             )
                     )
       ).

-define(P_GET(K, Prop), props:get_value(K, Prop)).
-define(P_GET(K, Prop, D), props:get_value(K, Prop, D)).

-type amqp_payload() :: iolist() | kz_term:ne_binary().

-type amqp_property() :: {kz_term:ne_binary(), atom(), any()}.
-type amqp_properties() :: [amqp_property()].

%%------------------------------------------------------------------------------
%% @doc Publish AMQP messages.
%% @end
%%------------------------------------------------------------------------------

-spec targeted_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
targeted_publish(Queue, Payload) ->
    targeted_publish(Queue, Payload, ?DEFAULT_CONTENT_TYPE).

-spec targeted_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
targeted_publish(?NE_BINARY = Queue, Payload, ContentType) ->
    targeted_publish(Queue, Payload, ContentType, []).

-spec targeted_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
targeted_publish(?NE_BINARY = Queue, Payload, ContentType, Options) ->
    basic_publish(?EXCHANGE_TARGETED, Queue, Payload, ContentType, Options).

-spec nodes_publish(amqp_payload()) -> 'ok'.
nodes_publish(Payload) ->
    nodes_publish(Payload, ?DEFAULT_CONTENT_TYPE).

-spec nodes_publish(amqp_payload(), kz_term:ne_binary()) -> 'ok'.
nodes_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_NODES, <<>>, Payload, ContentType, ['maybe_publish']).

-spec kapps_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
kapps_publish(Routing, Payload) ->
    kapps_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec kapps_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
kapps_publish(Routing, Payload, ContentType) ->
    kapps_publish(Routing, Payload, ContentType, []).

-spec kapps_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
kapps_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_KAPPS, Routing, Payload, ContentType, Opts).

-spec presence_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
presence_publish(Routing, Payload) ->
    presence_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec presence_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
presence_publish(Routing, Payload, ContentType) ->
    presence_publish(Routing, Payload, ContentType, []).

-spec presence_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
presence_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_PRESENCE, Routing, Payload, ContentType, Opts).

-spec notifications_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
notifications_publish(Routing, Payload) ->
    notifications_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec notifications_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
notifications_publish(Routing, Payload, ContentType) ->
    notifications_publish(Routing, Payload, ContentType, []).

-spec notifications_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
notifications_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_NOTIFICATIONS, Routing, Payload, ContentType, Opts).

-spec sysconf_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
sysconf_publish(Routing, Payload) ->
    sysconf_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec sysconf_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
sysconf_publish(Routing, Payload, ContentType) ->
    sysconf_publish(Routing, Payload, ContentType, []).

-spec sysconf_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
sysconf_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_SYSCONF, Routing, Payload, ContentType, Opts).

-spec bookkeepers_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
bookkeepers_publish(Routing, Payload) ->
    bookkeepers_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec bookkeepers_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
bookkeepers_publish(Routing, Payload, ContentType) ->
    bookkeepers_publish(Routing, Payload, ContentType, []).

-spec bookkeepers_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bookkeepers_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_BOOKKEEPERS, Routing, Payload, ContentType, Opts).

%%------------------------------------------------------------------------------
%% @doc
%% @todo The routing key on this function should be the first argument for consistency.
%% @end
%%------------------------------------------------------------------------------

-spec callmgr_publish(amqp_payload(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
callmgr_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType).

-spec callmgr_publish(amqp_payload(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
callmgr_publish(Payload, ContentType, RoutingKey, Opts) ->
    basic_publish(?EXCHANGE_CALLMGR, RoutingKey, Payload, ContentType, Opts).

-spec configuration_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
configuration_publish(RoutingKey, Payload) ->
    configuration_publish(RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).

-spec configuration_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
configuration_publish(RoutingKey, Payload, ContentType) ->
    configuration_publish(RoutingKey, Payload, ContentType, []).

-spec configuration_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
configuration_publish(RoutingKey, Payload, ContentType, Props) ->
    basic_publish(?EXCHANGE_CONFIGURATION, RoutingKey, Payload, ContentType, Props).

-spec document_change_publish(Action, Db, Type, Id, Payload) -> 'ok' when
      Action :: atom(), %% edited | created | deleted
      Db :: kz_term:ne_binary(),
      Type :: kz_term:ne_binary(),
      Id :: kz_term:ne_binary(),
      Payload :: amqp_payload().
document_change_publish(Action, Db, Type, Id, JSON) ->
    document_change_publish(Action, Db, Type, Id, JSON, ?DEFAULT_CONTENT_TYPE).

-spec document_change_publish(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
document_change_publish(Action, Db, Type, Id, Payload, ContentType) ->
    RoutingKey = document_routing_key(Action, Db, Type, Id),
    configuration_publish(RoutingKey, Payload, ContentType).

-spec document_routing_key() -> kz_term:ne_binary().
document_routing_key() ->
    document_routing_key(<<"*">>).

-spec document_routing_key(atom() | kz_term:ne_binary()) -> kz_term:ne_binary().
document_routing_key(Action) ->
    document_routing_key(Action, <<"*">>).

-spec document_routing_key(atom() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
document_routing_key(Action, Db) ->
    document_routing_key(Action, Db, <<"*">>).

-spec document_routing_key(atom() | kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
document_routing_key(Action, Db, Type) ->
    document_routing_key(Action, Db, Type, <<"*">>).

-spec document_routing_key(atom() | kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
document_routing_key(<<"*">>, Db, Type, Id) ->
    list_to_binary([<<"*.">>, kz_term:to_binary(Db)
                   ,".", kz_term:to_binary(Type)
                   ,".", encode(kz_term:to_binary(Id))
                   ]);
document_routing_key(<<"db_", _/binary>> = Action, Db, Type, Id) ->
    list_to_binary([Action
                   ,".", kz_term:to_binary(Db)
                   ,".", kz_term:to_binary(Type)
                   ,".", encode(kz_term:to_binary(Id))
                   ]);
document_routing_key(<<"doc_", _/binary>> = Action, Db, Type, Id) ->
    list_to_binary([Action
                   ,".", kz_term:to_binary(Db)
                   ,".", kz_term:to_binary(Type)
                   ,".", encode(kz_term:to_binary(Id))
                   ]);
document_routing_key(Action, Db, <<"database">>=Type, Id) ->
    list_to_binary(["db_", kz_term:to_list(Action)
                   ,".", kz_term:to_binary(Db)
                   ,".", kz_term:to_binary(Type)
                   ,".", encode(kz_term:to_binary(Id))
                   ]);
document_routing_key(Action, Db, Type, Id) ->
    list_to_binary(["doc_", kz_term:to_list(Action)
                   ,".", kz_term:to_binary(Db)
                   ,".", kz_term:to_binary(Type)
                   ,".", encode(kz_term:to_binary(Id))
                   ]).

-spec callctl_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
callctl_publish(CtrlQ, Payload) ->
    callctl_publish(CtrlQ, Payload, ?DEFAULT_CONTENT_TYPE).

-spec callctl_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
callctl_publish(CtrlQ, Payload, ContentType) ->
    callctl_publish(CtrlQ, Payload, ContentType, []).

-spec callctl_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
callctl_publish(CtrlQ, Payload, ContentType, Props) ->
    basic_publish(?EXCHANGE_CALLCTL, CtrlQ, Payload, ContentType, Props).

-spec callevt_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
callevt_publish(RoutingKey, Payload) ->
    callevt_publish(RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).

-spec callevt_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
callevt_publish(RoutingKey, Payload, ContentType) ->
    callevt_publish(RoutingKey, Payload, ContentType, []).

-spec callevt_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
callevt_publish(RoutingKey, Payload, ContentType, Prop) ->
    basic_publish(?EXCHANGE_CALLEVT, RoutingKey, Payload, ContentType, Prop).

-spec originate_resource_publish(amqp_payload()) -> 'ok'.
originate_resource_publish(Payload) ->
    originate_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).

-spec originate_resource_publish(amqp_payload(), kz_term:ne_binary()) -> 'ok'.
originate_resource_publish(Payload, ContentType) ->
    basic_publish(?EXCHANGE_RESOURCE, ?KEY_ORGN_RESOURCE_REQ, Payload, ContentType).

-spec offnet_resource_publish(amqp_payload()) -> 'ok'.
offnet_resource_publish(Payload) ->
    offnet_resource_publish(Payload, ?DEFAULT_CONTENT_TYPE).

-spec offnet_resource_publish(amqp_payload(), kz_term:ne_binary()) -> 'ok'.
offnet_resource_publish(Payload, ContentType) ->
    offnet_resource_publish(Payload, ContentType, ?KEY_OFFNET_RESOURCE_REQ).

-spec offnet_resource_publish(amqp_payload(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
offnet_resource_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_RESOURCE, RoutingKey, Payload, ContentType).

%% monitor
-spec monitor_publish(amqp_payload(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
monitor_publish(Payload, ContentType, RoutingKey) ->
    basic_publish(?EXCHANGE_MONITOR, RoutingKey, Payload, ContentType).

-type conf_routing_type() :: 'discovery' | 'event' | 'command' | 'config'.

-spec conference_publish(amqp_payload(), conf_routing_type()) -> 'ok'.
conference_publish(Payload, 'discovery') ->
    conference_publish(Payload, 'discovery', <<"*">>);
conference_publish(Payload, 'event') ->
    conference_publish(Payload, 'event', <<"*">>);
conference_publish(Payload, 'config') ->
    conference_publish(Payload, 'config', <<"*">>);
conference_publish(Payload, 'command') ->
    conference_publish(Payload, 'command', <<"*">>).

-spec conference_publish(amqp_payload(), conf_routing_type(), kz_term:api_binary()) -> 'ok'.
conference_publish(Payload, 'discovery', ConfId) ->
    conference_publish(Payload, 'discovery', ConfId, []);
conference_publish(Payload, 'config', ConfId) ->
    conference_publish(Payload, 'config', ConfId, []);
conference_publish(Payload, 'event', ConfId) ->
    conference_publish(Payload, 'event', ConfId, []);
conference_publish(Payload, 'command', ConfId) ->
    conference_publish(Payload, 'command', ConfId, []).

-spec conference_publish(amqp_payload(), conf_routing_type(), kz_term:api_binary(), kz_term:proplist()) -> 'ok'.
conference_publish(Payload, 'discovery', ConfId, Options) ->
    conference_publish(Payload, 'discovery', ConfId, Options, ?DEFAULT_CONTENT_TYPE);
conference_publish(Payload, 'config', ConfId, Options) ->
    conference_publish(Payload, 'config', ConfId, Options, ?DEFAULT_CONTENT_TYPE);
conference_publish(Payload, 'event', ConfId, Options) ->
    conference_publish(Payload, 'event', ConfId, Options, ?DEFAULT_CONTENT_TYPE);
conference_publish(Payload, 'command', ConfId, Options) ->
    conference_publish(Payload, 'command', ConfId, Options, ?DEFAULT_CONTENT_TYPE).

-spec conference_publish(amqp_payload(), conf_routing_type(), kz_term:api_binary(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
conference_publish(Payload, 'discovery', _, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, ?KEY_CONFERENCE_DISCOVERY, Payload, ContentType, Options);
conference_publish(Payload, 'config', ConfProfile, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONFERENCE_CONFIG/binary, ConfProfile/binary>>, Payload, ContentType, Options);
conference_publish(Payload, 'event', ConfId, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary>>, Payload, ContentType, Options);
conference_publish(Payload, 'command', ConfId, Options, ContentType) ->
    basic_publish(?EXCHANGE_CONFERENCE, conference_command_binding(ConfId), Payload, ContentType, Options).

-spec conference_publish(amqp_payload(), 'event', kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
conference_publish(Payload, 'event', ConfId, CallId, Options, ContentType) ->
    RoutingKey = <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary, ".", CallId/binary>>,
    basic_publish(?EXCHANGE_CONFERENCE, RoutingKey, Payload, ContentType, Options).

-spec registrar_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
registrar_publish(Routing, Payload) ->
    registrar_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec registrar_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
registrar_publish(Routing, Payload, ContentType) ->
    registrar_publish(Routing, Payload, ContentType, []).

-spec registrar_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
registrar_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_REGISTRAR, Routing, Payload, ContentType, Opts).

-spec leader_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
leader_publish(Routing, Payload) ->
    leader_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec leader_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
leader_publish(Routing, Payload, ContentType) ->
    leader_publish(Routing, Payload, ContentType, []).

-spec leader_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
leader_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_LEADER, Routing, Payload, ContentType, Opts).

-spec tasks_publish(kz_term:ne_binary(), amqp_payload()) -> 'ok'.
tasks_publish(Routing, Payload) ->
    tasks_publish(Routing, Payload, ?DEFAULT_CONTENT_TYPE).

-spec tasks_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
tasks_publish(Routing, Payload, ContentType) ->
    tasks_publish(Routing, Payload, ContentType, []).

-spec tasks_publish(kz_term:ne_binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
tasks_publish(Routing, Payload, ContentType, Opts) ->
    basic_publish(?EXCHANGE_TASKS, Routing, Payload, ContentType, Opts).


%%------------------------------------------------------------------------------
%% @doc Generic publisher for an `Exchange.Queue'.
%% Use `<<"#">>' for a default Queue
%% @end
%%------------------------------------------------------------------------------

-spec basic_publish(kz_term:ne_binary(), binary(), amqp_payload()) -> 'ok'.
basic_publish(Exchange, RoutingKey, Payload) ->
    basic_publish(Exchange, RoutingKey, Payload, ?DEFAULT_CONTENT_TYPE).

-spec basic_publish(kz_term:ne_binary(), binary(), amqp_payload(), kz_term:ne_binary()) -> 'ok'.
basic_publish(Exchange, RoutingKey, Payload, ContentType) ->
    basic_publish(Exchange, RoutingKey, Payload, ContentType, []).

-spec basic_publish(kz_term:ne_binary(), binary(), amqp_payload(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
basic_publish(Exchange, RoutingKey, Payload, ContentType, Prop)
  when is_list(Payload) ->
    basic_publish(Exchange, RoutingKey, iolist_to_binary(Payload), ContentType, Prop);
basic_publish(Exchange, <<"pid://", _/binary>>=RoutingKey, ?NE_BINARY = Payload, ContentType, Props)
  when is_binary(Exchange),
       is_binary(RoutingKey),
       is_binary(ContentType),
       is_list(Props) ->
    Headers = props:get_value('headers', Props, []),
    {'match', [Pid, RK]}= re:run(RoutingKey, <<"pid://(.*)/(.*)">>, [{'capture', [1,2], 'binary'}]),
    NewProps = props:set_value('headers', [{?KEY_DELIVER_TO_PID, binary, Pid} | Headers], Props),
    basic_publish(Exchange, RK, Payload, ContentType, NewProps);
basic_publish(Exchange, RoutingKey, ?NE_BINARY = Payload, ContentType, Props)
  when is_binary(Exchange),
       is_binary(RoutingKey),
       is_binary(ContentType),
       is_list(Props) ->
    BP = #'basic.publish'{exchange = Exchange
                         ,routing_key = RoutingKey
                         ,mandatory = ?P_GET('mandatory', Props, 'false')
                         ,immediate = ?P_GET('immediate', Props, 'false')
                         },

    %% Add the message to the publish, converting to binary
    %% See http://www.rabbitmq.com/amqp-0-9-1-reference.html#class.basic
    MsgProps =
        #'P_basic'{content_type = ContentType % MIME content type
                  ,content_encoding = ?P_GET('content_encoding', Props) % MIME encoding
                  ,headers = ?P_GET('headers', Props) % message headers
                  ,delivery_mode = ?P_GET('delivery_mode', Props) % persistent(2) or not(1)
                  ,priority = ?P_GET('priority', Props) % message priority, 0-9
                  ,correlation_id = ?P_GET('correlation_id', Props) % correlation identifier
                  ,reply_to = ?P_GET('reply_to', Props) % address to reply to

                   %% TODO:: new rabbit wants an integer...
                  ,expiration = ?P_GET('expiration', Props) % expires time

                  ,message_id = ?P_GET('message_id', Props) % app message id
                  ,timestamp = ?P_GET('timestamp', Props, kz_time:now_us()) % message timestamp
                  ,type = ?P_GET('type', Props) % message type
                  ,user_id = ?P_GET('user_id', Props) % creating user
                  ,app_id = ?P_GET('app_id', Props) % creating app
                  ,cluster_id = ?P_GET('cluster_id', Props) % cluster
                  },

    AM = #'amqp_msg'{payload = encode(MsgProps#'P_basic'.content_encoding, Payload)
                    ,props = MsgProps
                    },

    case ?P_GET('maybe_publish', Props, 'false') of
        'true' -> kz_amqp_channel:maybe_publish(BP, AM);
        'false' -> kz_amqp_channel:publish(BP, AM)
    end.

encode(<<"gzip">>, Payload) -> zlib:gzip(Payload);
encode(_ContentType, Payload) -> Payload.

%%------------------------------------------------------------------------------
%% @doc Create AMQP exchanges.
%% @end
%%------------------------------------------------------------------------------
-spec kapps_exchange() -> 'ok'.
kapps_exchange() ->
    new_exchange(?EXCHANGE_KAPPS, ?TYPE_KAPPS).

-spec presence_exchange() -> 'ok'.
presence_exchange() ->
    new_exchange(?EXCHANGE_PRESENCE, ?TYPE_PRESENCE).

-spec targeted_exchange() -> 'ok'.
targeted_exchange() ->
    new_exchange(?EXCHANGE_TARGETED, ?TYPE_TARGETED).

-spec nodes_exchange() -> 'ok'.
nodes_exchange() ->
    new_exchange(?EXCHANGE_NODES, ?TYPE_NODES).

-spec notifications_exchange() -> 'ok'.
notifications_exchange() ->
    new_exchange(?EXCHANGE_NOTIFICATIONS, ?TYPE_NOTIFICATIONS).

-spec sysconf_exchange() -> 'ok'.
sysconf_exchange() ->
    new_exchange(?EXCHANGE_SYSCONF, ?TYPE_SYSCONF).

-spec bookkeepers_exchange() -> 'ok'.
bookkeepers_exchange() ->
    new_exchange(?EXCHANGE_BOOKKEEPERS, ?TYPE_BOOKKEEPERS).

-spec callctl_exchange() -> 'ok'.
callctl_exchange() ->
    new_exchange(?EXCHANGE_CALLCTL, ?TYPE_CALLCTL).

-spec callevt_exchange() -> 'ok'.
callevt_exchange() ->
    new_exchange(?EXCHANGE_CALLEVT, ?TYPE_CALLEVT).

-spec resource_exchange() -> 'ok'.
resource_exchange() ->
    new_exchange(?EXCHANGE_RESOURCE, ?TYPE_RESOURCE).

-spec callmgr_exchange() -> 'ok'.
callmgr_exchange() ->
    new_exchange(?EXCHANGE_CALLMGR, ?TYPE_CALLMGR).

-spec configuration_exchange() -> 'ok'.
configuration_exchange() ->
    new_exchange(?EXCHANGE_CONFIGURATION, ?TYPE_CONFIGURATION).

-spec monitor_exchange() -> 'ok'.
monitor_exchange() ->
    new_exchange(?EXCHANGE_MONITOR, ?TYPE_MONITOR).

-spec conference_exchange() -> 'ok'.
conference_exchange() ->
    new_exchange(?EXCHANGE_CONFERENCE, ?TYPE_CONFERENCE).

-spec registrar_exchange() -> 'ok'.
registrar_exchange() ->
    new_exchange(?EXCHANGE_REGISTRAR, ?TYPE_REGISTRAR).

-spec leader_exchange() -> 'ok'.
leader_exchange() ->
    new_exchange(?EXCHANGE_LEADER, ?TYPE_LEADER).

-spec tasks_exchange() -> 'ok'.
tasks_exchange() ->
    new_exchange(?EXCHANGE_TASKS, ?TYPE_TASKS).


%%------------------------------------------------------------------------------
%% @doc A generic Exchange maker.
%% @end
%%------------------------------------------------------------------------------

-spec new_exchange(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
new_exchange(Exchange, Type) ->
    new_exchange(Exchange, Type, []).

-spec new_exchange(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
new_exchange(Exchange, Type, Options) ->
    ED = #'exchange.declare'{exchange = Exchange
                            ,type = Type
                            ,passive = ?P_GET('passive', Options, 'false')
                            ,durable = ?P_GET('durable', Options, 'false')
                            ,auto_delete = ?P_GET('auto_delete', Options, 'false')
                            ,internal = ?P_GET('internal', Options, 'false')
                            ,nowait = ?P_GET('nowait', Options, 'false')
                            ,arguments = ?P_GET('arguments', Options, [])
                            },
    kz_amqp_channel:command(ED).

-spec declare_exchange(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_amqp_exchange().
declare_exchange(Exchange, Type) ->
    declare_exchange(Exchange, Type, []).

-spec declare_exchange(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_amqp_exchange().
declare_exchange(Exchange, Type, Options) ->
    #'exchange.declare'{exchange = Exchange
                       ,type = Type
                       ,passive = ?P_GET('passive', Options, 'false')
                       ,durable = ?P_GET('durable', Options, 'false')
                       ,auto_delete = ?P_GET('auto_delete', Options, 'false')
                       ,internal = ?P_GET('internal', Options, 'false')
                       ,nowait = ?P_GET('nowait', Options, 'false')
                       ,arguments = ?P_GET('arguments', Options, [])
                       }.

%%------------------------------------------------------------------------------
%% @doc Create AMQP queues.
%% @end
%%------------------------------------------------------------------------------

-spec new_targeted_queue() -> kz_term:ne_binary() | {'error', any()}.
new_targeted_queue() -> new_targeted_queue(<<>>).

-spec new_targeted_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_targeted_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_nodes_queue() -> kz_term:ne_binary() | {'error', any()}.
new_nodes_queue() -> new_nodes_queue(<<>>).

-spec new_nodes_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_nodes_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_kapps_queue() -> kz_term:ne_binary() | {'error', any()}.
new_kapps_queue() -> new_kapps_queue(<<>>).

-spec new_kapps_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_kapps_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_presence_queue() -> kz_term:ne_binary() | {'error', any()}.
new_presence_queue() -> new_presence_queue(<<>>).

-spec new_presence_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_presence_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_registrar_queue() -> kz_term:ne_binary() | {'error', any()}.
new_registrar_queue() -> new_registrar_queue(<<>>).

-spec new_registrar_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_registrar_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_notifications_queue() -> kz_term:ne_binary() | {'error', any()}.
new_notifications_queue() -> new_notifications_queue(<<>>).

-spec new_notifications_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_notifications_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_sysconf_queue() -> kz_term:ne_binary() | {'error', any()}.
new_sysconf_queue() -> new_sysconf_queue(<<>>).

-spec new_sysconf_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_sysconf_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_bookkeepers_queue() -> kz_term:ne_binary() | {'error', any()}.
new_bookkeepers_queue() -> new_bookkeepers_queue(<<>>).

-spec new_bookkeepers_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_bookkeepers_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).

-spec new_callevt_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_callevt_queue(<<>>) ->
    new_queue(<<>>, [{'exclusive', 'false'}
                    ,{'auto_delete', 'true'}
                    ,{'nowait', 'false'}
                    ]);
new_callevt_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLEVT, ".", encode(CallID)])
             ,[{'exclusive', 'false'}
              ,{'auto_delete', 'true'}
              ,{'nowait', 'false'}
              ]).

-spec new_callctl_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_callctl_queue(<<>>) ->
    new_queue(<<>>, [{'exclusive', 'false'}
                    ,{'auto_delete', 'true'}
                    ,{'nowait', 'false'}
                    ]);
new_callctl_queue(CallID) ->
    new_queue(list_to_binary([?EXCHANGE_CALLCTL, ".", encode(CallID)])
             ,[{'exclusive', 'false'}
              ,{'auto_delete', 'true'}
              ,{'nowait', 'false'}
              ]).

-spec new_resource_queue() -> kz_term:ne_binary() | {'error', any()}.
new_resource_queue() -> new_resource_queue(?RESOURCE_QUEUE_NAME).

-spec new_resource_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_resource_queue(Queue) ->
    new_queue(Queue, [{'exclusive', 'false'}
                     ,{'auto_delete', 'true'}
                     ,{'nowait', 'false'}
                     ]).

-spec new_callmgr_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_callmgr_queue(Queue) -> new_callmgr_queue(Queue, []).

-spec new_callmgr_queue(binary(), kz_term:proplist()) -> kz_term:ne_binary() | {'error', any()}.
new_callmgr_queue(Queue, Opts) -> new_queue(Queue, Opts).

-spec new_configuration_queue(kz_term:ne_binary()) -> kz_term:ne_binary() | {'error', any()}.
new_configuration_queue(Queue) -> new_configuration_queue(Queue, []).

-spec new_configuration_queue(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary() | {'error', any()}.
new_configuration_queue(Queue, Options) -> new_queue(Queue, Options).

-spec new_monitor_queue() -> kz_term:ne_binary() | {'error', any()}.
new_monitor_queue() -> new_monitor_queue(<<>>).

-spec new_monitor_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_monitor_queue(Queue) ->
    new_queue(Queue, [{'exclusive', 'false'}
                     ,{'auto_delete', 'true'}
                     ]).

-spec new_conference_queue() -> kz_term:ne_binary() | {'error', any()}.
new_conference_queue() -> new_conference_queue(<<>>).

-spec new_conference_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_conference_queue(Queue) ->
    new_queue(Queue, [{'exclusive', 'false'}
                     ,{'auto_delete', 'true'}
                     ,{'nowait', 'false'}
                     ]).

-spec new_tasks_queue() -> kz_term:ne_binary() | {'error', any()}.
new_tasks_queue() -> new_tasks_queue(<<>>).

-spec new_tasks_queue(binary()) -> kz_term:ne_binary() | {'error', any()}.
new_tasks_queue(Queue) -> new_queue(Queue, [{'nowait', 'false'}]).


-type new_queue_ret() :: kz_term:api_binary() | integer() |
                         {kz_term:ne_binary(), integer(), integer()} |
                         {'error', any()}.

%%------------------------------------------------------------------------------
%% @doc Declare a queue and returns the queue Name.
%% It lets the client lib create a random queue name.
%% @see new_queue/1
%% @end
%%------------------------------------------------------------------------------
-spec new_queue() -> new_queue_ret().
new_queue() -> new_queue(new_queue_name()).

%%------------------------------------------------------------------------------
%% @doc Declare a queue and returns the queue Name.
%% @see new_queue/0
%% @end
%%------------------------------------------------------------------------------
-spec new_queue(binary()) -> new_queue_ret().
new_queue(Queue) -> new_queue(Queue, []).

-spec new_queue(binary(), kz_term:proplist()) -> new_queue_ret().
new_queue(<<"amq.", _/binary>>, Options) ->
    new_queue(new_queue_name(), Options);
new_queue(<<>>, Options) ->
    new_queue(new_queue_name(), Options);
new_queue(Queue, Options) when is_binary(Queue) ->
    QD = #'queue.declare'{queue = Queue
                         ,passive = ?P_GET('passive', Options, 'false')
                         ,durable = ?P_GET('durable', Options, 'false')
                         ,exclusive = ?P_GET('exclusive', Options, 'false')
                         ,auto_delete = ?P_GET('auto_delete', Options, 'true')
                         ,nowait = ?P_GET('nowait', Options, 'false')
                         ,arguments = queue_arguments(?P_GET('arguments', Options, []))
                         },

    %% can be queue | message_count | consumer_count | all
    Return = ?P_GET('return_field', Options, 'queue'),

    case catch kz_amqp_channel:command(QD) of
        {'ok', #'queue.declare_ok'{queue=Q}} when Return =:= 'queue' -> Q;
        {'ok', #'queue.declare_ok'{message_count=Cnt}} when Return =:= 'message_count' -> Cnt;
        {'ok', #'queue.declare_ok'{consumer_count=Cnt}} when Return =:= 'consumer_count' -> Cnt;
        {'ok', #'queue.declare_ok'{queue=Q
                                  ,message_count=MCnt
                                  ,consumer_count=CCnt
                                  }} when Return =:= 'all' ->
            {Q, MCnt, CCnt};
        {'error', _}=E -> E;
        {'EXIT', {{'shutdown', Reason}, _}} -> {'error', Reason};
        {'EXIT',{'noproc', _}} -> {'error', 'no_channel'}
    end.

-spec new_queue_name() -> kz_term:ne_binary().
new_queue_name() ->
    list_to_binary(io_lib:format("~s-~p-~s", [node(), self(), kz_binary:rand_hex(4)])).

-spec new_queue_name(kz_term:ne_binary() | atom()) -> kz_term:ne_binary().
new_queue_name(Name) ->
    list_to_binary(io_lib:format("~s-~s-~p-~s", [node(), Name, self(), kz_binary:rand_hex(4)])).

-spec queue_arguments(kz_term:proplist()) -> amqp_properties().
queue_arguments(Arguments) ->
    Routines = [fun max_length/2
               ,fun message_ttl/2
               ,fun max_priority/2
               ],
    lists:foldl(fun(F, Acc) -> F(Arguments, Acc) end, Arguments, Routines).

-spec max_priority(kz_term:proplist(), amqp_properties()) -> amqp_properties().
max_priority(Args, Acc) ->
    Property = props:get_value(<<"x-max-priority">>, Args),
    Acc1 = props:delete(<<"x-max-priority">>, Acc),
    case Property of
        Val when is_integer(Val) ->
            AMQPValue = trim(0, 255, Val),
            [{<<"x-max-priority">>, 'byte', AMQPValue}|Acc1];
        _Else -> Acc1
    end.

-spec trim(integer(), integer(), integer()) -> integer().
trim(Min, _  , Val) when Val < Min -> Min;
trim(_  , Max, Val) when Val > Max -> Max;
trim(_  , _  , Val)                -> Val.

-spec max_length(kz_term:proplist(), amqp_properties()) -> amqp_properties().
max_length(Args, Acc) ->
    case props:get_value(<<"x-max-length">>, Args) of
        'undefined' -> [{<<"x-max-length">>, 'short', 100}|Acc];
        'infinity' -> props:delete(<<"x-max-length">>, Acc);
        Value ->
            Acc1 = props:delete(<<"x-max-length">>, Acc),
            [{<<"x-max-length">>, 'short', Value}|Acc1]
    end.

-spec message_ttl(kz_term:proplist(), amqp_properties()) -> amqp_properties().
message_ttl(Args, Acc) ->
    case props:get_value(<<"x-message-ttl">>, Args) of
        'undefined' -> [{<<"x-message-ttl">>, 'signedint', 60 * ?MILLISECONDS_IN_SECOND}|Acc];
        'infinity' -> props:delete(<<"x-message-ttl">>, Acc);
        Value ->
            Acc1 = props:delete(<<"x-message-ttl">>, Acc),
            [{<<"x-message-ttl">>, 'signedint', Value}|Acc1]
    end.

%%------------------------------------------------------------------------------
%% @doc Delete AMQP queue.
%% @end
%%------------------------------------------------------------------------------

-spec delete_targeted_queue(kz_term:ne_binary()) -> command_ret().
delete_targeted_queue(Queue) -> queue_delete(Queue).

-spec delete_nodes_queue(kz_term:ne_binary()) -> command_ret().
delete_nodes_queue(Queue) -> queue_delete(Queue).

-spec delete_kapps_queue(kz_term:ne_binary()) -> command_ret().
delete_kapps_queue(Queue) -> queue_delete(Queue).

-spec delete_presence_queue(kz_term:ne_binary()) -> command_ret().
delete_presence_queue(Queue) -> queue_delete(Queue).

-spec delete_notifications_queue(kz_term:ne_binary()) -> command_ret().
delete_notifications_queue(Queue) -> queue_delete(Queue).

-spec delete_sysconf_queue(kz_term:ne_binary()) -> command_ret().
delete_sysconf_queue(Queue) -> queue_delete(Queue).

-spec delete_bookkeepers_queue(kz_term:ne_binary()) -> command_ret().
delete_bookkeepers_queue(Queue) -> queue_delete(Queue).

-spec delete_callmgr_queue(kz_term:ne_binary()) -> command_ret().
delete_callmgr_queue(Queue) -> queue_delete(Queue).

-spec delete_resource_queue(kz_term:ne_binary()) -> command_ret().
delete_resource_queue(Queue) -> queue_delete(Queue).

-spec delete_configuration_queue(kz_term:ne_binary()) -> command_ret().
delete_configuration_queue(Queue) -> queue_delete(Queue).

-spec delete_conference_queue(kz_term:ne_binary()) -> command_ret().
delete_conference_queue(Queue) -> queue_delete(Queue).

-spec delete_monitor_queue(kz_term:ne_binary()) -> command_ret().
delete_monitor_queue(Queue) -> queue_delete(Queue).

-spec delete_registrar_queue(kz_term:ne_binary()) -> command_ret().
delete_registrar_queue(Queue) -> queue_delete(Queue).

-spec delete_tasks_queue(kz_term:ne_binary()) -> command_ret().
delete_tasks_queue(Queue) -> queue_delete(Queue).

-spec delete_callevt_queue(kz_term:ne_binary()) -> command_ret().
delete_callevt_queue(CallID) -> delete_callevt_queue(CallID, []).
delete_callevt_queue(CallID, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLEVT, ".", encode(CallID)]), Prop).

-spec delete_callctl_queue(kz_term:ne_binary()) -> command_ret().
delete_callctl_queue(CallID) -> delete_callctl_queue(CallID, []).
delete_callctl_queue(CallID, Prop) ->
    queue_delete(list_to_binary([?EXCHANGE_CALLCTL, ".", encode(CallID)]), Prop).

-spec queue_delete(kz_term:ne_binary()) -> command_ret().
queue_delete(Queue) -> queue_delete(Queue, []).

-spec queue_delete(kz_term:ne_binary(), kz_term:proplist()) -> command_ret().
queue_delete(Queue, _Prop) when not is_binary(Queue) ->
    {'error', 'invalid_queue_name'};
queue_delete(Queue, Prop) ->
    QD = #'queue.delete'{queue=Queue
                        ,if_unused = ?P_GET('if_unused', Prop, 'false')
                        ,if_empty = ?P_GET('if_empty', Prop, 'false')
                        ,nowait = ?P_GET('nowait', Prop, 'true')
                        },
    kz_amqp_channel:command(QD).

%%------------------------------------------------------------------------------
%% @doc Bind a Queue to an Exchange (with optional Routing Key).
%% @end
%%------------------------------------------------------------------------------

-spec bind_q_to_targeted(kz_term:ne_binary()) -> 'ok'.
bind_q_to_targeted(Queue) ->
    bind_q_to_exchange(Queue, Queue, ?EXCHANGE_TARGETED).

-spec bind_q_to_targeted(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_targeted(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TARGETED).

-spec bind_q_to_nodes(kz_term:ne_binary()) -> 'ok'.
bind_q_to_nodes(Queue) ->
    bind_q_to_exchange(Queue, Queue, ?EXCHANGE_NODES).

-spec bind_q_to_nodes(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_nodes(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_NODES).

-spec bind_q_to_kapps(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_kapps(Queue, Routing) ->
    bind_q_to_kapps(Queue, Routing, []).

-spec bind_q_to_kapps(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_kapps(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_KAPPS, Options).

-spec bind_q_to_presence(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_presence(Queue, Routing) ->
    bind_q_to_presence(Queue, Routing, []).

-spec bind_q_to_presence(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_presence(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_PRESENCE, Options).

-spec bind_q_to_registrar(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_registrar(Queue, Routing) ->
    bind_q_to_registrar(Queue, Routing, []).

-spec bind_q_to_registrar(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_registrar(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_REGISTRAR, Options).

-spec bind_q_to_notifications(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_notifications(Queue, Routing) ->
    bind_q_to_notifications(Queue, Routing, []).

-spec bind_q_to_notifications(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_notifications(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_NOTIFICATIONS, Options).

-spec bind_q_to_sysconf(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_sysconf(Queue, Routing) ->
    bind_q_to_sysconf(Queue, Routing, []).

-spec bind_q_to_sysconf(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_sysconf(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_SYSCONF, Options).

-spec bind_q_to_bookkeepers(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_bookkeepers(Queue, Routing) ->
    bind_q_to_bookkeepers(Queue, Routing, []).

-spec bind_q_to_bookkeepers(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_bookkeepers(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_BOOKKEEPERS, Options).

-spec bind_q_to_callctl(kz_term:ne_binary()) -> 'ok'.
bind_q_to_callctl(Queue) ->
    bind_q_to_callctl(Queue, Queue).

-spec bind_q_to_callctl(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_callctl(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLCTL).

-spec bind_q_to_callevt(kz_term:ne_binary()) -> 'ok'.
bind_q_to_callevt(Queue) ->
    bind_q_to_callevt(Queue, Queue).

-spec bind_q_to_callevt(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_callevt(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

-spec bind_q_to_resource(kz_term:ne_binary()) -> 'ok'.
bind_q_to_resource(Queue) -> bind_q_to_resource(Queue, <<"#">>).

-spec bind_q_to_resource(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_resource(Queue, Routing) -> bind_q_to_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

-spec bind_q_to_callmgr(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_callmgr(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

-spec bind_q_to_configuration(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_configuration(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_CONFIGURATION).

-spec bind_q_to_monitor(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_monitor(Queue, Routing) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_MONITOR).

-spec unbind_q_from_monitor(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
unbind_q_from_monitor(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_MONITOR).


-spec bind_q_to_conference(kz_term:ne_binary(), conf_routing_type()) -> 'ok'.
bind_q_to_conference(Queue, 'discovery') ->
    bind_q_to_conference(Queue, 'discovery', 'undefined');
bind_q_to_conference(Queue, 'command') ->
    bind_q_to_conference(Queue, 'command', <<"*">>);
bind_q_to_conference(Queue, 'event') ->
    bind_q_to_conference(Queue, 'event', <<"#">>);
bind_q_to_conference(Queue, 'config') ->
    bind_q_to_conference(Queue, 'config', <<"*">>).

-spec bind_q_to_conference(kz_term:ne_binary(), conf_routing_type(), kz_term:api_binary()) -> 'ok'.
bind_q_to_conference(Queue, 'discovery', _) ->
    bind_q_to_exchange(Queue, ?KEY_CONFERENCE_DISCOVERY, ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, 'event', EventKey) ->
    bind_q_to_exchange(Queue, <<?KEY_CONFERENCE_EVENT/binary, EventKey/binary>>, ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, 'command', ConfId) ->
    bind_q_to_exchange(Queue, conference_command_binding(ConfId), ?EXCHANGE_CONFERENCE);
bind_q_to_conference(Queue, 'config', ConfProfile) ->
    bind_q_to_exchange(Queue, <<?KEY_CONFERENCE_CONFIG/binary, ConfProfile/binary>>, ?EXCHANGE_CONFERENCE).

-spec conference_command_binding(kz_term:ne_binary()) -> kz_term:ne_binary().
conference_command_binding(ConferenceId) ->
    <<?KEY_CONFERENCE_COMMAND_L, ConferenceId/binary>>.

-spec bind_q_to_leader(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_leader(Queue, Bind) ->
    bind_q_to_exchange(Queue, Bind, ?EXCHANGE_LEADER).

-spec unbind_q_from_leader(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
unbind_q_from_leader(Queue, Bind) ->
    unbind_q_from_exchange(Queue, Bind, ?EXCHANGE_LEADER).

-spec bind_q_to_tasks(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_tasks(Queue, Routing) ->
    bind_q_to_tasks(Queue, Routing, []).

-spec bind_q_to_tasks(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_tasks(Queue, Routing, Options) ->
    bind_q_to_exchange(Queue, Routing, ?EXCHANGE_TASKS, Options).

-spec bind_q_to_exchange(kz_term:ne_binary(), binary(), kz_term:ne_binary()) -> 'ok'.
bind_q_to_exchange(Queue, _Routing, _Exchange) when not is_binary(Queue) ->
    {'error', 'invalid_queue_name'};
bind_q_to_exchange(Queue, Routing, Exchange) ->
    bind_q_to_exchange(Queue, Routing, Exchange, []).

-spec bind_q_to_exchange(kz_term:ne_binary(), binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q_to_exchange(Queue, Routing, Exchange, Options) ->
    QB = #'queue.bind'{queue = Queue %% what queue does the binding attach to?
                      ,exchange = Exchange %% what exchange does the binding attach to?
                      ,routing_key = Routing %% how does an exchange know a message should go to a bound queue?
                      ,nowait = ?P_GET('nowait', Options, 'false')
                      ,arguments = []
                      },
    kz_amqp_channel:command(QB).

%%------------------------------------------------------------------------------
%% @doc Unbind a Queue from an Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec unbind_q_from_conference(kz_term:ne_binary(), conf_routing_type()) ->
          'ok' | {'error', any()}.
unbind_q_from_conference(Queue, 'discovery') ->
    unbind_q_from_conference(Queue, 'discovery', 'undefined');
unbind_q_from_conference(Queue, 'command') ->
    unbind_q_from_conference(Queue, 'command', <<"*">>);
unbind_q_from_conference(Queue, 'config') ->
    unbind_q_from_conference(Queue, 'config', 'undefined');
unbind_q_from_conference(Queue, 'event') ->
    unbind_q_from_conference(Queue, 'event', <<"*">>).

-spec unbind_q_from_conference(kz_term:ne_binary(), conf_routing_type(), kz_term:api_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_conference(Queue, 'discovery', _) ->
    unbind_q_from_exchange(Queue, ?KEY_CONFERENCE_DISCOVERY, ?EXCHANGE_CONFERENCE);
unbind_q_from_conference(Queue, 'event', ConfId) ->
    unbind_q_from_conference(Queue, 'event', ConfId, <<"*">>);
unbind_q_from_conference(Queue, 'config', ConfProfile) ->
    unbind_q_from_exchange(Queue, <<?KEY_CONFERENCE_CONFIG/binary, ConfProfile/binary>>, ?EXCHANGE_CONFERENCE);
unbind_q_from_conference(Queue, 'command', ConfId) ->
    unbind_q_from_exchange(Queue, conference_command_binding(ConfId), ?EXCHANGE_CONFERENCE).

-spec unbind_q_from_conference(kz_term:ne_binary(), 'event', kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_conference(Queue, 'event', ConfId, CallId) ->
    unbind_q_from_exchange(Queue, <<?KEY_CONFERENCE_EVENT/binary, ConfId/binary, ".", CallId/binary>>, ?EXCHANGE_CONFERENCE).

-spec unbind_q_from_callctl(kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_callctl(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_CALLCTL).

-spec unbind_q_from_notifications(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_notifications(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_NOTIFICATIONS).

-spec unbind_q_from_sysconf(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_sysconf(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_SYSCONF).

-spec unbind_q_from_bookkeepers(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_bookkeepers(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_BOOKKEEPERS).

-spec unbind_q_from_resource(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_resource(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_RESOURCE).

-spec unbind_q_from_callevt(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_callevt(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLEVT).

-spec unbind_q_from_callmgr(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_callmgr(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CALLMGR).

-spec unbind_q_from_configuration(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_configuration(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_CONFIGURATION).

-spec unbind_q_from_targeted(kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_targeted(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_TARGETED).

-spec unbind_q_from_nodes(kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_nodes(Queue) ->
    unbind_q_from_exchange(Queue, Queue, ?EXCHANGE_NODES).

-spec unbind_q_from_kapps(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_kapps(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_KAPPS).

-spec unbind_q_from_presence(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_presence(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_PRESENCE).

-spec unbind_q_from_registrar(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_registrar(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_REGISTRAR).

-spec unbind_q_from_tasks(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_tasks(Queue, Routing) ->
    unbind_q_from_exchange(Queue, Routing, ?EXCHANGE_TASKS).


-spec unbind_q_from_exchange(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', any()}.
unbind_q_from_exchange(Queue, Routing, Exchange) ->
    UB = #'queue.unbind'{queue = Queue
                        ,exchange = Exchange
                        ,routing_key = Routing
                        ,arguments = []
                        },
    kz_amqp_channel:command(UB).

%%------------------------------------------------------------------------------
%% @doc Bind a Queue to an Exchange (with optional Routing Key).
%% Creates a consumer for a Queue.
%% @end
%%------------------------------------------------------------------------------

-spec basic_consume(kz_term:ne_binary()) -> 'ok' | {'error', any()}.
basic_consume(Queue) -> basic_consume(Queue, []).

-spec basic_consume(kz_term:ne_binary(), kz_term:proplist()) -> 'ok' | {'error', any()}.
basic_consume(Queue, Options) ->
    BC = #'basic.consume'{queue = Queue
                         ,consumer_tag = <<>>
                         ,no_local = ?P_GET('no_local', Options, 'false')
                         ,no_ack = ?P_GET('no_ack', Options, 'true')
                         ,exclusive = ?P_GET('exclusive', Options, 'true')
                         ,nowait = ?P_GET('nowait', Options, 'false')
                         },
    kz_amqp_channel:command(BC).

%%------------------------------------------------------------------------------
%% @doc This method cancels a consumer. This does not affect already delivered messages,
%% but it does mean the server will not send any more messages for that consumer.
%% @end
%%------------------------------------------------------------------------------

-spec basic_cancel() -> 'ok'.
basic_cancel() -> kz_amqp_channel:command(#'basic.cancel'{}).

-spec basic_cancel(kz_term:ne_binary()) -> 'ok'.
basic_cancel(ConsumerTag) -> kz_amqp_channel:command(#'basic.cancel'{consumer_tag=ConsumerTag}).

%%------------------------------------------------------------------------------
%% @doc This method sets confirmation from server.
%% @end
%%------------------------------------------------------------------------------

-spec confirm_select() -> 'ok'.
confirm_select() -> kz_amqp_channel:command(#'confirm.select'{}).

%%------------------------------------------------------------------------------
%% @doc This method sets flow control.
%% @end
%%------------------------------------------------------------------------------

-spec flow_control() -> 'ok'.
flow_control() -> flow_control('true').

-spec flow_control(boolean()) -> 'ok'.
flow_control(Active) ->
    kz_amqp_channel:command(#'channel.flow'{active=Active}).

-spec flow_control_reply(boolean()) -> 'ok'.
flow_control_reply(Active) ->
    kz_amqp_channel:command(#'channel.flow_ok'{active=Active}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec access_request() -> #'access.request'{}.
access_request() -> access_request([]).

-spec access_request(kz_term:proplist()) -> #'access.request'{}.
access_request(Options) ->
    #'access.request'{realm = ?P_GET('realm', Options, <<"/data">>)
                     ,exclusive = ?P_GET('exclusive', Options, 'false')
                     ,passive = ?P_GET('passive', Options, 'true')
                     ,active = ?P_GET('active', Options, 'true')
                     ,write = ?P_GET('write', Options, 'true')
                     ,read = ?P_GET('read', Options, 'true')
                     }.

%%------------------------------------------------------------------------------
%% @doc Determines if the content is flagged as type JSON.
%% @end
%%------------------------------------------------------------------------------
-spec is_json(amqp_basic()) -> boolean().
is_json(#'P_basic'{content_type=CT}) -> CT =:= ?DEFAULT_CONTENT_TYPE.

%%------------------------------------------------------------------------------
%% @doc When sent by the client, this method acknowledges one or more messages
%% delivered via the Deliver or `Get-Ok' methods.
%% @end
%%------------------------------------------------------------------------------
-spec basic_ack(integer() | #'basic.deliver'{}) -> 'ok'.
basic_ack(#'basic.deliver'{delivery_tag=DTag}) -> basic_ack(DTag);
basic_ack(DTag) -> kz_amqp_channel:command(#'basic.ack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @doc  Reject one or more incoming messages.
%%
%% <div class="notice">This method is a RabbitMQ-specific extension of AMQP.</div>
%% @end
%%------------------------------------------------------------------------------
-spec basic_nack(integer() | #'basic.deliver'{}) -> 'ok'.
basic_nack(#'basic.deliver'{delivery_tag=DTag}) -> basic_nack(DTag);
basic_nack(DTag) -> kz_amqp_channel:command(#'basic.nack'{delivery_tag=DTag}).

%%------------------------------------------------------------------------------
%% @doc Determine if the AMQP host is currently reachable.
%% @end
%%------------------------------------------------------------------------------
-spec is_host_available() -> boolean().
is_host_available() -> kz_amqp_connections:is_available().

%%------------------------------------------------------------------------------
%% @doc Specify quality of service.
%%
%%
%% global: https://www.rabbitmq.com/amqp-0-9-1-reference.html#basic.qos.global
%% global=false applies QoS settings to new consumers on the channel (existing are unaffected).
%% global=true applies per-channel
%% @end
%%------------------------------------------------------------------------------
-spec basic_qos(non_neg_integer()) -> 'ok'.
basic_qos(PreFetch) when is_integer(PreFetch) ->
    kz_amqp_channel:command(#'basic.qos'{prefetch_count = PreFetch
                                        ,prefetch_size = 0
                                        ,global = 'false'
                                        }).

%%------------------------------------------------------------------------------
%% @doc Encode a key so characters like dot won't interfere with routing separator.
%% @end
%%------------------------------------------------------------------------------
-spec encode(binary()) -> binary().
encode(<<>>) -> <<>>;
encode(<<"*">>) -> <<"*">>;
encode(<<"#">>) -> <<"#">>;
encode(Bin) -> << <<(encode_char(B))/binary>> || <<B>> <= Bin >>.

-define(HI4(C), (C band 2#11110000) bsr 4).
-define(LO4(C), (C band 2#00001111)).

encode_char(C) when ?KEY_SAFE(C) -> <<C>>;
encode_char($\s) -> <<$+>>;
encode_char($.) -> <<$%, $2, $E>>;
encode_char(C) ->
    Hi4 = ?HI4(C),
    Lo4 = ?LO4(C),
    <<$%, (hexint(Hi4)), (hexint(Lo4))>>.

hexint(C) when C < 10 -> ($0 + C);
hexint(C) when C < 16 -> ($A + (C - 10)).

-spec split_routing_key(binary()) -> {kz_term:api_pid(), binary()}.
split_routing_key(<<"consumer://", _/binary>> = RoutingKey) ->
    Size = byte_size(RoutingKey),
    {Start, _} = lists:last(binary:matches(RoutingKey, <<"/">>)),
    {list_to_pid(kz_term:to_list(binary:part(RoutingKey, 11, Start - 11)))
    ,binary:part(RoutingKey, Start + 1, Size - Start - 1)
    };
split_routing_key(RoutingKey) ->
    {'undefined', RoutingKey}.
