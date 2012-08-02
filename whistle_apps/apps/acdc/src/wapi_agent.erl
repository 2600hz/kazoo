%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Bindings and JSON APIs for dealing with agents, as part of ACDc
%%%
%%% Status Updates:
%%%   If an agent signs in, out, goes on break, comes back from break,
%%%   publish to all processes managing that agent
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_agent).

-export([status_update/1
        ]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([publish_status_update/1, publish_status_update/2
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(STATUS_UPDATE_KEY, "agent.status_update.").

-define(STATUS_UPDATE_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"New-Status">>]).
-define(OPTIONAL_STATUS_UPDATE_HEADERS, []).
-define(STATUS_UPDATE_VALUES, [{<<"New-Status">>, [<<"signed_in">>, <<"signed_off">>
                                                   ,<<"away">>, <<"returned">>
                                                  ]}
                               ,{<<"Event-Category">>, <<"agents">>}
                               ,{<<"Event-Name">>, <<"status_update">>}
                              ]).
-define(STATUS_UPDATE_TYPES, []).

-spec status_update/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
status_update(Props) when is_list(Props) ->
    case status_update_v(Props) of
        true -> wh_api:build_message(Props, ?STATUS_UPDATE_HEADERS, ?OPTIONAL_STATUS_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for status_update"}
    end;
status_update(JObj) ->
    status_update(wh_json:to_proplist(JObj)).

-spec status_update_v/1 :: (api_terms()) -> boolean().
status_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STATUS_UPDATE_HEADERS, ?STATUS_UPDATE_VALUES, ?STATUS_UPDATE_TYPES);
status_update_v(JObj) ->
    status_update_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Q, Props) ->
    AgentId = props:get_value(agent_id, Props, <<"*">>),
    AgentDb = props:get_value(agent_db, Props, <<"*">>),

    amqp_util:whapps_exchange(),
    amqp_util:bind_q_to_whapps(Q, status_routing_key(AgentDb, AgentId)).

-spec unbind_q/2 :: (binary(), proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    AgentId = props:get_value(agent_id, Props, <<"*">>),
    AgentDb = props:get_value(agent_db, Props, <<"*">>),

    amqp_util:unbind_q_from_whapps(Q, status_routing_key(AgentDb, AgentId)).

-spec status_routing_key/1 :: (wh_json:json_object() | wh_proplist()) -> ne_binary().
-spec status_routing_key/2 :: (ne_binary(), ne_binary()) -> ne_binary().
status_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    Db = props:get_value(<<"Agent-DB">>, Props, <<"*">>),
    status_routing_key(Db, Id);
status_routing_key(JObj) ->
    Id = wh_json:get_value(<<"Agent-ID">>, JObj, <<"*">>),
    Db = wh_json:get_value(<<"Agent-DB">>, JObj, <<"*">>),
    status_routing_key(Db, Id).

status_routing_key(Db, Id) ->
    <<?STATUS_UPDATE_KEY, Db/binary, ".", Id/binary>>.

-spec publish_status_update/1 :: (api_terms()) -> 'ok'.
-spec publish_status_update/2 :: (api_terms(), binary()) -> 'ok'.
publish_status_update(JObj) ->
    publish_status_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_status_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?STATUS_UPDATE_VALUES, fun status_update/1),
    amqp_util:callmgr_publish(Payload, ContentType, status_routing_key(API)).
