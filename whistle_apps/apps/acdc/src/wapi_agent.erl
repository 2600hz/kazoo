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
         ,sync_req/1
         ,sync_resp/1
        ]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([publish_status_update/1, publish_status_update/2
         ,publish_sync_req/1, publish_sync_req/2
         ,publish_sync_resp/2, publish_sync_resp/3
        ]).

-include_lib("whistle/include/wh_api.hrl").

%%------------------------------------------------------------------------------
%% Status Update
%%   Agent processes are updated with what the real agent is up to via this API.
%%   Most probably, the callflow action for agent login/logout/away/back will
%%   send these API messages
%%------------------------------------------------------------------------------
-define(STATUS_UPDATE_KEY, "agent.status_update.").

-define(STATUS_UPDATE_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"New-Status">>]).
-define(OPTIONAL_STATUS_UPDATE_HEADERS, []).
-define(STATUS_UPDATE_VALUES, [{<<"New-Status">>, [<<"signed_in">>, <<"signed_off">>
                                                   ,<<"away">>, <<"returned">>
                                                  ]}
                               ,{<<"Event-Category">>, <<"agent">>}
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

%%------------------------------------------------------------------------------
%% Agent Sync Req
%%   When an agent process starts up, it will ask for other agent processes
%%   handling the same agent what state they are in. 0 or more will respond, and
%%   the calling agent process will join them in that state.
%%   After a timeout period of no responses, the agent will assume its alone and
%%   will transition to the ready state.
%%------------------------------------------------------------------------------
-define(SYNC_REQ_KEY, "agent.sync_req.").

-define(SYNC_REQ_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(OPTIONAL_SYNC_REQ_HEADERS, [<<"Process-ID">>]).
-define(SYNC_REQ_VALUES, [{<<"Event-Category">>, <<"agent">>}
                          ,{<<"Event-Name">>, <<"sync_req">>}
                         ]).
-define(SYNC_REQ_TYPES, []).

-spec sync_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
sync_req(Props) when is_list(Props) ->
    case sync_req_v(Props) of
        true -> wh_api:build_message(Props, ?SYNC_REQ_HEADERS, ?OPTIONAL_SYNC_REQ_HEADERS);
        false -> {error, "Proplist failed validation for sync_req"}
    end;
sync_req(JObj) ->
    sync_req(wh_json:to_proplist(JObj)).

-spec sync_req_v/1 :: (api_terms()) -> boolean().
sync_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_REQ_HEADERS, ?SYNC_REQ_VALUES, ?SYNC_REQ_TYPES);
sync_req_v(JObj) ->
    sync_req_v(wh_json:to_proplist(JObj)).

-spec sync_req_routing_key/1 :: (wh_json:json_object() | wh_proplist()) -> ne_binary().
-spec sync_req_routing_key/2 :: (ne_binary(), ne_binary()) -> ne_binary().
sync_req_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    Db = props:get_value(<<"Agent-DB">>, Props, <<"*">>),
    sync_req_routing_key(Db, Id);
sync_req_routing_key(JObj) ->
    Id = wh_json:get_value(<<"Agent-ID">>, JObj, <<"*">>),
    Db = wh_json:get_value(<<"Agent-DB">>, JObj, <<"*">>),
    sync_req_routing_key(Db, Id).

sync_req_routing_key(Db, Id) ->
    <<?SYNC_REQ_KEY, Db/binary, ".", Id/binary>>.

%% And the response
-define(SYNC_RESP_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"Status">>]).
-define(OPTIONAL_SYNC_RESP_HEADERS, [<<"Call-ID">>, <<"Time-Left">>, <<"Process-ID">>]).
-define(SYNC_RESP_VALUES, [{<<"Event-Category">>, <<"agent">>}
                           ,{<<"Event-Name">>, <<"sync_resp">>}
                           ,{<<"Status">>, [<<"init">>, <<"ready">>, <<"waiting">>, <<"ringing">>
                                           ,<<"answered">>, <<"wrapup">>, <<"paused">>
                                           ]}
                         ]).
-define(SYNC_RESP_TYPES, []).

-spec sync_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
sync_resp(Props) when is_list(Props) ->
    case sync_resp_v(Props) of
        true -> wh_api:build_message(Props, ?SYNC_RESP_HEADERS, ?OPTIONAL_SYNC_RESP_HEADERS);
        false -> {error, "Proplist failed validation for sync_resp"}
    end;
sync_resp(JObj) ->
    sync_resp(wh_json:to_proplist(JObj)).

-spec sync_resp_v/1 :: (api_terms()) -> boolean().
sync_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_RESP_HEADERS, ?SYNC_RESP_VALUES, ?SYNC_RESP_TYPES);
sync_resp_v(JObj) ->
    sync_resp_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Bind/Unbind the queue as appropriate
%%------------------------------------------------------------------------------

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Q, Props) ->
    AgentId = props:get_value(agent_id, Props, <<"*">>),
    AgentDb = props:get_value(agent_db, Props, <<"*">>),

    amqp_util:whapps_exchange(),
    amqp_util:bind_q_to_whapps(Q, status_routing_key(AgentDb, AgentId)),
    amqp_util:bind_q_to_whapps(Q, sync_req_routing_key(AgentDb, AgentId)).

-spec unbind_q/2 :: (binary(), proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    AgentId = props:get_value(agent_id, Props, <<"*">>),
    AgentDb = props:get_value(agent_db, Props, <<"*">>),

    amqp_util:unbind_q_from_whapps(Q, status_routing_key(AgentDb, AgentId)).

%%------------------------------------------------------------------------------
%% Publishers for convenience
%%------------------------------------------------------------------------------

-spec publish_status_update/1 :: (api_terms()) -> 'ok'.
-spec publish_status_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_status_update(JObj) ->
    publish_status_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_status_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?STATUS_UPDATE_VALUES, fun status_update/1),
    amqp_util:whapps_publish(Payload, ContentType, status_routing_key(API)).

-spec publish_sync_req/1 :: (api_terms()) -> 'ok'.
-spec publish_sync_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_sync_req(JObj) ->
    publish_sync_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync_req(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?SYNC_REQ_VALUES, fun sync_req/1),
    amqp_util:whapps_publish(Payload, ContentType, sync_req_routing_key(API)).

-spec publish_sync_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_sync_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_sync_resp(Q, JObj) ->
    publish_sync_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync_resp(Q, API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?SYNC_RESP_VALUES, fun sync_resp/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).
