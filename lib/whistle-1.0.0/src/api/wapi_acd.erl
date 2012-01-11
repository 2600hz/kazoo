%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(wapi_acd).

-export([agent_online/1, agent_online_v/1]).
-export([agent_offline/1, agent_offline_v/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_agent_online/1, publish_agent_online/2]).
-export([publish_agent_offline/1, publish_agent_offline/2]).

-include("../wh_api.hrl").

-define(AGENT_ONLINE_HEADERS, [<<"Agent-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_AGENT_ONLINE_HEADERS, [<<"Skills">>]).
-define(AGENT_ONLINE_VALUES, [{<<"Event-Category">>, <<"acd">>}
                              ,{<<"Event-Name">>, <<"agent_online">>}
                              ]).
-define(AGENT_ONLINE_TYPES, []).

-define(AGENT_OFFLINE_HEADERS, [<<"Agent-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_AGENT_OFFLINE_HEADERS, []).
-define(AGENT_OFFLINE_VALUES, [{<<"Event-Category">>, <<"acd">>}
                              ,{<<"Event-Name">>, <<"agent_offline">>}
                              ]).
-define(AGENT_OFFLINE_TYPES, []).

-spec agent_online/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
agent_online(Prop) when is_list(Prop) ->
        case agent_online_v(Prop) of
            true -> wh_api:build_message(Prop, ?AGENT_ONLINE_HEADERS, ?OPTIONAL_AGENT_ONLINE_HEADERS);
            false -> {error, "Proplist failed validation for agent_online"}
    end;
agent_online(JObj) ->
    agent_online(wh_json:to_proplist(JObj)).

-spec agent_online_v/1 :: (api_terms()) -> boolean().
agent_online_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AGENT_ONLINE_HEADERS, ?AGENT_ONLINE_VALUES, ?AGENT_ONLINE_TYPES);
agent_online_v(JObj) ->
    agent_online_v(wh_json:to_proplist(JObj)).

-spec agent_offline/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
agent_offline(Prop) when is_list(Prop) ->
        case agent_offline_v(Prop) of
            true -> wh_api:build_message(Prop, ?AGENT_OFFLINE_HEADERS, ?OPTIONAL_AGENT_OFFLINE_HEADERS);
            false -> {error, "Proplist failed validation for agent_offline"}
    end;
agent_offline(JObj) ->
    agent_offline(wh_json:to_proplist(JObj)).

-spec agent_offline_v/1 :: (api_terms()) -> boolean().
agent_offline_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AGENT_OFFLINE_HEADERS, ?AGENT_OFFLINE_VALUES, ?AGENT_OFFLINE_TYPES);
agent_offline_v(JObj) ->
    agent_offline_v(wh_json:to_proplist(JObj)).


bind_q(Q, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, <<"acd.*">>).

unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_callmgr(Q, <<"acd.*">>).

-spec publish_agent_online/1 :: (api_terms()) -> 'ok'.
-spec publish_agent_online/2 :: (api_terms(), binary()) -> 'ok'.
publish_agent_online(JObj) ->
    publish_agent_online(JObj, ?DEFAULT_CONTENT_TYPE).
publish_agent_online(Agent_Online, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Agent_Online, ?AGENT_ONLINE_VALUES, fun ?MODULE:agent_online/1),
    amqp_util:callmgr_publish(Payload, ContentType, <<"acd.agent.online">>).

-spec publish_agent_offline/1 :: (api_terms()) -> 'ok'.
-spec publish_agent_offline/2 :: (api_terms(), binary()) -> 'ok'.
publish_agent_offline(JObj) ->
    publish_agent_offline(JObj, ?DEFAULT_CONTENT_TYPE).
publish_agent_offline(Agent_Offline, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Agent_Offline, ?AGENT_OFFLINE_VALUES, fun ?MODULE:agent_offline/1),
    amqp_util:callmgr_publish(Payload, ContentType, <<"acd.agent.offline">>).
