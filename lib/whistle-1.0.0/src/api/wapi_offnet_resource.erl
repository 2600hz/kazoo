%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_offnet_resource).

-export([req/1, req_v/1, publish_req/1, publish_req/2, bind_q/2, unbind_q/1, unbind_q/2]).

-include("../wh_api.hrl").

%% Offnet Resource Request
-define(OFFNET_RESOURCE_REQ_HEADERS, [<<"Call-ID">>, <<"Resource-Type">>, <<"To-DID">>
                                      ,<<"Account-ID">>, <<"Control-Queue">>, <<"Application-Name">>
                                     ]).
-define(OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS, [<<"Timeout">>, <<"Ignore-Early-Media">>, <<"Flags">>, <<"Media">>
                                               ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
                                               ,<<"Emergency-Caller-ID-Name">>, <<"Emergency-Caller-ID-Number">>
                                               ,<<"Ringback">>, <<"SIP-Headers">>, <<"Custom-Channel-Vars">>
                                               ,<<"Hold-Media">>, <<"Presence-ID">>
                                              ]).
-define(OFFNET_RESOURCE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                     ,{<<"Event-Name">>, <<"offnet_req">>}
                                     ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>]}
                                     ,{<<"Application-Name">>, [<<"bridge">>]}
                                     ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                                    ]).
-define(OFFNET_RESOURCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                                    ,{<<"Account-ID">>, fun is_binary/1}
                                    ,{<<"Control-Queue">>, fun is_binary/1}
                                    ,{<<"To-DID">>, fun is_binary/1}
                                    ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                                    ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
                                    ,{<<"Flags">>, fun is_list/1}
                                   ]).

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for offnet_resource_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    _ = amqp_util:resource_exchange(),
    amqp_util:bind_q_to_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_resource(Queue).
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_resource(Queue).

-spec publish_req/1 :: (api_terms()) -> 'ok'.
-spec publish_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?OFFNET_RESOURCE_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:offnet_resource_publish(Payload, ContentType).
