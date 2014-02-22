%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_authz).

-export([authz_req/1, authz_req_v/1
         ,authz_resp/1, authz_resp_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_authz_req/1, publish_authz_req/2
         ,publish_authz_resp/2, publish_authz_resp/3
         ,broadcast_authz_resp/1, broadcast_authz_resp/2
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(EVENT_CATEGORY, <<"authz">>).
-define(KEY_AUTHZ_REQ, <<"authz.authorize">>).
-define(KEY_AUTHZ_BROADCAST, <<"authz.authorize.broadcast">>).

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"To">>, <<"From">>, <<"Request">>
                                ,<<"Call-ID">>, <<"Call-Direction">>
                           ]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>, <<"Switch-Hostname">>
                                     ,<<"Other-Leg-Call-ID">>
                                    ]).
-define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"authz_req">>}
                          ]).
-define(AUTHZ_REQ_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Call-ID">>, fun is_binary/1}
                          ,{<<"Account-ID">>, fun is_binary/1}
                          ,{<<"Caller-ID-Name">>, fun is_binary/1}
                          ,{<<"Caller-ID-Number">>, fun is_binary/1}
                          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                          ,{<<"Usage">>, fun wh_json:is_json_object/1}
                         ]).

%% Authorization Responses
-define(AUTHZ_RESP_HEADERS, [<<"Call-ID">>, <<"Is-Authorized">>]).
-define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Account-ID">>, <<"Account-Billing">>
                                      ,<<"Reseller-ID">>, <<"Reseller-Billing">>
                                      ,<<"Custom-Channel-Vars">>, <<"Call-Direction">>
                                      ,<<"Soft-Limit">>, <<"Other-Leg-Call-ID">>
                                     ]).
-define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, <<"authz_resp">>}
                            ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
                            ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                           ]).
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec authz_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_req(Prop) when is_list(Prop) ->
    case authz_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
authz_req(JObj) ->
    authz_req(wh_json:to_proplist(JObj)).

-spec authz_req_v/1 :: (api_terms()) -> boolean().
authz_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_REQ_HEADERS, ?AUTHZ_REQ_VALUES, ?AUTHZ_REQ_TYPES);
authz_req_v(JObj) ->
    authz_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec authz_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_resp(Prop) when is_list(Prop) ->
    case authz_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
authz_resp(JObj) ->
    authz_resp(wh_json:to_proplist(JObj)).

-spec authz_resp_v/1 :: (api_terms()) -> boolean().
authz_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
authz_resp_v(JObj) ->
    authz_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.

bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, <<"authz.*">>);
bind_to_q(Q, ['authorize'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, <<"authz.*">>);
unbind_q_from(Q, ['authorize'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
unbind_q_from(_Q, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_authz_req/1 :: (api_terms()) -> 'ok'.
-spec publish_authz_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_authz_req(JObj) ->
    publish_authz_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?AUTHZ_REQ_VALUES, fun ?MODULE:authz_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ).

-spec publish_authz_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_authz_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_authz_resp(Queue, JObj) ->
    publish_authz_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec broadcast_authz_resp/1 :: (api_terms()) -> 'ok'.
-spec broadcast_authz_resp/2 :: (api_terms(), ne_binary()) -> 'ok'.
broadcast_authz_resp(JObj) ->
    broadcast_authz_resp(JObj, ?DEFAULT_CONTENT_TYPE).
broadcast_authz_resp(Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_BROADCAST).
