%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
         ,balance_check_req/1, balance_check_req_v/1
         ,balance_check_resp/1, balance_check_resp_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_authz_req/1, publish_authz_req/2
         ,publish_authz_resp/2, publish_authz_resp/3
         ,broadcast_authz_resp/1, broadcast_authz_resp/2
         ,publish_balance_check_req/1, publish_balance_check_req/2
         ,publish_balance_check_resp/2, publish_balance_check_resp/3
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(EVENT_CATEGORY, <<"authz">>).
-define(KEY_AUTHZ_REQ, <<"authz.authorize">>).
-define(KEY_AUTHZ_BROADCAST, <<"authz.authorize.broadcast">>).
-define(KEY_BALANCE_CHECK_REQ, <<"authz.balance_check">>).

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"Call-Direction">>
                            ,<<"Call-ID">>
                            ,<<"Caller-ID-Name">>
                            ,<<"Caller-ID-Number">>
                            ,<<"From">>
                            ,<<"Request">>
                            ,<<"To">>
                           ]).
-define(BALANCE_CHECK_REQ_HEADERS, [<<"Accounts">>]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>
                                     ,<<"From-Network-Addr">>
                                     ,<<"From-Network-Port">>
                                     ,<<"Other-Leg-Call-ID">>
                                     ,<<"From-Network-Addr">>
                                     ,<<"From-Network-Port">>
                                     ,<<"Switch-Hostname">>
                                    ]).
-define(OPTIONAL_BALANCE_CHECK_REQ_HEADERS, []).
-define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"authz_req">>}
                          ]).
-define(BALANCE_CHECK_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                   ,{<<"Event-Name">>, <<"balance_check_req">>}
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
%% TODO: allow only ne_binaries()
-define(BALANCE_CHECK_REQ_TYPES, [{<<"Accounts">>, fun is_list/1}]).

%% Authorization Responses
-define(AUTHZ_RESP_HEADERS, [<<"Call-ID">>, <<"Is-Authorized">>]).
-define(BALANCE_CHECK_RESP_HEADERS, [<<"Balances">>]).
-define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Account-ID">>, <<"Account-Billing">>
                                      ,<<"Reseller-ID">>, <<"Reseller-Billing">>
                                      ,<<"Custom-Channel-Vars">>, <<"Call-Direction">>
                                      ,<<"Soft-Limit">>, <<"Other-Leg-Call-ID">>
                                     ]).
-define(OPTIONAL_BALANCE_CHECK_RESP_HEADERS, []).
-define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, <<"authz_resp">>}
                            ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
                            ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                           ]).
-define(BALANCE_CHECK_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                    ,{<<"Event-Name">>, <<"balance_check_resp">>}
                                   ]).
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).
-define(BALANCE_CHECK_RESP_TYPES, [{<<"Balances">>, fun wh_json:is_json_object/1}]).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec authz_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_req(Prop) when is_list(Prop) ->
    case authz_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
authz_req(JObj) ->
    authz_req(wh_json:to_proplist(JObj)).

-spec authz_req_v(api_terms()) -> boolean().
authz_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_REQ_HEADERS, ?AUTHZ_REQ_VALUES, ?AUTHZ_REQ_TYPES);
authz_req_v(JObj) ->
    authz_req_v(wh_json:to_proplist(JObj)).

-spec balance_check_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_check_req(Prop) when is_list(Prop) ->
    case balance_check_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?BALANCE_CHECK_REQ_HEADERS, ?OPTIONAL_BALANCE_CHECK_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
balance_check_req(JObj) ->
    balance_check_req(wh_json:to_proplist(JObj)).

-spec balance_check_req_v(api_terms()) -> boolean().
balance_check_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BALANCE_CHECK_REQ_HEADERS, ?BALANCE_CHECK_REQ_VALUES, ?BALANCE_CHECK_REQ_TYPES);
balance_check_req_v(JObj) ->
    balance_check_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec authz_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_resp(Prop) when is_list(Prop) ->
    case authz_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
authz_resp(JObj) ->
    authz_resp(wh_json:to_proplist(JObj)).

-spec authz_resp_v(api_terms()) -> boolean().
authz_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
authz_resp_v(JObj) ->
    authz_resp_v(wh_json:to_proplist(JObj)).

-spec balance_check_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_check_resp(Prop) when is_list(Prop) ->
    case balance_check_resp_v(Prop) of
        'true' -> 
            wh_api:build_message(Prop, ?BALANCE_CHECK_RESP_HEADERS, ?OPTIONAL_BALANCE_CHECK_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
balance_check_resp(JObj) ->
    balance_check_resp(wh_json:to_proplist(JObj)).

-spec balance_check_resp_v(api_terms()) -> boolean().
balance_check_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BALANCE_CHECK_RESP_HEADERS, ?BALANCE_CHECK_RESP_VALUES, ?BALANCE_CHECK_RESP_TYPES);
balance_check_resp_v(JObj) ->
    balance_check_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
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
bind_to_q(Q, ['balance_check'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_BALANCE_CHECK_REQ),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
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
unbind_q_from(Q, ['balance_check'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_BALANCE_CHECK_REQ),
    unbind_q_from(Q, T);
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
-spec publish_authz_req(api_terms()) -> 'ok'.
-spec publish_authz_req(api_terms(), ne_binary()) -> 'ok'.
publish_authz_req(JObj) ->
    publish_authz_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?AUTHZ_REQ_VALUES, fun ?MODULE:authz_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ).

-spec publish_authz_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_authz_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_authz_resp(Queue, JObj) ->
    publish_authz_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_balance_check_req(api_terms()) -> 'ok'.
-spec publish_balance_check_req(api_terms(), ne_binary()) -> 'ok'.
publish_balance_check_req(JObj) ->
    publish_balance_check_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_balance_check_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?BALANCE_CHECK_REQ_VALUES, fun ?MODULE:balance_check_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_BALANCE_CHECK_REQ).

-spec publish_balance_check_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_balance_check_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_balance_check_resp(Queue, JObj) ->
    publish_balance_check_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_balance_check_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?BALANCE_CHECK_RESP_VALUES, fun ?MODULE:balance_check_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec broadcast_authz_resp(api_terms()) -> 'ok'.
-spec broadcast_authz_resp(api_terms(), ne_binary()) -> 'ok'.
broadcast_authz_resp(JObj) ->
    broadcast_authz_resp(JObj, ?DEFAULT_CONTENT_TYPE).
broadcast_authz_resp(Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_BROADCAST).
