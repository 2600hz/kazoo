%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Handles authorization requests, responses, queue bindings AMQP API.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_authz).

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

-include_lib("kz_amqp_util.hrl").

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
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Application-Vars">>
                                    ,<<"Custom-Channel-Vars">>
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
                         ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                         ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                         ,{<<"Usage">>, fun kz_json:is_json_object/1}
                         ]).
%% TODO: allow only kz_term:ne_binaries()
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
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}]).
-define(BALANCE_CHECK_RESP_TYPES, [{<<"Balances">>, fun kz_json:is_json_object/1}]).

%%------------------------------------------------------------------------------
%% @doc Authorization Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error..
%% @end
%%------------------------------------------------------------------------------
-spec authz_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_req(Prop) when is_list(Prop) ->
    case authz_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
authz_req(JObj) ->
    authz_req(kz_json:to_proplist(JObj)).

-spec authz_req_v(kz_term:api_terms()) -> boolean().
authz_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AUTHZ_REQ_HEADERS, ?AUTHZ_REQ_VALUES, ?AUTHZ_REQ_TYPES);
authz_req_v(JObj) ->
    authz_req_v(kz_json:to_proplist(JObj)).

-spec balance_check_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_check_req(Prop) when is_list(Prop) ->
    case balance_check_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?BALANCE_CHECK_REQ_HEADERS, ?OPTIONAL_BALANCE_CHECK_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
balance_check_req(JObj) ->
    balance_check_req(kz_json:to_proplist(JObj)).

-spec balance_check_req_v(kz_term:api_terms()) -> boolean().
balance_check_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BALANCE_CHECK_REQ_HEADERS, ?BALANCE_CHECK_REQ_VALUES, ?BALANCE_CHECK_REQ_TYPES);
balance_check_req_v(JObj) ->
    balance_check_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Authorization Response.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec authz_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_resp(Prop) when is_list(Prop) ->
    case authz_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
authz_resp(JObj) ->
    authz_resp(kz_json:to_proplist(JObj)).

-spec authz_resp_v(kz_term:api_terms()) -> boolean().
authz_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
authz_resp_v(JObj) ->
    authz_resp_v(kz_json:to_proplist(JObj)).

-spec balance_check_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_check_resp(Prop) when is_list(Prop) ->
    case balance_check_resp_v(Prop) of
        'true' ->
            kz_api:build_message(Prop, ?BALANCE_CHECK_RESP_HEADERS, ?OPTIONAL_BALANCE_CHECK_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
balance_check_resp(JObj) ->
    balance_check_resp(kz_json:to_proplist(JObj)).

-spec balance_check_resp_v(kz_term:api_terms()) -> boolean().
balance_check_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BALANCE_CHECK_RESP_HEADERS, ?BALANCE_CHECK_RESP_VALUES, ?BALANCE_CHECK_RESP_TYPES);
balance_check_resp_v(JObj) ->
    balance_check_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz `gen_listeners'.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, <<"authz.*">>);
bind_to_q(Q, ['authorize'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(Q, ['balance_check'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, ?KEY_BALANCE_CHECK_REQ),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, <<"authz.*">>);
unbind_q_from(Q, ['authorize'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
unbind_q_from(Q, ['balance_check'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, ?KEY_BALANCE_CHECK_REQ),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_authz_req(kz_term:api_terms()) -> 'ok'.
publish_authz_req(JObj) ->
    publish_authz_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_authz_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_authz_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?AUTHZ_REQ_VALUES, fun authz_req/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ).

-spec publish_authz_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_authz_resp(Queue, JObj) ->
    publish_authz_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_authz_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_authz_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun authz_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_balance_check_req(kz_term:api_terms()) -> 'ok'.
publish_balance_check_req(JObj) ->
    publish_balance_check_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_check_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_check_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?BALANCE_CHECK_REQ_VALUES, fun balance_check_req/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_BALANCE_CHECK_REQ).

-spec publish_balance_check_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_balance_check_resp(Queue, JObj) ->
    publish_balance_check_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_check_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_check_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?BALANCE_CHECK_RESP_VALUES, fun balance_check_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec broadcast_authz_resp(kz_term:api_terms()) -> 'ok'.
broadcast_authz_resp(JObj) ->
    broadcast_authz_resp(JObj, ?DEFAULT_CONTENT_TYPE).

-spec broadcast_authz_resp(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
broadcast_authz_resp(Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun authz_resp/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_BROADCAST).
