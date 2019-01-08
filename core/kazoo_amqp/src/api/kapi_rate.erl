%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handles authorization requests, responses, queue bindings.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_rate).

-export([req/1, req_v/1
        ,resp/1, resp_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_req/1, publish_req/2
        ,publish_resp/2, publish_resp/3
        ,broadcast_resp/1, broadcast_resp/2
        ]).

-type req()  :: kz_json:object().
-type resp() :: kz_json:object().

-export_type([req/0
             ,resp/0
             ]).

-include_lib("amqp_util.hrl").

-define(EVENT_CATEGORY, <<"rate">>).
-define(KEY_RATE_REQ, <<"rate.req">>).
-define(KEY_RATE_BROADCAST, <<"rate.resp.broadcast">>).

%% AMQP fields for Rating Request
-define(RATE_REQ_HEADERS, [<<"To-DID">>]).
-define(OPTIONAL_RATE_REQ_HEADERS, [<<"Account-ID">>
                                   ,<<"Call-ID">>
                                   ,<<"Direction">>
                                   ,<<"From-DID">>
                                   ,<<"Options">>
                                   ,<<"Outbound-Flags">>
                                   ,<<"Ratedeck-ID">>
                                   ,<<"Resource-ID">>
                                   ,<<"Resource-Type">>
                                   ,<<"Send-Empty">>
                                   ,<<"Authorizing-Type">>
                                   ]).
-define(RATE_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                         ,{<<"Event-Name">>, <<"req">>}
                         ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                         ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>, <<"sms">>]}
                         ]).
-define(RATE_REQ_TYPES, [{<<"Options">>, fun is_list/1}
                        ,{<<"Send-Empty">>, fun kz_term:is_boolean/1}
                        ]).

%% AMQP fields for Rating Response
-define(RATE_RESP_HEADERS, []).
-define(OPTIONAL_RATE_RESP_HEADERS, [<<"Base-Cost">>
                                    ,<<"Call-ID">>
                                    ,<<"Discount-Percentage">>
                                    ,<<"Prefix">>
                                    ,<<"Pvt-Cost">>
                                    ,<<"Rate">>
                                    ,<<"Rate-Description">>
                                    ,<<"Rate-Increment">>
                                    ,<<"Rate-Minimum">>
                                    ,<<"Rate-Name">>
                                    ,<<"Rate-NoCharge-Time">>
                                    ,<<"Rate-Version">>
                                    ,<<"Ratedeck-ID">>
                                    ,<<"Surcharge">>
                                    ,<<"Update-Callee-ID">>
                                    ]).
-define(RATE_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                          ,{<<"Event-Name">>, <<"resp">>}
                          ]).
-define(RATE_RESP_TYPES, [{<<"Rate-Increment">>, fun is_integer/1}
                         ,{<<"Rate-NoCharge-Time">>, fun is_integer/1}
                         ,{<<"Update-Callee-ID">>, fun kz_term:is_boolean/1}
                         ]).

%%------------------------------------------------------------------------------
%% @doc Authorization Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RATE_REQ_HEADERS, ?OPTIONAL_RATE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for rate_req"}
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RATE_REQ_HEADERS, ?RATE_REQ_VALUES, ?RATE_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Authorization Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RATE_RESP_HEADERS, ?OPTIONAL_RATE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for rate_resp"}
    end;
resp(JObj) ->
    resp(kz_json:to_proplist(JObj)).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RATE_RESP_HEADERS, ?RATE_RESP_VALUES, ?RATE_RESP_TYPES);
resp_v(JObj) ->
    resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Setup and tear down bindings for rate `gen_listeners'.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_REQ);
bind_to_q(Q, ['req'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(Q, [_|T]) ->
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_REQ);
unbind_q_from(Q, ['req'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_BROADCAST),
    unbind_q_from(Q, T);
unbind_q_from(Q, [_|T]) ->
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RATE_REQ_VALUES, fun req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RATE_REQ).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?RATE_RESP_VALUES, fun resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec broadcast_resp(kz_term:api_terms()) -> 'ok'.
broadcast_resp(JObj) ->
    broadcast_resp(JObj, ?DEFAULT_CONTENT_TYPE).

-spec broadcast_resp(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
broadcast_resp(Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?RATE_RESP_VALUES, fun resp/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RATE_BROADCAST).
