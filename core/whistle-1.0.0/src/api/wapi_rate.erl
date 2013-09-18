%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wapi_rate).

-export([req/1, req_v/1
         ,resp/1, resp_v/1
         ,bind_q/2, unbind_q/2
         ,publish_req/1, publish_req/2
         ,publish_resp/2, publish_resp/3
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(EVENT_CATEGORY, <<"rate">>).
-define(KEY_RATE_REQ, <<"rate.req">>).

%% AMQP fields for Rating Request
-define(RATE_REQ_HEADERS, [<<"To-DID">>, <<"Call-ID">>]).
-define(OPTIONAL_RATE_REQ_HEADERS, [<<"Call-ID">>, <<"Account-ID">>, <<"From-DID">>
                                    ,<<"Options">>, <<"Direction">>
                                   ]).
-define(RATE_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                          ,{<<"Event-Name">>, <<"req">>}
                          ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                         ]).
-define(RATE_REQ_TYPES, [
                         {<<"Options">>, fun is_list/1}
                        ]).

%% AMQP fields for Rating Response
-define(RATE_RESP_HEADERS, [<<"Rate">>, <<"Call-ID">>]).
-define(OPTIONAL_RATE_RESP_HEADERS, [<<"Rate-Increment">>, <<"Rate-Minimum">>
                                         ,<<"Surcharge">>, <<"Base-Cost">>
                                         ,<<"Rate-Name">>, <<"Discount-Percentage">>
                                         ,<<"Update-Callee-ID">>
                                    ]).
-define(RATE_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"resp">>}
                          ]).
-define(RATE_RESP_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec req(api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RATE_REQ_HEADERS, ?OPTIONAL_RATE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for rate_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATE_REQ_HEADERS, ?RATE_REQ_VALUES, ?RATE_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resp(api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RATE_RESP_HEADERS, ?OPTIONAL_RATE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for rate_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v(api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATE_RESP_HEADERS, ?RATE_RESP_VALUES, ?RATE_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for rate gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    amqp_util:callmgr_exchange(),
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_REQ);
bind_to_q(Q, ['req'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, [_|T]) ->
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_REQ);
unbind_q_from(Q, ['req'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, [_|T]) ->
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    'ok'.

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RATE_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RATE_REQ).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?RATE_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
