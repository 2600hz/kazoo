%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_hangups).

-export([query_req/1, query_req_v/1
        ,query_resp/1, query_resp_v/1

        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0

        ,publish_query_req/1, publish_query_req/2
        ,publish_query_resp/2, publish_query_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(QUERY_REQ_ROUTING_KEY, <<"hangups.query">>).

-define(QUERY_REQ_HEADERS, [<<"Hangup-Cause">>]).
-define(OPTIONAL_QUERY_REQ_HEADERS, [<<"Account-ID">>, <<"Raw-Data">>]).
-define(QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"hangups">>}
                          ,{<<"Event-Name">>, <<"query_req">>}
                          ]).
-define(QUERY_REQ_TYPES, [{<<"Raw-Data">>, fun kz_term:is_boolean/1}]).

-define(QUERY_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_RESP_HEADERS, [<<"mean">>
                                     ,<<"one_to_five">>, <<"five_to_fifteen">>, <<"one_to_fifteen">>
                                     ,<<"start_time">>
                                     ,<<"account_id">>
                                     ,<<"hangup_cause">>
                                     ,<<"one">>, <<"five">>, <<"fifteen">>, <<"day">>
                                     ,<<"count">>
                                     ,<<"meters">>
                                     ]).
-define(QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"hangups">>}
                           ,{<<"Event-Name">>, <<"query_resp">>}
                           ]).
-define(QUERY_RESP_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
query_req(Prop) when is_list(Prop) ->
    case query_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_REQ_HEADERS, ?OPTIONAL_QUERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for query_req"}
    end;
query_req(JObj) -> query_req(kz_json:to_proplist(JObj)).

-spec query_req_v(kz_term:api_terms()) -> boolean().
query_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_REQ_HEADERS, ?QUERY_REQ_VALUES, ?QUERY_REQ_TYPES);
query_req_v(JObj) -> query_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_RESP_HEADERS, ?OPTIONAL_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for query_resp"}
    end;
query_resp(JObj) -> query_resp(kz_json:to_proplist(JObj)).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_RESP_HEADERS, ?QUERY_RESP_VALUES, ?QUERY_RESP_TYPES);
query_resp_v(JObj) -> query_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    'ok' = kz_amqp_util:bind_q_to_kapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    'ok' = kz_amqp_util:unbind_q_from_kapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

-spec publish_query_req(kz_term:api_terms()) -> 'ok'.
publish_query_req(JObj) ->
    publish_query_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?QUERY_REQ_VALUES, fun query_req/1),
    kz_amqp_util:kapps_publish(?QUERY_REQ_ROUTING_KEY, Payload, ContentType).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(RespQ, JObj) ->
    publish_query_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(RespQ, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?QUERY_RESP_VALUES, fun query_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
