%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_hangups).

-export([query_req/1, query_req_v/1
         ,query_resp/1, query_resp_v/1

         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0

         ,publish_query_req/1, publish_query_req/2
         ,publish_query_resp/2, publish_query_resp/3
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(QUERY_REQ_ROUTING_KEY, <<"hangups.query">>).

-define(QUERY_REQ_HEADERS, [<<"Hangup-Cause">>]).
-define(OPTIONAL_QUERY_REQ_HEADERS, [<<"Account-ID">>]).
-define(QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"hangups">>}
                           ,{<<"Event-Name">>, <<"query_req">>}
                          ]).
-define(QUERY_REQ_TYPES, []).

-define(QUERY_RESP_HEADERS, [<<"Hangup-Cause">>]).
-define(OPTIONAL_QUERY_RESP_HEADERS, [<<"Account-ID">>]).
-define(QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"hangups">>}
                            ,{<<"Event-Name">>, <<"query_resp">>}
                          ]).
-define(QUERY_RESP_TYPES, []).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_req(api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_req(Prop) when is_list(Prop) ->
    case query_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_REQ_HEADERS, ?OPTIONAL_QUERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for query_req"}
    end;
query_req(JObj) -> query_req(wh_json:to_proplist(JObj)).

-spec query_req_v(api_terms()) -> boolean().
query_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_REQ_HEADERS, ?QUERY_REQ_VALUES, ?QUERY_REQ_TYPES);
query_req_v(JObj) -> query_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_resp(api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_RESP_HEADERS, ?OPTIONAL_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for query_resp"}
    end;
query_resp(JObj) -> query_resp(wh_json:to_proplist(JObj)).

-spec query_resp_v(api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_RESP_HEADERS, ?QUERY_RESP_VALUES, ?QUERY_RESP_TYPES);
query_resp_v(JObj) -> query_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    'ok' = amqp_util:bind_q_to_whapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    'ok' = amqp_util:unbind_q_from_whapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:whapps_exchange().

-spec publish_query_req(wh_json:object()) -> 'ok'.
-spec publish_query_req(api_terms(), ne_binary()) -> 'ok'.
publish_query_req(JObj) ->
    publish_query_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_req(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?QUERY_REQ_VALUES, fun ?MODULE:query_req/1),
    amqp_util:whapps_publish(?QUERY_REQ_ROUTING_KEY, Payload, ContentType).

-spec publish_query_resp(ne_binary(), wh_json:object()) -> 'ok'.
-spec publish_query_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_resp(RespQ, JObj) ->
    publish_query_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_resp(RespQ, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?QUERY_RESP_VALUES, fun ?MODULE:query_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).
