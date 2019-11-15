%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_discovery).

-export([declare_exchanges/0
        ,bind_q/2, unbind_q/2
        ]).

-export([flush/1, flush_v/1
        ,req/1, req_v/1
        ,resp/1, resp_v/1
        ,number_req/1, number_req_v/1

        ,publish_flush/1, publish_flush/2
        ,publish_number_req/1, publish_number_req/2
        ,publish_req/1, publish_req/2
        ,publish_resp/2, publish_resp/3

        ,query_id/1
        ,quantity/1
        ,offset/1
        ,results/1
        ,prefix/1
        ,number/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(KEY_QUERY_ID, <<"Query-ID">>).
-define(KEY_QUANTITY, <<"Quantity">>).
-define(KEY_OFFSET, <<"Offset">>).
-define(KEY_RESULTS, <<"Results">>).
-define(KEY_PREFIX, <<"Prefix">>).
-define(KEY_NUMBER, <<"Number">>).

-define(DISCOVERY_EXCHANGE, <<"discovery">>).
-define(DISCOVERY_EXCHANGE_TYPE, <<"topic">>).


-define(DISCOVERY_EVENT_CATEGORY, <<"discovery">>).
-define(DISCOVERY_RK, <<"discovery.*">>).

%% Discovery Flush Request
-define(DISCOVERY_FLUSH_RK, <<"discovery.flush">>).
-define(DISCOVERY_FLUSH_EVENT_NAME, <<"flush">>).
-define(DISCOVERY_FLUSH_HEADERS, [?KEY_QUERY_ID]).
-define(DISCOVERY_FLUSH_OPTIONAL_HEADERS, []).
-define(DISCOVERY_FLUSH_VALUES, [{<<"Event-Category">>, ?DISCOVERY_EVENT_CATEGORY}
                                ,{<<"Event-Name">>, ?DISCOVERY_FLUSH_EVENT_NAME}
                                ]).
-define(DISCOVERY_FLUSH_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                               ]).


%% Discovery Request
-define(DISCOVERY_REQ_RK, <<"discovery.request">>).
-define(DISCOVERY_REQ_EVENT_NAME, <<"request">>).
-define(DISCOVERY_REQ_HEADERS, [?KEY_QUERY_ID, ?KEY_PREFIX
                               ,?KEY_OFFSET, ?KEY_QUANTITY
                               ]).
-define(DISCOVERY_REQ_OPTIONAL_HEADERS, []).
-define(DISCOVERY_REQ_VALUES, [{<<"Event-Category">>, ?DISCOVERY_EVENT_CATEGORY}
                              ,{<<"Event-Name">>, ?DISCOVERY_REQ_EVENT_NAME}
                              ]).
-define(DISCOVERY_REQ_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                             ,{?KEY_OFFSET, fun is_integer/1}
                             ,{?KEY_QUANTITY, fun is_integer/1}
                             ]).


%% Number Request
-define(NUMBER_REQ_RK, <<"discovery.number">>).
-define(NUMBER_REQ_EVENT_NAME, <<"number">>).
-define(NUMBER_REQ_HEADERS, [?KEY_NUMBER]).
-define(NUMBER_REQ_OPTIONAL_HEADERS, []).
-define(NUMBER_REQ_VALUES, [{<<"Event-Category">>, ?DISCOVERY_EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?NUMBER_REQ_EVENT_NAME}
                           ]).
-define(NUMBER_REQ_TYPES, [{?KEY_NUMBER, fun is_binary/1}
                          ]).

%% Discovery/Number Response
-define(DISCOVERY_RESP_EVENT_NAME, <<"response">>).
-define(DISCOVERY_RESP_HEADERS, [?KEY_RESULTS]).
-define(DISCOVERY_RESP_OPTIONAL_HEADERS, [?KEY_QUERY_ID]).
-define(DISCOVERY_RESP_VALUES, [{<<"Event-Category">>, ?DISCOVERY_EVENT_CATEGORY}
                               ,{<<"Event-Name">>, ?DISCOVERY_RESP_EVENT_NAME}
                               ]).
-define(DISCOVERY_RESP_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                              ]).


%%------------------------------------------------------------------------------
%% @doc Number Request
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec number_req(kz_term:api_terms()) ->
                        {'ok', iolist()} |
                        {'error', string()}.
number_req(Prop) when is_list(Prop) ->
    case number_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NUMBER_REQ_HEADERS, ?NUMBER_REQ_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for number request"}
    end;
number_req(JObj) -> number_req(kz_json:to_proplist(JObj)).

-spec number_req_v(kz_term:api_terms()) -> boolean().
number_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NUMBER_REQ_HEADERS, ?NUMBER_REQ_VALUES, ?NUMBER_REQ_TYPES);
number_req_v(JObj) -> number_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Discovery Request
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DISCOVERY_REQ_HEADERS, ?DISCOVERY_REQ_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery request"}
    end;
req(JObj) -> req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DISCOVERY_REQ_HEADERS, ?DISCOVERY_REQ_VALUES, ?DISCOVERY_REQ_TYPES);
req_v(JObj) -> req_v(kz_json:to_proplist(JObj)).


%%------------------------------------------------------------------------------
%% @doc Discovery Response
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DISCOVERY_RESP_HEADERS, ?DISCOVERY_RESP_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery response"}
    end;
resp(JObj) -> resp(kz_json:to_proplist(JObj)).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DISCOVERY_RESP_HEADERS, ?DISCOVERY_RESP_VALUES, ?DISCOVERY_RESP_TYPES);
resp_v(JObj) -> resp_v(kz_json:to_proplist(JObj)).


%%------------------------------------------------------------------------------
%% @doc Discovery Flush
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:api_terms()) ->
                   {'ok', iolist()} |
                   {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DISCOVERY_FLUSH_HEADERS, ?DISCOVERY_FLUSH_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery response"}
    end;
flush(JObj) -> flush(kz_json:to_proplist(JObj)).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DISCOVERY_FLUSH_HEADERS, ?DISCOVERY_FLUSH_VALUES, ?DISCOVERY_FLUSH_TYPES);
flush_v(JObj) -> flush_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?DISCOVERY_RK, ?DISCOVERY_EXCHANGE).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    kz_amqp_util:unbind_q_from_exchange(Queue, ?DISCOVERY_RK, ?DISCOVERY_EXCHANGE).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?DISCOVERY_EXCHANGE, ?DISCOVERY_EXCHANGE_TYPE).

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), binary()) -> 'ok'.
publish_flush(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DISCOVERY_FLUSH_VALUES, fun flush/1),
    kz_amqp_util:basic_publish(?DISCOVERY_EXCHANGE, ?DISCOVERY_FLUSH_RK, Payload, ContentType).

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DISCOVERY_REQ_VALUES, fun req/1),
    kz_amqp_util:basic_publish(?DISCOVERY_EXCHANGE, ?DISCOVERY_REQ_RK, Payload, ContentType).

-spec publish_number_req(kz_term:api_terms()) -> 'ok'.
publish_number_req(JObj) ->
    publish_number_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_number_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_number_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NUMBER_REQ_VALUES, fun number_req/1),
    kz_amqp_util:basic_publish(?DISCOVERY_EXCHANGE, ?NUMBER_REQ_RK, Payload, ContentType).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(RespQ, JObj) ->
    publish_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?DISCOVERY_RESP_VALUES, fun resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec query_id(kz_json:object() | kz_term:proplist()) -> kz_term:ne_binary().
query_id(Props) when is_list(Props) ->
    props:get_ne_binary_value(?KEY_QUERY_ID, Props);
query_id(JObj) ->
    kz_json:get_ne_binary_value(?KEY_QUERY_ID, JObj).

-spec quantity(kz_json:object()) -> integer().
quantity(JObj) ->
    kz_json:get_integer_value(?KEY_QUANTITY, JObj).

-spec offset(kz_json:object()) -> integer().
offset(JObj) ->
    kz_json:get_integer_value(?KEY_OFFSET, JObj).

-spec results(kz_json:object()) -> kz_term:api_objects() | kz_json:object().
results(JObj) ->
    kz_json:get_value(?KEY_RESULTS, JObj).

-spec prefix(kz_json:object()) -> kz_term:ne_binary().
prefix(JObj) ->
    kz_json:get_value(?KEY_PREFIX, JObj).

-spec number(kz_json:object()) -> kz_term:ne_binary().
number(JObj) ->
    kz_json:get_value(?KEY_NUMBER, JObj).
