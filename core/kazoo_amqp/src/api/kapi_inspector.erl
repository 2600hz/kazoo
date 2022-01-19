%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_inspector).

-export([lookup_req/1, lookup_req_v/1]).
-export([lookup_resp/1, lookup_resp_v/1]).
-export([filter_req/1, filter_req_v/1]).
-export([filter_resp/1, filter_resp_v/1]).
-export([publish_lookup_req/1, publish_lookup_req/2]).
-export([publish_lookup_resp/2, publish_lookup_resp/3]).
-export([publish_filter_req/1, publish_filter_req/2]).
-export([publish_filter_resp/2, publish_filter_resp/3]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(LOOKUP_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_LOOKUP_REQ_HEADERS, []).
-define(LOOKUP_REQ_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                           ,{<<"Event-Name">>, <<"lookup_req">>}
                           ]).
-define(LOOKUP_REQ_TYPES, []).

-define(LOOKUP_RESP_HEADERS, [<<"Chunks">>]).
-define(OPTIONAL_LOOKUP_RESP_HEADERS, [<<"Chunks">>, <<"Analysis">>, <<"Dialog-Entities">>]).
-define(LOOKUP_RESP_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                            ,{<<"Event-Name">>, <<"lookup_resp">>}
                            ]).
-define(LOOKUP_RESP_TYPES, []).

-define(FILTER_REQ_HEADERS, [<<"Call-IDs">>]).
-define(OPTIONAL_FILTER_REQ_HEADERS, []).
-define(FILTER_REQ_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                           ,{<<"Event-Name">>, <<"filter_req">>}
                           ]).
-define(FILTER_REQ_TYPES, []).

-define(FILTER_RESP_HEADERS, []).
-define(OPTIONAL_FILTER_RESP_HEADERS, [<<"Call-IDs">>]).
-define(FILTER_RESP_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                            ,{<<"Event-Name">>, <<"filter_resp">>}
                            ]).
-define(FILTER_RESP_TYPES, []).

-define(CI_AMQP_KEY(SubKey), <<"call_inspector.", SubKey/binary>>).

-spec lookup_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
lookup_req(Prop) when is_list(Prop) ->
    case lookup_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?LOOKUP_REQ_HEADERS, ?OPTIONAL_LOOKUP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup req"}
    end;
lookup_req(JObj) ->
    lookup_req(kz_json:to_proplist(JObj)).

-spec lookup_req_v(kz_term:api_terms()) -> boolean().
lookup_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LOOKUP_REQ_HEADERS, ?LOOKUP_REQ_VALUES, ?LOOKUP_REQ_TYPES);
lookup_req_v(JObj) ->
    lookup_req_v(kz_json:to_proplist(JObj)).

-spec lookup_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
lookup_resp(Prop) when is_list(Prop) ->
    case lookup_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?LOOKUP_RESP_HEADERS, ?OPTIONAL_LOOKUP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup resp"}
    end;
lookup_resp(JObj) ->
    lookup_resp(kz_json:to_proplist(JObj)).

-spec lookup_resp_v(kz_term:api_terms()) -> boolean().
lookup_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LOOKUP_RESP_HEADERS, ?LOOKUP_RESP_VALUES, ?LOOKUP_RESP_TYPES);
lookup_resp_v(JObj) ->
    lookup_resp_v(kz_json:to_proplist(JObj)).

-spec filter_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
filter_req(Prop) when is_list(Prop) ->
    case filter_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FILTER_REQ_HEADERS, ?OPTIONAL_FILTER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for filter req"}
    end;
filter_req(JObj) ->
    filter_req(kz_json:to_proplist(JObj)).

-spec filter_req_v(kz_term:api_terms()) -> boolean().
filter_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FILTER_REQ_HEADERS, ?FILTER_REQ_VALUES, ?FILTER_REQ_TYPES);
filter_req_v(JObj) ->
    filter_req_v(kz_json:to_proplist(JObj)).

-spec filter_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
filter_resp(Prop) when is_list(Prop) ->
    case filter_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FILTER_RESP_HEADERS, ?OPTIONAL_FILTER_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for filter resp"}
    end;
filter_resp(JObj) ->
    filter_resp(kz_json:to_proplist(JObj)).

-spec filter_resp_v(kz_term:api_terms()) -> boolean().
filter_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FILTER_RESP_HEADERS, ?FILTER_RESP_VALUES, ?FILTER_RESP_TYPES);
filter_resp_v(JObj) ->
    filter_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:monitor_exchange().

-spec publish_lookup_req(kz_term:api_terms()) -> 'ok'.
publish_lookup_req(JObj) ->
    publish_lookup_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?LOOKUP_REQ_VALUES, fun lookup_req/1),
    kz_amqp_util:monitor_publish(Payload, ContentType, ?CI_AMQP_KEY(<<"lookup">>)).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_lookup_resp(RespQ, JObj) ->
    publish_lookup_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?LOOKUP_RESP_VALUES, fun lookup_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_filter_req(kz_term:api_terms()) -> 'ok'.
publish_filter_req(JObj) ->
    publish_filter_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_filter_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_filter_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FILTER_REQ_VALUES, fun filter_req/1),
    kz_amqp_util:monitor_publish(Payload, ContentType, ?CI_AMQP_KEY(<<"filter">>)).

-spec publish_filter_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_filter_resp(RespQ, JObj) ->
    publish_filter_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_filter_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_filter_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?FILTER_RESP_VALUES, fun filter_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
