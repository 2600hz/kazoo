%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wapi_inspector).

-export([lookup_req/1, lookup_req_v/1]).
-export([lookup_resp/1, lookup_resp_v/1]).
-export([publish_lookup_req/1, publish_lookup_req/2]).
-export([publish_lookup_resp/2, publish_lookup_resp/3]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("whistle/include/wh_api.hrl").

-define(LOOKUP_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_LOOKUP_REQ_HEADERS, []).
-define(LOOKUP_REQ_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                          ,{<<"Event-Name">>, <<"lookup_req">>}
                         ]).
-define(LOOKUP_REQ_TYPES, []).

-define(LOOKUP_RESP_HEADERS, [<<"Chunks">>]).
-define(OPTIONAL_LOOKUP_RESP_HEADERS, [<<"Analysis">>]).
-define(LOOKUP_RESP_VALUES, [{<<"Event-Category">>, <<"call_inspector">>}
                            ,{<<"Event-Name">>, <<"lookup_resp">>}
                            ]).
-define(LOOKUP_RESP_TYPES, []).

-define(CI_AMQP_KEY(SubKey), <<"call_inspector.", SubKey/binary>>).

-spec lookup_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
lookup_req(Prop) when is_list(Prop) ->
    case lookup_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?LOOKUP_REQ_HEADERS, ?OPTIONAL_LOOKUP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup req"}
    end;
lookup_req(JObj) ->
    lookup_req(wh_json:to_proplist(JObj)).

-spec lookup_req_v(api_terms()) -> boolean().
lookup_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LOOKUP_REQ_HEADERS, ?LOOKUP_REQ_VALUES, ?LOOKUP_REQ_TYPES);
lookup_req_v(JObj) ->
    lookup_req_v(wh_json:to_proplist(JObj)).

-spec lookup_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
lookup_resp(Prop) when is_list(Prop) ->
    case lookup_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?LOOKUP_RESP_HEADERS, ?OPTIONAL_LOOKUP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup resp"}
    end;
lookup_resp(JObj) ->
    lookup_resp(wh_json:to_proplist(JObj)).

-spec lookup_resp_v(api_terms()) -> boolean().
lookup_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LOOKUP_RESP_HEADERS, ?LOOKUP_RESP_VALUES, ?LOOKUP_RESP_TYPES);
lookup_resp_v(JObj) ->
    lookup_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:bind_q_to_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:monitor_exchange().

-spec publish_lookup_req(api_terms()) -> 'ok'.
-spec publish_lookup_req(api_terms(), binary()) -> 'ok'.
publish_lookup_req(JObj) ->
    publish_lookup_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_lookup_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?LOOKUP_REQ_VALUES, fun ?MODULE:lookup_req/1),
    amqp_util:monitor_publish(Payload, ContentType, ?CI_AMQP_KEY(<<"lookup">>)).

-spec publish_lookup_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_lookup_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_lookup_resp(RespQ, JObj) ->
    publish_lookup_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_lookup_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(JObj, ?LOOKUP_RESP_VALUES, fun lookup_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).
