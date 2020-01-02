%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_tasks).

-export([category/1
        ,action/1
        ,task_id/1
        ,reply/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([lookup_req/1, lookup_req_v/1]).
-export([lookup_resp/1, lookup_resp_v/1]).
-export([publish_lookup_req/1, publish_lookup_req/2]).
-export([publish_lookup_resp/2, publish_lookup_resp/3]).

-export([start_req/1, start_req_v/1]).
-export([start_resp/1, start_resp_v/1]).
-export([publish_start_req/1, publish_start_req/2]).
-export([publish_start_resp/2, publish_start_resp/3]).

-export([stop_req/1, stop_req_v/1]).
-export([stop_resp/1, stop_resp_v/1]).
-export([publish_stop_req/1, publish_stop_req/2]).
-export([publish_stop_resp/2, publish_stop_resp/3]).

-export([remove_req/1, remove_req_v/1]).
-export([remove_resp/1, remove_resp_v/1]).
-export([publish_remove_req/1, publish_remove_req/2]).
-export([publish_remove_resp/2, publish_remove_resp/3]).


-include_lib("kz_amqp_util.hrl").


-define(LOOKUP_REQ_HEADERS, []).
-define(OPTIONAL_LOOKUP_REQ_HEADERS, [<<"Category">>, <<"Action">>]).
-define(LOOKUP_REQ_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                           ,{<<"Event-Name">>, <<"lookup_req">>}
                           ]).
-define(LOOKUP_REQ_TYPES, []).

-define(LOOKUP_RESP_HEADERS, [<<"Help">>]).
-define(OPTIONAL_LOOKUP_RESP_HEADERS, []).
-define(LOOKUP_RESP_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                            ,{<<"Event-Name">>, <<"lookup_resp">>}
                            ]).
-define(LOOKUP_RESP_TYPES, []).


-define(START_REQ_HEADERS, [<<"Task-ID">>]).
-define(OPTIONAL_START_REQ_HEADERS, []).
-define(START_REQ_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                          ,{<<"Event-Name">>, <<"start_req">>}
                          ]).
-define(START_REQ_TYPES, []).

-define(START_RESP_HEADERS, [<<"Reply">>]).
-define(OPTIONAL_START_RESP_HEADERS, []).
-define(START_RESP_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                           ,{<<"Event-Name">>, <<"start_resp">>}
                           ]).
-define(START_RESP_TYPES, []).


-define(STOP_REQ_HEADERS, [<<"Task-ID">>]).
-define(OPTIONAL_STOP_REQ_HEADERS, []).
-define(STOP_REQ_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                         ,{<<"Event-Name">>, <<"stop_req">>}
                         ]).
-define(STOP_REQ_TYPES, []).

-define(STOP_RESP_HEADERS, [<<"Reply">>]).
-define(OPTIONAL_STOP_RESP_HEADERS, []).
-define(STOP_RESP_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                          ,{<<"Event-Name">>, <<"stop_resp">>}
                          ]).
-define(STOP_RESP_TYPES, []).


-define(REMOVE_REQ_HEADERS, [<<"Task-ID">>]).
-define(OPTIONAL_REMOVE_REQ_HEADERS, []).
-define(REMOVE_REQ_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                           ,{<<"Event-Name">>, <<"remove_req">>}
                           ]).
-define(REMOVE_REQ_TYPES, []).

-define(REMOVE_RESP_HEADERS, [<<"Reply">>]).
-define(OPTIONAL_REMOVE_RESP_HEADERS, []).
-define(REMOVE_RESP_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                            ,{<<"Event-Name">>, <<"remove_resp">>}
                            ]).
-define(REMOVE_RESP_TYPES, []).


-define(TASKS_AMQP_KEY(SubKey), <<"tasks.", SubKey>>).


-spec category(kz_json:object()) -> kz_term:api_binary().
category(JObj) ->
    kz_json:get_value(<<"Category">>, JObj).

-spec action(kz_json:object()) -> kz_term:api_binary().
action(JObj) ->
    kz_json:get_value(<<"Action">>, JObj).

-spec task_id(kz_json:object()) -> kz_tasks:id().
task_id(JObj) ->
    kz_json:get_value(<<"Task-ID">>, JObj).

-spec reply(kz_json:object()) -> kz_json:object() | kz_term:ne_binary().
reply(JObj) ->
    kz_json:get_value(<<"Reply">>, JObj).


-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_to_q(Q, props:get_value('restrict_to', Props)).

-spec bind_to_q(kz_term:ne_binary(), kz_term:atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_tasks(Q, ?TASKS_AMQP_KEY("*"));
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_from_q(Q, props:get_value('restrict_to', Props)).

-spec unbind_from_q(kz_term:ne_binary(), kz_term:atoms()) -> 'ok'.
unbind_from_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_tasks(Q, ?TASKS_AMQP_KEY("*"));
unbind_from_q(_Q, []) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:tasks_exchange().


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

-spec publish_lookup_req(kz_term:api_terms()) -> 'ok'.
publish_lookup_req(JObj) ->
    publish_lookup_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?LOOKUP_REQ_VALUES, fun lookup_req/1),
    kz_amqp_util:tasks_publish(?TASKS_AMQP_KEY("lookup"), Payload, ContentType).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_lookup_resp(RespQ, JObj) ->
    publish_lookup_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?LOOKUP_RESP_VALUES, fun lookup_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).


-spec start_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
start_req(Prop) when is_list(Prop) ->
    case start_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?START_REQ_HEADERS, ?OPTIONAL_START_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for start req"}
    end;
start_req(JObj) ->
    start_req(kz_json:to_proplist(JObj)).

-spec start_req_v(kz_term:api_terms()) -> boolean().
start_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?START_REQ_HEADERS, ?START_REQ_VALUES, ?START_REQ_TYPES);
start_req_v(JObj) ->
    start_req_v(kz_json:to_proplist(JObj)).

-spec start_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
start_resp(Prop) when is_list(Prop) ->
    case start_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?START_RESP_HEADERS, ?OPTIONAL_START_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for start resp"}
    end;
start_resp(JObj) ->
    start_resp(kz_json:to_proplist(JObj)).

-spec start_resp_v(kz_term:api_terms()) -> boolean().
start_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?START_RESP_HEADERS, ?START_RESP_VALUES, ?START_RESP_TYPES);
start_resp_v(JObj) ->
    start_resp_v(kz_json:to_proplist(JObj)).

-spec publish_start_req(kz_term:api_terms()) -> 'ok'.
publish_start_req(JObj) ->
    publish_start_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_start_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?START_REQ_VALUES, fun start_req/1),
    kz_amqp_util:tasks_publish(?TASKS_AMQP_KEY("start"), Payload, ContentType).

-spec publish_start_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_start_resp(RespQ, JObj) ->
    publish_start_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_start_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?START_RESP_VALUES, fun start_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).


-spec stop_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
stop_req(Prop) when is_list(Prop) ->
    case stop_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?STOP_REQ_HEADERS, ?OPTIONAL_STOP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for stop req"}
    end;
stop_req(JObj) ->
    stop_req(kz_json:to_proplist(JObj)).

-spec stop_req_v(kz_term:api_terms()) -> boolean().
stop_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STOP_REQ_HEADERS, ?STOP_REQ_VALUES, ?STOP_REQ_TYPES);
stop_req_v(JObj) ->
    stop_req_v(kz_json:to_proplist(JObj)).

-spec stop_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
stop_resp(Prop) when is_list(Prop) ->
    case stop_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?STOP_RESP_HEADERS, ?OPTIONAL_STOP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for stop resp"}
    end;
stop_resp(JObj) ->
    stop_resp(kz_json:to_proplist(JObj)).

-spec stop_resp_v(kz_term:api_terms()) -> boolean().
stop_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STOP_RESP_HEADERS, ?STOP_RESP_VALUES, ?STOP_RESP_TYPES);
stop_resp_v(JObj) ->
    stop_resp_v(kz_json:to_proplist(JObj)).

-spec publish_stop_req(kz_term:api_terms()) -> 'ok'.
publish_stop_req(JObj) ->
    publish_stop_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stop_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_stop_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?STOP_REQ_VALUES, fun stop_req/1),
    kz_amqp_util:tasks_publish(?TASKS_AMQP_KEY("stop"), Payload, ContentType).

-spec publish_stop_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_stop_resp(RespQ, JObj) ->
    publish_stop_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stop_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_stop_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?STOP_RESP_VALUES, fun stop_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).


-spec remove_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
remove_req(Prop) when is_list(Prop) ->
    case remove_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REMOVE_REQ_HEADERS, ?OPTIONAL_REMOVE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for remove req"}
    end;
remove_req(JObj) ->
    remove_req(kz_json:to_proplist(JObj)).

-spec remove_req_v(kz_term:api_terms()) -> boolean().
remove_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REMOVE_REQ_HEADERS, ?REMOVE_REQ_VALUES, ?REMOVE_REQ_TYPES);
remove_req_v(JObj) ->
    remove_req_v(kz_json:to_proplist(JObj)).

-spec remove_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
remove_resp(Prop) when is_list(Prop) ->
    case remove_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REMOVE_RESP_HEADERS, ?OPTIONAL_REMOVE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for remove resp"}
    end;
remove_resp(JObj) ->
    remove_resp(kz_json:to_proplist(JObj)).

-spec remove_resp_v(kz_term:api_terms()) -> boolean().
remove_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REMOVE_RESP_HEADERS, ?REMOVE_RESP_VALUES, ?REMOVE_RESP_TYPES);
remove_resp_v(JObj) ->
    remove_resp_v(kz_json:to_proplist(JObj)).

-spec publish_remove_req(kz_term:api_terms()) -> 'ok'.
publish_remove_req(JObj) ->
    publish_remove_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_remove_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_remove_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REMOVE_REQ_VALUES, fun remove_req/1),
    kz_amqp_util:tasks_publish(?TASKS_AMQP_KEY("remove"), Payload, ContentType).

-spec publish_remove_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_remove_resp(RespQ, JObj) ->
    publish_remove_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_remove_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_remove_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?REMOVE_RESP_VALUES, fun remove_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
