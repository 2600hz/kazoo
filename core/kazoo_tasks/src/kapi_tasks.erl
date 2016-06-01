%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kapi_tasks).

-export([help_req/1, help_req_v/1]).
-export([help_resp/1, help_resp_v/1]).
-export([publish_help_req/1, publish_help_req/2]).
-export([publish_help_resp/2, publish_help_resp/3]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kazoo/include/kz_api.hrl").

-define(HELP_REQ_HEADERS, []).
-define(OPTIONAL_HELP_REQ_HEADERS, []).
-define(HELP_REQ_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                         ,{<<"Event-Name">>, <<"help_req">>}
                         ]).
-define(HELP_REQ_TYPES, []).

-define(HELP_RESP_HEADERS, [<<"Tasks">>
                           ,<<"Tasks-For">>
                           ,<<"Tasks-Module">>
                           ]).
-define(OPTIONAL_HELP_RESP_HEADERS, []).
-define(HELP_RESP_VALUES, [{<<"Event-Category">>, <<"tasks">>}
                          ,{<<"Event-Name">>, <<"help_resp">>}
                          ]).
-define(HELP_RESP_TYPES, [{<<"Tasks">>, fun kz_json:is_json_object/1}
                         ,{<<"Tasks-For">>, fun erlang:is_binary/1}
                         ,{<<"Tasks-Module">>, fun erlang:is_binary/1}
                         ]).

-define(TASKS_AMQP_KEY(SubKey), <<"tasks.", SubKey>>).

-spec help_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
help_req(Prop) when is_list(Prop) ->
    case help_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?HELP_REQ_HEADERS, ?OPTIONAL_HELP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup req"}
    end;
help_req(JObj) ->
    help_req(kz_json:to_proplist(JObj)).

-spec help_req_v(api_terms()) -> boolean().
help_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?HELP_REQ_HEADERS, ?HELP_REQ_VALUES, ?HELP_REQ_TYPES);
help_req_v(JObj) ->
    help_req_v(kz_json:to_proplist(JObj)).

-spec help_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
help_resp(Prop) when is_list(Prop) ->
    case help_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?HELP_RESP_HEADERS, ?OPTIONAL_HELP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for lookup resp"}
    end;
help_resp(JObj) ->
    help_resp(kz_json:to_proplist(JObj)).

-spec help_resp_v(api_terms()) -> boolean().
help_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?HELP_RESP_HEADERS, ?HELP_RESP_VALUES, ?HELP_RESP_TYPES);
help_resp_v(JObj) ->
    help_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_to_q(Q, props:get_value('restrict_to', Props)).

-spec bind_to_q(ne_binary(), atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_tasks(Q, ?TASKS_AMQP_KEY("*"));
bind_to_q(Q, ['help'|T]) ->
    'ok' = amqp_util:bind_q_to_tasks(Q, ?TASKS_AMQP_KEY("help")),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_from_q(Q, props:get_value('restrict_to', Props)).

-spec unbind_from_q(ne_binary(), atoms()) -> 'ok'.
unbind_from_q(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_tasks(Q, ?TASKS_AMQP_KEY("*"));
unbind_from_q(Q, ['help'|T]) ->
    'ok' = amqp_util:unbind_q_from_tasks(Q, ?TASKS_AMQP_KEY("help")),
    unbind_from_q(Q, T);
unbind_from_q(_Q, []) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:tasks_exchange().

-spec publish_help_req(api_terms()) -> 'ok'.
-spec publish_help_req(api_terms(), binary()) -> 'ok'.
publish_help_req(JObj) ->
    publish_help_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_help_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?HELP_REQ_VALUES, fun ?MODULE:help_req/1),
    amqp_util:tasks_publish(?TASKS_AMQP_KEY("help"), Payload, ContentType).

-spec publish_help_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_help_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_help_resp(RespQ, JObj) ->
    publish_help_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_help_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?HELP_RESP_VALUES, fun help_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).
