%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kapi_pusher).

-export([push_req/1, push_req_v/1]).
-export([push_resp/1, push_resp_v/1]).
-export([publish_push_req/1, publish_push_req/2]).
-export([publish_push_resp/1, publish_push_resp/2]).
-export([publish_targeted_push_resp/2, publish_targeted_push_resp/3]).

-export([bind_q/2, unbind_q/2]).

-export([declare_exchanges/0]).

-include_lib("kazoo_types/include/kz_types.hrl").

-define(PUSH_EXCHANGE, <<"pushes">>).

-define(PUSH_REQ_HEADERS, [<<"Token-ID">>
                          ,<<"Token-Type">>, <<"Token-App">>
                          ,[<<"Alert-Body">>,[<<"Alert-Key">>,<<"Alert-Params">>]]
                          ]).
-define(OPTIONAL_PUSH_REQ_HEADERS, [<<"Queue">>, <<"Call-ID">>
                                   ,<<"Badge">>, <<"Sound">>
                                   ,<<"Account-ID">>, <<"Endpoint-ID">>
                                   ,<<"Expires">>
                                   ,<<"Token-Reg">>
                                   ]).
-define(PUSH_REQ_VALUES, [{<<"Event-Category">>, <<"notification">>}
                         ,{<<"Event-Name">>, <<"push_req">>}
                         ]).
-define(PUSH_REQ_TYPES, [{<<"Expires">>, fun(V) -> is_integer(kz_term:to_integer(V)) end}]).

-define(PUSH_RESP_HEADERS, [<<"Token-ID">>]).
-define(OPTIONAL_PUSH_RESP_HEADERS, []).
-define(PUSH_RESP_VALUES, [{<<"Event-Category">>, <<"notification">>}
                          ,{<<"Event-Name">>, <<"push_resp">>}
                          ]).
-define(PUSH_RESP_TYPES, []).

-define(KEY_PUSH, <<"notification.push">>).

-spec push_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
push_req(Prop) when is_list(Prop) ->
    case push_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PUSH_REQ_HEADERS, ?OPTIONAL_PUSH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for push"}
    end;
push_req(JObj) ->
    push_req(kz_json:to_proplist(JObj)).

-spec push_req_v(api_terms()) -> boolean().
push_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PUSH_REQ_HEADERS, ?PUSH_REQ_VALUES, ?PUSH_REQ_TYPES);
push_req_v(JObj) ->
    push_req_v(kz_json:to_proplist(JObj)).

-spec push_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
push_resp(Prop) when is_list(Prop) ->
    case push_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PUSH_RESP_HEADERS, ?OPTIONAL_PUSH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for push"}
    end;
push_resp(JObj) ->
    push_resp(kz_json:to_proplist(JObj)).

-spec push_resp_v(api_terms()) -> boolean().
push_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PUSH_RESP_HEADERS, ?PUSH_RESP_VALUES, ?PUSH_RESP_TYPES);
push_resp_v(JObj) ->
    push_resp_v(kz_json:to_proplist(JObj)).

-spec publish_push_req(api_terms()) -> 'ok'.
-spec publish_push_req(api_terms(), binary()) -> 'ok'.
publish_push_req(JObj) ->
    publish_push_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_push_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PUSH_REQ_VALUES, fun push_req/1),
    amqp_util:basic_publish(?PUSH_EXCHANGE, push_routing_key(Req), Payload, ContentType).

-spec publish_push_resp(api_terms()) -> 'ok'.
-spec publish_push_resp(api_terms(), binary()) -> 'ok'.
publish_push_resp(JObj) ->
    publish_push_resp(JObj, ?DEFAULT_CONTENT_TYPE).
publish_push_resp(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PUSH_RESP_VALUES, fun push_resp/1),
    amqp_util:basic_publish(?PUSH_EXCHANGE, push_routing_key(Req), Payload, ContentType).

-spec publish_targeted_push_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_targeted_push_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_targeted_push_resp(RespQ, JObj) ->
    publish_targeted_push_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_targeted_push_resp(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?PUSH_RESP_VALUES, fun push_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec push_routing_key(ne_binary() | api_terms()) -> ne_binary().
push_routing_key(Req) when is_list(Req) ->
    push_routing_key(props:get_value(<<"Token-Type">>, Req, <<"*">>), props:get_value(<<"Token">>, Req,<<"*">>));
push_routing_key(Req) ->
    push_routing_key(kz_json:get_value(<<"Token-Type">>, Req, <<"*">>), kz_json:get_value(<<"Token">>, Req, <<"*">>)).
push_routing_key(Type, Token) ->
    list_to_binary([?KEY_PUSH, ".", amqp_util:encode(Type), ".", amqp_util:encode(Token)]).

%% API Helpers

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec bind_q(ne_binary(), ne_binary(), ne_binary(), api_binaries()) -> 'ok'.
bind_q(Queue, Props) ->
    Token = props:get_value('token', Props, <<"*">>),
    Type = props:get_value('type', Props, <<"*">>),
    bind_q(Queue, Type, Token, props:get_value('restrict_to', Props)).

bind_q(Queue, Type, Token, 'undefined') ->
    amqp_util:bind_q_to_exchange(Queue, push_routing_key(Type, Token), ?PUSH_EXCHANGE);
bind_q(Queue, Type, Token, ['push'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, push_routing_key(Type, Token), ?PUSH_EXCHANGE),
    bind_q(Queue, Type, Token, Restrict);
bind_q(Queue, Type, Token, [_|Restrict]) ->
    bind_q(Queue, Type, Token, Restrict);
bind_q(_Queue, _Type, _Token, []) -> 'ok'.

-spec unbind_q(binary(), kz_proplist()) -> 'ok'.
-spec unbind_q(ne_binary(), ne_binary(), ne_binary(), api_binaries()) -> 'ok'.
unbind_q(Queue, Props) ->
    Token = props:get_value('token', Props, <<"*">>),
    Type = props:get_value('type', Props, <<"*">>),
    unbind_q(Queue, Type, Token, props:get_value('restrict_to', Props)).

unbind_q(Queue, Type, Token, 'undefined') ->
    amqp_util:unbind_q_from_exchange(Queue, push_routing_key(Type, Token), ?PUSH_EXCHANGE);
unbind_q(Queue, Type, Token, ['push'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, push_routing_key(Type, Token), ?PUSH_EXCHANGE),
    unbind_q(Queue, Type, Token, Restrict);
unbind_q(Queue, Type, Token, [_|Restrict]) ->
    unbind_q(Queue, Type, Token, Restrict);
unbind_q(_Queue, _Type, _Token, []) -> 'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?PUSH_EXCHANGE, <<"topic">>),
    amqp_util:kapps_exchange().
