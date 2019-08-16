%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Globals API.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_globals).

-compile({'no_auto_import',[unregister/1]}).

-export([name/1]).
-export([message/1, reply/1, reason/1]).
-export([encode/1, encode_req/1, decode/1]).
-export([state/1]).
-export([is_pending/1
        ,is_none/1
        ]).
-export([timestamp/1]).
-export([node/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([query/1, query_v/1]).
-export([query_resp/1, query_resp_v/1]).
-export([send/1, send_v/1]).
-export([call/1, call_v/1]).
-export([reply_msg/1, reply_msg_v/1]).
-export([register/1, register_v/1]).
-export([register_resp/1, register_resp_v/1]).
-export([unregister/1, unregister_v/1]).

-export([publish_query/1, publish_query/2]).
-export([publish_query_resp/2, publish_query_resp/3]).
-export([publish_send/1, publish_send/2]).
-export([publish_targeted_send/2, publish_targeted_send/3]).
-export([publish_call/1, publish_call/2]).
-export([publish_targeted_call/2, publish_targeted_call/3]).
-export([publish_reply/2, publish_reply/3]).
-export([publish_register/1, publish_register/2]).
-export([publish_register_resp/2, publish_register_resp/3]).
-export([publish_unregister/1, publish_unregister/2]).

-include_lib("kz_amqp_util.hrl").

%% Types & Accessors

-type state() :: 'none' | 'local' | 'pending' | 'remote' | 'registered'.
-export_type([state/0]).

-spec encode(any()) -> kz_term:ne_binary().
encode(Term) ->
    base64:encode(term_to_binary(Term)).

-spec decode(kz_term:api_binary()) -> any().
decode('undefined') -> 'undefined';
decode(Bin) ->
    binary_to_term(base64:decode(Bin)).

-spec maybe_encode(any()) -> any().
maybe_encode(<<131, _/binary>>=Encoded) ->
    base64:encode(Encoded);
maybe_encode(Term) ->
    encode(Term).

-spec maybe_decode(any()) -> any().
maybe_decode(<<131, _/binary>>=Encoded) ->
    binary_to_term(Encoded);
maybe_decode(MaybeEncoded) ->
    try base64:decode(MaybeEncoded) of
        Decoded ->
            binary_to_term(Decoded)
    catch
        'error':'function_clause' -> MaybeEncoded;
        'error':{'badmatch','false'} -> MaybeEncoded;
        'error':{'badarg', _} -> MaybeEncoded
    end.

-spec encode_req(kz_json:object() | kz_term:proplist()) -> any().
encode_req(Req) ->
    set_name(Req, name(Req)).

-spec name(kz_json:object() | kz_term:proplist()) -> any().
name(Props)
  when is_list(Props) ->
    maybe_decode(props:get_value(<<"Name">>, Props));
name(JObj) ->
    maybe_decode(kz_json:get_value(<<"Name">>, JObj)).

-spec set_name(kz_term:api_terms(), any()) -> kz_term:api_terms().
set_name(Req, 'undefined') -> Req;
set_name(Props, Name)
  when is_list(Props) ->
    props:set_value(<<"Name">>, maybe_encode(Name), Props);
set_name(JObj, Name) ->
    kz_json:set_value(<<"Name">>, maybe_encode(Name), JObj).

-spec message(kz_json:object()) -> kz_term:ne_binary().
message(JObj) ->
    maybe_decode(kz_json:get_value(<<"Message">>, JObj)).

-spec reply(kz_json:object()) -> kz_term:ne_binary().
reply(JObj) ->
    maybe_decode(kz_json:get_value(<<"Reply">>, JObj)).

-spec state(kz_json:object()) -> state().
state(JObj) ->
    kz_json:get_atom_value(<<"State">>, JObj).

-spec reason(kz_json:object()) -> any().
reason(JObj) ->
    maybe_decode(kz_json:get_value(<<"Reason">>, JObj)).

-spec is_pending(kz_json:object()) -> boolean().
is_pending(JObj) ->
    state(JObj) =:= 'pending'.

-spec is_none(kz_json:object()) -> boolean().
is_none(JObj) ->
    state(JObj) =:= 'none'.

-spec timestamp(kz_json:object()) -> integer().
timestamp(JObj) ->
    kz_json:get_integer_value(<<"Timestamp">>, JObj).

-spec node(kz_json:object()) -> atom().
node(JObj) ->
    kz_term:to_atom(kz_api:node(JObj), 'true').

-define(GLOBALS_EXCHANGE, <<"globals">>).
-define(GLOBALS_EXCHANGE_TYPE, <<"topic">>).

routing_key(Event, Name) when is_binary(Name) ->
    list_to_binary(["globals."
                   ,kz_term:to_binary(Event)
                   ,"."
                   ,kz_amqp_util:encode(Name)
                   ]);
routing_key(Event, Name) ->
    list_to_binary(["globals."
                   ,kz_term:to_binary(Event)
                   ,"."
                   ,kz_term:to_hex_binary(maybe_encode(Name))
                   ]).

%% Globals Events
-define(GLOBALS_EVENT_ROUTING_KEY(Event, Name)
       ,routing_key(Event, Name)
       ).

-define(QUERY_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_QUERY_REQ_HEADERS, []).
-define(QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                          ,{<<"Event-Name">>, <<"query">>}
                          ]).
-define(QUERY_REQ_TYPES, []).

-define(QUERY_RESP_HEADERS, [<<"Name">>]).
-define(OPTIONAL_QUERY_RESP_HEADERS, [<<"State">>, <<"Timestamp">>]).
-define(QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"globals">>}
                           ,{<<"Event-Name">>, <<"query_resp">>}
                           ]).
-define(QUERY_RESP_TYPES, [{<<"Timestamp">>, fun is_integer/1}]).

-define(REGISTER_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_REGISTER_REQ_HEADERS, [<<"State">>, <<"Timestamp">>]).
-define(REGISTER_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                             ,{<<"Event-Name">>, <<"register">>}
                             ]).
-define(REGISTER_REQ_TYPES, [{<<"Timestamp">>, fun is_integer/1}]).

-define(REGISTER_RESP_HEADERS, [<<"Name">>]).
-define(OPTIONAL_REGISTER_RESP_HEADERS, [<<"State">>, <<"Timestamp">>]).
-define(REGISTER_RESP_VALUES, [{<<"Event-Category">>, <<"globals">>}
                              ,{<<"Event-Name">>, <<"register_resp">>}
                              ]).
-define(REGISTER_RESP_TYPES, [{<<"Timestamp">>, fun is_integer/1}]).

-define(UNREGISTER_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_UNREGISTER_REQ_HEADERS, [<<"Reason">>]).
-define(UNREGISTER_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                               ,{<<"Event-Name">>, <<"unregister">>}
                               ]).
-define(UNREGISTER_REQ_TYPES, []).

-define(CALL_REQ_HEADERS, [<<"Name">>, <<"Message">>]).
-define(OPTIONAL_CALL_REQ_HEADERS, []).
-define(CALL_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                         ,{<<"Event-Name">>, <<"call">>}
                         ]).
-define(CALL_REQ_TYPES, []).

-define(SEND_REQ_HEADERS, [<<"Name">>, <<"Message">>]).
-define(OPTIONAL_SEND_REQ_HEADERS, []).
-define(SEND_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                         ,{<<"Event-Name">>, <<"send">>}
                         ]).
-define(SEND_REQ_TYPES, []).

-define(REPLY_REQ_HEADERS, [<<"Reply">>]).
-define(OPTIONAL_REPLY_REQ_HEADERS, [<<"Name">>]).
-define(REPLY_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                          ,{<<"Event-Name">>, <<"reply">>}
                          ]).
-define(REPLY_REQ_TYPES, []).

-spec register(kz_term:api_terms()) -> api_formatter_return().
register(Prop) when is_list(Prop) ->
    case register_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_REQ_HEADERS, ?OPTIONAL_REGISTER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals register"}
    end;
register(JObj) ->
    register(kz_json:to_proplist(JObj)).

-spec register_v(kz_term:api_terms()) -> boolean().
register_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_REQ_HEADERS, ?REGISTER_REQ_VALUES, ?REGISTER_REQ_TYPES);
register_v(JObj) ->
    register_v(kz_json:to_proplist(JObj)).

-spec register_resp(kz_term:api_terms()) -> api_formatter_return().
register_resp(Prop) when is_list(Prop) ->
    case register_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_RESP_HEADERS, ?OPTIONAL_REGISTER_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals register"}
    end;
register_resp(JObj) ->
    register_resp(kz_json:to_proplist(JObj)).

-spec register_resp_v(kz_term:api_terms()) -> boolean().
register_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_RESP_HEADERS, ?REGISTER_RESP_VALUES, ?REGISTER_RESP_TYPES);
register_resp_v(JObj) ->
    register_resp_v(kz_json:to_proplist(JObj)).

-spec unregister(kz_term:api_terms()) -> api_formatter_return().
unregister(Prop) when is_list(Prop) ->
    case unregister_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UNREGISTER_REQ_HEADERS, ?OPTIONAL_UNREGISTER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals unregister"}
    end;
unregister(JObj) ->
    unregister(kz_json:to_proplist(JObj)).

-spec unregister_v(kz_term:api_terms()) -> boolean().
unregister_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UNREGISTER_REQ_HEADERS, ?UNREGISTER_REQ_VALUES, ?UNREGISTER_REQ_TYPES);
unregister_v(JObj) ->
    unregister_v(kz_json:to_proplist(JObj)).

-spec call(kz_term:api_terms()) -> api_formatter_return().
call(Prop) when is_list(Prop) ->
    case call_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CALL_REQ_HEADERS, ?OPTIONAL_CALL_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
call(JObj) ->
    call(kz_json:to_proplist(JObj)).

-spec call_v(kz_term:api_terms()) -> boolean().
call_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?CALL_REQ_VALUES, ?CALL_REQ_TYPES);
call_v(JObj) ->
    call_v(kz_json:to_proplist(JObj)).

-spec send(kz_term:api_terms()) -> api_formatter_return().
send(Prop) when is_list(Prop) ->
    case send_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEND_REQ_HEADERS, ?OPTIONAL_SEND_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals send"}
    end;
send(JObj) ->
    send(kz_json:to_proplist(JObj)).

-spec send_v(kz_term:api_terms()) -> boolean().
send_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEND_REQ_HEADERS, ?SEND_REQ_VALUES, ?SEND_REQ_TYPES);
send_v(JObj) ->
    send_v(kz_json:to_proplist(JObj)).

-spec reply_msg(kz_term:api_terms()) -> api_formatter_return().
reply_msg(Prop) when is_list(Prop) ->
    case reply_msg_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REPLY_REQ_HEADERS, ?OPTIONAL_REPLY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals reply"}
    end;
reply_msg(JObj) ->
    reply_msg(kz_json:to_proplist(JObj)).

-spec reply_msg_v(kz_term:api_terms()) -> boolean().
reply_msg_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REPLY_REQ_HEADERS, ?REPLY_REQ_VALUES, ?REPLY_REQ_TYPES);
reply_msg_v(JObj) ->
    reply_msg_v(kz_json:to_proplist(JObj)).

-spec query(kz_term:api_terms()) -> api_formatter_return().
query(Prop) when is_list(Prop) ->
    case query_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_REQ_HEADERS, ?OPTIONAL_QUERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
query(JObj) ->
    query(kz_json:to_proplist(JObj)).

-spec query_v(kz_term:api_terms()) -> boolean().
query_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_REQ_HEADERS, ?QUERY_REQ_VALUES, ?QUERY_REQ_TYPES);
query_v(JObj) ->
    query_v(kz_json:to_proplist(JObj)).

-spec query_resp(kz_term:api_terms()) -> api_formatter_return().
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_RESP_HEADERS, ?OPTIONAL_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
query_resp(JObj) ->
    query_resp(kz_json:to_proplist(JObj)).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_RESP_HEADERS, ?QUERY_RESP_VALUES, ?QUERY_RESP_TYPES);
query_resp_v(JObj) ->
    query_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?GLOBALS_EXCHANGE, ?GLOBALS_EXCHANGE_TYPE).

-spec publish_targeted_call(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_call(ServerId, JObj) ->
    publish_targeted_call(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_call(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_call(ServerId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?CALL_REQ_VALUES, fun call/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_call(kz_term:api_terms()) -> 'ok'.
publish_call(JObj) ->
    publish_call(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_call(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?CALL_REQ_VALUES, fun call/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"call">>, Name),
    publish(RoutingKey, Payload, ContentType).

-spec publish_targeted_send(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_send(ServerId, JObj) ->
    publish_targeted_send(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_send(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_send(ServerId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?SEND_REQ_VALUES, fun send/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_send(kz_term:api_terms()) -> 'ok'.
publish_send(JObj) ->
    publish_send(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_send(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_send(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?SEND_REQ_VALUES, fun send/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"send">>, Name),
    publish(RoutingKey, Payload, ContentType).

-spec publish_reply(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_reply(ServerId, JObj) ->
    publish_reply(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_reply(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_reply(ServerId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?REPLY_REQ_VALUES, fun reply_msg/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_register(kz_term:api_terms()) -> 'ok'.
publish_register(Req) ->
    publish_register(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_register(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?REGISTER_REQ_VALUES, fun register/1),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"register">>, name(Req)),
    publish(RoutingKey, Payload, ContentType).

-spec publish_register_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_register_resp(ServerId, JObj) ->
    publish_register_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_register_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register_resp(ServerId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?REGISTER_RESP_VALUES, fun register_resp/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_unregister(kz_term:api_terms()) -> 'ok'.
publish_unregister(Req) ->
    publish_unregister(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_unregister(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unregister(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?UNREGISTER_REQ_VALUES, fun unregister/1),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"unregister">>, name(Req)),
    publish(RoutingKey, Payload, ContentType).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(ServerId, JObj) ->
    publish_query_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(ServerId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?QUERY_RESP_VALUES, fun query_resp/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_query(kz_term:api_terms()) -> 'ok'.
publish_query(JObj) ->
    publish_query(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req), ?QUERY_REQ_VALUES, fun query/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"query">>, Name),
    publish(RoutingKey, Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, Name).

bind_q(Q, [Event|T], Name) ->
    _ = kz_amqp_util:bind_q_to_exchange(Q, ?GLOBALS_EVENT_ROUTING_KEY(Event, Name), ?GLOBALS_EXCHANGE),
    bind_q(Q, T, Name);
bind_q(_Q, [], _Name) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), any()) -> 'ok'.
unbind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, Name).

unbind_q(Q, [Event|T], Name) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Q, ?GLOBALS_EVENT_ROUTING_KEY(Event, Name), ?GLOBALS_EXCHANGE),
    unbind_q(Q, T, Name);
unbind_q(_Q, [], _Name) -> 'ok'.

publish(Routing, Payload, ContentType) ->
    kz_amqp_util:basic_publish(?GLOBALS_EXCHANGE, Routing, Payload, ContentType).
