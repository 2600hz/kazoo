%%%-------------------------------------------------------------------
%%% @copyright (C) 2011 VoIP INC
%%% @doc
%%% Globals API
%%% @end
%%%-------------------------------------------------------------------
-module(kapi_globals).

-compile({no_auto_import,[unregister/1]}).

-export([name/1]).
-export([message/1, reply/1]).
-export([encode/1, decode/1]).
-export([state/1]).
-export([is_pending/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([query/1, query_v/1]).
-export([query_resp/1, query_resp_v/1]).
-export([send/1, send_v/1]).
-export([call/1, call_v/1]).
-export([reply_msg/1, reply_msg_v/1]).
-export([register/1, register_v/1]).
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

-include_lib("kazoo/include/kz_api.hrl").

%% Types & Accessors
-type state() :: 'none' | 'local' | 'pending' | 'remote'.
-export_type([state/0]).

-spec encode(term()) -> ne_binary().
encode(Term) ->
    base64:encode(term_to_binary(Term)).

-spec decode(binary()) -> term().
decode(Bin) ->
    binary_to_term(base64:decode(Bin)).

-spec name(kz_json:object() | kz_proplist()) -> ne_binary().
name(Props)
  when is_list(Props) ->
    props:get_ne_binary_value(<<"Name">>, Props);
name(JObj) ->
    kz_json:get_ne_binary_value(<<"Name">>, JObj).

-spec message(kz_json:object()) -> ne_binary().
message(JObj) ->
    decode(kz_json:get_value(<<"Message">>, JObj)).

-spec reply(kz_json:object()) -> ne_binary().
reply(JObj) ->
    decode(kz_json:get_value(<<"Reply">>, JObj)).

-spec state(kz_json:object()) -> state().
state(JObj) ->
    kz_json:get_atom_value(<<"State">>, JObj).

-spec is_pending(kz_json:object()) -> boolean().
is_pending(JObj) ->
    state(JObj) =:= 'pending'.

-define(GLOBALS_EXCHANGE, <<"globals">>).
-define(GLOBALS_EXCHANGE_TYPE, <<"topic">>).

%% Globals Events
-define(GLOBALS_EVENT_ROUTING_KEY(Event, Name), <<"globals."
                                                 ,(kz_util:to_binary(Event))/binary
                                                 ,"."
                                                 ,(amqp_util:encode(Name))/binary
                                               >>).

-define(QUERY_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_QUERY_REQ_HEADERS, []).
-define(QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                              ,{<<"Event-Name">>, <<"query">>}
                             ]).
-define(QUERY_REQ_TYPES, []).

-define(QUERY_RESP_HEADERS, [<<"Name">>]).
-define(OPTIONAL_QUERY_RESP_HEADERS, []).
-define(QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"globals">>}
                              ,{<<"Event-Name">>, <<"query_resp">>}
                             ]).
-define(QUERY_RESP_TYPES, []).

-define(REGISTER_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_REGISTER_REQ_HEADERS, [<<"State">>]).
-define(REGISTER_REQ_VALUES, [{<<"Event-Category">>, <<"globals">>}
                              ,{<<"Event-Name">>, <<"register">>}
                             ]).
-define(REGISTER_REQ_TYPES, []).

-define(UNREGISTER_REQ_HEADERS, [<<"Name">>]).
-define(OPTIONAL_UNREGISTER_REQ_HEADERS, []).
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


-spec register(api_terms()) -> api_formatter_return().
register(Prop) when is_list(Prop) ->
    case register_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_REQ_HEADERS, ?OPTIONAL_REGISTER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals register"}
    end;
register(JObj) ->
    register(kz_json:to_proplist(JObj)).

-spec register_v(api_terms()) -> boolean().
register_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_REQ_HEADERS, ?REGISTER_REQ_VALUES, ?REGISTER_REQ_TYPES);
register_v(JObj) ->
    register_v(kz_json:to_proplist(JObj)).


-spec unregister(api_terms()) -> api_formatter_return().
unregister(Prop) when is_list(Prop) ->
    case unregister_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UNREGISTER_REQ_HEADERS, ?OPTIONAL_UNREGISTER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals unregister"}
    end;
unregister(JObj) ->
    unregister(kz_json:to_proplist(JObj)).

-spec unregister_v(api_terms()) -> boolean().
unregister_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UNREGISTER_REQ_HEADERS, ?UNREGISTER_REQ_VALUES, ?UNREGISTER_REQ_TYPES);
unregister_v(JObj) ->
    unregister_v(kz_json:to_proplist(JObj)).


-spec call(api_terms()) -> api_formatter_return().
call(Prop) when is_list(Prop) ->
    case call_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CALL_REQ_HEADERS, ?OPTIONAL_CALL_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
call(JObj) ->
    call(kz_json:to_proplist(JObj)).

-spec call_v(api_terms()) -> boolean().
call_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?CALL_REQ_VALUES, ?CALL_REQ_TYPES);
call_v(JObj) ->
    call_v(kz_json:to_proplist(JObj)).


-spec send(api_terms()) -> api_formatter_return().
send(Prop) when is_list(Prop) ->
    case send_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEND_REQ_HEADERS, ?OPTIONAL_SEND_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals send"}
    end;
send(JObj) ->
    send(kz_json:to_proplist(JObj)).

-spec send_v(api_terms()) -> boolean().
send_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEND_REQ_HEADERS, ?SEND_REQ_VALUES, ?SEND_REQ_TYPES);
send_v(JObj) ->
    send_v(kz_json:to_proplist(JObj)).

-spec reply_msg(api_terms()) -> api_formatter_return().
reply_msg(Prop) when is_list(Prop) ->
    case reply_msg_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REPLY_REQ_HEADERS, ?OPTIONAL_REPLY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals reply"}
    end;
reply_msg(JObj) ->
    reply_msg(kz_json:to_proplist(JObj)).

-spec reply_msg_v(api_terms()) -> boolean().
reply_msg_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REPLY_REQ_HEADERS, ?REPLY_REQ_VALUES, ?REPLY_REQ_TYPES);
reply_msg_v(JObj) ->
    reply_msg_v(kz_json:to_proplist(JObj)).

-spec query(api_terms()) -> api_formatter_return().
query(Prop) when is_list(Prop) ->
    case query_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_REQ_HEADERS, ?OPTIONAL_QUERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
query(JObj) ->
    query(kz_json:to_proplist(JObj)).

-spec query_v(api_terms()) -> boolean().
query_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_REQ_HEADERS, ?QUERY_REQ_VALUES, ?QUERY_REQ_TYPES);
query_v(JObj) ->
    query_v(kz_json:to_proplist(JObj)).


-spec query_resp(api_terms()) -> api_formatter_return().
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_RESP_HEADERS, ?OPTIONAL_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for globals call"}
    end;
query_resp(JObj) ->
    query_resp(kz_json:to_proplist(JObj)).

-spec query_resp_v(api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_RESP_HEADERS, ?QUERY_RESP_VALUES, ?QUERY_RESP_TYPES);
query_resp_v(JObj) ->
    query_resp_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?GLOBALS_EXCHANGE, ?GLOBALS_EXCHANGE_TYPE).

-spec publish_targeted_call(ne_binary(), api_terms()) -> 'ok'.
-spec publish_targeted_call(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_targeted_call(ServerId, JObj) ->
    publish_targeted_call(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_targeted_call(ServerId, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?CALL_REQ_VALUES, fun ?MODULE:call/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_call(api_terms()) -> 'ok'.
-spec publish_call(api_terms(), ne_binary()) -> 'ok'.
publish_call(JObj) ->
    publish_call(JObj, ?DEFAULT_CONTENT_TYPE).
publish_call(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?CALL_REQ_VALUES, fun ?MODULE:call/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"call">>, Name),
    publish(RoutingKey, Payload, ContentType).

-spec publish_targeted_send(ne_binary(), api_terms()) -> 'ok'.
-spec publish_targeted_send(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_targeted_send(ServerId, JObj) ->
    publish_targeted_send(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_targeted_send(ServerId, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?SEND_REQ_VALUES, fun ?MODULE:send/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_send(api_terms()) -> 'ok'.
-spec publish_send(api_terms(), ne_binary()) -> 'ok'.
publish_send(JObj) ->
    publish_send(JObj, ?DEFAULT_CONTENT_TYPE).
publish_send(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?SEND_REQ_VALUES, fun ?MODULE:send/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"send">>, Name),
    publish(RoutingKey, Payload, ContentType).

-spec publish_reply(ne_binary(), api_terms()) -> 'ok'.
-spec publish_reply(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_reply(ServerId, JObj) ->
    publish_reply(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_reply(ServerId, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?REPLY_REQ_VALUES, fun ?MODULE:reply_msg/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_register(api_terms()) -> 'ok'.
-spec publish_register(api_terms(), ne_binary()) -> 'ok'.
publish_register(Req) ->
    publish_register(Req, ?DEFAULT_CONTENT_TYPE).
publish_register(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?REGISTER_REQ_VALUES, fun ?MODULE:register/1),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"register">>, name(Req)),
    publish(RoutingKey, Payload, ContentType).

-spec publish_register_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_register_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_register_resp(ServerId, JObj) ->
    publish_register_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_register_resp(ServerId, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?REGISTER_REQ_VALUES, fun ?MODULE:register/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_unregister(api_terms()) -> 'ok'.
-spec publish_unregister(api_terms(), ne_binary()) -> 'ok'.
publish_unregister(Req) ->
    publish_unregister(Req, ?DEFAULT_CONTENT_TYPE).
publish_unregister(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?UNREGISTER_REQ_VALUES, fun ?MODULE:unregister/1),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"unregister">>, name(Req)),
    publish(RoutingKey, Payload, ContentType).

-spec publish_query_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_resp(ServerId, JObj) ->
    publish_query_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_resp(ServerId, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?QUERY_RESP_VALUES, fun ?MODULE:query_resp/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_query(api_terms()) -> 'ok'.
-spec publish_query(api_terms(), ne_binary()) -> 'ok'.
publish_query(JObj) ->
    publish_query(JObj, ?DEFAULT_CONTENT_TYPE).
publish_query(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?QUERY_REQ_VALUES, fun ?MODULE:query/1),
    Name = name(Req),
    RoutingKey = ?GLOBALS_EVENT_ROUTING_KEY(<<"query">>, Name),
    publish(RoutingKey, Payload, ContentType).


-spec bind_q(ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, Name).

bind_q(Q, [Event|T], Name) ->
    _ = amqp_util:bind_q_to_exchange(Q, ?GLOBALS_EVENT_ROUTING_KEY(Event, Name), ?GLOBALS_EXCHANGE),
    bind_q(Q, T, Name);
bind_q(_Q, [], _Name) -> 'ok'.


-spec unbind_q(ne_binary(), any()) -> 'ok'.
unbind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, Name).

unbind_q(Q, [Event|T], Name) ->
    _ = amqp_util:unbind_q_from_exchange(Q, ?GLOBALS_EVENT_ROUTING_KEY(Event, Name), ?GLOBALS_EXCHANGE),
    unbind_q(Q, T, Name);
unbind_q(_Q, [], _Name) -> 'ok'.

publish(Routing, Payload, ContentType) ->
    amqp_util:basic_publish(?GLOBALS_EXCHANGE, Routing, Payload, ContentType).
