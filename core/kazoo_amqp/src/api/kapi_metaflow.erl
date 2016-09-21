%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Metaflow requests, responses, and errors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_metaflow).

-export([action/1, action_v/1]).
-export([bind_req/1, bind_req_v/1]).
-export([binding/1, binding_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_action/1, publish_action/2]).
-export([publish_bind_req/1, publish_bind_req/2]).
-export([publish_bind_reply/2, publish_bind_reply/3]).
-export([publish_binding/1, publish_binding/2]).

-include_lib("amqp_util.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(METAFLOW_EXCHANGE, <<"metaflow">>).
-define(METAFLOW_EXCHANGE_TYPE, <<"topic">>).

%% Metaflow Request - when streaming is needed
-define(METAFLOW_ACTION_HEADERS, [<<"Action">>, <<"Call-ID">>]).
-define(OPTIONAL_METAFLOW_ACTION_HEADERS, [<<"Data">>]).
-define(METAFLOW_ACTION_VALUES, [{<<"Event-Category">>, <<"metaflow">>}
                                ,{<<"Event-Name">>, <<"action">>}
                                ,{<<"Action">>, [<<"transfer">>
                                                ,<<"hangup">>
                                                ,<<"callflow">>
                                                ,<<"break">>
                                                ,<<"intercept">>
                                                ,<<"move">>
                                                ,<<"park">>
                                                ,<<"unpark">>
                                                ,<<"play">>
                                                ,<<"say">>
                                                ,<<"audio_level">>
                                                ,<<"hold">>
                                                ,<<"record_call">>
                                                ,<<"resume">>
                                                ,<<"tts">>
                                                ]
                                 }
                                ]).
-define(METAFLOW_ACTION_TYPES, []).

-define(METAFLOW_ACTION_ROUTING_KEY(CallId, Action)
       ,<<"metaflow.action.", (amqp_util:encode(CallId))/binary, ".", (Action)/binary>>
       ).

%% Metaflow Bind
-define(METAFLOW_BIND_REQ_HEADERS, [<<"Account-ID">>, <<"Binding-Leg">>]).
-define(OPTIONAL_METAFLOW_BIND_REQ_HEADERS, [<<"Call-ID">>
                                            ,<<"Authorizing-ID">>
                                            ,<<"Authorizing-Type">>
                                            ,<<"Resource-ID">>
                                            ,<<"CallFlow-ID">>
                                            ]).
-define(METAFLOW_BIND_REQ_VALUES, [{<<"Event-Category">>, <<"metaflow">>}
                                  ,{<<"Event-Name">>, <<"bind_req">>}
                                  ]).
-define(METAFLOW_BIND_REQ_TYPES, []).

-define(METAFLOW_BIND_REQ_ROUTING_KEY(AccountId, Leg)
       ,<<"metaflow.bind_req.", (Leg)/binary, ".", (amqp_util:encode(AccountId))/binary>>
       ).


-define(METAFLOW_BIND_HEADERS, [[<<"Call">> , <<"Call-ID">>]]).
-define(OPTIONAL_METAFLOW_BIND_HEADERS, [<<"Numbers">>, <<"Patterns">>
                                        ,<<"Binding-Digit">>, <<"Digit-Timeout">>
                                        ,<<"Endpoint-ID">>, <<"Listen-On">>
                                        ]).
-define(METAFLOW_BIND_VALUES, [{<<"Event-Category">>, <<"metaflow">>}
                              ,{<<"Event-Name">>, <<"bind">>}
                              ,{<<"Binding-Digit">>, ?ANY_DIGIT}
                              ,{<<"Listen-On">>, [<<"both">>, <<"self">>, <<"peer">>, <<"aleg">>, <<"bleg">>]}
                              ]).
-define(METAFLOW_BIND_TYPES, [{<<"Numbers">>, fun kz_json:is_json_object/1}
                             ,{<<"Patterns">>, fun kz_json:is_json_object/1}
                             ,{<<"Digit-Timeout">>, fun binding_digit_timeout_v/1}
                             ]).
-define(METAFLOW_BIND_ROUTING_KEY(AccountId, CallId), <<"metaflow.bind.", (amqp_util:encode(AccountId))/binary, ".", (amqp_util:encode(CallId))/binary>>).

%%--------------------------------------------------------------------
%% @doc Request metaflow - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec action(kz_json:object() | kz_proplist()) ->
                    {'ok', iolist()} |
                    {'error', string()}.
action(Prop) when is_list(Prop) ->
    case action_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?METAFLOW_ACTION_HEADERS, ?OPTIONAL_METAFLOW_ACTION_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow_action"}
    end;
action(JObj) -> action(kz_json:to_proplist(JObj)).

-spec action_v(kz_json:object() | kz_proplist()) -> boolean().
action_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?METAFLOW_ACTION_HEADERS, ?METAFLOW_ACTION_VALUES, ?METAFLOW_ACTION_TYPES);
action_v(JObj) -> action_v(kz_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc bind metaflow - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bind_req(kz_json:object() | kz_proplist()) ->
                      {'ok', iolist()} |
                      {'error', string()}.
bind_req(Prop) when is_list(Prop) ->
    case bind_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?METAFLOW_BIND_REQ_HEADERS, ?OPTIONAL_METAFLOW_BIND_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow_bind"}
    end;
bind_req(JObj) -> bind_req(kz_json:to_proplist(JObj)).

-spec bind_req_v(kz_json:object() | kz_proplist()) -> boolean().
bind_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?METAFLOW_BIND_REQ_HEADERS, ?METAFLOW_BIND_REQ_VALUES, ?METAFLOW_BIND_REQ_TYPES);
bind_req_v(JObj) -> bind_req_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    ALL = ['bind_req', 'action', 'bindings'],
    bind_q(Queue, Props, props:get_value('restrict_to', Props, ALL)).

-spec bind_q(ne_binary(), kz_proplist(), list()) -> 'ok'.
bind_q(Queue, Props, ['bind_req' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    Leg = props:get_value('leg', Props, <<"*">>),
    amqp_util:bind_q_to_exchange(Queue, ?METAFLOW_BIND_REQ_ROUTING_KEY(AccountId, Leg), ?METAFLOW_EXCHANGE),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, ['action' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Action = props:get_value('action', Props, <<"*">>),
    amqp_util:bind_q_to_exchange(Queue, ?METAFLOW_ACTION_ROUTING_KEY(CallId, Action), ?METAFLOW_EXCHANGE),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, ['bindings' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    amqp_util:bind_q_to_exchange(Queue, ?METAFLOW_BIND_ROUTING_KEY(AccountId, CallId), ?METAFLOW_EXCHANGE),
    bind_q(Queue, Props, T);
bind_q(_, _, []) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    ALL = ['bind_req', 'action', 'bindings'],
    unbind_q(Queue, Props, props:get_value('restrict_to', Props, ALL)).

-spec unbind_q(ne_binary(), list(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props, ['bind_req' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    Leg = props:get_value('leg', Props, <<"*">>),
    amqp_util:unbind_q_from_exchange(Queue, ?METAFLOW_BIND_REQ_ROUTING_KEY(AccountId, Leg), ?METAFLOW_EXCHANGE),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, ['action' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Action = props:get_value('action', Props, <<"*">>),
    amqp_util:unbind_q_from_exchange(Queue, ?METAFLOW_ACTION_ROUTING_KEY(CallId, Action), ?METAFLOW_EXCHANGE),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, ['bindings' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    amqp_util:unbind_q_from_exchange(Queue, ?METAFLOW_BIND_ROUTING_KEY(AccountId, CallId), ?METAFLOW_EXCHANGE),
    unbind_q(Queue, Props, T);
unbind_q(_, _, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?METAFLOW_EXCHANGE, ?METAFLOW_EXCHANGE_TYPE).

-spec publish_action(api_terms()) -> 'ok'.
-spec publish_action(api_terms(), ne_binary()) -> 'ok'.
publish_action(JObj) ->
    publish_action(JObj, ?DEFAULT_CONTENT_TYPE).
publish_action(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?METAFLOW_ACTION_VALUES, fun action/1),
    RK = ?METAFLOW_ACTION_ROUTING_KEY(rk_call_id(Req), rk_action(Req)),
    amqp_util:basic_publish(?METAFLOW_EXCHANGE, RK, Payload, ContentType).

-spec publish_bind_req(api_terms()) -> 'ok'.
-spec publish_bind_req(api_terms(), ne_binary()) -> 'ok'.
publish_bind_req(JObj) ->
    publish_bind_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_bind_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?METAFLOW_BIND_REQ_VALUES, fun bind_req/1),
    RK = ?METAFLOW_BIND_REQ_ROUTING_KEY(rk_account_id(Req), rk_binding_leg(Req)),
    amqp_util:basic_publish(?METAFLOW_EXCHANGE, RK, Payload, ContentType).

rk_action([_|_]=API) ->
    props:get_value(<<"Action">>, API);
rk_action(JObj) ->
    kz_json:get_value(<<"Action">>, JObj).

rk_call_id([_|_]=API) ->
    props:get_value(<<"Call-ID">>, API);
rk_call_id(JObj) ->
    kz_json:get_value(<<"Call-ID">>, JObj).

rk_account_id([_|_]=API) ->
    props:get_value(<<"Account-ID">>, API);
rk_account_id(JObj) ->
    kz_json:get_value(<<"Account-ID">>, JObj).

rk_binding_leg([_|_]=API) ->
    props:get_value(<<"Binding-Leg">>, API);
rk_binding_leg(JObj) ->
    kz_json:get_value(<<"Binding-Leg">>, JObj).

-spec publish_binding(api_terms()) -> 'ok'.
-spec publish_binding(api_terms(), ne_binary()) -> 'ok'.
publish_binding(API) ->
    publish_binding(API, ?DEFAULT_CONTENT_TYPE).
publish_binding(API0, ContentType) ->
    API = ensure_callid(API0),
    CallId = rk_call_id(API),
    AccountId = rk_account_id(API),
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?METAFLOW_BIND_VALUES, fun binding/1),
    RK = ?METAFLOW_BIND_ROUTING_KEY(AccountId, CallId),
    amqp_util:basic_publish(?METAFLOW_EXCHANGE, RK, Payload, ContentType).

-spec publish_bind_reply(ne_binary(), api_terms()) -> 'ok'.
-spec publish_bind_reply(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_bind_reply(Q, API) ->
    publish_bind_reply(Q, API, ?DEFAULT_CONTENT_TYPE).
publish_bind_reply(Q, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?METAFLOW_BIND_VALUES, fun binding/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec callid(api_terms()) -> api_binary().
callid([_|_]=Props) ->
    case props:get_value(<<"Call-ID">>, Props) of
        'undefined' -> callid(props:get_value(<<"Call">>, Props));
        CallId -> CallId
    end;
callid(JObj) ->
    kz_json:get_first_defined([<<"Call-ID">>
                              ,[<<"Call">>, <<"Call-ID">>]
                              ], JObj).

-spec ensure_callid(api_terms()) -> api_terms().
ensure_callid([_|_]=Props) ->
    props:insert_value(<<"Call-ID">>, callid(Props), Props);
ensure_callid(JObj) ->
    kz_json:set_value(<<"Call-ID">>, callid(JObj), JObj).

%%--------------------------------------------------------------------
%% @doc Asks for metaflows to be enabled for a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec binding(api_terms()) -> api_formatter_return().
binding(Prop) when is_list(Prop) ->
    case binding_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?METAFLOW_BIND_HEADERS, ?OPTIONAL_METAFLOW_BIND_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow binding"}
    end;
binding(JObj) -> binding(kz_json:to_proplist(JObj)).

-spec binding_v(api_terms()) -> boolean().
binding_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?METAFLOW_BIND_HEADERS, ?METAFLOW_BIND_VALUES, ?METAFLOW_BIND_TYPES);
binding_v(JObj) -> binding_v(kz_json:to_proplist(JObj)).

-spec binding_digit_timeout_v(any()) -> boolean().
binding_digit_timeout_v(X) ->
    is_integer(kz_util:to_integer(X)).
