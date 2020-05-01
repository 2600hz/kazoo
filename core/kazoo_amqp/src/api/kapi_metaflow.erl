%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Metaflow requests, responses, and errors.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_metaflow).

-export([api_definitions/0, api_definition/1]).

-export([flow/1
        ,flow_v/1
        ,publish_flow/1
        ,publish_flow/2
        ]).
-export([action/1
        ,action_v/1
        ,publish_action/1
        ,publish_action/2
        ]).
-export([bind_req/1
        ,bind_req_v/1
        ,publish_bind_req/1
        ,publish_bind_req/2
        ]).
-export([binding/1
        ,binding_v/1
        ,publish_binding/1
        ,publish_binding/2
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_bind_reply/2, publish_bind_reply/3]).

-include_lib("kz_amqp_util.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(METAFLOW_EXCHANGE, <<"metaflow">>).
-define(METAFLOW_EXCHANGE_TYPE, <<"topic">>).

-ifdef(TEST).
-export([flow_routing_key/1
        ,action_routing_key/2
        ,bind_req_routing_key/1
        ,binding_routing_key/2
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [flow_definition()
    ,action_definition()
    ,bind_req_definition()
    ,binding_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"flow">>) ->
    flow_definition();
api_definition(<<"action">>) ->
    action_definition();
api_definition(<<"bind_req">>) ->
    bind_req_definition();
api_definition(<<"binding">>) ->
    binding_definition().

-spec flow_definition() -> kapi_definition:api().
flow_definition() ->
    EventName = <<"flow">>,
    Category = <<"metaflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Metaflow Flow">>}
              ,{fun kapi_definition:set_description/2, <<"Metaflow Flow">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun flow/1}
              ,{fun kapi_definition:set_validate_fun/2, fun flow_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_flow/1}
              ,{fun kapi_definition:set_binding/2, fun flow_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Flow">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Flow">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec action_definition() -> kapi_definition:api().
action_definition() ->
    EventName = <<"action">>,
    Category = <<"metaflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Metaflow Request">>}
              ,{fun kapi_definition:set_description/2, <<"Metaflow Request - when streaming is needed">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun action/1}
              ,{fun kapi_definition:set_validate_fun/2, fun action_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_action/1}
              ,{fun kapi_definition:set_binding/2, fun action_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Data">>]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Action">>, [<<"audio_level">>
                                ,<<"break">>
                                ,<<"callflow">>
                                ,<<"hangup">>
                                ,<<"hold">>
                                ,<<"intercept">>
                                ,<<"move">>
                                ,<<"park">>
                                ,<<"play">>
                                ,<<"record_call">>
                                ,<<"resume">>
                                ,<<"say">>
                                ,<<"transfer">>
                                ,<<"tts">>
                                ,<<"unpark">>
                                ]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec bind_req_definition() -> kapi_definition:api().
bind_req_definition() ->
    EventName = <<"bind_req">>,
    Category = <<"metaflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Metaflow Bind">>}
              ,{fun kapi_definition:set_description/2, <<"Metaflow Bind">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun bind_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun bind_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_bind_req/1}
              ,{fun kapi_definition:set_binding/2, fun bind_req_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Authorizing-ID">>
                                                            ,<<"Authorizing-Type">>
                                                            ,<<"Binding-Leg">>
                                                            ,<<"Call-ID">>
                                                            ,<<"CallFlow-ID">>
                                                            ,<<"Resource-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec binding_definition() -> kapi_definition:api().
binding_definition() ->
    EventName = <<"bind">>,
    Category = <<"metaflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"">>}
              ,{fun kapi_definition:set_description/2, <<"">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun binding/1}
              ,{fun kapi_definition:set_validate_fun/2, fun binding_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_binding/1}
              ,{fun kapi_definition:set_binding/2, fun binding_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [[<<"Call">>, <<"Call-ID">>]
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Binding-Digit">>
                                                            ,<<"Digit-Timeout">>
                                                            ,<<"Endpoint-ID">>
                                                            ,<<"Listen-On">>
                                                            ,<<"Numbers">>
                                                            ,<<"Patterns">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Binding-Digit">>, ?ANY_DIGIT}
                ,{<<"Listen-On">>, [<<"aleg">>
                                   ,<<"bleg">>
                                   ,<<"both">>
                                   ,<<"peer">>
                                   ,<<"self">>
                                   ]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Digit-Timeout">>, fun binding_digit_timeout_v/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Flow.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec flow(kz_json:object() | kz_term:proplist()) -> kz_api:api_formatter_return().
flow(Req) ->
    kapi_definition:build_message(Req, flow_definition()).

-spec flow_v(kz_json:object() | kz_term:proplist()) -> boolean().
flow_v(Req) ->
    kapi_definition:validate(Req, flow_definition()).

-spec publish_flow(kz_term:api_terms()) -> 'ok'.
publish_flow(JObj) ->
    publish_flow(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flow(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flow(Req, ContentType) ->
    Definition = flow_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:basic_publish(?METAFLOW_EXCHANGE
                              ,(kapi_definition:binding(Definition))(rk_call_id(Req))
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Request metaflow.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec action(kz_json:object() | kz_term:proplist()) -> kz_api:api_formatter_return().
action(Req) ->
    kapi_definition:build_message(Req, action_definition()).

-spec action_v(kz_json:object() | kz_term:proplist()) -> boolean().
action_v(Req) ->
    kapi_definition:validate(Req, action_definition()).

-spec publish_action(kz_term:api_terms()) -> 'ok'.
publish_action(JObj) ->
    publish_action(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_action(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_action(Req, ContentType) ->
    Definition = action_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?METAFLOW_EXCHANGE
                              ,(kapi_definition:binding(Definition))(rk_call_id(Req), rk_action(Req))
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Bind metaflow.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec bind_req(kz_json:object() | kz_term:proplist()) -> kz_api:api_formatter_return().
bind_req(Req) ->
    kapi_definition:build_message(Req, bind_req_definition()).

-spec bind_req_v(kz_json:object() | kz_term:proplist()) -> boolean().
bind_req_v(Req) ->
    kapi_definition:validate(Req, bind_req_definition()).

-spec publish_bind_req(kz_term:api_terms()) -> 'ok'.
publish_bind_req(JObj) ->
    publish_bind_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_bind_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_bind_req(Req, ContentType) ->
    Definition = bind_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?METAFLOW_EXCHANGE
                              ,(kapi_definition:binding(Definition))(rk_account_id(Req))
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Asks for metaflows to be enabled for a call.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec binding(kz_term:api_terms()) -> kz_api:api_formatter_return().
binding(Req) ->
    kapi_definition:build_message(Req, binding_definition()).

-spec binding_v(kz_term:api_terms()) -> boolean().
binding_v(Req) ->
    kapi_definition:validate(Req, binding_definition()).

-spec publish_binding(kz_term:api_terms()) -> 'ok'.
publish_binding(API) ->
    publish_binding(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_binding(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_binding(API0, ContentType) ->
    API = ensure_callid(API0),
    Definition = binding_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?METAFLOW_EXCHANGE
                              ,(kapi_definition:binding(Definition))(rk_account_id(API), rk_call_id(API))
                              ,Payload
                              ,ContentType
                              ).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    ALL = ['bind_req', 'action', 'bindings'],
    bind_q(Queue, Props, props:get_value('restrict_to', Props, ALL)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist(), list()) -> 'ok'.
bind_q(Queue, Props, ['bind_req' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(bind_req_definition()))(AccountId)
                                   ,?METAFLOW_EXCHANGE
                                   ),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, ['action' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Action = props:get_value('action', Props, <<"*">>),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(action_definition()))(CallId, Action)
                                   ,?METAFLOW_EXCHANGE
                                   ),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, ['flow' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(flow_definition()))(CallId)
                                   ,?METAFLOW_EXCHANGE
                                   ),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, ['bindings' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(binding_definition()))(AccountId, CallId)
                                   ,?METAFLOW_EXCHANGE
                                   ),
    bind_q(Queue, Props, T);
bind_q(Queue, Props, [_U | T]) ->
    lager:debug("unknown restriction ~p in metaflow bind", [_U]),
    bind_q(Queue, Props, T);
bind_q(_, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    ALL = ['bind_req', 'action', 'bindings'],
    unbind_q(Queue, Props, props:get_value('restrict_to', Props, ALL)).

-spec unbind_q(kz_term:ne_binary(), list(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props, ['bind_req' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue
                                              ,(kapi_definition:binding(bind_req_definition()))(AccountId)
                                              ,?METAFLOW_EXCHANGE
                                              ),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, ['action' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Action = props:get_value('action', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue
                                              ,(kapi_definition:binding(action_definition()))(CallId, Action)
                                              ,?METAFLOW_EXCHANGE
                                              ),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, ['flow' | T]) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue
                                              ,(kapi_definition:binding(flow_definition()))(CallId)
                                              ,?METAFLOW_EXCHANGE
                                              ),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, ['bindings' | T]) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue
                                              ,(kapi_definition:binding(binding_definition()))(AccountId, CallId)
                                              ,?METAFLOW_EXCHANGE
                                              ),
    unbind_q(Queue, Props, T);
unbind_q(Queue, Props, [_U | T]) ->
    lager:debug("unknown restriction ~p in metaflow unbind", [_U]),
    unbind_q(Queue, Props, T);
unbind_q(_, _, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?METAFLOW_EXCHANGE, ?METAFLOW_EXCHANGE_TYPE).

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

-spec publish_bind_reply(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_bind_reply(Q, API) ->
    publish_bind_reply(Q, API, ?DEFAULT_CONTENT_TYPE).

-spec publish_bind_reply(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_bind_reply(Q, API, ContentType) ->
    Definition = binding_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

-spec callid(kz_term:api_terms()) -> kz_term:api_binary().
callid([_|_]=Props) ->
    case props:get_value(<<"Call-ID">>, Props) of
        'undefined' -> callid(props:get_value(<<"Call">>, Props));
        CallId -> CallId
    end;
callid(JObj) ->
    kz_json:get_first_defined([<<"Call-ID">>
                              ,[<<"Call">>, <<"Call-ID">>]
                              ], JObj).

-spec ensure_callid(kz_term:api_terms()) -> kz_term:api_terms().
ensure_callid([_|_]=Props) ->
    props:insert_value(<<"Call-ID">>, callid(Props), Props);
ensure_callid(JObj) ->
    kz_json:set_value(<<"Call-ID">>, callid(JObj), JObj).

-spec binding_digit_timeout_v(any()) -> boolean().
binding_digit_timeout_v(X) ->
    is_integer(kz_term:to_integer(X)).

-spec flow_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
flow_routing_key(CallId) ->
    <<"metaflow.flow.", (kz_amqp_util:encode(CallId))/binary>>.

-spec action_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
action_routing_key(CallId, Action) ->
    <<"metaflow.action.", (kz_amqp_util:encode(CallId))/binary, ".", (Action)/binary>>.

-spec bind_req_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
bind_req_routing_key(AccountId) ->
    <<"metaflow.bind_req.", (kz_amqp_util:encode(AccountId))/binary>>.

-spec binding_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
binding_routing_key(AccountId, CallId) ->
    <<"metaflow.bind."
     ,(kz_amqp_util:encode(AccountId))/binary
     ,"."
     ,(kz_amqp_util:encode(CallId))/binary
    >>.
