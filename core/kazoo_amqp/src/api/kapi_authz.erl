%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handles authorization requests, responses, queue bindings AMQP API.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_authz).

-export([api_definitions/0, api_definition/1]).

-export([authz_req/1
        ,authz_req_v/1
        ,publish_authz_req/1
        ,publish_authz_req/2
        ]).
-export([authz_resp/1
        ,authz_resp_v/1
        ,publish_authz_resp/2
        ,publish_authz_resp/3
        ]).
-export([balance_check_req/1
        ,balance_check_req_v/1
        ,publish_balance_check_req/1
        ,publish_balance_check_req/2
        ]).
-export([balance_check_resp/1
        ,balance_check_resp_v/1
        ,publish_balance_check_resp/2
        ,publish_balance_check_resp/3
        ]).
-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,broadcast_authz_resp/1, broadcast_authz_resp/2
        ]).

-include_lib("kz_amqp_util.hrl").

-export_type([req/0, resp/0]).

-type req() :: kz_json:object().
-type resp() :: kz_json:object().

-define(EVENT_CATEGORY, <<"authz">>).
-define(KEY_AUTHZ_BROADCAST, <<"authz.authorize.broadcast">>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [authz_req_definition()
    ,authz_resp_definition()
    ,balance_check_req_definition()
    ,balance_check_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"authz_req">>) ->
    authz_req_definition();
api_definition(<<"authz_resp">>) ->
    authz_resp_definition();
api_definition(<<"balance_check_req">>) ->
    balance_check_req_definition();
api_definition(<<"balance_check_resp">>) ->
    balance_check_resp_definition().

-spec authz_req_definition() -> kapi_definition:api().
authz_req_definition() ->
    EventName = <<"authz_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Authorization Request">>}
              ,{fun kapi_definition:set_description/2, <<"Authorization Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun authz_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun authz_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_authz_req/1}
              ,{fun kapi_definition:set_binding/2, <<"authz.authorize">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-Direction">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"From">>
                                                            ,<<"Request">>
                                                            ,<<"To">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"From-Network-Addr">>
                                                            ,<<"From-Network-Port">>
                                                            ,<<"Other-Leg-Call-ID">>
                                                            ,<<"From-Network-Addr">>
                                                            ,<<"From-Network-Port">>
                                                            ,<<"Switch-Hostname">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"To">>, fun is_binary/1}
                ,{<<"From">>, fun is_binary/1}
                ,{<<"Call-ID">>, fun is_binary/1}
                ,{<<"Account-ID">>, fun is_binary/1}
                ,{<<"Caller-ID-Name">>, fun is_binary/1}
                ,{<<"Caller-ID-Number">>, fun is_binary/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Usage">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec authz_resp_definition() -> kapi_definition:api().
authz_resp_definition() ->
    EventName = <<"authz_resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Authorization Responses">>}
              ,{fun kapi_definition:set_description/2, <<"Authorization Responses">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun authz_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun authz_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_authz_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Is-Authorized">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Account-Billing">>
                                                            ,<<"Reseller-ID">>
                                                            ,<<"Reseller-Billing">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Call-Direction">>
                                                            ,<<"Soft-Limit">>
                                                            ,<<"Other-Leg-Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Is-Authorized">>, fun is_boolean/1}
                ,{<<"Global-Resource">>, fun is_boolean/1}
                ,{<<"Soft-Limit">>, fun is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec balance_check_req_definition() -> kapi_definition:api().
balance_check_req_definition() ->
    EventName = <<"balance_check_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Balance Check Request">>}
              ,{fun kapi_definition:set_description/2, <<"Balance Check Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun balance_check_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun balance_check_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_balance_check_req/1}
              ,{fun kapi_definition:set_binding/2, <<"authz.balance_check">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Accounts">>]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Accounts">>, fun is_list/1} %% TODO: allow only kz_term:ne_binaries()
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec balance_check_resp_definition() -> kapi_definition:api().
balance_check_resp_definition() ->
    EventName = <<"balance_check_resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Balance Check Response">>}
              ,{fun kapi_definition:set_description/2, <<"Balance Check Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun balance_check_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun balance_check_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_balance_check_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Balances">>]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Balances">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Authorization Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error..
%% @end
%%------------------------------------------------------------------------------
-spec authz_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
authz_req(Req) ->
    kapi_definition:build_message(Req, authz_req_definition()).

-spec authz_req_v(kz_term:api_terms()) -> boolean().
authz_req_v(Req) ->
    kapi_definition:validate(Req, authz_req_definition()).

-spec publish_authz_req(kz_term:api_terms()) -> 'ok'.
publish_authz_req(JObj) ->
    publish_authz_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_authz_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_authz_req(Req, ContentType) ->
    Definition = authz_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload
                                ,ContentType
                                ,kapi_definition:binding(authz_req_definition())
                                ).

%%------------------------------------------------------------------------------
%% @doc Authorization Response.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec authz_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
authz_resp(Req) ->
    kapi_definition:build_message(Req, authz_resp_definition()).

-spec authz_resp_v(kz_term:api_terms()) -> boolean().
authz_resp_v(Req) ->
    kapi_definition:validate(Req, authz_resp_definition()).

-spec publish_authz_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_authz_resp(Queue, JObj) ->
    publish_authz_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_authz_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_authz_resp(Queue, Resp, ContentType) ->
    Definition = authz_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Balance Check Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error..
%% @end
%%------------------------------------------------------------------------------
-spec balance_check_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
balance_check_req(Req) ->
    kapi_definition:build_message(Req, balance_check_req_definition()).

-spec balance_check_req_v(kz_term:api_terms()) -> boolean().
balance_check_req_v(Req) ->
    kapi_definition:validate(Req, balance_check_req_definition()).

-spec publish_balance_check_req(kz_term:api_terms()) -> 'ok'.
publish_balance_check_req(JObj) ->
    publish_balance_check_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_check_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_check_req(Req, ContentType) ->
    Definition = balance_check_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload
                                ,ContentType
                                ,kapi_definition:binding(balance_check_req_definition())
                                ).

%%------------------------------------------------------------------------------
%% @doc Balance Check Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error..
%% @end
%%------------------------------------------------------------------------------
-spec balance_check_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
balance_check_resp(Req) ->
    kapi_definition:build_message(Req, balance_check_resp_definition()).

-spec balance_check_resp_v(kz_term:api_terms()) -> boolean().
balance_check_resp_v(Req) ->
    kapi_definition:validate(Req, balance_check_resp_definition()).

-spec publish_balance_check_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_balance_check_resp(Queue, JObj) ->
    publish_balance_check_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_check_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_check_resp(Queue, Resp, ContentType) ->
    Definition = balance_check_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz `gen_listeners'.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, <<"authz.*">>);
bind_to_q(Q, ['authorize'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, kapi_definition:binding(authz_req_definition())),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(Q, ['balance_check'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, kapi_definition:binding(balance_check_req_definition())),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, <<"authz.*">>);
unbind_q_from(Q, ['authorize'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, kapi_definition:binding(authz_req_definition())),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
unbind_q_from(Q, ['balance_check'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, kapi_definition:binding(balance_check_req_definition())),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec broadcast_authz_resp(kz_term:api_terms()) -> 'ok'.
broadcast_authz_resp(JObj) ->
    broadcast_authz_resp(JObj, ?DEFAULT_CONTENT_TYPE).

-spec broadcast_authz_resp(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
broadcast_authz_resp(Resp, ContentType) ->
    Definition = authz_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_BROADCAST).
