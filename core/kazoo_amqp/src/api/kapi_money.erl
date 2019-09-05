%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc APIs for events concerning money (like credits, debits, and others).
%%%
%%% Types of events known:
%%% <dl>
%%%   <dt>`credit'</dt><dd>A credit has been added to account-id</dd>
%%%   <dt>`debit'</dt><dd>A debit has been applied to account-id</dd>
%%%   <dt>`balance'</dt><dd>A request for any whapp with the balance of account-id to reply</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_money).

-export([api_definitions/0, api_definition/1]).

-export([credit/1
        ,credit_v/1
        ,publish_credit/1
        ,publish_credit/2
        ]).
-export([debit/1
        ,debit_v/1
        ,publish_debit/1
        ,publish_debit/2
        ]).
-export([balance_req/1
        ,balance_req_v/1
        ,publish_balance_req/1
        ,publish_balance_req/2
        ]).
-export([balance_resp/1
        ,balance_resp_v/1
        ,publish_balance_resp/2
        ,publish_balance_resp/3
        ]).
-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include_lib("kz_amqp_util.hrl").

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [credit_definition()
    ,debit_definition()
    ,balance_req_definition()
    ,balance_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"credit">>) ->
    credit_definition();
api_definition(<<"debit">>) ->
    debit_definition();
api_definition(<<"balance_req">>) ->
    balance_req_definition();
api_definition(<<"balance_resp">>) ->
    balance_resp_definition().

-spec credit_definition() -> kapi_definition:api().
credit_definition() ->
    EventName = <<"credit">>,
    Category = <<"transaction">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Credit Update">>}
              ,{fun kapi_definition:set_description/2, <<"Credit Update">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun credit/1}
              ,{fun kapi_definition:set_validate_fun/2, fun credit_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_credit/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Amount">>
                                                            ,<<"Transaction-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec debit_definition() -> kapi_definition:api().
debit_definition() ->
    EventName = <<"debit">>,
    Category = <<"transaction">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Debit Update">>}
              ,{fun kapi_definition:set_description/2, <<"Debit Update">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun debit/1}
              ,{fun kapi_definition:set_validate_fun/2, fun debit_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_debit/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Amount">>
                                                            ,<<"Transaction-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec balance_req_definition() -> kapi_definition:api().
balance_req_definition() ->
    EventName = <<"balance_req">>,
    Category = <<"transaction">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Balance Request">>}
              ,{fun kapi_definition:set_description/2, <<"Balance Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun balance_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun balance_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_balance_req/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec balance_resp_definition() -> kapi_definition:api().
balance_resp_definition() ->
    EventName = <<"balance_resp">>,
    Category = <<"transaction">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Balance Response">>}
              ,{fun kapi_definition:set_description/2, <<"Balance Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun balance_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun balance_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_balance_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Two-Way">>
                                                            ,<<"Inbound">>
                                                            ,<<"Prepay">>
                                                            ,<<"Max-Two-Way">>
                                                            ,<<"Max-Inbound">>
                                                            ,<<"Trunks">>
                                                            ,<<"Node">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Credit Update.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec credit(kz_term:api_terms()) -> kz_api:api_formatter_return().
credit(Req) ->
    kapi_definition:build_message(Req, credit_definition()).

-spec credit_v(kz_term:api_terms()) -> boolean().
credit_v(Req) ->
    kapi_definition:validate(Req, credit_definition()).

-spec publish_credit(kz_term:api_terms()) -> 'ok'.
publish_credit(Req) ->
    publish_credit(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_credit(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_credit(Req, ContentType) ->
    Definition = credit_definition(),
    RoutingKey = list_to_binary([<<"transaction.credit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Debit Update.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec debit(kz_term:api_terms()) -> kz_api:api_formatter_return().
debit(Req) ->
    kapi_definition:build_message(Req, debit_definition()).

-spec debit_v(kz_term:api_terms()) -> boolean().
debit_v(Req) ->
    kapi_definition:validate(Req, debit_definition()).

-spec publish_debit(kz_term:api_terms()) -> 'ok'.
publish_debit(Req) ->
    publish_debit(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_debit(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_debit(Req, ContentType) ->
    Definition = debit_definition(),
    RoutingKey = list_to_binary([<<"transaction.debit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Balance Request.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec balance_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
balance_req(Req) ->
    kapi_definition:build_message(Req, balance_req_definition()).

-spec balance_req_v(kz_term:api_terms()) -> boolean().
balance_req_v(Req) ->
    kapi_definition:validate(Req, balance_req_definition()).

-spec publish_balance_req(kz_term:api_terms()) -> 'ok'.
publish_balance_req(Req) ->
    publish_balance_req(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_req(Req, ContentType) ->
    Definition = balance_req_definition(),
    RoutingKey = list_to_binary([<<"transaction.balance.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Balance Response.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec balance_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
balance_resp(Req) ->
    kapi_definition:build_message(Req, balance_resp_definition()).

-spec balance_resp_v(kz_term:api_terms()) -> boolean().
balance_resp_v(Req) ->
    kapi_definition:validate(Req, balance_resp_definition()).

-spec publish_balance_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_balance_resp(Queue, Req) ->
    publish_balance_resp(Queue, Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_resp(Queue, Req, ContentType) ->
    Definition = balance_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Routing = routing_key(Props),
    kz_amqp_util:bind_q_to_configuration(Queue, Routing).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Routing = routing_key(Props),
    kz_amqp_util:unbind_q_from_configuration(Queue, Routing).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:configuration_exchange().

routing_key(Props) ->
    list_to_binary([<<"transaction.">>
                   ,props:get_value('type', Props, <<"*">>) %% credit/debit/balance/other
                   ,<<".">>
                   ,props:get_value('account_id', Props, <<"*">>)
                   ]).
