%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
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
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_money).

-export([credit/1, credit_v/1
        ,debit/1, debit_v/1
        ,balance_req/1, balance_req_v/1
        ,balance_resp/1, balance_resp_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_credit/1, publish_credit/2
        ,publish_debit/1, publish_debit/2
        ,publish_balance_req/1, publish_balance_req/2
        ,publish_balance_resp/2, publish_balance_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(CREDIT_HEADERS, [<<"Account-ID">>, <<"Amount">>, <<"Transaction-ID">>]).
-define(OPTIONAL_CREDIT_HEADERS, []).
-define(CREDIT_VALUES, [{<<"Event-Category">>, <<"transaction">>}
                       ,{<<"Event-Name">>, <<"credit">>}
                       ]).
-define(CREDIT_TYPES, []).

-define(DEBIT_HEADERS, [<<"Account-ID">>, <<"Amount">>, <<"Transaction-ID">>]).
-define(OPTIONAL_DEBIT_HEADERS, []).
-define(DEBIT_VALUES, [{<<"Event-Category">>, <<"transaction">>}
                      ,{<<"Event-Name">>, <<"debit">>}
                      ]).
-define(DEBIT_TYPES, []).

-define(BALANCE_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_BALANCE_REQ_HEADERS, []).
-define(BALANCE_REQ_VALUES, [{<<"Event-Category">>, <<"transaction">>}
                            ,{<<"Event-Name">>, <<"balance_req">>}
                            ]).
-define(BALANCE_REQ_TYPES, []).

-define(BALANCE_RESP_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_BALANCE_RESP_HEADERS, [<<"Two-Way">>, <<"Inbound">>, <<"Prepay">>
                                       ,<<"Max-Two-Way">>, <<"Max-Inbound">>
                                       ,<<"Trunks">>, <<"Node">>
                                       ]).
-define(BALANCE_RESP_VALUES, [{<<"Event-Category">>, <<"transaction">>}
                             ,{<<"Event-Name">>, <<"balance_resp">>}
                             ]).
-define(BALANCE_RESP_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Credit Update.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec credit(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
credit(Prop) when is_list(Prop) ->
    case credit_v(Prop) of
        true -> kz_api:build_message(Prop, ?CREDIT_HEADERS, ?OPTIONAL_CREDIT_HEADERS);
        false -> {error, "Proplist failed validation for credit"}
    end;
credit(JObj) ->
    credit(kz_json:to_proplist(JObj)).

-spec credit_v(kz_term:api_terms()) -> boolean().
credit_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CREDIT_HEADERS, ?CREDIT_VALUES, ?CREDIT_TYPES);
credit_v(JObj) ->
    credit_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Debit Update.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec debit(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
debit(Prop) when is_list(Prop) ->
    case debit_v(Prop) of
        true -> kz_api:build_message(Prop, ?DEBIT_HEADERS, ?OPTIONAL_DEBIT_HEADERS);
        false -> {error, "Proplist failed validation for debit"}
    end;
debit(JObj) ->
    debit(kz_json:to_proplist(JObj)).

-spec debit_v(kz_term:api_terms()) -> boolean().
debit_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DEBIT_HEADERS, ?DEBIT_VALUES, ?DEBIT_TYPES);
debit_v(JObj) ->
    debit_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Balance Request.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec balance_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_req(Prop) when is_list(Prop) ->
    case balance_req_v(Prop) of
        true -> kz_api:build_message(Prop, ?BALANCE_REQ_HEADERS, ?OPTIONAL_BALANCE_REQ_HEADERS);
        false -> {error, "Proplist failed validation for balance_req"}
    end;
balance_req(JObj) ->
    balance_req(kz_json:to_proplist(JObj)).

-spec balance_req_v(kz_term:api_terms()) -> boolean().
balance_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BALANCE_REQ_HEADERS, ?BALANCE_REQ_VALUES, ?BALANCE_REQ_TYPES);
balance_req_v(JObj) ->
    balance_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Balance Response.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec balance_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_resp(Prop) when is_list(Prop) ->
    case balance_resp_v(Prop) of
        true -> kz_api:build_message(Prop, ?BALANCE_RESP_HEADERS, ?OPTIONAL_BALANCE_RESP_HEADERS);
        false -> {error, "Proplist failed validation for balance_resp"}
    end;
balance_resp(JObj) ->
    balance_resp(kz_json:to_proplist(JObj)).

-spec balance_resp_v(kz_term:api_terms()) -> boolean().
balance_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BALANCE_RESP_HEADERS, ?BALANCE_RESP_VALUES, ?BALANCE_RESP_TYPES);
balance_resp_v(JObj) ->
    balance_resp_v(kz_json:to_proplist(JObj)).

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

-spec publish_credit(kz_term:api_terms()) -> 'ok'.
publish_credit(Req) ->
    publish_credit(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_credit(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_credit(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.credit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CREDIT_VALUES, fun credit/1),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

-spec publish_debit(kz_term:api_terms()) -> 'ok'.
publish_debit(Req) ->
    publish_debit(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_debit(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_debit(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.debit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DEBIT_VALUES, fun debit/1),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

-spec publish_balance_req(kz_term:api_terms()) -> 'ok'.
publish_balance_req(Req) ->
    publish_balance_req(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_req(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.balance.">>, props:get_value(<<"Account-ID">>, Req)]),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?BALANCE_REQ_VALUES, fun balance_req/1),
    kz_amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

-spec publish_balance_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_balance_resp(Queue, Req) ->
    publish_balance_resp(Queue, Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_balance_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_balance_resp(Queue, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?BALANCE_RESP_VALUES, fun balance_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).
