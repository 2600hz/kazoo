%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% APIs for events concerning money (like credits, debits, and others)
%%%
%%% Types of events known:
%%%   credit - a credit has been added to account-id
%%%   debit - a debit has been applied to account-id
%%%   balance - a request for any whapp with the balance of account-id to reply
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_money).

-export([credit/1, credit_v/1
         ,debit/1, debit_v/1
         ,balance_req/1, balance_req_v/1
         ,balance_resp/1, balance_resp_v/1
         ,bind_q/2, unbind_q/2
         ,publish_credit/1, publish_credit/2
         ,publish_debit/1, publish_debit/2
         ,publish_balance_req/1, publish_balance_req/2
         ,publish_balance_resp/2, publish_balance_resp/3
         ,dollars_to_units/1, units_to_dollars/1
         ,default_per_min_charge/0
         ,base_call_cost/3, version/0
         ,convert_units/3
        ]).

-include_lib("whistle/include/wh_api.hrl").

%% tracked in hundred-ths of a cent
-define(DOLLAR_TO_UNIT, 10000).
-define(WAPI_MONEY_VERSION, 2).

%% track how the above has changed, for migration
-define(MONEY_VERSION_TO_UNIT, [{1, 1000}
                                ,{2, 10000}
                               ]).

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

%%--------------------------------------------------------------------
%% @doc Credit Update - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec credit/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
credit(Prop) when is_list(Prop) ->
        case credit_v(Prop) of
            true -> wh_api:build_message(Prop, ?CREDIT_HEADERS, ?OPTIONAL_CREDIT_HEADERS);
            false -> {error, "Proplist failed validation for credit"}
    end;
credit(JObj) ->
    credit(wh_json:to_proplist(JObj)).

-spec credit_v/1 :: (api_terms()) -> boolean().
credit_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CREDIT_HEADERS, ?CREDIT_VALUES, ?CREDIT_TYPES);
credit_v(JObj) ->
    credit_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Debit Update - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec debit/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
debit(Prop) when is_list(Prop) ->
        case debit_v(Prop) of
            true -> wh_api:build_message(Prop, ?DEBIT_HEADERS, ?OPTIONAL_DEBIT_HEADERS);
            false -> {error, "Proplist failed validation for debit"}
    end;
debit(JObj) ->
    debit(wh_json:to_proplist(JObj)).

-spec debit_v/1 :: (api_terms()) -> boolean().
debit_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DEBIT_HEADERS, ?DEBIT_VALUES, ?DEBIT_TYPES);
debit_v(JObj) ->
    debit_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Balance Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec balance_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_req(Prop) when is_list(Prop) ->
    case balance_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?BALANCE_REQ_HEADERS, ?OPTIONAL_BALANCE_REQ_HEADERS);
        false -> {error, "Proplist failed validation for balance_req"}
    end;
balance_req(JObj) ->
    balance_req(wh_json:to_proplist(JObj)).

-spec balance_req_v/1 :: (api_terms()) -> boolean().
balance_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BALANCE_REQ_HEADERS, ?BALANCE_REQ_VALUES, ?BALANCE_REQ_TYPES);
balance_req_v(JObj) ->
    balance_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Balance Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec balance_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
balance_resp(Prop) when is_list(Prop) ->
    case balance_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?BALANCE_RESP_HEADERS, ?OPTIONAL_BALANCE_RESP_HEADERS);
        false -> {error, "Proplist failed validation for balance_resp"}
    end;
balance_resp(JObj) ->
    balance_resp(wh_json:to_proplist(JObj)).

-spec balance_resp_v/1 :: (api_terms()) -> boolean().
balance_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BALANCE_RESP_HEADERS, ?BALANCE_RESP_VALUES, ?BALANCE_RESP_TYPES);
balance_resp_v(JObj) ->
    balance_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Routing = routing_key(Props),
    amqp_util:configuration_exchange(),
    amqp_util:bind_q_to_configuration(Queue, Routing).

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Routing = routing_key(Props),
    amqp_util:unbind_q_from_configuration(Queue, Routing).

routing_key(Props) ->
    list_to_binary([<<"transaction.">>
                    ,props:get_value(type, Props, <<"*">>) %% credit/debit/balance/other
                    ,<<".">>
                    ,props:get_value(account_id, Props, <<"*">>)
                   ]).

publish_credit(Req) ->
    publish_credit(Req, ?DEFAULT_CONTENT_TYPE).
publish_credit(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.credit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CREDIT_VALUES, fun ?MODULE:credit/1),
    amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

publish_debit(Req) ->
    publish_debit(Req, ?DEFAULT_CONTENT_TYPE).
publish_debit(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.debit.">>, props:get_value(<<"Account-ID">>, Req)]),
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?DEBIT_VALUES, fun ?MODULE:debit/1),
    amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

publish_balance_req(Req) ->
    publish_balance_req(Req, ?DEFAULT_CONTENT_TYPE).
publish_balance_req(Req, ContentType) ->
    RoutingKey = list_to_binary([<<"transaction.balance.">>, props:get_value(<<"Account-ID">>, Req)]),
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?BALANCE_REQ_VALUES, fun ?MODULE:balance_req/1),
    amqp_util:configuration_publish(RoutingKey, Payload, ContentType).

publish_balance_resp(Queue, Req) ->
    publish_balance_resp(Queue, Req, ?DEFAULT_CONTENT_TYPE).
publish_balance_resp(Queue, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?BALANCE_RESP_VALUES, fun ?MODULE:balance_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec dollars_to_units/1 :: (float() | integer()) -> integer().
dollars_to_units(Dollars) when is_number(Dollars) ->
    round(Dollars * ?DOLLAR_TO_UNIT).

-spec units_to_dollars/1 :: (integer()) -> float().
units_to_dollars(Units) when is_integer(Units) ->
    Units / ?DOLLAR_TO_UNIT.

%% How much to charge a per_min call at the outset
-spec default_per_min_charge/0 :: () -> pos_integer().
default_per_min_charge() ->
    dollars_to_units(whapps_config:get_float(?MODULE, <<"default_per_min_charge">>, 0.5)). % $0.50 default removed

-spec base_call_cost/3 :: (float() | integer(), integer(), float() | integer()) -> float().
base_call_cost(RateCost, RateMin, RateSurcharge) when is_number(RateCost),
                                                      is_integer(RateMin),
                                                      is_number(RateSurcharge) ->
    dollars_to_units(RateCost * ( RateMin div 60 ) + RateSurcharge).

-spec version/0 :: () -> pos_integer().
version() ->
    ?WAPI_MONEY_VERSION.

-spec convert_units/3 :: (non_neg_integer(), pos_integer(), pos_integer()) -> pos_integer().
convert_units(0, _, _) -> 0;
convert_units(Units, Ver, Ver) -> Units;
convert_units(Units, FromVersion, ToVersion) when is_integer(Units), is_integer(FromVersion), is_integer(ToVersion) ->
    FromUnitFactor = props:get_integer_value(wh_util:to_integer(FromVersion), ?MONEY_VERSION_TO_UNIT),
    ToUnitFactor = props:get_integer_value(wh_util:to_integer(ToVersion), ?MONEY_VERSION_TO_UNIT),
    round(Units * (ToUnitFactor / FromUnitFactor)).
