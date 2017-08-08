%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Max Lay
%%%-------------------------------------------------------------------
-module(kapi_edr_blackhole).

-include_lib("../edr.hrl").

-export([event/1, event_v/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_event/1]).

-define(EVENT_ROUTING_KEY(Severity, Verbosity, AccountId), <<"edr_blackhole.", (kz_term:to_binary(Severity))/binary
                                                            ,".", (kz_term:to_binary(Verbosity))/binary
                                                            ,".", (kz_term:to_binary(AccountId))/binary
                                                           >>).

-define(EVENT_HEADERS, [<<"Body">>, <<"ID">>, <<"Severity">>, <<"Timestamp">>, <<"Gregorian-Time">>, <<"Verbosity">>]).
-define(OPTIONAL_EVENT_HEADERS, [<<"Account-ID">>, <<"Account-Tree">>]).
-define(EVENT_VALUES, []).
-define(EVENT_TYPES, [{<<"Body">>, fun kz_json:is_json_object/1}]).

%%--------------------------------------------------------------------
%% @doc Event a callflow's flow
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec event(api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?EVENT_HEADERS, ?OPTIONAL_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?EVENT_HEADERS, ?EVENT_VALUES, ?EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    Verbosity = props:get_value(<<"verbosity">>, Props, <<"*">>),
    Exact = props:get_is_true(<<"exact">>, Props, 'false'),
    AccountId = props:get_value(<<"account_id">>, Props, <<"*">>),
    bind_q(Q, Exact, Verbosity, AccountId).
bind_q(Q, 'true', [Verbosity|Verbosities], AccountId) ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?EVENT_ROUTING_KEY(<<"*">>, Verbosity, AccountId)),
    bind_q(Q, 'true', Verbosities, AccountId);
bind_q(_Q, 'true', [], _AccountId) ->
    'ok';
bind_q(Q, Exact, Verbosity, AccountId) when Exact =:= 'true' orelse Verbosity =:= <<"*">> ->
    bind_q(Q, 'true', [Verbosity], AccountId);
bind_q(Q, 'false', Verbosity, AccountId) ->
    Verbosities = lists:dropwhile(fun(L) -> L =:= Verbosity end, ?EDR_VERBOSITY_LEVELS),
    bind_q(Q, 'true', Verbosities, AccountId).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    Verbosity = props:get_value(<<"verbosity">>, Props, <<"*">>),
    Exact = props:get_is_true(<<"exact">>, Props, 'false'),
    AccountId = props:get_value(<<"account_id">>, Props, <<"*">>),
    unbind_q(Q, Exact, Verbosity, AccountId).
unbind_q(Q, 'true', [Verbosity|Verbosities], AccountId) ->
    'ok' = amqp_util:unbind_q_to_kapps(Q, ?EVENT_ROUTING_KEY(<<"*">>, Verbosity, AccountId)),
    unbind_q(Q, 'true', Verbosities, AccountId);
unbind_q(_Q, 'true', [], _AccountId) ->
    'ok';
unbind_q(Q, Exact, Verbosity, AccountId) when Exact =:= 'true' orelse Verbosity =:= <<"*">> ->
    unbind_q(Q, 'true', [Verbosity], AccountId);
unbind_q(Q, 'false', Verbosity, AccountId) ->
    Verbosities = lists:dropwhile(fun(L) -> L =:= Verbosity end, ?EDR_VERBOSITY_LEVELS),
    unbind_q(Q, 'true', Verbosities, AccountId).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:kapps_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_event(api_terms()) -> 'ok'.
publish_event(JObj) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?EVENT_VALUES, fun event/1),
    Severity = kz_json:get_value(<<"Severity">>, JObj),
    Verbosity = kz_json:get_value(<<"Verbosity">>, JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    amqp_util:kapps_publish(?EVENT_ROUTING_KEY(Severity, Verbosity, AccountId), Payload).
