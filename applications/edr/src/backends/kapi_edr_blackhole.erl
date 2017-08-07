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

-define(EVENT_ROUTING_KEY(Level, AccountId), <<"edr_blackhole.", (kz_term:to_binary(Level))/binary
                                              ,".", (kz_term:to_binary(AccountId))/binary
                                             >>).

-define(EVENT_HEADERS, [<<"Body">>, <<"ID">>, <<"Level">>, <<"Timestamp">>, <<"Gregorian-Time">>]).
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
    Level = props:get_value(<<"level">>, Props, <<"*">>),
    Exact = props:get_is_true(<<"exact">>, Props, 'false'),
    AccountId = props:get_value(<<"account_id">>, Props, <<"*">>),
    bind_q(Q, Exact, Level, AccountId).
bind_q(Q, 'true', [Level|Levels], AccountId) ->
    'ok' = amqp_util:bind_q_to_kapps(Q, ?EVENT_ROUTING_KEY(Level, AccountId)),
    bind_q(Q, 'true', Levels, AccountId);
bind_q(_Q, 'true', [], _AccountId) ->
    'ok';
bind_q(Q, Exact, Level, AccountId) when Exact =:= 'true' orelse Level =:= <<"*">> ->
    bind_q(Q, 'true', [Level], AccountId);
bind_q(Q, 'false', Level, AccountId) ->
    Levels = lists:dropwhile(fun(L) -> L =:= Level end, ?EDR_LEVELS),
    bind_q(Q, 'true', Levels, AccountId).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    Level = props:get_value(<<"level">>, Props, <<"*">>),
    Exact = props:get_is_true(<<"exact">>, Props, 'false'),
    AccountId = props:get_value(<<"account_id">>, Props, <<"*">>),
    unbind_q(Q, Exact, Level, AccountId).
unbind_q(Q, 'true', [Level|Levels], AccountId) ->
    'ok' = amqp_util:unbind_q_to_kapps(Q, ?EVENT_ROUTING_KEY(Level, AccountId)),
    unbind_q(Q, 'true', Levels, AccountId);
unbind_q(_Q, 'true', [], _AccountId) ->
    'ok';
unbind_q(Q, Exact, Level, AccountId) when Exact =:= 'true' orelse Level =:= <<"*">> ->
    unbind_q(Q, 'true', [Level], AccountId);
unbind_q(Q, 'false', Level, AccountId) ->
    Levels = lists:dropwhile(fun(L) -> L =:= Level end, ?EDR_LEVELS),
    unbind_q(Q, 'true', Levels, AccountId).

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
    Level = kz_json:get_value(<<"Level">>, JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    amqp_util:kapps_publish(?EVENT_ROUTING_KEY(Level, AccountId), Payload).
