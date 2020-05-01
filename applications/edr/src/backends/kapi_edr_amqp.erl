%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_edr_amqp).

-include_lib("../edr.hrl").

-export([event/1, event_v/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_event/1]).

-define(EVENT_HEADERS, [<<"Body">>, <<"ID">>, <<"Severity">>, <<"Timestamp">>, <<"Gregorian-Time">>, <<"Verbosity">>, <<"Node">>]).
-define(OPTIONAL_EVENT_HEADERS, [<<"Account-ID">>, <<"Account-Tree">>]).
-define(EVENT_VALUES, [{<<"Event-Category">>, <<"edr">>}
                      ,{<<"Event-Name">>, <<"event">>}
                      ]).
-define(EVENT_TYPES, [{<<"Body">>, fun kz_json:is_json_object/1}]).

%%------------------------------------------------------------------------------
%% @doc Event a callflow's flow
%% Takes proplist, creates JSON iolist or error
%% @end
%%------------------------------------------------------------------------------
-spec event(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?EVENT_HEADERS, ?OPTIONAL_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(kz_term:api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?EVENT_HEADERS, ?EVENT_VALUES, ?EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q(Q, Props, binding_keys_from_props(Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries()) -> 'ok'.
bind_q(Q, _Props, [Key | RemainingKeys]) ->
    'ok' = kz_amqp_util:bind_q_to_kapps(Q, Key),
    bind_q(Q, _Props, RemainingKeys);
bind_q(_Q, _Props, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q(Q, Props, binding_keys_from_props(Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries()) -> 'ok'.
unbind_q(Q, _Props, [Key | RemainingKeys]) ->
    'ok' = kz_amqp_util:unbind_q_from_kapps(Q, Key),
    unbind_q(Q, _Props, RemainingKeys);
unbind_q(_Q, _Props, []) ->
    'ok'.

-spec binding_keys_from_props(kz_term:proplist()) -> kz_term:ne_binaries().
binding_keys_from_props(Props) ->
    %% We don't support filtering that can't be done with a routing key,
    %% so we can't use include descendants
    JProps = kz_json:from_list(props:delete("account_id", Props)),
    Bindings = edr_bindings:bindings_from_json(JProps),
    edr_bindings:binding_keys(Bindings).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%------------------------------------------------------------------------------
-spec publish_event(edr_event()) -> 'ok'.
publish_event(#edr_event{}=Event) ->
    {'ok', Payload} = kz_api:prepare_api_payload(event_to_payload(Event), ?EVENT_VALUES, fun event/1),
    RoutingKey = edr_bindings:event_binding_key(Event),
    kz_amqp_util:kapps_publish(RoutingKey, Payload).

-spec event_to_payload(edr_event()) -> kz_json:object().
event_to_payload(#edr_event{}=Event) ->
    FormatterOptions = kz_json:from_list([{<<"include_metadata">>, 'true'}
                                         ,{<<"normalize">>, 'false'}
                                         ]),
    edr_fmt_json:to_jobj(FormatterOptions, Event).
