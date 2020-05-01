%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_konami).

-export([transferred/1, transferred_v/1]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ]).

-export([publish_transferred/2]).

-include("konami.hrl").

-define(TRANSFERRED_HEADERS, [<<"Transferee">>, <<"Target">>, <<"Call">>]).
-define(OPTIONAL_TRANSFERRED_HEADERS, [<<"Transferor">>]).
-define(TRANSFERRED_VALUES, [{<<"Event-Category">>, ?APP_NAME}
                            ,{<<"Event-Name">>, <<"transferred">>}
                            ]).
-define(TRANSFERRED_TYPES, []).

-spec transferred(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', kz_term:ne_binary()}.
transferred(API) ->
    case transferred_v(API) of
        'true' -> kz_api:build_message(API, ?TRANSFERRED_HEADERS, ?OPTIONAL_TRANSFERRED_HEADERS);
        'false' -> {'error', <<"API failed validation for transferred">>}
    end.

-spec transferred_v(kz_term:api_terms()) -> boolean().
transferred_v(API) ->
    kz_api:validate(API, ?TRANSFERRED_HEADERS, ?TRANSFERRED_VALUES, ?TRANSFERRED_TYPES).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    bind_q(Queue, CallId, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist() | 'undefined') -> 'ok'.
bind_q(_Queue, 'undefined', 'undefined') -> 'ok';
bind_q(Queue, CallId, 'undefined') ->
    bind_for_transferred(Queue, CallId);
bind_q(_Queue, _CallId, []) -> 'ok';
bind_q(Queue, 'undefined' = CallId, ['transferred'|Restrictions]) ->
    bind_q(Queue, CallId, Restrictions);
bind_q(Queue, CallId, ['transferred'|Restrictions]) ->
    bind_for_transferred(Queue, CallId),
    bind_q(Queue, CallId, Restrictions);
bind_q(Queue, CallId, [_Restriction|Restrictions]) ->
    bind_q(Queue, CallId, Restrictions).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    unbind_q(Queue, CallId, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist() | 'undefined') -> 'ok'.
unbind_q(_Queue, 'undefined', 'undefined') -> 'ok';
unbind_q(Queue, CallId, 'undefined') ->
    unbind_for_transferred(Queue, CallId);
unbind_q(_Queue, _CallId, []) -> 'ok';
unbind_q(Queue, 'undefined' = CallId, ['transferred'|Restrictions]) ->
    unbind_q(Queue, CallId, Restrictions);
unbind_q(Queue, CallId, ['transferred'|Restrictions]) ->
    unbind_for_transferred(Queue, CallId),
    unbind_q(Queue, CallId, Restrictions);
unbind_q(Queue, CallId, [_Restriction|Restrictions]) ->
    unbind_q(Queue, CallId, Restrictions).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

-spec publish_transferred(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_transferred(TargetCallId, API) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?TRANSFERRED_VALUES, fun transferred/1),
    kz_amqp_util:kapps_publish(transferred_routing_key(TargetCallId), Payload).

-spec bind_for_transferred(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
bind_for_transferred(Queue, CallId) ->
    manipulate_queue_bindings(Queue, transferred_routing_key(CallId), fun kz_amqp_util:bind_q_to_kapps/2).

-spec unbind_for_transferred(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
unbind_for_transferred(Queue, CallId) ->
    manipulate_queue_bindings(Queue, transferred_routing_key(CallId), fun kz_amqp_util:unbind_q_from_kapps/2).

-spec manipulate_queue_bindings(kz_term:ne_binary(), kz_term:ne_binary(), fun()) -> 'ok'.
manipulate_queue_bindings(Queue, RoutingKey, Fun) ->
    Fun(Queue, RoutingKey).

-spec transferred_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
transferred_routing_key(<<_/binary>> = CallId) ->
    <<"konami.transferred.", CallId/binary>>.
