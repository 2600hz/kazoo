%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Intra-whapp comm
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_omnipresence).

-export([subscribe/1, subscribe_v/1
        ,notify/1, notify_v/1
        ]).

-export([publish_subscribe/1, publish_subscribe/2
        ,publish_notify/1, publish_notify/2
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include("omnipresence.hrl").

-define(ROUTING_KEY(A, B), <<A/binary, ".", (props:get_value('realm', B, <<"*">>))/binary, ".", (props:get_value('presence_id', B, <<"*">>))/binary >>).
-define(SUBSCRIBE_RK(A), ?ROUTING_KEY(<<"subscribe">>, A)).
-define(NOTIFY_RK(A), ?ROUTING_KEY(<<"notify">>, A)).

-define(OMNIPRESENCE_EXCHANGE, <<"omnipresence">>).

-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, [<<"Queue">>, <<"From">>
                                    ,<<"Event-Package">>, <<"Call-ID">>
                                    ,<<"From-Tag">>, <<"To-Tag">>
                                    ,<<"Contact">>
                                    ]).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                          ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(kz_term:to_integer(V)) end}]).

-define(NOTIFY_HEADERS, [<<"To">>, <<"From">>]).
-define(OPTIONAL_NOTIFY_HEADERS, [<<"Call-ID">>, <<"Event-Package">>
                                 ,<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"Body">>
                                 ]).
-define(NOTIFY_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"notify">>}
                       ]).
-define(NOTIFY_TYPES, []).


%% Reset presence dialog cache entry
-define(RESET_HEADERS, [<<"Realm">>, <<"Username">>]).
-define(OPTIONAL_RESET_HEADERS, [<<"Event-Package">>]).
-define(RESET_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"reset">>}
                      ]).
-define(RESET_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Subscribing for updates
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec subscribe(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
subscribe(Prop) when is_list(Prop) ->
    case subscribe_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SUBSCRIBE_HEADERS, ?OPTIONAL_SUBSCRIBE_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
subscribe(JObj) -> subscribe(kz_json:to_proplist(JObj)).

-spec subscribe_v(kz_term:api_terms()) -> boolean().
subscribe_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SUBSCRIBE_HEADERS, ?SUBSCRIBE_VALUES, ?SUBSCRIBE_TYPES);
subscribe_v(JObj) -> subscribe_v(kz_json:to_proplist(JObj)).

-spec publish_subscribe(kz_term:api_terms()) -> 'ok'.
publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_subscribe(kz_term:api_terms(), binary()) -> 'ok'.
publish_subscribe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SUBSCRIBE_VALUES, fun subscribe/1),
    kz_amqp_util:basic_publish(?OMNIPRESENCE_EXCHANGE, <<>>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc notifying subscribers
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec notify(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
notify(Prop) when is_list(Prop) ->
    case notify_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NOTIFY_HEADERS, ?OPTIONAL_NOTIFY_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
notify(JObj) -> notify(kz_json:to_proplist(JObj)).

-spec notify_v(kz_term:api_terms()) -> boolean().
notify_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NOTIFY_HEADERS, ?NOTIFY_VALUES, ?NOTIFY_TYPES);
notify_v(JObj) -> notify_v(kz_json:to_proplist(JObj)).

-spec publish_notify(kz_term:api_terms()) -> 'ok'.
publish_notify(JObj) -> publish_notify(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_notify(kz_term:api_terms(), binary()) -> 'ok'.
publish_notify(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NOTIFY_VALUES, fun notify/1),
    kz_amqp_util:basic_publish(?OMNIPRESENCE_EXCHANGE, <<>>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, props:get_value('restrict_to', Props), Props).

bind_q(Queue, 'undefined', Props) ->
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,?SUBSCRIBE_RK(Props)
                                   ,?OMNIPRESENCE_EXCHANGE
                                   ),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,?NOTIFY_RK(Props)
                                   ,?OMNIPRESENCE_EXCHANGE
                                   );
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,?SUBSCRIBE_RK(Props)
                                   ,?OMNIPRESENCE_EXCHANGE
                                   ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['notify'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,?NOTIFY_RK(Props)
                                   ,?OMNIPRESENCE_EXCHANGE
                                   ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, props:get_value('restrict_to', Props), Props).

-spec unbind_q(kz_term:ne_binary(), kz_term:atoms() | 'undefined', kz_term:proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', Props) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue
                                           ,?SUBSCRIBE_RK(Props)
                                           ,?OMNIPRESENCE_EXCHANGE
                                           ),
    kz_amqp_util:unbind_q_from_exchange(Queue
                                       ,?NOTIFY_RK(Props)
                                       ,?OMNIPRESENCE_EXCHANGE
                                       );
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue
                                           ,?SUBSCRIBE_RK(Props)
                                           ,?OMNIPRESENCE_EXCHANGE
                                           ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['notify'|Restrict], Props) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue
                                           ,?NOTIFY_RK(Props)
                                           ,?OMNIPRESENCE_EXCHANGE
                                           ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, [_|Restrict], Props) ->
    unbind_q(Queue, Restrict, Props);
unbind_q(_, _, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?OMNIPRESENCE_EXCHANGE, <<"topic">>).
