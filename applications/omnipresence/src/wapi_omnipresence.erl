%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Intra-whapp comm
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_omnipresence).

-export([subscribe/1, subscribe_v/1
         ,bind_q/2
         ,unbind_q/2
         ,publish_subscribe/1, publish_subscribe/2
         ]).

-include("omnipresence.hrl").
-include("omnipresence_api.hrl").

%%--------------------------------------------------------------------
%% @doc Subscribing for updates
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec subscribe(api_terms()) -> {'ok', iolist()} | {'error', string()}.
subscribe(Prop) when is_list(Prop) ->
    case subscribe_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SUBSCRIBE_HEADERS, ?OPTIONAL_SUBSCRIBE_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
subscribe(JObj) -> subscribe(wh_json:to_proplist(JObj)).

-spec subscribe_v(api_terms()) -> boolean().
subscribe_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SUBSCRIBE_HEADERS, ?SUBSCRIBE_VALUES, ?SUBSCRIBE_TYPES);
subscribe_v(JObj) -> subscribe_v(wh_json:to_proplist(JObj)).

bind_q(Queue, Props) ->
    amqp_util:whapps_exchange(),

    Realm = props:get_value('realm', Props, <<"*">>),
    bind_q(Queue, Realm, props:get_value('restrict_to', Props)).

bind_q(Queue, Realm, 'undefined') ->
    amqp_util:bind_q_to_whapps(Queue, wapi_presence:subscribe_routing_key(Realm));
bind_q(Queue, Realm, ['subscribe'|Ps]) ->
    amqp_util:bind_q_to_whapps(Queue, wapi_presence:subscribe_routing_key(Realm)),
    bind_q(Queue, Realm, Ps);
bind_q(Queue, Realm, [_|Ps]) ->
    bind_q(Queue, Realm, Ps);
bind_q(_, _, []) -> 'ok'.

unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    unbind_q(Queue, Realm, props:get_value('restrict_to', Props)).

unbind_q(Queue, Realm, 'undefined') ->
    amqp_util:unbind_q_from_whapps(Queue, wapi_presence:subscribe_routing_key(Realm));
unbind_q(Queue, Realm, ['subscribe'|Ps]) ->
    amqp_util:unbind_q_from_whapps(Queue, wapi_presence:subscribe_routing_key(Realm)),
    unbind_q(Queue, Realm, Ps);
unbind_q(Queue, Realm, [_|Ps]) ->
    unbind_q(Queue, Realm, Ps);
unbind_q(_, _, []) -> 'ok'.

publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?SUBSCRIBE_VALUES, fun ?MODULE:subscribe/1),
    amqp_util:whapps_publish(wapi_presence:subscribe_routing_key(API), Payload, ContentType).
