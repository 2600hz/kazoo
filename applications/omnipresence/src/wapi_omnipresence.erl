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
         ,presence_update/1, presence_update_v/1
         ,bind_q/2
         ,unbind_q/2
         ,publish_subscribe/1, publish_subscribe/2
         ,publish_presence_update/1, publish_presence_update/2
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

%%--------------------------------------------------------------------
%% @doc Someone's presence is updated, update tracking
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec presence_update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
presence_update(Prop) when is_list(Prop) ->
    case presence_update_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_HEADERS, ?OPTIONAL_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for presence_update"}
    end;
presence_update(JObj) -> presence_update(wh_json:to_proplist(JObj)).

-spec presence_update_v(api_terms()) -> boolean().
presence_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_HEADERS, ?UPDATE_VALUES, ?UPDATE_TYPES);
presence_update_v(JObj) -> presence_update_v(wh_json:to_proplist(JObj)).

bind_q(Queue, Props) ->
    amqp_util:whapps_exchange(),
    bind_q(Queue, Props, props:get_value('restrict_to', Props)).

bind_q(Queue, Props, 'undefined') ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, wapi_presence:subscribe_routing_key(Realm)),

    To = props:get_value('to', Props, <<"*">>),
    State = props:get_value('presence_state', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, presence_update_routing_key(To, State));
bind_q(Queue, Props, ['subscribe'|Ps]) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, wapi_presence:subscribe_routing_key(Realm)),
    bind_q(Queue, Props, Ps);
bind_q(Queue, Props, ['presence_update'|Ps]) ->
    To = props:get_value('to', Props, <<"*">>),
    State = props:get_value('presence_state', Props, <<"*">>),

    amqp_util:bind_q_to_whapps(Queue, presence_update_routing_key(To, State)),
    bind_q(Queue, Props, Ps);
bind_q(Queue, Props, [_|Ps]) ->
    bind_q(Queue, Props, Ps);
bind_q(_, _, []) -> 'ok'.

unbind_q(Queue, Props) ->
    unbind_q(Queue, Props, props:get_value('restrict_to', Props)).

unbind_q(Queue, Props, 'undefined') ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:unbind_q_from_whapps(Queue, wapi_presence:subscribe_routing_key(Realm));
unbind_q(Queue, Props, ['subscribe'|Ps]) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:unbind_q_from_whapps(Queue, wapi_presence:subscribe_routing_key(Realm)),
    unbind_q(Queue, Props, Ps);
unbind_q(Queue, Props, ['presence_update'|Ps]) ->
    To = props:get_value('to', Props, <<"*">>),
    State = props:get_value('presence_state', Props, <<"*">>),

    amqp_util:unbind_q_from_whapps(Queue, presence_update_routing_key(To, State)),
    unbind_q(Queue, Props, Ps);
unbind_q(Queue, Props, [_|Ps]) ->
    unbind_q(Queue, Props, Ps);
unbind_q(_, _, []) -> 'ok'.

publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?SUBSCRIBE_VALUES, fun ?MODULE:subscribe/1),
    amqp_util:whapps_publish(wapi_presence:subscribe_routing_key(API), Payload, ContentType).

publish_presence_update(JObj) ->
    publish_presence_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_presence_update(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?UPDATE_VALUES, fun ?MODULE:presence_update/1),
    amqp_util:whapps_publish(presence_update_routing_key(API), Payload, ContentType).

presence_update_routing_key(Prop) when is_list(Prop) ->
    presence_update_routing_key(props:get_value(<<"To">>, Prop), props:get_value(<<"State">>, Prop));
presence_update_routing_key(JObj) ->
    presence_update_routing_key(wh_json:get_value(<<"To">>, JObj), wh_json:get_value(<<"State">>, JObj)).

presence_update_routing_key(To, State) ->
    <<"omnipresence.update.", (amqp_util:encode(To))/binary, ".", State/binary>>.
