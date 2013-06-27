%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_presence).

-export([subscribe/1, subscribe_v/1
         ,update/1, update_v/1
        ]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([publish_subscribe/1, publish_subscribe/2
         ,publish_update/2, publish_update/3
        ]).

-include("omnipresence.hrl").

-define(SUBSCRIPTIONS_EXCHANGE, <<"dialoginfo_subs">>).
-define(UPDATES_EXCHANGE, <<"dialoginfo">>).

-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>, <<"Queue">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, []).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(wh_util:to_integer(V)) end}]).

-define(UPDATE_HEADERS, [<<"To">>, <<"From">>, <<"State">>, <<"Call-ID">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"From-Tag">>, <<"To-Tag">>]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"update">>}
                        ,{<<"State">>, [<<"trying">>, <<"early">>
                                        ,<<"confirmed">>, <<"terminated">>
                                       ]}
                       ]).
-define(UPDATE_TYPES, []).

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
%% @doc Subscribing for updates
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_HEADERS, ?OPTIONAL_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi query"}
    end;
update(JObj) -> update(wh_json:to_proplist(JObj)).

-spec update_v(api_terms()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_HEADERS, ?UPDATE_VALUES, ?UPDATE_TYPES);
update_v(JObj) -> update_v(wh_json:to_proplist(JObj)).

bind_q(Queue, Props) ->
    amqp_util:new_exchange(?SUBSCRIPTIONS_EXCHANGE, <<"fanout">>),
    amqp_util:new_exchange(?UPDATES_EXCHANGE, <<"direct">>),

    Realm = props:get_value('realm', Props, <<"*">>),

    bind_q(Queue, Realm, props:get_value('restrict_to', Props)).

bind_q(Queue, Realm, 'undefined') ->
    amqp_util:bind_q_to_exchange(Queue
                                 ,subscribe_routing_key(Realm)
                                 ,?SUBSCRIPTIONS_EXCHANGE
                                );
bind_q(Queue, Realm, ['subscribe'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue
                                 ,subscribe_routing_key(Realm)
                                 ,?SUBSCRIPTIONS_EXCHANGE
                                ),
    bind_q(Queue, Realm, Restrict);
bind_q(Queue, Realm, [_|Restrict]) ->
    bind_q(Queue, Realm, Restrict);
bind_q(_, _, []) -> 'ok'.

unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    unbind_q(Queue, Realm, props:get_value('restrict_to', Props)).

unbind_q(Queue, Realm, 'undefined') ->
    amqp_util:unbind_q_from_exchange(Queue
                                     ,subscribe_routing_key(Realm)
                                     ,?SUBSCRIPTIONS_EXCHANGE
                                    );
unbind_q(Queue, Realm, ['subscribe'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue
                                     ,subscribe_routing_key(Realm)
                                     ,?SUBSCRIPTIONS_EXCHANGE
                                    ),
    unbind_q(Queue, Realm, Restrict);
unbind_q(Queue, Realm, [_|Restrict]) ->
    unbind_q(Queue, Realm, Restrict);
unbind_q(_, _, []) -> 'ok'.

publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?SUBSCRIBE_VALUES, fun ?MODULE:subscribe/1),
    amqp_util:basic_publish(?SUBSCRIPTIONS_EXCHANGE, subscribe_routing_key(API), Payload, ContentType).

publish_update(Q, JObj) ->
    publish_update(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?UPDATE_VALUES, fun ?MODULE:update/1),
    amqp_util:basic_publish(?UPDATES_EXCHANGE, Q, Payload, ContentType).

subscribe_routing_key(Prop) when is_list(Prop) ->
    subscribe_routing_key(props:get_value(<<"User">>, Prop));
subscribe_routing_key(User) when is_binary(User) ->
    R = case binary:split(User, <<"@">>) of
            [_To, Realm] -> amqp_util:encode(Realm);
            [Realm] -> amqp_util:encode(Realm)
        end,
    <<"presence.subscriptions.", R/binary>>;
subscribe_routing_key(JObj) ->
    subscribe_routing_key(wh_json:get_value(<<"User">>, JObj)).
