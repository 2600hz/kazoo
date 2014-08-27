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
         ,update/1, update_v/1
         ,bind_q/2
         ,unbind_q/2
         ,publish_subscribe/1, publish_subscribe/2
         ,publish_update/2, publish_update/3
         ,declare_exchanges/0
         ]).

-include("omnipresence.hrl").

-define(SUBSCRIPTIONS_EXCHANGE, <<"dialoginfo_subs">>).
-define(UPDATES_EXCHANGE, <<"dialoginfo">>).

-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, [<<"Queue">>, <<"From">>
                                    ,<<"Event-Package">>, <<"Call-ID">>
                                    ,<<"From-Tag">>, <<"To-Tag">>
                                    ,<<"Contact">>
                                    ]).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(wh_util:to_integer(V)) end}]).

-define(UPDATE_HEADERS, [<<"To">>, <<"From">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"Call-ID">>, <<"Direction">>
                                 ,<<"Event-Package">>, <<"State">>
                                 ,<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"From-User">>, <<"From-Realm">>
                                 ,<<"To-User">>, <<"To-Realm">>
                                 ,<<"Messages-Waiting">>, <<"Messages-New">>
                                 ,<<"Messages-Saved">>, <<"Messages-Urgent">>
                                 ,<<"Messages-Urgent-Saved">>, <<"Message-Account">>
                                 ,<<"Expires">>
                                 ]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"update">>}
                       ]).
-define(UPDATE_TYPES, [
                       {<<"State">>, fun(A) -> lists:member(A, wapi_presence:presence_states()) end}
                      ]).

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

publish_subscribe(JObj) -> publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?SUBSCRIBE_VALUES, fun ?MODULE:subscribe/1),
    amqp_util:basic_publish(?SUBSCRIPTIONS_EXCHANGE, <<>>, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc Someone's presence is updated, update tracking
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_HEADERS, ?OPTIONAL_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for update"}
    end;
update(JObj) -> update(wh_json:to_proplist(JObj)).

-spec update_v(api_terms()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_HEADERS, ?UPDATE_VALUES, ?UPDATE_TYPES);
update_v(JObj) -> update_v(wh_json:to_proplist(JObj)).

-spec publish_update(ne_binary(), api_terms()) -> 'ok'.
publish_update(Q, JObj) -> publish_update(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Q, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?UPDATE_VALUES, fun ?MODULE:update/1),
    amqp_util:basic_publish(?UPDATES_EXCHANGE, Q, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
bind_q(Queue, Props) ->
    bind_q(Queue, props:get_value('restrict_to', Props), Props).

bind_q(Queue, 'undefined', _Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                 ,Queue
                                 ,?SUBSCRIPTIONS_EXCHANGE
                                ),
    amqp_util:bind_q_to_exchange(Queue
                                 ,Queue
                                 ,?UPDATES_EXCHANGE
                                );
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                 ,Queue
                                 ,?SUBSCRIPTIONS_EXCHANGE
                                ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['update'|Restrict], Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                 ,Queue
                                 ,?UPDATES_EXCHANGE
                                ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

unbind_q(Queue, Props) ->
    unbind_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_q(Queue, _Props, 'undefined') ->
    amqp_util:unbind_q_from_exchange(Queue
                                     ,Queue
                                     ,?SUBSCRIPTIONS_EXCHANGE
                                    ),
    amqp_util:unbind_q_from_exchange(Queue
                                     ,Queue
                                     ,?UPDATES_EXCHANGE
                                    );
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                     ,Queue
                                     ,?SUBSCRIPTIONS_EXCHANGE
                                    ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['update'|Restrict], Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                     ,Queue
                                     ,?UPDATES_EXCHANGE
                                    ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, [_|Restrict], Props) ->
    unbind_q(Queue, Restrict, Props);
unbind_q(_, _, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?SUBSCRIPTIONS_EXCHANGE, <<"fanout">>),
    amqp_util:new_exchange(?UPDATES_EXCHANGE, <<"direct">>).
