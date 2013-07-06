%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_presence).

-export([search_req/1, search_req_v/1
         ,search_resp/1, search_resp_v/1
        ]).

-export([subscribe/1, subscribe_v/1
         ,subscribe_routing_key/1
         ,update/1, update_v/1
         ,flush/1, flush_v/1
        ]).

-export([reset/1, reset_v/1]).

-export([is_valid_state/1]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([publish_subscribe/1, publish_subscribe/2
         ,publish_update/2, publish_update/3
         ,publish_flush/2, publish_flush/3
        ]).

-export([publish_search_req/1
         ,publish_search_resp/2
        ]).

-export([publish_reset/1]).

-include("omnipresence.hrl").
-include("omnipresence_api.hrl").

-spec reset(api_terms()) -> {'ok', iolist()} | {'error', string()}.
reset(Prop) when is_list(Prop) ->
    case reset_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RESET_HEADERS, ?OPTIONAL_RESET_HEADERS);
        'false' -> {'error', "Proplist failed validation for reset"}
    end;
reset(JObj) ->
    reset(wh_json:to_proplist(JObj)).

-spec reset_v(api_terms()) -> boolean().
reset_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESET_HEADERS, ?RESET_VALUES, ?RESET_TYPES);
reset_v(JObj) ->
    reset_v(wh_json:to_proplist(JObj)).

-spec search_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_req(Prop) when is_list(Prop) ->
    case search_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?OPTIONAL_SEARCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_req"}
    end;
search_req(JObj) ->
    search_req(wh_json:to_proplist(JObj)).

-spec search_req_v(api_terms()) -> boolean().
search_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
search_req_v(JObj) ->
    search_req_v(wh_json:to_proplist(JObj)).

-spec search_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_resp(Prop) when is_list(Prop) ->
    case search_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?OPTIONAL_SEARCH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_resp"}
    end;
search_resp(JObj) ->
    search_resp(wh_json:to_proplist(JObj)).

-spec search_resp_v(api_terms()) -> boolean().
search_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
search_resp_v(JObj) ->
    search_resp_v(wh_json:to_proplist(JObj)).

-spec publish_reset(api_terms()) -> 'ok'.
-spec publish_reset(api_terms(), binary()) -> 'ok'.
publish_reset(JObj) ->
    publish_reset(JObj, ?DEFAULT_CONTENT_TYPE).
publish_reset(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RESET_VALUES, fun ?MODULE:reset/1),
    amqp_util:callmgr_publish(Payload, ContentType, reset_routing_key(Req)).

-spec publish_search_req(api_terms()) -> 'ok'.
-spec publish_search_req(api_terms(), binary()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun ?MODULE:search_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, search_req_routing_key(Req)).

-spec publish_search_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_search_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_search_resp(Queue, JObj) ->
    publish_search_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun ?MODULE:search_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).


-spec reset_routing_key(ne_binary() | api_terms()) -> ne_binary().
-spec reset_routing_key(ne_binary(), ne_binary()) -> ne_binary().
reset_routing_key(Req) when is_list(Req) ->
    reset_routing_key(props:get_value(<<"Realm">>, Req)
                      ,props:get_value(<<"Username">>, Req));
reset_routing_key(Req) ->
    reset_routing_key(wh_json:get_value(<<"Realm">>, Req)
                      ,wh_json:get_value(<<"Username">>, Req)).

reset_routing_key(Realm, Username) when is_binary(Realm) ->
    list_to_binary([?KEY_RESET, ".", amqp_util:encode(Realm), ".", amqp_util:encode(Username)]).

-spec search_req_routing_key(ne_binary() | api_terms()) -> ne_binary().
search_req_routing_key(Req) when is_list(Req) ->
    search_req_routing_key(props:get_value(<<"Realm">>, Req));
search_req_routing_key(Realm) when is_binary(Realm) ->
    list_to_binary([?KEY_SEARCH_REQ, ".", amqp_util:encode(Realm)]);
search_req_routing_key(Req) ->
    search_req_routing_key(wh_json:get_value(<<"Realm">>, Req)).

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


-spec flush(api_terms()) -> {'ok', iolist()} | {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?FLUSH_HEADERS, ?OPTIONAL_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for flush query"}
    end;
flush(JObj) -> flush(wh_json:to_proplist(JObj)).

-spec flush_v(api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FLUSH_HEADERS, ?FLUSH_VALUES, ?FLUSH_TYPES);
flush_v(JObj) -> flush_v(wh_json:to_proplist(JObj)).

%% API Helpers
-spec is_valid_state(api_binary() | api_terms()) -> boolean().
is_valid_state(State) when is_binary(State) ->
    lists:member(State, ?PRESENCE_STATES);
is_valid_state(Prop) when is_list(Prop) ->
    is_valid_state(props:get_value(<<"State">>, Prop));
is_valid_state(JObj) ->
    is_valid_state(wh_json:get_value(<<"State">>, JObj)).

bind_q(Queue, Props) ->
    amqp_util:new_exchange(?SUBSCRIPTIONS_EXCHANGE, <<"fanout">>),
    amqp_util:new_exchange(?UPDATES_EXCHANGE, <<"direct">>),
    amqp_util:callmgr_exchange(),

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
bind_q(Queue, Realm, ['search_req'|Restrict]) ->
    amqp_util:bind_q_to_callmgr(Queue
                                ,search_req_routing_key(Realm)
                                ),
    bind_q(Queue, Realm, Restrict);
bind_q(Queue, Realm, ['reset'|Restrict]) ->
    amqp_util:bind_q_to_callmgr(Queue
                                ,reset_routing_key(Realm, <<"*">>)
                                ),
    bind_q(Queue, Realm, Restrict);
bind_q(Queue, Realm, ['updates'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, Queue, ?UPDATES_EXCHANGE),
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
unbind_q(Queue, Realm, ['search_req'|Restrict]) ->
    amqp_util:unbind_q_from_callmgr(Queue
                                     ,search_req_routing_key(Realm)
                                    ),
    unbind_q(Queue, Realm, Restrict);
unbind_q(Queue, Realm, ['reset'|Restrict]) ->
    amqp_util:unbind_q_from_callmgr(Queue
                                     ,reset_routing_key(Realm, <<"*">>)
                                    ),
    unbind_q(Queue, Realm, Restrict);
unbind_q(Queue, Realm, ['updates'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, Queue, ?UPDATES_EXCHANGE),
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

publish_flush(Q, JObj) ->
    publish_flush(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_flush(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?FLUSH_VALUES, fun ?MODULE:flush/1),
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
