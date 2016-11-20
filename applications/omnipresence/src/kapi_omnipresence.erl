%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Intra-whapp comm
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_omnipresence).

-export([subscribe/1, subscribe_v/1
        ,update/1, update_v/1
        ,notify/1, notify_v/1
        ,search_req/1, search_req_v/1
        ,search_partial_resp/1, search_partial_resp_v/1
        ,search_resp/1, search_resp_v/1
        ]).

-export([publish_subscribe/1, publish_subscribe/2
        ,publish_update/2, publish_update/3
        ,publish_notify/1, publish_notify/2
        ,publish_search_req/1, publish_search_req/2
        ,publish_search_partial_resp/2, publish_search_partial_resp/3
        ,publish_search_resp/2, publish_search_resp/3
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include("omnipresence.hrl").

-define(ROUTING_KEY(A, B), <<A/binary, ".", (props:get_value('realm', B, <<"*">>))/binary, ".", (props:get_value('presence_id', B, <<"*">>))/binary >>).
-define(SUBSCRIBE_RK(A), ?ROUTING_KEY(<<"subscribe">>, A)).
-define(NOTIFY_RK(A), ?ROUTING_KEY(<<"notify">>, A)).

-define(DIALOGINFO_SUBS_EXCHANGE, <<"dialoginfo_subs">>).
-define(DIALOGINFO_EXCHANGE, <<"dialoginfo">>).
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
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(kz_util:to_integer(V)) end}]).

-define(UPDATE_HEADERS, [<<"To">>, <<"From">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"Call-ID">>, <<"Direction">>
                                 ,<<"Event-Package">>, <<"State">>
                                 ,<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"From-User">>, <<"From-Realm">>, <<"From-URI">>
                                 ,<<"To-User">>, <<"To-Realm">>, <<"To-URI">>
                                 ,<<"Messages-Waiting">>, <<"Messages-New">>
                                 ,<<"Messages-Saved">>, <<"Messages-Urgent">>
                                 ,<<"Messages-Urgent-Saved">>, <<"Message-Account">>
                                 ,<<"Expires">>, <<"Flush-Level">>
                                 ,<<"Presentity">>, <<"Presentity-User">>, <<"Presentity-Realm">>
                                 ,<<"Target-Call-ID">>, <<"Switch-URI">>, <<"Call-Cookie">>
                                 ]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"update">>}
                       ]).
-define(UPDATE_TYPES, [
                       {<<"State">>, fun(A) -> lists:member(A, kapi_presence:presence_states()) end}
                      ]).

-define(NOTIFY_HEADERS, [<<"To">>, <<"From">>]).
-define(OPTIONAL_NOTIFY_HEADERS, [<<"Call-ID">>, <<"Event-Package">>
                                 ,<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"Body">>
                                 ]).
-define(NOTIFY_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"notify">>}
                       ]).
-define(NOTIFY_TYPES, []).

%% Search request for active subscriptions
-define(SEARCH_REQ_HEADERS, [<<"Realm">>, <<"Scope">>]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, [<<"Username">>, <<"Event-Package">>]).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"search_req">>}
                           ,{<<"Scope">>, [<<"presentity">>, <<"watchers">>]}
                           ]).
-define(SEARCH_REQ_TYPES, []).

%% Search partial response for active subscriptions
-define(SEARCH_PARTIAL_RESP_HEADERS, [<<"Subscriptions">>]).
-define(OPTIONAL_SEARCH_PARTIAL_RESP_HEADERS, []).
-define(SEARCH_PARTIAL_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                                    ,{<<"Event-Name">>, <<"search_partial_resp">>}
                                    ]).
-define(SEARCH_PARTIAL_RESP_TYPES, []).

%% Search response for active subscriptions
-define(SEARCH_RESP_HEADERS, []).
-define(OPTIONAL_SEARCH_RESP_HEADERS, [<<"Subscriptions">>]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                            ,{<<"Event-Name">>, <<"search_resp">>}
                            ]).
-define(SEARCH_RESP_TYPES, []).

%% Reset presence dialog cache entry
-define(RESET_HEADERS, [<<"Realm">>, <<"Username">>]).
-define(OPTIONAL_RESET_HEADERS, [<<"Event-Package">>]).
-define(RESET_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"reset">>}
                      ]).
-define(RESET_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Subscribing for updates
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec subscribe(api_terms()) -> {'ok', iolist()} | {'error', string()}.
subscribe(Prop) when is_list(Prop) ->
    case subscribe_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SUBSCRIBE_HEADERS, ?OPTIONAL_SUBSCRIBE_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
subscribe(JObj) -> subscribe(kz_json:to_proplist(JObj)).

-spec subscribe_v(api_terms()) -> boolean().
subscribe_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SUBSCRIBE_HEADERS, ?SUBSCRIBE_VALUES, ?SUBSCRIBE_TYPES);
subscribe_v(JObj) -> subscribe_v(kz_json:to_proplist(JObj)).

-spec publish_subscribe(api_terms()) -> 'ok'.
-spec publish_subscribe(api_terms(), binary()) -> 'ok'.
publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SUBSCRIBE_VALUES, fun subscribe/1),
    amqp_util:basic_publish(?DIALOGINFO_SUBS_EXCHANGE, <<>>, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc Someone's presence is updated, update tracking
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UPDATE_HEADERS, ?OPTIONAL_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for update"}
    end;
update(JObj) -> update(kz_json:to_proplist(JObj)).

-spec update_v(api_terms()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UPDATE_HEADERS, ?UPDATE_VALUES, ?UPDATE_TYPES);
update_v(JObj) -> update_v(kz_json:to_proplist(JObj)).

-spec publish_update(ne_binary(), api_terms()) -> 'ok'.
-spec publish_update(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_update(Q, JObj) -> publish_update(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Q, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UPDATE_VALUES, fun update/1),
    amqp_util:basic_publish(?DIALOGINFO_EXCHANGE, Q, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc notifying subscribers
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec notify(api_terms()) -> {'ok', iolist()} | {'error', string()}.
notify(Prop) when is_list(Prop) ->
    case notify_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NOTIFY_HEADERS, ?OPTIONAL_NOTIFY_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
notify(JObj) -> notify(kz_json:to_proplist(JObj)).

-spec notify_v(api_terms()) -> boolean().
notify_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NOTIFY_HEADERS, ?NOTIFY_VALUES, ?NOTIFY_TYPES);
notify_v(JObj) -> notify_v(kz_json:to_proplist(JObj)).

-spec publish_notify(api_terms()) -> 'ok'.
-spec publish_notify(api_terms(), binary()) -> 'ok'.
publish_notify(JObj) -> publish_notify(JObj, ?DEFAULT_CONTENT_TYPE).
publish_notify(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NOTIFY_VALUES, fun notify/1),
    amqp_util:basic_publish(?OMNIPRESENCE_EXCHANGE, <<>>, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec search_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_req(Prop) when is_list(Prop) ->
    case search_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?OPTIONAL_SEARCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_req"}
    end;
search_req(JObj) ->
    search_req(kz_json:to_proplist(JObj)).

-spec search_req_v(api_terms()) -> boolean().
search_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
search_req_v(JObj) ->
    search_req_v(kz_json:to_proplist(JObj)).

-spec publish_search_req(api_terms()) -> 'ok'.
-spec publish_search_req(api_terms(), binary()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun search_req/1),
    amqp_util:basic_publish(?OMNIPRESENCE_EXCHANGE, search_req_routing_key(Req), Payload, ContentType).

-spec search_req_routing_key(binary() | api_terms()) -> ne_binary().
search_req_routing_key(Req) when is_list(Req) ->
    search_req_routing_key(props:get_value(<<"Realm">>, Req));
search_req_routing_key(Realm) when is_binary(Realm) ->
    list_to_binary([<<"presence.search_req.">>, amqp_util:encode(Realm)]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec search_partial_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_partial_resp(Prop) when is_list(Prop) ->
    case search_partial_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_PARTIAL_RESP_HEADERS, ?OPTIONAL_SEARCH_PARTIAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_resp"}
    end;
search_partial_resp(JObj) ->
    search_partial_resp(kz_json:to_proplist(JObj)).

-spec search_partial_resp_v(api_terms()) -> boolean().
search_partial_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_PARTIAL_RESP_HEADERS, ?SEARCH_PARTIAL_RESP_VALUES, ?SEARCH_PARTIAL_RESP_TYPES);
search_partial_resp_v(JObj) ->
    search_partial_resp_v(kz_json:to_proplist(JObj)).

-spec publish_search_partial_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_search_partial_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_search_partial_resp(Queue, JObj) ->
    publish_search_partial_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_partial_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_PARTIAL_RESP_VALUES, fun search_partial_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec search_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_resp(Prop) when is_list(Prop) ->
    case search_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?OPTIONAL_SEARCH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_resp"}
    end;
search_resp(JObj) ->
    search_resp(kz_json:to_proplist(JObj)).

-spec search_resp_v(api_terms()) -> boolean().
search_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
search_resp_v(JObj) ->
    search_resp_v(kz_json:to_proplist(JObj)).

-spec publish_search_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_search_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_search_resp(Queue, JObj) ->
    publish_search_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun search_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, props:get_value('restrict_to', Props), Props).

bind_q(Queue, 'undefined', Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                ,?SUBSCRIBE_RK(Props)
                                ,?DIALOGINFO_SUBS_EXCHANGE
                                ),
    amqp_util:bind_q_to_exchange(Queue
                                ,Queue
                                ,?DIALOGINFO_EXCHANGE
                                ),
    amqp_util:bind_q_to_exchange(Queue
                                ,?NOTIFY_RK(Props)
                                ,?OMNIPRESENCE_EXCHANGE
                                );
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                ,?SUBSCRIBE_RK(Props)
                                ,?DIALOGINFO_SUBS_EXCHANGE
                                ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['update'|Restrict], Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                ,Queue
                                ,?DIALOGINFO_EXCHANGE
                                ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['notify'|Restrict], Props) ->
    amqp_util:bind_q_to_exchange(Queue
                                ,?NOTIFY_RK(Props)
                                ,?OMNIPRESENCE_EXCHANGE
                                ),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec unbind_q(ne_binary(), atoms() | 'undefined', kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_q(Queue, 'undefined', Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                    ,?SUBSCRIBE_RK(Props)
                                    ,?DIALOGINFO_SUBS_EXCHANGE
                                    ),
    amqp_util:unbind_q_from_exchange(Queue
                                    ,Queue
                                    ,?DIALOGINFO_EXCHANGE
                                    ),
    amqp_util:unbind_q_from_exchange(Queue
                                    ,?NOTIFY_RK(Props)
                                    ,?OMNIPRESENCE_EXCHANGE
                                    );
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                    ,?SUBSCRIBE_RK(Props)
                                    ,?DIALOGINFO_SUBS_EXCHANGE
                                    ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['update'|Restrict], Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                    ,Queue
                                    ,?DIALOGINFO_EXCHANGE
                                    ),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['notify'|Restrict], Props) ->
    amqp_util:unbind_q_from_exchange(Queue
                                    ,?NOTIFY_RK(Props)
                                    ,?OMNIPRESENCE_EXCHANGE
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
    amqp_util:new_exchange(?DIALOGINFO_SUBS_EXCHANGE, <<"fanout">>),
    amqp_util:new_exchange(?DIALOGINFO_EXCHANGE, <<"direct">>),
    amqp_util:new_exchange(?OMNIPRESENCE_EXCHANGE, <<"topic">>).
