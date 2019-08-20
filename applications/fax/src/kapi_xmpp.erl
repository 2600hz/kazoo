%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_xmpp).

-export([event/1, event_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_event/1, publish_event/2
        ]).

-export([jid_short/1
        ,jid_username/1
        ,jid_server/1
        ,jid_resource/1
        ]).

-include("fax.hrl").

-define(XMPP_EXCHANGE, <<"xmpp">>).

-define(XMPP_EVENT_ROUTING_KEY(Event, JID)
       ,list_to_binary(["xmpp."
                       ,kz_term:to_binary(Event)
                       ,"."
                       ,kz_amqp_util:encode(JID)
                       ])
       ).
-define(XMPP_EVENT_HEADERS, [<<"JID">>]).
-define(OPTIONAL_XMPP_EVENT_HEADERS, [<<"Application-Name">>
                                     ,<<"Application-Event">>
                                     ,<<"Application-Data">>
                                     ]).
-define(XMPP_EVENT_VALUES, [{<<"Event-Category">>, <<"xmpp_event">>}]).
-define(XMPP_EVENT_TYPES, []).

-spec event(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?XMPP_EVENT_HEADERS, ?OPTIONAL_XMPP_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_event"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(kz_term:api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?XMPP_EVENT_HEADERS, ?XMPP_EVENT_VALUES, ?XMPP_EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    JID = props:get_value('jid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, JID).

bind_q(Q, [Event|T], JID) ->
    _ = kz_amqp_util:bind_q_to_exchange(Q, ?XMPP_EVENT_ROUTING_KEY(Event, JID), ?XMPP_EXCHANGE),
    bind_q(Q, T, JID);
bind_q(_Q, [], _JID) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    JID = props:get_value('jid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, JID).

unbind_q(Q, [Event|T], JID) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Q, ?XMPP_EVENT_ROUTING_KEY(Event, JID), ?XMPP_EXCHANGE),
    unbind_q(Q, T, JID);
unbind_q(_Q, [], _JID) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?XMPP_EXCHANGE, <<"fanout">>).

-spec publish_event(kz_term:api_terms()) -> 'ok'.
publish_event(Event) -> publish_event(Event, ?DEFAULT_CONTENT_TYPE).

-spec publish_event(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_event(Event, ContentType) when is_list(Event) ->
    JID = props:get_value(<<"JID">>, Event),
    EventName = props:get_value(<<"Event-Name">>, Event),
    {'ok', Payload} = kz_api:prepare_api_payload(Event, ?XMPP_EVENT_VALUES, fun event/1),
    kz_amqp_util:basic_publish(?XMPP_EXCHANGE, ?XMPP_EVENT_ROUTING_KEY(EventName, JID), Payload, ContentType);
publish_event(Event, ContentType) ->
    publish_event(kz_json:to_proplist(Event), ContentType).

regexp_get(Jid, Regex) ->
    {'match', [ShortJid]} =
        re:run(Jid, Regex, [{'capture', 'all_but_first', 'binary'}]),
    ShortJid.

-spec jid_short(kz_term:ne_binary()) -> kz_term:ne_binary().
jid_short(JID) ->
    regexp_get(JID, <<"^([^/]*)">>).

-spec jid_username(kz_term:ne_binary()) -> kz_term:ne_binary().
jid_username(JID) ->
    regexp_get(JID, <<"^([^@]*)">>).

-spec jid_server(kz_term:ne_binary()) -> kz_term:ne_binary().
jid_server(JID) ->
    regexp_get(JID, <<"^[^@]*[@]([^/]*)">>).

-spec jid_resource(kz_term:ne_binary()) -> kz_term:ne_binary().
jid_resource(JID) ->
    regexp_get(JID, <<"^[^/]*[/](.*)">>).
