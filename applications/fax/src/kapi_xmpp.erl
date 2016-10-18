%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
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

-define(XMPP_EVENT_ROUTING_KEY(Event, JID),
        <<"xmpp."
          ,(kz_util:to_binary(Event))/binary
          ,"."
          ,(amqp_util:encode(JID))/binary>>).
-define(XMPP_EVENT_HEADERS, [<<"JID">>]).
-define(OPTIONAL_XMPP_EVENT_HEADERS, [<<"Application-Name">>
                                     ,<<"Application-Event">>
                                     ,<<"Application-Data">>
                                     ]).
-define(XMPP_EVENT_VALUES, [{<<"Event-Category">>, <<"xmpp_event">>}]).
-define(XMPP_EVENT_TYPES, []).




-spec event(api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?XMPP_EVENT_HEADERS, ?OPTIONAL_XMPP_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_event"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?XMPP_EVENT_HEADERS, ?XMPP_EVENT_VALUES, ?XMPP_EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).



-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    JID = props:get_value('jid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, JID).

bind_q(Q, [Event|T], JID) ->
    _ = amqp_util:bind_q_to_exchange(Q, ?XMPP_EVENT_ROUTING_KEY(Event, JID), ?XMPP_EXCHANGE),
    bind_q(Q, T, JID);
bind_q(_Q, [], _JID) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    JID = props:get_value('jid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, JID).

unbind_q(Q, [Event|T], JID) ->
    _ = amqp_util:unbind_q_from_exchange(Q, ?XMPP_EVENT_ROUTING_KEY(Event, JID), ?XMPP_EXCHANGE),
    unbind_q(Q, T, JID);
unbind_q(_Q, [], _JID) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?XMPP_EXCHANGE, <<"fanout">>).


-spec publish_event(api_terms()) -> 'ok'.
-spec publish_event(api_terms(), ne_binary()) -> 'ok'.
publish_event(Event) -> publish_event(Event, ?DEFAULT_CONTENT_TYPE).
publish_event(Event, ContentType) when is_list(Event) ->
    JID = props:get_value(<<"JID">>, Event),
    EventName = props:get_value(<<"Event-Name">>, Event),
    {'ok', Payload} = kz_api:prepare_api_payload(Event, ?XMPP_EVENT_VALUES, fun event/1),
    amqp_util:basic_publish(?XMPP_EXCHANGE, ?XMPP_EVENT_ROUTING_KEY(EventName, JID), Payload, ContentType);
publish_event(Event, ContentType) ->
    publish_event(kz_json:to_proplist(Event), ContentType).


regexp_get(Jid, Regex) ->
    {'match', [ShortJid]} =
        re:run(Jid, Regex, [{'capture', 'all_but_first', 'binary'}]),
    ShortJid.

-spec jid_short(ne_binary()) -> ne_binary().
jid_short(JID) ->
    regexp_get(JID, <<"^([^/]*)">>).

-spec jid_username(ne_binary()) -> ne_binary().
jid_username(JID) ->
    regexp_get(JID, <<"^([^@]*)">>).

-spec jid_server(ne_binary()) -> ne_binary().
jid_server(JID) ->
    regexp_get(JID, <<"^[^@]*[@]([^/]*)">>).

-spec jid_resource(ne_binary()) -> ne_binary().
jid_resource(JID) ->
    regexp_get(JID, <<"^[^/]*[/](.*)">>).
