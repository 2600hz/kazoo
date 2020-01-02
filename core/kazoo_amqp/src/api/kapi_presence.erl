%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_presence).

-export([search_req/1, search_req_v/1
        ,search_partial_resp/1, search_partial_resp_v/1
        ,search_resp/1, search_resp_v/1
        ]).
-export([subscribe/1, subscribe_v/1]).
-export([dialog/1, dialog_v/1]).
-export([update/1, update_v/1]).
-export([probe/1, probe_v/1]).
-export([mwi_update/1, mwi_update_v/1
        ,mwi_unsolicited_update/1, mwi_unsolicited_update_v/1
        ,mwi_query/1, mwi_query_v/1
        ,sync/1, sync_v/1
        ]).
-export([register_overwrite/1, register_overwrite_v/1]).
-export([flush/1, flush_v/1]).
-export([reset/1, reset_v/1]).

-export([publish_search_req/1
        ,publish_search_partial_resp/2, publish_search_partial_resp/3
        ,publish_search_resp/2
        ,publish_subscribe/1, publish_subscribe/2
        ,publish_dialog/1, publish_dialog/2
        ,publish_update/1, publish_update/2
        ,publish_probe/1, publish_probe/2
        ,publish_mwi_update/1, publish_mwi_update/2
        ,publish_unsolicited_mwi_update/1, publish_unsolicited_mwi_update/2
        ,publish_mwi_query/1, publish_mwi_query/2
        ,publish_register_overwrite/1, publish_register_overwrite/2
        ,publish_flush/1, publish_flush/2
        ,publish_reset/1
        ,publish_sync/1, publish_sync/2
        ]).

-export([subscribe_routing_key/1]).
-export([presence_states/0]).
-export([is_valid_state/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).

-export([declare_exchanges/0]).

-include_lib("kazoo_amqp/src/kz_amqp_util.hrl").
-include("kapi_presence.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec search_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_req(Prop) when is_list(Prop) ->
    case search_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?OPTIONAL_SEARCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_req"}
    end;
search_req(JObj) ->
    search_req(kz_json:to_proplist(JObj)).

-spec search_req_v(kz_term:api_terms()) -> boolean().
search_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
search_req_v(JObj) ->
    search_req_v(kz_json:to_proplist(JObj)).

-spec publish_search_req(kz_term:api_terms()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_search_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun search_req/1),
    kz_amqp_util:presence_publish(search_req_routing_key(Req), Payload, ContentType).

-spec search_req_routing_key(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
search_req_routing_key(Req) when is_list(Req) ->
    search_req_routing_key(props:get_value(<<"Realm">>, Req));
search_req_routing_key(Realm) when is_binary(Realm) ->
    list_to_binary([<<"presence.search_req.">>, kz_amqp_util:encode(Realm)]);
search_req_routing_key(Req) ->
    search_req_routing_key(kz_json:get_value(<<"Realm">>, Req)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec search_partial_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_partial_resp(Prop) when is_list(Prop) ->
    case search_partial_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_PARTIAL_RESP_HEADERS, ?OPTIONAL_SEARCH_PARTIAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_resp"}
    end;
search_partial_resp(JObj) ->
    search_partial_resp(kz_json:to_proplist(JObj)).

-spec search_partial_resp_v(kz_term:api_terms()) -> boolean().
search_partial_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_PARTIAL_RESP_HEADERS, ?SEARCH_PARTIAL_RESP_VALUES, ?SEARCH_PARTIAL_RESP_TYPES);
search_partial_resp_v(JObj) ->
    search_partial_resp_v(kz_json:to_proplist(JObj)).

-spec publish_search_partial_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_search_partial_resp(Queue, JObj) ->
    publish_search_partial_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_partial_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_search_partial_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_PARTIAL_RESP_VALUES, fun search_partial_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec search_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_resp(Prop) when is_list(Prop) ->
    case search_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?OPTIONAL_SEARCH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search_resp"}
    end;
search_resp(JObj) ->
    search_resp(kz_json:to_proplist(JObj)).

-spec search_resp_v(kz_term:api_terms()) -> boolean().
search_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
search_resp_v(JObj) ->
    search_resp_v(kz_json:to_proplist(JObj)).

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_search_resp(Queue, JObj) ->
    publish_search_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_search_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun search_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Subscribing for updates.
%% Takes proplist, creates JSON string or error.
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

-spec publish_subscribe(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_subscribe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SUBSCRIBE_VALUES, fun subscribe/1),
    kz_amqp_util:presence_publish(subscribe_routing_key(Req), Payload, ContentType).

-spec subscribe_routing_key(kz_term:api_terms() | kz_term:ne_binary()) -> kz_term:ne_binary().
subscribe_routing_key(Prop) when is_list(Prop) ->
    subscribe_routing_key(props:get_value(<<"User">>, Prop));
subscribe_routing_key(User) when is_binary(User) ->
    R = case binary:split(User, <<"@">>) of
            [_To, Realm] -> kz_amqp_util:encode(Realm);
            [Realm] -> kz_amqp_util:encode(Realm)
        end,
    <<"subscriptions.", R/binary>>;
subscribe_routing_key(JObj) ->
    subscribe_routing_key(kz_json:get_value(<<"User">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UPDATE_HEADERS, ?OPTIONAL_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for update"}
    end;
update(JObj) -> update(kz_json:to_proplist(JObj)).

-spec update_v(kz_term:api_terms()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UPDATE_HEADERS, ?UPDATE_VALUES, ?UPDATE_TYPES);
update_v(JObj) -> update_v(kz_json:to_proplist(JObj)).

-spec publish_update(kz_term:api_terms()) -> 'ok'.
publish_update(JObj) ->
    publish_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UPDATE_VALUES, fun update/1),
    kz_amqp_util:presence_publish(update_routing_key(Req), Payload, ContentType).

-spec update_routing_key(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
update_routing_key(Req) when is_list(Req) ->
    update_routing_key(props:get_value(<<"Call-ID">>, Req)
                      ,props:get_value(<<"Presence-ID">>, Req)
                      );
update_routing_key(Req) ->
    update_routing_key(kz_json:get_value(<<"Call-ID">>, Req)
                      ,kz_json:get_value(<<"Presence-ID">>, Req)
                      ).

-spec update_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
update_routing_key(CallId, PresenceID) ->
    list_to_binary([<<"update.">>
                   ,kz_amqp_util:encode(realm_from_presence_id(PresenceID))
                   ,"."
                   ,kz_amqp_util:encode(CallId)
                   ]).

-spec realm_from_presence_id(kz_term:ne_binary()) -> kz_term:ne_binary().
realm_from_presence_id(PresenceID) ->
    case binary:split(PresenceID, <<"@">>) of
        [_To, Realm] -> Realm;
        [Realm] -> Realm
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dialog(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
dialog(Prop) when is_list(Prop) ->
    case dialog_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DIALOG_HEADERS, ?OPTIONAL_DIALOG_HEADERS);
        'false' -> {'error', "Proplist failed validation for update"}
    end;
dialog(JObj) -> dialog(kz_json:to_proplist(JObj)).

-spec dialog_v(kz_term:api_terms()) -> boolean().
dialog_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DIALOG_HEADERS, ?DIALOG_VALUES, ?DIALOG_TYPES);
dialog_v(JObj) -> dialog_v(kz_json:to_proplist(JObj)).

-spec publish_dialog(kz_term:api_terms()) -> 'ok'.
publish_dialog(JObj) ->
    publish_dialog(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_dialog(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_dialog(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DIALOG_VALUES, fun dialog/1),
    kz_amqp_util:presence_publish(dialog_routing_key(Req), Payload, ContentType).

-spec dialog_routing_key(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
dialog_routing_key(Req) when is_list(Req) ->
    dialog_routing_key(props:get_value(<<"Call-ID">>, Req)
                      ,props:get_value(<<"Presence-ID">>, Req)
                      );
dialog_routing_key(Req) ->
    dialog_routing_key(kz_json:get_value(<<"Call-ID">>, Req)
                      ,kz_json:get_value(<<"Presence-ID">>, Req)
                      ).

-spec dialog_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
dialog_routing_key(CallId, PresenceID) ->
    list_to_binary([<<"dialog.">>
                   ,kz_amqp_util:encode(realm_from_presence_id(PresenceID))
                   ,"."
                   ,kz_amqp_util:encode(CallId)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec probe(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
probe(Prop) when is_list(Prop) ->
    case probe_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PROBE_HEADERS, ?OPTIONAL_PROBE_HEADERS);
        'false' -> {'error', "Proplist failed validation for probe"}
    end;
probe(JObj) -> probe(kz_json:to_proplist(JObj)).

-spec probe_v(kz_term:api_terms()) -> boolean().
probe_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PROBE_HEADERS, ?PROBE_VALUES, ?PROBE_TYPES);
probe_v(JObj) -> probe_v(kz_json:to_proplist(JObj)).

-spec publish_probe(kz_term:api_terms()) -> 'ok'.
publish_probe(JObj) -> publish_probe(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_probe(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_probe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PROBE_VALUES, fun probe/1),
    kz_amqp_util:presence_publish(probe_routing_key(Req), Payload, ContentType).

probe_routing_key(Prop) when is_list(Prop) ->
    probe_routing_key(props:get_value(<<"Event-Package">>, Prop));
probe_routing_key(SubscriptionType) when is_binary(SubscriptionType) ->
    <<"probes.", SubscriptionType/binary>>;
probe_routing_key(JObj) ->
    probe_routing_key(kz_json:get_value(<<"Event-Package">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mwi_extended_update(kz_term:proplist()) -> kz_term:proplist().
mwi_extended_update(Prop) ->
    MessagesNew = props:get_integer_value(<<"Messages-New">>, Prop, 0),
    MessagesWaiting = case MessagesNew of 0 -> <<"no">>; _ -> <<"yes">> end,
    To = props:get_value(<<"To">>, Prop),
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    CallId = ?FAKE_CALLID(To),
    props:delete_keys([<<"Call-ID">>, <<"To">>], Prop)
        ++ [{<<"From">>, <<"sip:", To/binary>>}
           ,{<<"From-User">>, ToUsername}
           ,{<<"From-Realm">>, ToRealm}
           ,{<<"To">>, <<"sip:", To/binary>>}
           ,{<<"To-User">>, ToUsername}
           ,{<<"To-Realm">>, ToRealm}
           ,{<<"Message-Account">>, <<"sip:", To/binary>>}
           ,{<<"Messages-Waiting">>, MessagesWaiting}
           ,{<<"Messages-New">>, MessagesNew}
           ,{<<"Messages-Saved">>, 0}
           ,{<<"Messages-Urgent">>, 0}
           ,{<<"Messages-Urgent-Saved">>, 0}
           ,{<<"Presence-ID">>, To}
           ,{<<"Call-ID">>, CallId}
           ].

-spec mwi_update(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_update(Prop) when is_list(Prop) ->
    case mwi_update_v(Prop) of
        'true' -> kz_api:build_message(mwi_extended_update(Prop), ?MWI_REQ_HEADERS, ?OPTIONAL_MWI_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi_req"}
    end;
mwi_update(JObj) -> mwi_update(kz_json:to_proplist(JObj)).

-spec mwi_update_v(kz_term:api_terms()) -> boolean().
mwi_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MWI_REQ_HEADERS, ?MWI_REQ_VALUES, ?MWI_REQ_TYPES);
mwi_update_v(JObj) -> mwi_update_v(kz_json:to_proplist(JObj)).

-spec publish_mwi_update(kz_term:api_terms()) -> 'ok'.
publish_mwi_update(JObj) -> publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_mwi_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_mwi_update(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MWI_REQ_VALUES, fun mwi_update/1),
    kz_amqp_util:presence_publish(mwi_update_routing_key(Req), Payload, ContentType).

-spec mwi_update_routing_key(kz_term:api_terms() | kz_term:api_binary()) -> kz_term:ne_binary().
mwi_update_routing_key(Prop) when is_list(Prop) ->
    mwi_update_routing_key(props:get_value(<<"To">>, Prop));
mwi_update_routing_key(To) when is_binary(To) ->
    [User, Realm] = binary:split(To, <<"@">>),
    mwi_update_routing_key(User, Realm);
mwi_update_routing_key(JObj) ->
    mwi_update_routing_key(kz_json:get_value(<<"To">>, JObj)).

-spec mwi_update_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
mwi_update_routing_key(User, Realm) ->
    list_to_binary([<<"mwi_updates.">>
                   ,kz_amqp_util:encode(Realm)
                   ,"."
                   ,kz_amqp_util:encode(User)
                   ]).

-spec mwi_unsolicited_update(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_unsolicited_update(Prop) when is_list(Prop) ->
    case mwi_unsolicited_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MWI_REQ_HEADERS, ?OPTIONAL_MWI_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi_req"}
    end;
mwi_unsolicited_update(JObj) -> mwi_unsolicited_update(kz_json:to_proplist(JObj)).

-spec mwi_unsolicited_update_v(kz_term:api_terms()) -> boolean().
mwi_unsolicited_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MWI_REQ_HEADERS, ?MWI_UNSOLICITED_REQ_VALUES, ?MWI_REQ_TYPES);
mwi_unsolicited_update_v(JObj) -> mwi_unsolicited_update_v(kz_json:to_proplist(JObj)).

-spec publish_unsolicited_mwi_update(kz_term:api_terms()) -> 'ok'.
publish_unsolicited_mwi_update(JObj) -> publish_unsolicited_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_unsolicited_mwi_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unsolicited_mwi_update(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MWI_UNSOLICITED_REQ_VALUES, fun mwi_unsolicited_update/1),
    kz_amqp_util:presence_publish(mwi_unsolicited_update_routing_key(Req), Payload, ContentType).

-spec mwi_unsolicited_update_routing_key(kz_term:api_terms() | kz_term:api_binary()) -> kz_term:ne_binary().
mwi_unsolicited_update_routing_key(Prop) when is_list(Prop) ->
    mwi_unsolicited_update_routing_key(props:get_value(<<"To">>, Prop));
mwi_unsolicited_update_routing_key(To) when is_binary(To) ->
    R = case binary:split(To, <<"@">>) of
            [_To, Realm] -> kz_amqp_util:encode(Realm);
            [Realm] -> kz_amqp_util:encode(Realm)
        end,
    <<"mwi_unsolicited_updates.", R/binary>>;
mwi_unsolicited_update_routing_key(JObj) ->
    mwi_unsolicited_update_routing_key(kz_json:get_value(<<"To">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc MWI - Query the Message Waiting Indicator on a device.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mwi_query(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_query(Prop) when is_list(Prop) ->
    case mwi_query_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MWI_QUERY_HEADERS, ?OPTIONAL_MWI_QUERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi query"}
    end;
mwi_query(JObj) -> mwi_query(kz_json:to_proplist(JObj)).

-spec mwi_query_v(kz_term:api_terms()) -> boolean().
mwi_query_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MWI_QUERY_HEADERS, ?MWI_QUERY_VALUES, ?MWI_QUERY_TYPES);
mwi_query_v(JObj) -> mwi_query_v(kz_json:to_proplist(JObj)).

-spec publish_mwi_query(kz_term:api_terms()) -> 'ok'.
publish_mwi_query(JObj) -> publish_mwi_query(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_mwi_query(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_mwi_query(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MWI_QUERY_VALUES, fun mwi_query/1),
    kz_amqp_util:presence_publish(mwi_query_routing_key(Req), Payload, ContentType).

-spec mwi_query_routing_key(kz_term:api_terms() | kz_term:ne_binary()) -> kz_term:ne_binary().
mwi_query_routing_key(Prop) when is_list(Prop) ->
    mwi_query_routing_key(props:get_value(<<"Realm">>, Prop));
mwi_query_routing_key(Realm) when is_binary(Realm) ->
    <<"mwi_queries.", (kz_amqp_util:encode(Realm))/binary>>;
mwi_query_routing_key(JObj) ->
    mwi_query_routing_key(kz_json:get_value(<<"Realm">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc Register_Overwrite (unregister is a key word).
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec register_overwrite(kz_term:api_terms()) -> api_formatter_return().
register_overwrite(Prop) when is_list(Prop) ->
    case register_overwrite_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_OVERWRITE_HEADERS, ?OPTIONAL_REGISTER_OVERWRITE_HEADERS);
        'false' -> {'error', "Proplist failed validation for register_overwrite"}
    end;
register_overwrite(JObj) -> register_overwrite(kz_json:to_proplist(JObj)).

-spec register_overwrite_v(kz_term:api_terms()) -> boolean().
register_overwrite_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_OVERWRITE_HEADERS, ?REGISTER_OVERWRITE_VALUES, ?REGISTER_OVERWRITE_TYPES);
register_overwrite_v(JObj) -> register_overwrite_v(kz_json:to_proplist(JObj)).

-spec publish_register_overwrite(kz_term:api_terms()) -> 'ok'.
publish_register_overwrite(JObj) -> publish_register_overwrite(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_register_overwrite(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register_overwrite(Req, ContentType) when is_list(Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REGISTER_OVERWRITE_VALUES, fun register_overwrite/1),
    kz_amqp_util:presence_publish(register_overwrite_routing_key(Req), Payload, ContentType);
publish_register_overwrite(JObj, ContentType) ->
    publish_register_overwrite(kz_json:to_proplist(JObj), ContentType).

register_overwrite_routing_key(Prop) when is_list(Prop) ->
    register_overwrite_routing_key(props:get_value(<<"Realm">>, Prop));
register_overwrite_routing_key(Realm) when is_binary(Realm) ->
    <<"register_overwrites.", (kz_amqp_util:encode(Realm))/binary>>;
register_overwrite_routing_key(JObj) ->
    register_overwrite_routing_key(kz_json:get_value(<<"Realm">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
reset(Prop) when is_list(Prop) ->
    case reset_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESET_HEADERS, ?OPTIONAL_RESET_HEADERS);
        'false' -> {'error', "Proplist failed validation for reset"}
    end;
reset(JObj) ->
    reset(kz_json:to_proplist(JObj)).

-spec reset_v(kz_term:api_terms()) -> boolean().
reset_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESET_HEADERS, ?RESET_VALUES, ?RESET_TYPES);
reset_v(JObj) ->
    reset_v(kz_json:to_proplist(JObj)).

-spec publish_reset(kz_term:api_terms()) -> 'ok'.
publish_reset(JObj) ->
    publish_reset(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_reset(kz_term:api_terms(), binary()) -> 'ok'.
publish_reset(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RESET_VALUES, fun reset/1),
    kz_amqp_util:presence_publish(reset_routing_key(Req), Payload, ContentType).

-spec reset_routing_key(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
reset_routing_key(Req) when is_list(Req) ->
    reset_routing_key(props:get_value(<<"Realm">>, Req)
                     ,props:get_value(<<"Username">>, Req)
                     );
reset_routing_key(Req) ->
    reset_routing_key(kz_json:get_value(<<"Realm">>, Req)
                     ,kz_json:get_value(<<"Username">>, Req)
                     ).

-spec reset_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
reset_routing_key(Realm, Username) when is_binary(Realm) ->
    list_to_binary([<<"presence.reset.">>
                   ,kz_amqp_util:encode(Realm)
                   ,"."
                   ,kz_amqp_util:encode(Username)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FLUSH_HEADERS, ?OPTIONAL_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for flush query"}
    end;
flush(JObj) -> flush(kz_json:to_proplist(JObj)).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FLUSH_HEADERS, ?FLUSH_VALUES, ?FLUSH_TYPES);
flush_v(JObj) -> flush_v(kz_json:to_proplist(JObj)).

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FLUSH_VALUES, fun flush/1),
    kz_amqp_util:presence_publish(<<"flush">>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
sync(Prop) when is_list(Prop) ->
    case sync_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYNC_HEADERS, ?OPTIONAL_SYNC_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync query"}
    end;
sync(JObj) -> sync(kz_json:to_proplist(JObj)).

-spec sync_v(kz_term:api_terms()) -> boolean().
sync_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYNC_HEADERS, ?SYNC_VALUES, ?SYNC_TYPES);
sync_v(JObj) -> sync_v(kz_json:to_proplist(JObj)).

-spec publish_sync(kz_term:api_terms()) -> 'ok'.
publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SYNC_VALUES, fun sync/1),
    kz_amqp_util:presence_publish(<<"sync">>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    bind_q(Queue, RestrictTo, Props).

-spec bind_q(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, 'undefined', _) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"#">>);
bind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = search_req_routing_key(Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['sync'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"sync">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = subscribe_routing_key(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['update'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = update_routing_key(CallId, PresenceId),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['dialog'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = dialog_routing_key(CallId, PresenceId),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = probe_routing_key(ProbeType),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = mwi_update_routing_key(User, Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_unsolicited_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_unsolicited_update_routing_key(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_query_routing_key(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = register_overwrite_routing_key(Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['flush'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"flush">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = reset_routing_key(Realm, Username),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    unbind_q(Queue, RestrictTo, Props).

-spec unbind_q(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', _) ->
    kz_amqp_util:unbind_q_from_presence(Queue, <<"#">>);
unbind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = search_req_routing_key(Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['sync'|Restrict], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, <<"sync">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = subscribe_routing_key(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['update'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = update_routing_key(CallId, PresenceId),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['dialog'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = dialog_routing_key(CallId, PresenceId),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = probe_routing_key(ProbeType),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = mwi_update_routing_key(User, Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_unsolicited_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_unsolicited_update_routing_key(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_query_routing_key(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = register_overwrite_routing_key(Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['flush'|Restrict], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, <<"flush">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = reset_routing_key(Realm, Username),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, [_|Restrict], Props) ->
    unbind_q(Queue, Restrict, Props);
unbind_q(_, [], _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec presence_states() -> kz_term:ne_binaries().
presence_states() -> ?PRESENCE_STATES.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_valid_state(kz_term:api_binary() | kz_term:api_terms()) -> boolean().
is_valid_state(State) when is_binary(State) ->
    lists:member(State, ?PRESENCE_STATES);
is_valid_state(Prop) when is_list(Prop) ->
    is_valid_state(props:get_value(<<"State">>, Prop));
is_valid_state(JObj) ->
    is_valid_state(kz_json:get_value(<<"State">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:presence_exchange().
