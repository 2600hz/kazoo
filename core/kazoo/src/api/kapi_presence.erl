%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_presence).

-export([search_req/1, search_req_v/1
         ,search_resp/1, search_resp_v/1
        ]).
-export([subscribe/1, subscribe_v/1]).
-export([update/1, update_v/1]).
-export([probe/1, probe_v/1]).
-export([mwi_update/1, mwi_update_v/1
         ,mwi_query/1, mwi_query_v/1
         ,sync/1, sync_v/1
        ]).
-export([register_overwrite/1, register_overwrite_v/1]).
-export([flush/1, flush_v/1]).
-export([reset/1, reset_v/1]).

-export([publish_search_req/1
         ,publish_search_resp/2
         ,publish_subscribe/1, publish_subscribe/2
         ,publish_update/1, publish_update/2
         ,publish_probe/1, publish_probe/2
         ,publish_mwi_update/1, publish_mwi_update/2
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

-include_lib("kazoo/include/kz_api.hrl").

-define(PRESENCE_STATES, [<<"trying">>, <<"early">>
                          ,<<"confirmed">>, <<"terminated">>
                          ,<<"online">>, <<"offline">>
                         ]).

%% Search request for active subscriptions
-define(SEARCH_REQ_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, [<<"Username">>, <<"Event-Package">>]).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                            ,{<<"Event-Name">>, <<"search_req">>}
                           ]).
-define(SEARCH_REQ_TYPES, []).

%% Search response for active subscriptions
-define(SEARCH_RESP_HEADERS, []).
-define(OPTIONAL_SEARCH_RESP_HEADERS, [<<"Subscriptions">>]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                             ,{<<"Event-Name">>, <<"search_resp">>}
                            ]).
-define(SEARCH_RESP_TYPES, []).

%% Presence subscription from Kamailio
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

%% Presence state updates
-define(UPDATE_HEADERS, [<<"Presence-ID">>, <<"State">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"To">>, <<"To-Tag">>
                                  ,<<"From">>, <<"From-Tag">>
                                  ,<<"Call-Direction">>, <<"Call-ID">>
                                  ,<<"Target-Call-ID">>, <<"Switch-URI">>
                                  ,<<"Event-Package">>
                                 ]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"update">>}
                        ,{<<"State">>, ?PRESENCE_STATES}
                       ]).
-define(UPDATE_TYPES, []).

%% Presence_Probe
-define(PROBE_HEADERS, [<<"Username">>, <<"Realm">>, <<"Event-Package">>]).
-define(OPTIONAL_PROBE_HEADERS, [<<"From-User">>, <<"From-Realm">>
                                 ,<<"To-User">>, <<"To-Realm">>
                                 ,<<"Expires">>, <<"Call-ID">>
                                ]).
-define(PROBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"probe">>}
                      ]).
-define(PROBE_TYPES, []).

%% MWI Update
-define(MWI_REQ_HEADERS, [<<"To">>
                          ,<<"Messages-New">>
                          ,<<"Messages-Saved">>
                         ]).
-define(OPTIONAL_MWI_REQ_HEADERS, [<<"Messages-Urgent">>
                                   ,<<"Messages-Urgent-Saved">>
                                   ,<<"Call-ID">>
                                  ]).
-define(MWI_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                         ,{<<"Event-Name">>, <<"mwi_update">>}
                        ]).
-define(MWI_REQ_TYPES, [{<<"Messages-New">>, fun is_integer/1}
                        ,{<<"Messages-Saved">>, fun is_integer/1}
                        ,{<<"Messages-Urgent">>, fun is_integer/1}
                        ,{<<"Messages-Urgent-Saved">>, fun is_integer/1}
                       ]).

%% MWI Query
-define(MWI_QUERY_HEADERS, [<<"Username">>, <<"Realm">>]).
-define(OPTIONAL_MWI_QUERY_HEADERS, [<<"Call-ID">>]).
-define(MWI_QUERY_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"mwi_query">>}
                          ]).
-define(MWI_QUERY_TYPES, []).

%% Register_Overwrite
-define(REGISTER_OVERWRITE_HEADERS, [<<"Previous-Contact">>, <<"Contact">>
                                     ,<<"Username">>, <<"Realm">>
                                    ]).
-define(OPTIONAL_REGISTER_OVERWRITE_HEADERS, []).
-define(REGISTER_OVERWRITE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                                    ,{<<"Event-Name">>, <<"register_overwrite">>}
                                   ]).
-define(REGISTER_OVERWRITE_TYPES, []).

%% Flush presence dialog cache
-define(FLUSH_HEADERS, [<<"Type">>]).
-define(OPTIONAL_FLUSH_HEADERS, [<<"User">>, <<"Event-Package">>]).
-define(FLUSH_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"flush">>}
                      ]).
-define(FLUSH_TYPES, []).

%% Reset presence dialog cache entry
-define(RESET_HEADERS, [<<"Realm">>, <<"Username">>]).
-define(OPTIONAL_RESET_HEADERS, [<<"Event-Package">>]).
-define(RESET_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"reset">>}
                      ]).
-define(RESET_TYPES, []).

%% Sync presence
-define(SYNC_HEADERS, [<<"Action">>]).
-define(OPTIONAL_SYNC_HEADERS, [<<"Event-Package">>]).
-define(SYNC_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"sync">>}
                      ,{<<"Action">>, [<<"Request">>, <<"Start">>, <<"End">>]}
                      ]).
-define(SYNC_TYPES, []).

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
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun ?MODULE:search_req/1),
    amqp_util:presence_publish(search_req_routing_key(Req), Payload, ContentType).

-spec search_req_routing_key(ne_binary() | api_terms()) -> ne_binary().
search_req_routing_key(Req) when is_list(Req) ->
    search_req_routing_key(props:get_value(<<"Realm">>, Req));
search_req_routing_key(Realm) when is_binary(Realm) ->
    list_to_binary([<<"presence.search_req.">>, amqp_util:encode(Realm)]);
search_req_routing_key(Req) ->
    search_req_routing_key(kz_json:get_value(<<"Realm">>, Req)).

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
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun ?MODULE:search_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

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

publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_subscribe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SUBSCRIBE_VALUES, fun ?MODULE:subscribe/1),
     amqp_util:presence_publish(subscribe_routing_key(Req), Payload, ContentType).

subscribe_routing_key(Prop) when is_list(Prop) ->
    subscribe_routing_key(props:get_value(<<"User">>, Prop));
subscribe_routing_key(User) when is_binary(User) ->
    R = case binary:split(User, <<"@">>) of
            [_To, Realm] -> amqp_util:encode(Realm);
            [Realm] -> amqp_util:encode(Realm)
        end,
    <<"subscriptions.", R/binary>>;
subscribe_routing_key(JObj) ->
    subscribe_routing_key(kz_json:get_value(<<"User">>, JObj)).

%%--------------------------------------------------------------------
%% @doc
%%
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

publish_update(JObj) ->
    publish_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UPDATE_VALUES, fun ?MODULE:update/1),
    amqp_util:presence_publish(update_routing_key(Req), Payload, ContentType).

-spec update_routing_key(ne_binary() | api_terms()) -> ne_binary().
-spec update_routing_key(ne_binary(), ne_binary()) -> ne_binary().
update_routing_key(Req) when is_list(Req) ->
    update_routing_key(props:get_value(<<"State">>, Req)
                      ,props:get_value(<<"Presence-ID">>, Req));
update_routing_key(Req) ->
    update_routing_key(kz_json:get_value(<<"State">>, Req)
                      ,kz_json:get_value(<<"Presence-ID">>, Req)).

update_routing_key(State, PresenceID) when is_binary(State) ->
    R = case binary:split(PresenceID, <<"@">>) of
            [_To, Realm] -> amqp_util:encode(Realm);
            [Realm] -> amqp_util:encode(Realm)
        end,
    list_to_binary([<<"update.">>
                      ,amqp_util:encode(State)
                      ,"."
                      ,amqp_util:encode(R)
                   ]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec probe(api_terms()) -> {'ok', iolist()} | {'error', string()}.
probe(Prop) when is_list(Prop) ->
    case probe_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PROBE_HEADERS, ?OPTIONAL_PROBE_HEADERS);
        'false' -> {'error', "Proplist failed validation for probe"}
    end;
probe(JObj) -> probe(kz_json:to_proplist(JObj)).

-spec probe_v(api_terms()) -> boolean().
probe_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PROBE_HEADERS, ?PROBE_VALUES, ?PROBE_TYPES);
probe_v(JObj) -> probe_v(kz_json:to_proplist(JObj)).

-spec publish_probe(api_terms()) -> 'ok'.
-spec publish_probe(api_terms(), ne_binary()) -> 'ok'.
publish_probe(JObj) -> publish_probe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_probe(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PROBE_VALUES, fun ?MODULE:probe/1),
     amqp_util:presence_publish(probe_routing_key(Req), Payload, ContentType).

probe_routing_key(Prop) when is_list(Prop) ->
    probe_routing_key(props:get_value(<<"Event-Package">>, Prop));
probe_routing_key(SubscriptionType) when is_binary(SubscriptionType) ->
    <<"probes.", SubscriptionType/binary>>;
probe_routing_key(JObj) ->
    probe_routing_key(kz_json:get_value(<<"Event-Package">>, JObj)).

%%--------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mwi_update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_update(Prop) when is_list(Prop) ->
    case mwi_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MWI_REQ_HEADERS, ?OPTIONAL_MWI_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi_req"}
    end;
mwi_update(JObj) -> mwi_update(kz_json:to_proplist(JObj)).

-spec mwi_update_v(api_terms()) -> boolean().
mwi_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MWI_REQ_HEADERS, ?MWI_REQ_VALUES, ?MWI_REQ_TYPES);
mwi_update_v(JObj) -> mwi_update_v(kz_json:to_proplist(JObj)).

-spec publish_mwi_update(api_terms()) -> 'ok'.
-spec publish_mwi_update(api_terms(), ne_binary()) -> 'ok'.
publish_mwi_update(JObj) -> publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_update(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MWI_REQ_VALUES, fun ?MODULE:mwi_update/1),
     amqp_util:presence_publish(mwi_update_routing_key(Req), Payload, ContentType).

-spec mwi_update_routing_key(api_terms() | api(binary())) -> ne_binary().
mwi_update_routing_key(Prop) when is_list(Prop) ->
    mwi_update_routing_key(props:get_value(<<"To">>, Prop));
mwi_update_routing_key(To) when is_binary(To) ->
    R = case binary:split(To, <<"@">>) of
            [_To, Realm] -> amqp_util:encode(Realm);
            [Realm] -> amqp_util:encode(Realm)
        end,
    <<"mwi_updates.", R/binary>>;
mwi_update_routing_key(JObj) ->
    mwi_update_routing_key(kz_json:get_value(<<"To">>, JObj)).

%%--------------------------------------------------------------------
%% @doc MWI - Query the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mwi_query(api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_query(Prop) when is_list(Prop) ->
    case mwi_query_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MWI_QUERY_HEADERS, ?OPTIONAL_MWI_QUERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for mwi query"}
    end;
mwi_query(JObj) -> mwi_query(kz_json:to_proplist(JObj)).

-spec mwi_query_v(api_terms()) -> boolean().
mwi_query_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MWI_QUERY_HEADERS, ?MWI_QUERY_VALUES, ?MWI_QUERY_TYPES);
mwi_query_v(JObj) -> mwi_query_v(kz_json:to_proplist(JObj)).

-spec publish_mwi_query(api_terms()) -> 'ok'.
-spec publish_mwi_query(api_terms(), ne_binary()) -> 'ok'.
publish_mwi_query(JObj) -> publish_mwi_query(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_query(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MWI_QUERY_VALUES, fun ?MODULE:mwi_query/1),
     amqp_util:presence_publish(mwi_query_routing_key(Req), Payload, ContentType).

%% <<"Notify-User">>, <<"Notify-Realm">>
mwi_query_routing_key(Prop) when is_list(Prop) ->
    mwi_query_routing_key(props:get_value(<<"Realm">>, Prop));
mwi_query_routing_key(Realm) when is_binary(Realm) ->
    <<"mwi_queries.", (amqp_util:encode(Realm))/binary>>;
mwi_query_routing_key(JObj) ->
    mwi_query_routing_key(kz_json:get_value(<<"Realm">>, JObj)).

%%--------------------------------------------------------------------
%% @doc Register_Overwrite (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
register_overwrite(Prop) when is_list(Prop) ->
    case register_overwrite_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_OVERWRITE_HEADERS, ?OPTIONAL_REGISTER_OVERWRITE_HEADERS);
        'false' -> {'error', "Proplist failed validation for register_overwrite"}
    end;
register_overwrite(JObj) -> register_overwrite(kz_json:to_proplist(JObj)).

-spec register_overwrite_v(api_terms()) -> boolean().
register_overwrite_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_OVERWRITE_HEADERS, ?REGISTER_OVERWRITE_VALUES, ?REGISTER_OVERWRITE_TYPES);
register_overwrite_v(JObj) -> register_overwrite_v(kz_json:to_proplist(JObj)).

-spec publish_register_overwrite(api_terms()) -> 'ok'.
-spec publish_register_overwrite(api_terms(), ne_binary()) -> 'ok'.
publish_register_overwrite(JObj) -> publish_register_overwrite(JObj, ?DEFAULT_CONTENT_TYPE).
publish_register_overwrite(Req, ContentType) when is_list(Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REGISTER_OVERWRITE_VALUES, fun ?MODULE:register_overwrite/1),
    amqp_util:presence_publish(register_overwrite_routing_key(Req), Payload, ContentType);
publish_register_overwrite(JObj, ContentType) ->
    publish_register_overwrite(kz_json:to_proplist(JObj), ContentType).

register_overwrite_routing_key(Prop) when is_list(Prop) ->
    register_overwrite_routing_key(props:get_value(<<"Realm">>, Prop));
register_overwrite_routing_key(Realm) when is_binary(Realm) ->
    <<"register_overwrites.", (amqp_util:encode(Realm))/binary>>;
register_overwrite_routing_key(JObj) ->
    register_overwrite_routing_key(kz_json:get_value(<<"Realm">>, JObj)).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(api_terms()) -> {'ok', iolist()} | {'error', string()}.
reset(Prop) when is_list(Prop) ->
    case reset_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESET_HEADERS, ?OPTIONAL_RESET_HEADERS);
        'false' -> {'error', "Proplist failed validation for reset"}
    end;
reset(JObj) ->
    reset(kz_json:to_proplist(JObj)).

-spec reset_v(api_terms()) -> boolean().
reset_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESET_HEADERS, ?RESET_VALUES, ?RESET_TYPES);
reset_v(JObj) ->
    reset_v(kz_json:to_proplist(JObj)).

-spec publish_reset(api_terms()) -> 'ok'.
-spec publish_reset(api_terms(), binary()) -> 'ok'.
publish_reset(JObj) ->
    publish_reset(JObj, ?DEFAULT_CONTENT_TYPE).
publish_reset(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RESET_VALUES, fun ?MODULE:reset/1),
    amqp_util:presence_publish(reset_routing_key(Req), Payload, ContentType).

-spec reset_routing_key(ne_binary() | api_terms()) -> ne_binary().
-spec reset_routing_key(ne_binary(), ne_binary()) -> ne_binary().
reset_routing_key(Req) when is_list(Req) ->
    reset_routing_key(props:get_value(<<"Realm">>, Req)
                      ,props:get_value(<<"Username">>, Req)
                     );
reset_routing_key(Req) ->
    reset_routing_key(kz_json:get_value(<<"Realm">>, Req)
                      ,kz_json:get_value(<<"Username">>, Req)
                     ).

reset_routing_key(Realm, Username) when is_binary(Realm) ->
    list_to_binary([<<"presence.reset.">>
                    ,amqp_util:encode(Realm)
                    ,"."
                    ,amqp_util:encode(Username)
                   ]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec flush(api_terms()) -> {'ok', iolist()} | {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FLUSH_HEADERS, ?OPTIONAL_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for flush query"}
    end;
flush(JObj) -> flush(kz_json:to_proplist(JObj)).

-spec flush_v(api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FLUSH_HEADERS, ?FLUSH_VALUES, ?FLUSH_TYPES);
flush_v(JObj) -> flush_v(kz_json:to_proplist(JObj)).

publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).
publish_flush(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FLUSH_VALUES, fun ?MODULE:flush/1),
    amqp_util:presence_publish(<<"flush">>, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(api_terms()) -> {'ok', iolist()} | {'error', string()}.
sync(Prop) when is_list(Prop) ->
    case sync_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYNC_HEADERS, ?OPTIONAL_SYNC_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync query"}
    end;
sync(JObj) -> sync(kz_json:to_proplist(JObj)).

-spec sync_v(api_terms()) -> boolean().
sync_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYNC_HEADERS, ?SYNC_VALUES, ?SYNC_TYPES);
sync_v(JObj) -> sync_v(kz_json:to_proplist(JObj)).

publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SYNC_VALUES, fun ?MODULE:sync/1),
    amqp_util:presence_publish(<<"sync">>, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    bind_q(Queue, RestrictTo, Props).

-spec bind_q(ne_binary(), api([api(binary())]), kz_proplist()) -> 'ok'.
bind_q(Queue, 'undefined', _) ->
    amqp_util:bind_q_to_presence(Queue, <<"#">>);
bind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = search_req_routing_key(Realm),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['sync'|Restrict], Props) ->
    amqp_util:bind_q_to_presence(Queue, <<"sync">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = subscribe_routing_key(User),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['update'|Restrict], Props) ->
    State = props:get_value('state', Props, <<"*">>),
    PresenceId = props:get_value('presence-id', Props, <<"*">>),
    RoutingKey = update_routing_key(State, PresenceId),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = probe_routing_key(ProbeType),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_update_routing_key(User),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_query_routing_key(User),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = register_overwrite_routing_key(Realm),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['flush'|Restrict], Props) ->
    amqp_util:bind_q_to_presence(Queue, <<"flush">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = reset_routing_key(Realm, Username),
    amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    unbind_q(Queue, RestrictTo, Props).

-spec unbind_q(ne_binary(), api(binary()), kz_proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', _) ->
    amqp_util:unbind_q_from_presence(Queue, <<"#">>);
unbind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = search_req_routing_key(Realm),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['sync'|Restrict], Props) ->
    amqp_util:unbind_q_from_presence(Queue, <<"sync">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = subscribe_routing_key(User),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['update'|Restrict], Props) ->
    State = props:get_value('state', Props, <<"*">>),
    PresenceId = props:get_value('presence-id', Props, <<"*">>),
    RoutingKey = update_routing_key(State, PresenceId),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = probe_routing_key(ProbeType),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_update_routing_key(User),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = mwi_query_routing_key(User),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = register_overwrite_routing_key(Realm),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['flush'|Restrict], Props) ->
    amqp_util:unbind_q_from_presence(Queue, <<"flush">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = reset_routing_key(Realm, Username),
    amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, [_|Restrict], Props) ->
    unbind_q(Queue, Restrict, Props);
unbind_q(_, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_states() -> ne_binaries().
presence_states() -> ?PRESENCE_STATES.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_state(api(binary()) | api_terms()) -> boolean().
is_valid_state(State) when is_binary(State) ->
    lists:member(State, ?PRESENCE_STATES);
is_valid_state(Prop) when is_list(Prop) ->
    is_valid_state(props:get_value(<<"State">>, Prop));
is_valid_state(JObj) ->
    is_valid_state(kz_json:get_value(<<"State">>, JObj)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:presence_exchange().
