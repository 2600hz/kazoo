%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Call-related messages, like switch events, status requests, etc AMQP API.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_call).

-export([optional_call_event_headers/0]).

-export([event/1, event_v/1]).

-export([channel_status_req/1, channel_status_req_v/1]).
-export([channel_status_resp/1, channel_status_resp_v/1]).

-export([query_auth_id_req/1, query_auth_id_req_v/1]).
-export([query_auth_id_resp/1, query_auth_id_resp_v/1]).

-export([query_user_channels_req/1, query_user_channels_req_v/1]).
-export([query_user_channels_resp/1, query_user_channels_resp_v/1]).

-export([query_account_channels_req/1, query_account_channels_req_v/1]).
-export([query_account_channels_resp/1, query_account_channels_resp_v/1]).

-export([query_channels_req/1, query_channels_req_v/1]).
-export([query_channels_resp/1, query_channels_resp_v/1]).

-export([usurp_control/1, usurp_control_v/1]).
-export([usurp_publisher/1, usurp_publisher_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_event/1, publish_event/2]).

-export([publish_channel_status_req/1 ,publish_channel_status_req/2, publish_channel_status_req/3]).
-export([publish_channel_status_resp/2, publish_channel_status_resp/3]).

-export([publish_query_auth_id_req/1 ,publish_query_auth_id_req/2, publish_query_auth_id_req/3]).
-export([publish_query_auth_id_resp/2, publish_query_auth_id_resp/3]).

-export([publish_query_user_channels_req/1 ,publish_query_user_channels_req/4]).
-export([publish_query_user_channels_resp/2 ,publish_query_user_channels_resp/3]).

-export([publish_query_account_channels_req/1 ,publish_query_account_channels_req/3]).
-export([publish_query_account_channels_resp/2 ,publish_query_account_channels_resp/3]).

-export([publish_query_channels_req/1 ,publish_query_channels_req/2]).
-export([publish_query_channels_resp/2 ,publish_query_channels_resp/3]).

-export([publish_usurp_control/2, publish_usurp_control/3]).
-export([publish_usurp_publisher/2, publish_usurp_publisher/3]).

-export([get_status/1]).
-export([event_routing_key/2]).

-export_type([event/0]).

-include_lib("kazoo_amqp/src/kz_amqp_util.hrl").
-include("kapi_call.hrl").

-type event() :: kz_json:object().

-spec optional_call_event_headers() -> kz_term:ne_binaries().
optional_call_event_headers() ->
    ?OPTIONAL_CALL_EVENT_HEADERS.

%%------------------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec event(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_event"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(kz_term:api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a channel.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec channel_status_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_req(Prop) when is_list(Prop) ->
    case channel_status_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?OPTIONAL_CHANNEL_STATUS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for channel status req"}
    end;
channel_status_req(JObj) -> channel_status_req(kz_json:to_proplist(JObj)).

-spec channel_status_req_v(kz_term:api_terms()) -> boolean().
channel_status_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?CHANNEL_STATUS_REQ_VALUES, ?CHANNEL_STATUS_REQ_TYPES);
channel_status_req_v(JObj) -> channel_status_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Respond with status of a channel, either active or nonexistent.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec channel_status_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_resp(Prop) when is_list(Prop) ->
    case channel_status_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?OPTIONAL_CHANNEL_STATUS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for channel status resp"}
    end;
channel_status_resp(JObj) -> channel_status_resp(kz_json:to_proplist(JObj)).

-spec channel_status_resp_v(kz_term:api_terms()) -> boolean().
channel_status_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?CHANNEL_STATUS_RESP_VALUES, ?CHANNEL_STATUS_RESP_TYPES);
channel_status_resp_v(JObj) -> channel_status_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_auth_id_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_req(Prop) when is_list(Prop) ->
    case query_auth_id_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth id query req"}
    end;
query_auth_id_req(JObj) -> query_auth_id_req(kz_json:to_proplist(JObj)).

-spec query_auth_id_req_v(kz_term:api_terms()) -> boolean().
query_auth_id_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?QUERY_AUTH_ID_REQ_VALUES, ?QUERY_AUTH_ID_REQ_TYPES);
query_auth_id_req_v(JObj) -> query_auth_id_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_auth_id_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_resp(Prop) when is_list(Prop) ->
    case query_auth_id_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth id query resp"}
    end;
query_auth_id_resp(JObj) -> query_auth_id_resp(kz_json:to_proplist(JObj)).

-spec query_auth_id_resp_v(kz_term:api_terms()) -> boolean().
query_auth_id_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?QUERY_AUTH_ID_RESP_VALUES, ?QUERY_AUTH_ID_RESP_TYPES);
query_auth_id_resp_v(JObj) -> query_auth_id_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_user_channels_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_user_channels_req(Prop) when is_list(Prop) ->
    case query_user_channels_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_USER_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_USER_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for users channels query req"}
    end;
query_user_channels_req(JObj) -> query_user_channels_req(kz_json:to_proplist(JObj)).

-spec query_user_channels_req_v(kz_term:api_terms()) -> boolean().
query_user_channels_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_USER_CHANNELS_REQ_HEADERS, ?QUERY_USER_CHANNELS_REQ_VALUES, ?QUERY_USER_CHANNELS_REQ_TYPES);
query_user_channels_req_v(JObj) -> query_user_channels_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_user_channels_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_user_channels_resp(Prop) when is_list(Prop) ->
    case query_user_channels_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_USER_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_USER_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for users channels query resp"}
    end;
query_user_channels_resp(JObj) -> query_user_channels_resp(kz_json:to_proplist(JObj)).

-spec query_user_channels_resp_v(kz_term:api_terms()) -> boolean().
query_user_channels_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_USER_CHANNELS_RESP_HEADERS, ?QUERY_USER_CHANNELS_RESP_VALUES, ?QUERY_USER_CHANNELS_RESP_TYPES);
query_user_channels_resp_v(JObj) -> query_user_channels_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_account_channels_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_account_channels_req(Prop) when is_list(Prop) ->
    case query_account_channels_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_ACCOUNT_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for account channels query req"}
    end;
query_account_channels_req(JObj) -> query_account_channels_req(kz_json:to_proplist(JObj)).

-spec query_account_channels_req_v(kz_term:api_terms()) -> boolean().
query_account_channels_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, ?QUERY_ACCOUNT_CHANNELS_REQ_VALUES, ?QUERY_ACCOUNT_CHANNELS_REQ_TYPES);
query_account_channels_req_v(JObj) -> query_account_channels_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_account_channels_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_account_channels_resp(Prop) when is_list(Prop) ->
    case query_account_channels_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_ACCOUNT_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for accounts channels query resp"}
    end;
query_account_channels_resp(JObj) -> query_account_channels_resp(kz_json:to_proplist(JObj)).

-spec query_account_channels_resp_v(kz_term:api_terms()) -> boolean().
query_account_channels_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, ?QUERY_ACCOUNT_CHANNELS_RESP_VALUES, ?QUERY_ACCOUNT_CHANNELS_RESP_TYPES);
query_account_channels_resp_v(JObj) -> query_account_channels_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_channels_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_channels_req(Prop) when is_list(Prop) ->
    case query_channels_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for channels query req"}
    end;
query_channels_req(JObj) -> query_channels_req(kz_json:to_proplist(JObj)).

-spec query_channels_req_v(kz_term:api_terms()) -> boolean().
query_channels_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_CHANNELS_REQ_HEADERS, ?QUERY_CHANNELS_REQ_VALUES, ?QUERY_CHANNELS_REQ_TYPES);
query_channels_req_v(JObj) -> query_channels_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_channels_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_channels_resp(Prop) when is_list(Prop) ->
    case query_channels_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?QUERY_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for channels query resp"}
    end;
query_channels_resp(JObj) -> query_channels_resp(kz_json:to_proplist(JObj)).

-spec query_channels_resp_v(kz_term:api_terms()) -> boolean().
query_channels_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?QUERY_CHANNELS_RESP_HEADERS, ?QUERY_CHANNELS_RESP_VALUES, ?QUERY_CHANNELS_RESP_TYPES);
query_channels_resp_v(JObj) -> query_channels_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec usurp_control(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_control(Prop) when is_list(Prop) ->
    case usurp_control_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CALL_USURP_CONTROL_HEADERS, ?OPTIONAL_CALL_USURP_CONTROL_HEADERS);
        'false' -> {'error', "Proplist failed validation for usurp_control"}
    end;
usurp_control(JObj) -> usurp_control(kz_json:to_proplist(JObj)).

-spec usurp_control_v(kz_term:api_terms()) -> boolean().
usurp_control_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_USURP_CONTROL_HEADERS, ?CALL_USURP_CONTROL_VALUES, ?CALL_USURP_CONTROL_TYPES);
usurp_control_v(JObj) -> usurp_control_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec usurp_publisher(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_publisher(Prop) when is_list(Prop) ->
    case usurp_publisher_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS);
        'false' -> {'error', "Proplist failed validation for usurp_publisher"}
    end;
usurp_publisher(JObj) -> usurp_publisher(kz_json:to_proplist(JObj)).

-spec usurp_publisher_v(kz_term:api_terms()) -> boolean().
usurp_publisher_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?PUBLISHER_USURP_CONTROL_VALUES, ?PUBLISHER_USURP_CONTROL_TYPES);
usurp_publisher_v(JObj) -> usurp_publisher_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, CallId).

bind_q(Q, [Event|T], CallId) ->
    _ = kz_amqp_util:bind_q_to_callevt(Q, ?CALL_EVENT_ROUTING_KEY(Event, CallId)),
    bind_q(Q, T, CallId);
bind_q(_Q, [], _CallId) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, CallId).

unbind_q(Q, [Event|T], CallId) ->
    _ = kz_amqp_util:unbind_q_from_callevt(Q, ?CALL_EVENT_ROUTING_KEY(Event, CallId)),
    unbind_q(Q, T, CallId);
unbind_q(_Q, [], _CallId) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callevt_exchange(),
    kz_amqp_util:callmgr_exchange().

-spec publish_event(kz_term:api_terms()) -> 'ok'.
publish_event(Event) -> publish_event(Event, ?DEFAULT_CONTENT_TYPE).

-spec publish_event(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_event(Event, ContentType) when is_list(Event) ->
    CallId = find_event_call_id(Event),
    EventName = props:get_value(<<"Event-Name">>, Event),
    {'ok', Payload} = kz_api:prepare_api_payload(Event
                                                ,?CALL_EVENT_VALUES
                                                ,[{'formatter', fun event/1}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    Props = [{'headers', [{<<"call-id">>, binary, CallId}]}],
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY(EventName, CallId), Payload, ContentType, Props);
publish_event(Event, ContentType) ->
    publish_event(kz_json:to_proplist(Event), ContentType).

-spec find_event_call_id(kz_term:proplist()) -> kz_term:api_ne_binary().
find_event_call_id(Event) ->
    Keys = case props:is_true(<<"Channel-Is-Loopback">>, Event, 'false') of
               'true' -> [<<"Call-ID">>, <<"Unique-ID">>];
               'false' -> [<<"Origination-Call-ID">>, <<"Call-ID">>, <<"Unique-ID">>]
           end,
    props:get_first_defined(Keys, Event).

-spec publish_channel_status_req(kz_term:api_terms()) -> 'ok'.
publish_channel_status_req(API) ->
    publish_channel_status_req(API, kz_api:call_id(API)).

-spec publish_channel_status_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_req(API, CallId) ->
    publish_channel_status_req(API, CallId, ?DEFAULT_CONTENT_TYPE).

-spec publish_channel_status_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_req(Req, CallId, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CHANNEL_STATUS_REQ_VALUES, fun channel_status_req/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', CallId), Payload, ContentType).

-spec publish_channel_status_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_channel_status_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?CHANNEL_STATUS_RESP_VALUES, fun channel_status_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_auth_id_req(kz_term:api_terms()) -> 'ok'.
publish_query_auth_id_req(API) ->
    publish_query_auth_id_req(API, auth_id(API)).

-spec publish_query_auth_id_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_req(API, AuthId) ->
    publish_query_auth_id_req(API, AuthId, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_auth_id_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_req(Req, AuthId, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?QUERY_AUTH_ID_REQ_VALUES, fun query_auth_id_req/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', AuthId), Payload, ContentType).

-spec publish_query_auth_id_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_auth_id_resp(RespQ, JObj) ->
    publish_query_auth_id_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_auth_id_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?QUERY_AUTH_ID_RESP_VALUES, fun query_auth_id_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_user_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_user_channels_req(API) ->
    publish_query_user_channels_req(API
                                   ,username(API)
                                   ,realm(API)
                                   ,?DEFAULT_CONTENT_TYPE
                                   ).

-spec publish_query_user_channels_req(kz_term:api_terms(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_user_channels_req(Req, 'undefined', 'undefined', ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?QUERY_USER_CHANNELS_REQ_VALUES, fun query_user_channels_req/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', <<>>), Payload, ContentType);
publish_query_user_channels_req(Req, 'undefined', Realm, ContentType) ->
    Username = first_username(Req),
    publish_query_user_channels_req(Req, Username, Realm, ContentType);
publish_query_user_channels_req(Req, Username, Realm, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?QUERY_USER_CHANNELS_REQ_VALUES, fun query_user_channels_req/1),
    User = <<Username/binary, ":", Realm/binary>>,
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', User), Payload, ContentType).

-spec publish_query_user_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_user_channels_resp(RespQ, JObj) ->
    publish_query_user_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_user_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_user_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?QUERY_USER_CHANNELS_RESP_VALUES, fun query_user_channels_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_account_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_account_channels_req(API) ->
    publish_query_account_channels_req(API
                                      ,kz_api:account_id(API)
                                      ,?DEFAULT_CONTENT_TYPE
                                      ).

-spec publish_query_account_channels_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_account_channels_req(Req, AccountId, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?QUERY_ACCOUNT_CHANNELS_REQ_VALUES, fun query_account_channels_req/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', AccountId), Payload, ContentType).

-spec publish_query_account_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_account_channels_resp(RespQ, JObj) ->
    publish_query_account_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_account_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_account_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?QUERY_ACCOUNT_CHANNELS_RESP_VALUES, fun query_account_channels_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_channels_req(ApiProps) -> publish_query_channels_req(ApiProps, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_channels_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_channels_req(ApiProps, ContentType) when is_list(ApiProps) ->
    {'ok', Payload} = kz_api:prepare_api_payload(ApiProps, ?QUERY_CHANNELS_REQ_VALUES, fun query_channels_req/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', <<"channels">>), Payload, ContentType);
publish_query_channels_req(JObj, ContentType) ->
    publish_query_channels_req(kz_json:to_proplist(JObj), ContentType).

-spec publish_query_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_channels_resp(RespQ, ApiProps) -> publish_query_channels_resp(RespQ, ApiProps, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_channels_resp(RespQ, ApiProps, ContentType) when is_list(ApiProps) ->
    {'ok', Payload} = kz_api:prepare_api_payload(ApiProps, ?QUERY_CHANNELS_RESP_VALUES, fun query_channels_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType);
publish_query_channels_resp(RespQ, JObj, ContentType) ->
    publish_query_channels_resp(RespQ, kz_json:to_proplist(JObj), ContentType).

-spec publish_usurp_control(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_usurp_control(CallId, JObj) ->
    publish_usurp_control(CallId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_usurp_control(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_usurp_control(CallId, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?CALL_USURP_CONTROL_VALUES, fun usurp_control/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('usurp_control', CallId), Payload, ContentType).

-spec publish_usurp_publisher(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_usurp_publisher(CallId, JObj) ->
    publish_usurp_publisher(CallId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_usurp_publisher(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_usurp_publisher(CallId, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?PUBLISHER_USURP_CONTROL_VALUES, fun usurp_publisher/1),
    kz_amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('usurp_publisher', CallId), Payload, ContentType).

-spec get_status(kz_term:api_terms()) -> kz_term:ne_binary().
get_status(API) when is_list(API) -> props:get_value(<<"Status">>, API);
get_status(API) -> kz_json:get_value(<<"Status">>, API).

-spec first_username(kz_term:api_terms()) -> kz_term:ne_binary().
first_username(Props) when is_list(Props) ->
    [U|_] = props:get_value(<<"Usernames">>, Props),
    U;
first_username(JObj) ->
    [U|_] = kz_json:get_value(<<"Usernames">>, JObj),
    U.

-spec event_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
event_routing_key(EventName, CallId) ->
    ?CALL_EVENT_ROUTING_KEY(EventName, CallId).

-spec auth_id(kz_term:api_terms()) -> kz_term:ne_binary().
auth_id(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Auth-ID">>, Props);
auth_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-ID">>, JObj).

-spec username(kz_term:api_terms()) -> kz_term:ne_binary().
username(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Username">>, Props);
username(JObj) ->
    kz_json:get_ne_binary_value(<<"Username">>, JObj).

-spec realm(kz_term:api_terms()) -> kz_term:ne_binary().
realm(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Realm">>, Props);
realm(JObj) ->
    kz_json:get_ne_binary_value(<<"Realm">>, JObj).
