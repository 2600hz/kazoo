%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_resource).

-export([originate_req/1, originate_req_v/1
        ,originate_resp/1, originate_resp_v/1
        ,originate_ready/1, originate_ready_v/1
        ,originate_execute/1, originate_execute_v/1
        ,originate_started/1, originate_started_v/1
        ,originate_uuid/1, originate_uuid_v/1
        ,eavesdrop_req/1, eavesdrop_req_v/1
        ,eavesdrop_resp/1, eavesdrop_resp_v/1
        ]).

-export([is_valid_mode/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_originate_req/1, publish_originate_req/2
        ,publish_originate_resp/2, publish_originate_resp/3
        ,publish_originate_started/2, publish_originate_started/3
        ,publish_originate_uuid/2, publish_originate_uuid/3
        ,publish_eavesdrop_req/1, publish_eavesdrop_req/2
        ,publish_eavesdrop_resp/2, publish_eavesdrop_resp/3
        ]).

-include("kz_amqp_util.hrl").

%% Eavesdrop: If you set a Group ID, the Call-ID is ignored and "all" is used instead
-define(EAVESDROP_VALID_MODES, [<<"listen">>   % hear both sides - default
                               ,<<"whisper">> % talk to one side
                               ,<<"full">>    % talk to both sides
                               ]).
-define(EAVESDROP_MODE, {<<"Eavesdrop-Mode">>, ?EAVESDROP_VALID_MODES}).

-define(KEY_EAVESDROP_REQ, <<"eavesdrop.resource.req">>). %% corresponds to eavesdrop_req/1 api call

-define(EAVESDROP_REQ_HEADERS, [<<"Account-ID">>, <<"Endpoint-ID">>]).
-define(OPTIONAL_EAVESDROP_REQ_HEADERS, [<<"Eavesdrop-Group-ID">>, <<"Eavesdrop-Mode">>
                                        ,<<"Eavesdrop-Call-ID">>
                                             | ?OPTIONAL_ORIGINATE_REQ_HEADERS
                                        ]).
-define(EAVESDROP_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
                              ,{<<"Event-Name">>, <<"eavesdrop_req">>}
                              ,?EAVESDROP_MODE
                              ]).
-define(EAVESDROP_REQ_TYPES, []).

-define(EAVESDROP_RESP_HEADERS, [<<"Status">>]).
-define(OPTIONAL_EAVESDROP_RESP_HEADERS, [<<"Eavesdropper-Call-ID">>
                                         ,<<"Error-Msg">>
                                         ]).
-define(EAVESDROP_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"eavesdrop_resp">>}
                               ,{<<"Status">>, [<<"started">>, <<"error">>]}
                               ]).
-define(EAVESDROP_RESP_TYPES, []).

%% corresponds to originate_req/1 api call
-define(KEY_RESOURCE_REQ, <<"originate.resource.req">>).

-define(ORIGINATE_REQ_HEADERS, [<<"Endpoints">>, <<"Application-Name">>]).
-define(OPTIONAL_ORIGINATE_REQ_HEADERS, [<<"Application-Data">>
                                        ,<<"Custom-Application-Vars">>
                                        ,<<"Custom-Channel-Vars">>
                                        ,<<"Existing-Call-ID">> % If set, use this node, otherwise ignore
                                        ,<<"Export-Custom-Channel-Vars">>
                                        ,<<"Originate-Immediate">>
                                        ,<<"Origination-Call-ID">>
                                        ,<<"Outbound-Call-ID">>

                                             %% Eavesdrop
                                        ,<<"Eavesdrop-Call-ID">>
                                        ,<<"Eavesdrop-Group-ID">>
                                        ,<<"Eavesdrop-Mode">>

                                        ,<<"Fax-Identity-Name">>
                                        ,<<"Fax-Identity-Number">>
                                        ,<<"Fax-Timezone">>
                                        ,<<"Intercept-Unbridged-Only">>
                                        ,<<"Loopback-Bowout">>
                                        ,<<"Simplify-Loopback">> %% loopback_bowout flag
                                        ,<<"Start-Control-Process">>
                                             | kapi_dialplan:optional_bridge_req_headers()
                                        ]).
-define(ORIGINATE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
                              ,{<<"Event-Name">>, <<"originate_req">>}
                              ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                              ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                              ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>
                                                        ,<<"fax">>, <<"eavesdrop">>
                                                        ]}
                               %% Eavesdrop
                              ,?EAVESDROP_MODE
                              ]).
-define(ORIGINATE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                             ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                             ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                             ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                             ,{<<"Continue-On-Fail">>, fun kz_term:is_boolean/1}
                             ,{<<"Simplify-Bowout">>, fun kz_term:is_boolean/1}
                             ]).

%% Originate Endpoints
-define(ORIGINATE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_ORIGINATE_REQ_ENDPOINT_HEADERS, kapi_dialplan:optional_bridge_req_endpoint_headers()).
-define(ORIGINATE_REQ_ENDPOINT_VALUES, [{<<"Endpoint-Type">>, [<<"sip">>, <<"freetdm">>]}
                                       ]).
-define(ORIGINATE_REQ_ENDPOINT_TYPES, [{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                                      ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                      ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                                      ,{<<"Endpoint-Options">>, fun kz_json:is_json_object/1}
                                      ,{<<"Ignore-Early-Media">>, fun kz_term:is_boolean/1}
                                      ,{<<"Bypass-Media">>, fun kz_term:is_boolean/1}
                                      ]).

%% Originate Resp
-define(ORIGINATE_RESP_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_ORIGINATE_RESP_HEADERS, [<<"Channel-Call-State">> | kapi_call:optional_call_event_headers()]).
-define(ORIGINATE_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_resp">>}
                               ]).
-define(ORIGINATE_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                              ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                              ]).

%% Originate Started
-define(ORIGINATE_STARTED_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_ORIGINATE_STARTED_HEADERS, [<<"Channel-Call-State">> | kapi_call:optional_call_event_headers()]).
-define(ORIGINATE_STARTED_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                  ,{<<"Event-Name">>, <<"originate_started">>}
                                  ]).
-define(ORIGINATE_STARTED_TYPES, [{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                 ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                                 ]).

%% Originate UUID
-define(ORIGINATE_UUID_HEADERS, [<<"Outbound-Call-ID">>]).
-define(OPTIONAL_ORIGINATE_UUID_HEADERS, [<<"Outbound-Call-Control-Queue">>]).
-define(ORIGINATE_UUID_VALUES, [{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_uuid">>}
                               ]).
-define(ORIGINATE_UUID_TYPES, []).

-spec originate_ready(kz_term:api_terms()) -> api_formatter_return().
originate_ready(API) ->
    kapi_dialplan:originate_ready(API).

-spec originate_ready_v(kz_term:api_terms()) -> boolean().
originate_ready_v(API) ->
    kapi_dialplan:originate_ready_v(API).

-spec originate_execute(kz_term:api_terms()) -> api_formatter_return().
originate_execute(API) ->
    kapi_dialplan:originate_execute(API).

-spec originate_execute_v(kz_term:api_terms()) -> boolean().
originate_execute_v(API) ->
    kapi_dialplan:originate_execute_v(API).

%%------------------------------------------------------------------------------
%% @doc Resource Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_req(kz_term:api_terms()) -> api_formatter_return().
originate_req(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = originate_req_endpoint_headers(EP),
               kz_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              originate_req_endpoint_v(EP)],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case originate_req_v(Prop1) of
        'true' -> kz_api:build_message(Prop1, ?ORIGINATE_REQ_HEADERS, ?OPTIONAL_ORIGINATE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate_req"}
    end;
originate_req(JObj) ->
    originate_req(kz_json:to_proplist(JObj)).

-spec originate_req_v(kz_term:api_terms()) -> boolean().
originate_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ORIGINATE_REQ_HEADERS, ?ORIGINATE_REQ_VALUES, ?ORIGINATE_REQ_TYPES);
originate_req_v(JObj) ->
    originate_req_v(kz_json:to_proplist(JObj)).

-spec originate_req_endpoint_headers(kz_term:api_terms()) -> api_formatter_return().
originate_req_endpoint_headers(Prop) when is_list(Prop) ->
    kz_api:build_message_specific_headers(Prop, ?ORIGINATE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_ORIGINATE_REQ_ENDPOINT_HEADERS);
originate_req_endpoint_headers(JObj) ->
    originate_req_endpoint_headers(kz_json:to_proplist(JObj)).

-spec originate_req_endpoint_v(kz_term:api_terms()) -> boolean().
originate_req_endpoint_v(Prop) when is_list(Prop) ->
    kz_api:validate_message(Prop, ?ORIGINATE_REQ_ENDPOINT_HEADERS, ?ORIGINATE_REQ_ENDPOINT_VALUES, ?ORIGINATE_REQ_ENDPOINT_TYPES);
originate_req_endpoint_v(JObj) ->
    originate_req_endpoint_v(kz_json:to_proplist(JObj)).


%%------------------------------------------------------------------------------
%% @doc Resource Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_resp(kz_term:api_terms()) -> api_formatter_return().
originate_resp(Prop) when is_list(Prop) ->
    case originate_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ORIGINATE_RESP_HEADERS, ?OPTIONAL_ORIGINATE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate response"}
    end;
originate_resp(JObj) ->
    originate_resp(kz_json:to_proplist(JObj)).

-spec originate_resp_v(kz_term:api_terms()) -> boolean().
originate_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ORIGINATE_RESP_HEADERS, ?ORIGINATE_RESP_VALUES, ?ORIGINATE_RESP_TYPES);
originate_resp_v(JObj) ->
    originate_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Resource Request started.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_started(kz_term:api_terms()) -> api_formatter_return().
originate_started(Prop) when is_list(Prop) ->
    case originate_started_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ORIGINATE_STARTED_HEADERS, ?OPTIONAL_ORIGINATE_STARTED_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate started"}
    end;
originate_started(JObj) ->
    originate_started(kz_json:to_proplist(JObj)).

-spec originate_started_v(kz_term:api_terms()) -> boolean().
originate_started_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ORIGINATE_STARTED_HEADERS, ?ORIGINATE_STARTED_VALUES, ?ORIGINATE_STARTED_TYPES);
originate_started_v(JObj) ->
    originate_started_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Resource Request UUID.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_uuid(kz_term:api_terms()) -> api_formatter_return().
originate_uuid(Prop) when is_list(Prop) ->
    case originate_uuid_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ORIGINATE_UUID_HEADERS, ?OPTIONAL_ORIGINATE_UUID_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate uuid"}
    end;
originate_uuid(JObj) ->
    originate_uuid(kz_json:to_proplist(JObj)).

-spec originate_uuid_v(kz_term:api_terms()) -> boolean().
originate_uuid_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ORIGINATE_UUID_HEADERS, ?ORIGINATE_UUID_VALUES, ?ORIGINATE_UUID_TYPES);
originate_uuid_v(JObj) ->
    originate_uuid_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Eavesdrop Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec eavesdrop_req(kz_term:api_terms()) -> api_formatter_return().
eavesdrop_req(Prop) when is_list(Prop) ->
    case eavesdrop_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?EAVESDROP_REQ_HEADERS, ?OPTIONAL_EAVESDROP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for eavesdrop request"}
    end;
eavesdrop_req(JObj) ->
    eavesdrop_req(kz_json:to_proplist(JObj)).

-spec eavesdrop_req_v(kz_term:api_terms()) -> boolean().
eavesdrop_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?EAVESDROP_REQ_HEADERS, ?EAVESDROP_REQ_VALUES, ?EAVESDROP_REQ_TYPES);
eavesdrop_req_v(JObj) ->
    eavesdrop_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Eavesdrop Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec eavesdrop_resp(kz_term:api_terms()) -> api_formatter_return().
eavesdrop_resp(Prop) when is_list(Prop) ->
    case eavesdrop_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?EAVESDROP_RESP_HEADERS, ?OPTIONAL_EAVESDROP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for eavesdrop response"}
    end;
eavesdrop_resp(JObj) ->
    eavesdrop_resp(kz_json:to_proplist(JObj)).

-spec eavesdrop_resp_v(kz_term:api_terms()) -> boolean().
eavesdrop_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?EAVESDROP_RESP_HEADERS, ?EAVESDROP_RESP_VALUES, ?EAVESDROP_RESP_TYPES);
eavesdrop_resp_v(JObj) ->
    eavesdrop_resp_v(kz_json:to_proplist(JObj)).

-spec is_valid_mode(kz_term:ne_binary()) -> boolean().
is_valid_mode(M) ->
    lists:member(M, ?EAVESDROP_VALID_MODES).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Prop) ->
    bind_q(Queue, Prop, props:get_value('restrict_to', Prop)).

bind_q(Queue, _Prop, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, ?KEY_RESOURCE_REQ),
    kz_amqp_util:bind_q_to_callmgr(Queue, ?KEY_EAVESDROP_REQ);
bind_q(Queue, Prop, ['originate'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, ?KEY_RESOURCE_REQ),
    bind_q(Queue, Prop, T);
bind_q(Queue, Prop, ['eavesdrop'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, ?KEY_EAVESDROP_REQ),
    bind_q(Queue, Prop, T);
bind_q(Queue, Prop, [_|T]) ->
    bind_q(Queue, Prop, T);
bind_q(_, _, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Prop) ->
    unbind_q(Queue, Prop, props:get_value('restrict_to', Prop)).

unbind_q(Queue, _Prop, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, ?KEY_RESOURCE_REQ),
    kz_amqp_util:unbind_q_from_callmgr(Queue, ?KEY_EAVESDROP_REQ);
unbind_q(Queue, Prop, ['originate'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, ?KEY_RESOURCE_REQ),
    unbind_q(Queue, Prop, T);
unbind_q(Queue, Prop, ['eavesdrop'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, ?KEY_EAVESDROP_REQ),
    unbind_q(Queue, Prop, T);
unbind_q(Queue, Prop, [_|T]) ->
    unbind_q(Queue, Prop, T);
unbind_q(_, _, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

-spec publish_originate_req(kz_term:api_terms()) -> 'ok'.
publish_originate_req(JObj) ->
    publish_originate_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?ORIGINATE_REQ_VALUES, fun originate_req/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RESOURCE_REQ, [{'mandatory', 'true'}]).

-spec publish_originate_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_resp(TargetQ, JObj) ->
    publish_originate_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?ORIGINATE_RESP_VALUES, fun originate_resp/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_originate_started(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_started(TargetQ, JObj) ->
    publish_originate_started(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_started(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_started(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?ORIGINATE_STARTED_VALUES, fun originate_started/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_originate_uuid(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_uuid(TargetQ, JObj) ->
    publish_originate_uuid(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_uuid(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_uuid(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?ORIGINATE_UUID_VALUES, fun originate_uuid/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_eavesdrop_req(kz_term:api_terms()) -> 'ok'.
publish_eavesdrop_req(JObj) ->
    publish_eavesdrop_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_eavesdrop_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_eavesdrop_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?EAVESDROP_REQ_VALUES, fun eavesdrop_req/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_EAVESDROP_REQ).

-spec publish_eavesdrop_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_eavesdrop_resp(TargetQ, JObj) ->
    publish_eavesdrop_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_eavesdrop_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_eavesdrop_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?EAVESDROP_RESP_VALUES, fun eavesdrop_resp/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).
