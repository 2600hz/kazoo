%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_offnet_resource).

-export([req/1, req_v/1]).
-export([resp/1, resp_v/1]).
-export([publish_req/1, publish_req/2]).
-export([publish_resp/2, publish_resp/3]).
-export([bind_q/2]).
-export([unbind_q/2]).
-export([declare_exchanges/0]).

-export([account_id/1, account_id/2
        ,account_realm/1, account_realm/2
        ,b_leg_events/1, b_leg_events/2
        ,body/1, body/2
        ,bypass_e164/1, bypass_e164/2
        ,call_id/1, call_id/2
        ,control_queue/1, control_queue/2
        ,custom_channel_vars/1, custom_channel_vars/2
        ,custom_application_vars/1, custom_application_vars/2
        ,requestor_custom_channel_vars/1, requestor_custom_channel_vars/2
        ,custom_sip_headers/1, custom_sip_headers/2
        ,custom_sip_header/2
        ,requestor_custom_sip_headers/1, requestor_custom_sip_headers/2
        ,requestor_custom_sip_header/2
        ,emergency_caller_id_name/1, emergency_caller_id_name/2
        ,emergency_caller_id_number/1, emergency_caller_id_number/2
        ,fax_identity_name/1, fax_identity_name/2
        ,fax_identity_number/1, fax_identity_number/2
        ,flags/1, flags/2
        ,force_outbound/1, force_outbound/2
        ,format_from_uri/1, format_from_uri/2
        ,from_uri_realm/1, from_uri_realm/2
        ,hold_media/1, hold_media/2
        ,hunt_account_id/1, hunt_account_id/2
        ,ignore_early_media/1, ignore_early_media/2
        ,media/1, media/2
        ,message_id/1, message_id/2
        ,outbound_call_id/1, outbound_call_id/2
        ,outbound_callee_id_name/1, outbound_callee_id_name/2
        ,outbound_callee_id_number/1, outbound_callee_id_number/2
        ,outbound_caller_id_name/1, outbound_caller_id_name/2
        ,outbound_caller_id_number/1, outbound_caller_id_number/2
        ,asserted_identity_name/1, asserted_identity_name/2
        ,asserted_identity_number/1, asserted_identity_number/2
        ,asserted_identity_realm/1, asserted_identity_realm/2
        ,presence_id/1, presence_id/2
        ,resource_type/1, resource_type/2
        ,ringback/1, ringback/2
        ,timeout/1, timeout/2
        ,t38_enabled/1, t38_enabled/2
        ,to_did/1, to_did/2
        ,denied_call_restrictions/1, denied_call_restrictions/2
        ,outbound_actions/1, outbound_actions/2

        ,msg_id/1
        ,server_id/1

        ,set_outbound_call_id/2
        ,delete_keys/2
        ,set_values/2
        ]).

%% helpers for working with opaque object
-export([jobj_to_req/1
        ,req_to_jobj/1
        ,put_callid/1
        ]).

-include_lib("kz_amqp_util.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

-export_type([req/0
             ,resp/0
             ]).

-define(REQ_TYPE(JObj), JObj).
-define(RESP_TYPE(JObj), JObj).

-type req()  :: kz_json:object().
-type resp() :: kz_json:object().

%% Offnet Resource Request
-define(OFFNET_RESOURCE_REQ_HEADERS, [?KEY_APPLICATION_NAME
                                     ,?KEY_RESOURCE_TYPE
                                     ,?KEY_TO_DID
                                     ]).
-define(OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS
       ,[?KEY_ACCOUNT_ID
        ,?KEY_ACCOUNT_REALM
        ,?KEY_APPLICATION_DATA
        ,?KEY_BODY
        ,?KEY_BYPASS_E164
        ,?KEY_B_LEG_EVENTS
        ,?KEY_CALL_ID
        ,?KEY_CALL_ID
        ,?KEY_CAVS
        ,?KEY_CCVS
        ,?KEY_CONTROL_QUEUE
        ,?KEY_CSHS
        ,?KEY_ENABLE_T38
        ,?KEY_ENABLE_T38_GATEWAY
        ,?KEY_ENABLE_T38_PASSTHROUGH
        ,?KEY_ENABLE_T38_REQUEST
        ,?KEY_E_CALLER_ID_NAME
        ,?KEY_E_CALLER_ID_NUMBER
        ,?KEY_FAX_IDENTITY_NAME
        ,?KEY_FAX_IDENTITY_NUMBER
        ,?KEY_FAX_TIMEZONE
        ,?KEY_FLAGS
        ,?KEY_FORCE_FAX
        ,?KEY_FORCE_OUTBOUND
        ,?KEY_FORMAT_FROM_URI
        ,?KEY_FROM_URI_REALM
        ,?KEY_GROUP_ID
        ,?KEY_HOLD_MEDIA
        ,?KEY_HUNT_ACCOUNT_ID
        ,?KEY_IGNORE_EARLY_MEDIA
        ,?KEY_INCEPTION
        ,?KEY_MEDIA
        ,?KEY_MESSAGE_ID
        ,?KEY_MODE
        ,?KEY_ORIGINATION_CALL_ID
        ,?KEY_OUTBOUND_CALLER_ID_NAME
        ,?KEY_OUTBOUND_CALLER_ID_NUMBER
        ,?KEY_ASSERTED_IDENTITY_NAME
        ,?KEY_ASSERTED_IDENTITY_NUMBER
        ,?KEY_ASSERTED_IDENTITY_REALM
        ,?KEY_OUTBOUND_CALL_ID
        ,?KEY_PRESENCE_ID
        ,?KEY_REQUESTOR_CCVS
        ,?KEY_REQUESTOR_CSHS
        ,?KEY_RINGBACK
        ,?KEY_T38_ENABLED
        ,?KEY_TIMEOUT
        ,?KEY_DENIED_CALL_RESTRICTIONS
        ,?KEY_OUTBOUND_ACTIONS
        ]).
-define(OFFNET_RESOURCE_REQ_VALUES
       ,[{?KEY_EVENT_CATEGORY, ?CATEGORY_REQ}
        ,{?KEY_EVENT_NAME, ?EVENT_REQ}
        ,{?KEY_RESOURCE_TYPE, [?RESOURCE_TYPE_AUDIO, ?RESOURCE_TYPE_VIDEO, ?RESOURCE_TYPE_ORIGINATE, ?RESOURCE_TYPE_SMS]}
        ,{?KEY_APPLICATION_NAME, [?APPLICATION_BRIDGE
                                 ,?APPLICATION_EAVESDROP
                                 ,?APPLICATION_FAX
                                 ,?APPLICATION_PARK
                                 ,?APPLICATION_SMS
                                 ,?APPLICATION_TRANSFER
                                 ]}
        ,{?KEY_MEDIA, [?MEDIA_PROCESS, ?MEDIA_BYPASS, ?MEDIA_AUTO]}
         %% Eavesdrop
        ,{?KEY_MODE, [?MODE_FULL     % talk to both sides
                     ,?MODE_LISTEN  % hear both sides - default
                     ,?MODE_WHISPER % talk to one side
                     ]}
        ]).
-define(OFFNET_RESOURCE_REQ_TYPES
       ,[{?KEY_ACCOUNT_ID, fun erlang:is_binary/1}
        ,{?KEY_B_LEG_EVENTS, fun kapi_dialplan:b_leg_events_v/1}
        ,{?KEY_CALL_ID, fun erlang:is_binary/1}
        ,{?KEY_CONTROL_QUEUE, fun erlang:is_binary/1}
        ,{?KEY_CCVS, fun kz_json:is_json_object/1}
        ,{?KEY_CAVS, fun kz_json:is_json_object/1}
        ,{?KEY_REQUESTOR_CCVS, fun kz_json:is_json_object/1}
        ,{?KEY_CSHS, fun kz_json:is_json_object/1}
        ,{?KEY_REQUESTOR_CSHS, fun kz_json:is_json_object/1}
        ,{?KEY_ENABLE_T38_GATEWAY, fun erlang:is_binary/1}
        ,{?KEY_FLAGS, fun erlang:is_list/1}
        ,{?KEY_FORCE_FAX, fun kz_term:is_boolean/1}
        ,{?KEY_FORCE_OUTBOUND, fun kz_term:is_boolean/1}
        ,{?KEY_TO_DID, fun kz_term:is_ne_binary/1}
        ,{?KEY_BYPASS_E164, fun kz_term:is_boolean/1}
        ,{?KEY_DENIED_CALL_RESTRICTIONS, fun kz_json:is_json_object/1}
        ,{?KEY_OUTBOUND_ACTIONS, fun kz_json:is_json_object/1}
        ]).

%% Offnet Resource Response
-define(OFFNET_RESOURCE_RESP_HEADERS, [<<"Response-Message">>]).
-define(OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS, [<<"Error-Message">>, <<"Response-Code">>
                                               ,?KEY_CALL_ID, <<"Resource-Response">>
                                               ,?KEY_CONTROL_QUEUE
                                               ]).
-define(OFFNET_RESOURCE_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                     ,{<<"Event-Name">>, <<"offnet_resp">>}
                                     ]).
-define(OFFNET_RESOURCE_RESP_TYPES, []).


%%------------------------------------------------------------------------------
%% @doc Offnet resource request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_req"}
    end;
req(JObj) -> req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES);
req_v(JObj) -> req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Offnet resource request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_resp"}
    end;
resp(JObj) -> resp(kz_json:to_proplist(JObj)).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OFFNET_RESOURCE_RESP_VALUES, ?OFFNET_RESOURCE_RESP_TYPES);
resp_v(JObj) -> resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    kz_amqp_util:unbind_q_from_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:resource_exchange().

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?OFFNET_RESOURCE_REQ_VALUES, fun req/1),
    kz_amqp_util:offnet_resource_publish(Payload, ContentType).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(TargetQ, JObj) ->
    publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?OFFNET_RESOURCE_RESP_VALUES, fun resp/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec force_outbound(req()) -> boolean().
force_outbound(Req) ->
    force_outbound(Req, 'false').

-spec force_outbound(req(), Default) -> boolean() | Default.
force_outbound(?REQ_TYPE(JObj), Default) ->
    kz_json:is_true(?KEY_FORCE_OUTBOUND, JObj, Default).

-spec resource_type(req()) -> kz_term:ne_binary().
resource_type(Req) ->
    resource_type(Req, ?RESOURCE_TYPE_AUDIO).

-spec resource_type(req(), Default) -> kz_term:ne_binary() | Default.
resource_type(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_RESOURCE_TYPE, JObj, Default).

-spec account_id(req()) -> kz_term:api_binary().
account_id(Req) ->
    account_id(Req, 'undefined').

-spec account_id(req(), Default) -> kz_term:ne_binary() | Default.
account_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_ACCOUNT_ID, JObj, Default).

-spec hunt_account_id(req()) -> kz_term:api_binary().
hunt_account_id(Req) ->
    hunt_account_id(Req, 'undefined').

-spec hunt_account_id(req(), Default) -> kz_term:ne_binary() | Default.
hunt_account_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_HUNT_ACCOUNT_ID, JObj, Default).

-spec outbound_call_id(req()) -> kz_term:api_binary().
outbound_call_id(Req) ->
    outbound_call_id(Req, 'undefined').

-spec outbound_call_id(req(), Default) -> kz_term:ne_binary() | Default.
outbound_call_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_OUTBOUND_CALL_ID, JObj, Default).

-spec outbound_caller_id_number(req()) -> kz_term:api_binary().
outbound_caller_id_number(Req) ->
    outbound_caller_id_number(Req, 'undefined').

-spec outbound_caller_id_number(req(), Default) -> kz_term:ne_binary() | Default.
outbound_caller_id_number(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NUMBER, JObj, Default).

-spec outbound_caller_id_name(req()) -> kz_term:api_binary().
outbound_caller_id_name(Req) ->
    outbound_caller_id_name(Req, 'undefined').

-spec outbound_caller_id_name(req(), Default) -> kz_term:ne_binary() | Default.
outbound_caller_id_name(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NAME, JObj, Default).

-spec asserted_identity_number(req()) -> kz_term:api_binary().
asserted_identity_number(Req) ->
    asserted_identity_number(Req, 'undefined').

-spec asserted_identity_number(req(), Default) -> kz_term:ne_binary() | Default.
asserted_identity_number(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_ASSERTED_IDENTITY_NUMBER, JObj, Default).

-spec asserted_identity_name(req()) -> kz_term:api_binary().
asserted_identity_name(Req) ->
    asserted_identity_name(Req, 'undefined').

-spec asserted_identity_name(req(), Default) -> kz_term:ne_binary() | Default.
asserted_identity_name(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_ASSERTED_IDENTITY_NAME, JObj, Default).

-spec asserted_identity_realm(req()) -> kz_term:api_binary().
asserted_identity_realm(Req) ->
    asserted_identity_realm(Req, 'undefined').

-spec asserted_identity_realm(req(), Default) -> kz_term:ne_binary() | Default.
asserted_identity_realm(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_ASSERTED_IDENTITY_REALM, JObj, Default).

-spec emergency_caller_id_number(req()) -> kz_term:api_binary().
emergency_caller_id_number(Req) ->
    emergency_caller_id_number(Req, 'undefined').

-spec emergency_caller_id_number(req(), Default) -> kz_term:ne_binary() | Default.
emergency_caller_id_number(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_E_CALLER_ID_NUMBER, JObj, Default).

-spec emergency_caller_id_name(req()) -> kz_term:api_binary().
emergency_caller_id_name(Req) ->
    emergency_caller_id_name(Req, 'undefined').

-spec emergency_caller_id_name(req(), Default) -> kz_term:ne_binary() | Default.
emergency_caller_id_name(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_E_CALLER_ID_NAME, JObj, Default).

-spec to_did(req()) -> kz_term:api_binary().
to_did(Req) ->
    to_did(Req, 'undefined').

-spec to_did(req(), Default) -> kz_term:ne_binary() | Default.
to_did(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_TO_DID, JObj, Default).

-spec denied_call_restrictions(req()) -> kz_json:object().
denied_call_restrictions(Req) ->
    denied_call_restrictions(Req, kz_json:new()).

-spec denied_call_restrictions(req(), Default) -> kz_json:object() | Default.
denied_call_restrictions(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_DENIED_CALL_RESTRICTIONS, JObj, Default).

-spec call_id(req()) -> kz_term:api_binary().
call_id(Req) ->
    call_id(Req, 'undefined').

-spec call_id(req(), Default) -> kz_term:ne_binary() | Default.
call_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_CALL_ID, JObj, Default).

-spec control_queue(req()) -> kz_term:api_binary().
control_queue(Req) ->
    control_queue(Req, 'undefined').

-spec control_queue(req(), Default) -> kz_term:ne_binary() | Default.
control_queue(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_CONTROL_QUEUE, JObj, Default).

-spec flags(req()) -> kz_term:ne_binaries().
flags(Req) ->
    flags(Req, []).

-spec flags(req(), Default) -> kz_term:ne_binaries() | Default.
flags(?REQ_TYPE(JObj), Default) ->
    kz_json:get_list_value(?KEY_FLAGS, JObj, Default).

-spec jobj_to_req(kz_json:object()) -> req().
jobj_to_req(JObj) -> ?REQ_TYPE(JObj).

-spec req_to_jobj(req()) -> kz_json:object().
req_to_jobj(?REQ_TYPE(JObj)) -> JObj.

-spec put_callid(req()) -> 'ok'.
put_callid(?REQ_TYPE(JObj)) ->
    kz_util:put_callid(JObj).

-spec set_outbound_call_id(req(), kz_term:ne_binary()) -> req().
set_outbound_call_id(?REQ_TYPE(JObj), CallId) ->
    ?REQ_TYPE(kz_json:insert_value(?KEY_OUTBOUND_CALL_ID, CallId, JObj)).

-spec custom_channel_vars(req()) -> kz_term:api_object().
custom_channel_vars(Req) ->
    custom_channel_vars(Req, 'undefined').

-spec custom_channel_vars(req(), Default) -> kz_json:object() | Default.
custom_channel_vars(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_CCVS, JObj, Default).

-spec custom_application_vars(req()) -> kz_term:api_object().
custom_application_vars(Req) ->
    custom_application_vars(Req, 'undefined').

-spec custom_application_vars(req(), Default) -> kz_json:object() | Default.
custom_application_vars(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_CAVS, JObj, Default).

-spec requestor_custom_channel_vars(req()) -> kz_term:api_object().
requestor_custom_channel_vars(Req) ->
    requestor_custom_channel_vars(Req, 'undefined').

-spec requestor_custom_channel_vars(req(), Default) -> kz_json:object() | Default.
requestor_custom_channel_vars(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_REQUESTOR_CCVS, JObj, Default).

-spec custom_sip_headers(req()) -> kz_term:api_object().
custom_sip_headers(Req) ->
    custom_sip_headers(Req, 'undefined').

-spec custom_sip_headers(req(), Default) -> kz_json:object() | Default.
custom_sip_headers(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_CSHS, JObj, Default).

-spec custom_sip_header(req(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
custom_sip_header(Req, Header) ->
    SipHeaders = custom_sip_headers(Req, kz_json:new()),
    kz_json:get_value(Header, SipHeaders).

-spec requestor_custom_sip_headers(req()) -> kz_term:api_object().
requestor_custom_sip_headers(Req) ->
    requestor_custom_sip_headers(Req, 'undefined').

-spec requestor_custom_sip_headers(req(), Default) -> kz_json:object() | Default.
requestor_custom_sip_headers(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_REQUESTOR_CSHS, JObj, Default).

-spec requestor_custom_sip_header(req(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
requestor_custom_sip_header(Req, Header) ->
    SipHeaders = requestor_custom_sip_headers(Req, kz_json:new()),
    kz_json:get_value(Header, SipHeaders).

-spec timeout(req()) -> kz_term:api_integer().
timeout(Req) ->
    timeout(Req, 'undefined').

-spec timeout(req(), Default) -> integer() | Default.
timeout(?REQ_TYPE(JObj), Default) ->
    kz_json:get_integer_value(?KEY_TIMEOUT, JObj, Default).

-spec ignore_early_media(req()) -> kz_term:api_boolean().
ignore_early_media(Req) ->
    ignore_early_media(Req, 'undefined').

-spec ignore_early_media(req(), Default) -> boolean() | Default.
ignore_early_media(?REQ_TYPE(JObj), Default) ->
    kz_json:is_true(?KEY_IGNORE_EARLY_MEDIA, JObj, Default).

-spec media(req()) -> kz_term:api_binary().
media(Req) ->
    media(Req, 'undefined').

-spec media(req(), Default) -> kz_term:ne_binary() | Default.
media(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_MEDIA, JObj, Default).

-spec message_id(req()) -> kz_term:api_binary().
message_id(Req) ->
    message_id(Req, 'undefined').

-spec message_id(req(), Default) -> kz_term:ne_binary() | Default.
message_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_MESSAGE_ID, JObj, Default).

-spec hold_media(req()) -> kz_term:api_binary().
hold_media(Req) ->
    hold_media(Req, 'undefined').

-spec hold_media(req(), Default) -> kz_term:ne_binary() | Default.
hold_media(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_HOLD_MEDIA, JObj, Default).

-spec presence_id(req()) -> kz_term:api_binary().
presence_id(Req) ->
    presence_id(Req, 'undefined').

-spec presence_id(req(), Default) -> kz_term:ne_binary() | Default.
presence_id(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_PRESENCE_ID, JObj, Default).

-spec ringback(req()) -> kz_term:api_binary().
ringback(Req) ->
    ringback(Req, 'undefined').

-spec ringback(req(), Default) -> kz_term:ne_binary() | Default.
ringback(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_RINGBACK, JObj, Default).

-spec fax_identity_number(req()) -> kz_term:api_binary().
fax_identity_number(Req) ->
    fax_identity_number(Req, 'undefined').

-spec fax_identity_number(req(), Default) -> kz_term:ne_binary() | Default.
fax_identity_number(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_FAX_IDENTITY_NUMBER, JObj, Default).

-spec fax_identity_name(req()) -> kz_term:api_binary().
fax_identity_name(Req) ->
    fax_identity_name(Req, 'undefined').

-spec fax_identity_name(req(), Default) -> kz_term:ne_binary() | Default.
fax_identity_name(?REQ_TYPE(JObj), Default) ->
    kz_json:get_binary_value(?KEY_FAX_IDENTITY_NAME, JObj, Default).

-spec outbound_callee_id_number(req()) -> kz_term:api_binary().
outbound_callee_id_number(Req) ->
    outbound_callee_id_number(Req, 'undefined').

-spec outbound_callee_id_number(req(), Default) -> kz_term:ne_binary() | Default.
outbound_callee_id_number(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_OUTBOUND_CALLEE_ID_NUMBER, JObj, Default).

-spec outbound_callee_id_name(req()) -> kz_term:api_binary().
outbound_callee_id_name(Req) ->
    outbound_callee_id_name(Req, 'undefined').

-spec outbound_callee_id_name(req(), Default) -> kz_term:ne_binary() | Default.
outbound_callee_id_name(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_OUTBOUND_CALLEE_ID_NAME, JObj, Default).

-spec b_leg_events(req()) -> kz_term:api_binaries().
b_leg_events(Req) ->
    b_leg_events(Req, 'undefined').

-spec b_leg_events(req(), Default) -> kz_term:ne_binaries() | Default.
b_leg_events(?REQ_TYPE(JObj), Default) ->
    kz_json:get_list_value(?KEY_B_LEG_EVENTS, JObj, Default).

-spec from_uri_realm(req()) -> kz_term:api_binary().
from_uri_realm(Req) ->
    from_uri_realm(Req, 'undefined').

-spec from_uri_realm(req(), Default) -> kz_term:ne_binary() | Default.
from_uri_realm(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_FROM_URI_REALM, JObj, Default).

-spec account_realm(req()) -> kz_term:api_binary().
account_realm(Req) ->
    account_realm(Req, 'undefined').

-spec account_realm(req(), Default) -> kz_term:ne_binary() | Default.
account_realm(?REQ_TYPE(JObj), Default) ->
    kz_json:get_ne_value(?KEY_ACCOUNT_REALM, JObj, Default).

-spec format_from_uri(req()) -> boolean().
format_from_uri(Req) ->
    format_from_uri(Req, 'false').

-spec format_from_uri(req(), Default) -> boolean() | Default.
format_from_uri(?REQ_TYPE(JObj), Default) ->
    kz_json:is_true(?KEY_FORMAT_FROM_URI, JObj, Default).

-spec body(req()) -> kz_term:api_binary().
body(Req) ->
    body(Req, 'undefined').

-spec body(req(), Default) -> kz_term:ne_binary() | Default.
body(?REQ_TYPE(JObj), Default) ->
    kz_json:get_value(?KEY_BODY, JObj, Default).

-spec bypass_e164(req()) -> boolean().
bypass_e164(Req) ->
    bypass_e164(Req, 'false').

-spec bypass_e164(req(), Default) -> boolean() | Default.
bypass_e164(?REQ_TYPE(JObj), Default) ->
    kz_json:is_true(?KEY_BYPASS_E164, JObj, Default).

-spec t38_enabled(req()) -> boolean().
t38_enabled(Req) ->
    t38_enabled(Req, 'false').

-spec t38_enabled(req(), Default) -> boolean() | Default.
t38_enabled(?REQ_TYPE(JObj), Default) ->
    kz_json:is_true(?KEY_T38_ENABLED, JObj, Default).

-spec msg_id(req()) -> kz_term:api_binary().
msg_id(?REQ_TYPE(JObj)) ->
    kz_api:msg_id(JObj).

-spec server_id(req()) -> kz_term:api_binary().
server_id(?REQ_TYPE(JObj)) ->
    kz_api:server_id(JObj).

-spec delete_keys(req(), kz_term:ne_binaries()) -> req().
delete_keys(?REQ_TYPE(JObj), Keys) ->
    ?REQ_TYPE(kz_json:delete_keys(Keys, JObj)).

-spec set_values(req(), kz_term:proplist()) -> req().
set_values(?REQ_TYPE(JObj), Props) ->
    ?REQ_TYPE(kz_json:set_values(Props, JObj)).

-spec outbound_actions(req()) -> kz_term:api_object().
outbound_actions(Req) ->
    outbound_actions(Req, 'undefined').

-spec outbound_actions(req(), Default) -> kz_json:object() | Default.
outbound_actions(?REQ_TYPE(JObj), Default) ->
    kz_json:get_json_value(?KEY_OUTBOUND_ACTIONS, JObj, Default).
