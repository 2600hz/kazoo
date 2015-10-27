%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_offnet_resource).

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
         ,custom_sip_headers/1, custom_sip_headers/2
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
         ,presence_id/1, presence_id/2
         ,resource_type/1, resource_type/2
         ,ringback/1, ringback/2
         ,timeout/1, timeout/2
         ,to_did/1, to_did/2

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

-include_lib("whistle/src/wh_json.hrl").
-include_lib("whistle/include/wh_api.hrl").
-include_lib("whistle/include/wh_amqp.hrl").

-define(REQ_TYPE(JObj), ?JSON_WRAPPER(_)=JObj).
-define(RESP_TYPE(JObj), ?JSON_WRAPPER(_)=JObj).

-opaque req()  :: wh_json:object().
-opaque resp() :: wh_json:object().

-export_type([req/0
              ,resp/0
             ]).

-define(KEY_ACCOUNT_ID, <<"Account-ID">>).
-define(KEY_CALL_ID, <<"Call-ID">>).
-define(KEY_CONTROL_QUEUE, <<"Control-Queue">>).
-define(KEY_FLAGS, <<"Flags">>).
-define(KEY_FORCE_OUTBOUND, <<"Force-Outbound">>).
-define(KEY_HUNT_ACCOUNT_ID, <<"Hunt-Account-ID">>).
-define(KEY_OUTBOUND_CALL_ID, <<"Outbound-Call-ID">>).
-define(KEY_OUTBOUND_CALLER_ID_NAME, <<"Outbound-Caller-ID-Name">>).
-define(KEY_OUTBOUND_CALLER_ID_NUMBER, <<"Outbound-Caller-ID-Number">>).
-define(KEY_E_CALLER_ID_NAME, <<"Emergency-Caller-ID-Name">>).
-define(KEY_E_CALLER_ID_NUMBER, <<"Emergency-Caller-ID-Number">>).
-define(KEY_OUTBOUND_CALLEE_ID_NAME, <<"Outbound-Callee-ID-Name">>).
-define(KEY_OUTBOUND_CALLEE_ID_NUMBER, <<"Outbound-Callee-ID-Number">>).
-define(KEY_RESOURCE_TYPE, <<"Resource-Type">>).
-define(KEY_TO_DID, <<"To-DID">>).
-define(KEY_CCVS, <<"Custom-Channel-Vars">>).
-define(KEY_CSHS, <<"Custom-SIP-Headers">>).
-define(KEY_TIMEOUT, <<"Timeout">>).
-define(KEY_IGNORE_EARLY_MEDIA, <<"Ignore-Early-Media">>).
-define(KEY_MEDIA, <<"Media">>).
-define(KEY_MESSAGE_ID, <<"Message-ID">>).
-define(KEY_HOLD_MEDIA, <<"Hold-Media">>).
-define(KEY_PRESENCE_ID, <<"Presence-ID">>).
-define(KEY_RINGBACK, <<"Ringback">>).
-define(KEY_FAX_IDENTITY_NUMBER, <<"Fax-Identity-Number">>).
-define(KEY_FAX_IDENTITY_NAME, <<"Fax-Identity-Name">>).
-define(KEY_B_LEG_EVENTS, <<"B-Leg-Events">>).
-define(KEY_FROM_URI_REALM, <<"From-URI-Realm">>).
-define(KEY_ACCOUNT_REALM, <<"Account-Realm">>).
-define(KEY_FORMAT_FROM_URI, <<"Format-From-URI">>).
-define(KEY_BODY, <<"Body">>).
-define(KEY_BYPASS_E164, <<"Bypass-E164">>).

-define(RESOURCE_TYPE_AUDIO, <<"audio">>).
-define(RESOURCE_TYPE_ORIGINATE, <<"originate">>).
-define(RESOURCE_TYPE_SMS, <<"sms">>).
-define(RESOURCE_TYPE_VIDEO, <<"video">>).

%% Offnet Resource Request
-define(OFFNET_RESOURCE_REQ_HEADERS, [<<"Application-Name">>
                                      ,?KEY_RESOURCE_TYPE
                                      ,?KEY_TO_DID
                                     ]).
-define(OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS
        ,[?KEY_ACCOUNT_ID
          ,?KEY_ACCOUNT_REALM
          ,<<"Application-Data">>
          ,?KEY_B_LEG_EVENTS
          ,?KEY_BODY
          ,?KEY_BYPASS_E164
          ,?KEY_CALL_ID
          ,?KEY_CALL_ID
          ,?KEY_CONTROL_QUEUE
          ,?KEY_CCVS
          ,?KEY_CSHS
          ,?KEY_E_CALLER_ID_NAME
          ,?KEY_E_CALLER_ID_NUMBER
          ,<<"Enable-T38-Fax">>
          ,<<"Enable-T38-Fax-Request">>
          ,<<"Enable-T38-Gateway">>
          ,<<"Enable-T38-Passthrough">>
          ,?KEY_FAX_IDENTITY_NAME
          ,?KEY_FAX_IDENTITY_NUMBER
          ,<<"Fax-Timezone">>
          ,?KEY_FLAGS
          ,<<"Force-Fax">>
          ,?KEY_FORCE_OUTBOUND
          ,?KEY_FORMAT_FROM_URI
          ,?KEY_FROM_URI_REALM
          ,<<"Group-ID">>
          ,?KEY_HOLD_MEDIA
          ,?KEY_HUNT_ACCOUNT_ID
          ,?KEY_IGNORE_EARLY_MEDIA
          ,<<"Inception">>
          ,?KEY_MEDIA
          ,?KEY_MESSAGE_ID
          ,<<"Mode">>
          ,?KEY_OUTBOUND_CALL_ID
          ,?KEY_OUTBOUND_CALLER_ID_NAME
          ,?KEY_OUTBOUND_CALLER_ID_NUMBER
          ,?KEY_PRESENCE_ID
          ,?KEY_RINGBACK
          ,?KEY_TIMEOUT
         ]).
-define(OFFNET_RESOURCE_REQ_VALUES
        ,[{<<"Event-Category">>, <<"resource">>}
          ,{<<"Event-Name">>, <<"offnet_req">>}
          ,{?KEY_RESOURCE_TYPE, [?RESOURCE_TYPE_AUDIO, ?RESOURCE_TYPE_VIDEO, ?RESOURCE_TYPE_ORIGINATE, ?RESOURCE_TYPE_SMS]}
          ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>
                                     ,<<"fax">>, <<"eavesdrop">>, ?RESOURCE_TYPE_SMS
                                    ]}
          ,{?KEY_MEDIA, [<<"process">>, <<"bypass">>, <<"auto">>]}
          %% Eavesdrop
          ,{<<"Mode">>, [<<"full">>     % talk to both sides
                         ,<<"listen">>  % hear both sides - default
                         ,<<"whisper">> % talk to one side
                        ]}
         ]).
-define(OFFNET_RESOURCE_REQ_TYPES
        ,[{?KEY_ACCOUNT_ID, fun is_binary/1}
          ,{?KEY_B_LEG_EVENTS, fun wapi_dialplan:b_leg_events_v/1}
          ,{?KEY_CALL_ID, fun is_binary/1}
          ,{?KEY_CONTROL_QUEUE, fun is_binary/1}
          ,{?KEY_CCVS, fun wh_json:is_json_object/1}
          ,{?KEY_CSHS, fun wh_json:is_json_object/1}
          ,{<<"Enable-T38-Gateway">>, fun is_binary/1}
          ,{?KEY_FLAGS, fun is_list/1}
          ,{<<"Force-Fax">>, fun wh_util:is_boolean/1}
          ,{?KEY_FORCE_OUTBOUND, fun wh_util:is_boolean/1}
          ,{?KEY_TO_DID, fun is_binary/1}
          ,{?KEY_BYPASS_E164, fun wh_util:is_boolean/1}
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

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req(req() | api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_req"}
    end;
req(?REQ_TYPE(JObj)) -> req(wh_json:to_proplist(JObj));
req(JObj) -> req(wh_json:to_proplist(JObj)).

-spec req_v(req() | api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES);
req_v(?REQ_TYPE(JObj)) -> req_v(wh_json:to_proplist(JObj));
req_v(JObj) -> req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp(resp() | api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_resp"}
    end;
resp(?RESP_TYPE(JObj)) -> resp(wh_json:to_proplist(JObj));
resp(JObj) -> resp(wh_json:to_proplist(JObj)).

-spec resp_v(resp() | api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OFFNET_RESOURCE_RESP_VALUES, ?OFFNET_RESOURCE_RESP_TYPES);
resp_v(?RESP_TYPE(JObj)) -> resp_v(wh_json:to_proplist(JObj));
resp_v(JObj) -> resp_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:bind_q_to_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:resource_exchange().

-spec publish_req(req() | api_terms()) -> 'ok'.
-spec publish_req(req() | api_terms(), ne_binary()) -> 'ok'.
publish_req(?REQ_TYPE(JObj)) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE);
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

publish_req(?REQ_TYPE(JObj), _ContentType) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE);
publish_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?OFFNET_RESOURCE_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:offnet_resource_publish(Payload, ContentType).

-spec publish_resp(ne_binary(), resp() | api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), resp() | api_terms(), ne_binary()) -> 'ok'.

publish_resp(TargetQ, JObj) ->
    publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

publish_resp(TargetQ, ?RESP_TYPE(JObj), ContentType) ->
    publish_resp(TargetQ, JObj, ContentType);
publish_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?OFFNET_RESOURCE_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec force_outbound(req()) -> boolean().
-spec force_outbound(req(), Default) -> boolean() | Default.
force_outbound(Req) ->
    force_outbound(Req, 'false').
force_outbound(?REQ_TYPE(JObj), Default) ->
    wh_json:is_true(?KEY_FORCE_OUTBOUND, JObj, Default).

-spec resource_type(req()) -> ne_binary().
-spec resource_type(req(), Default) -> ne_binary() | Default.
resource_type(Req) ->
    resource_type(Req, ?RESOURCE_TYPE_AUDIO).
resource_type(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_RESOURCE_TYPE, JObj, Default).

-spec account_id(req()) -> api_binary().
-spec account_id(req(), Default) -> ne_binary() | Default.
account_id(Req) ->
    account_id(Req, 'undefined').
account_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_ACCOUNT_ID, JObj, Default).

-spec hunt_account_id(req()) -> api_binary().
-spec hunt_account_id(req(), Default) -> ne_binary() | Default.
hunt_account_id(Req) ->
    hunt_account_id(Req, 'undefined').
hunt_account_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_HUNT_ACCOUNT_ID, JObj, Default).

-spec outbound_call_id(req()) -> api_binary().
-spec outbound_call_id(req(), Default) -> ne_binary() | Default.
outbound_call_id(Req) ->
    outbound_call_id(Req, 'undefined').
outbound_call_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALL_ID, JObj, Default).

-spec outbound_caller_id_number(req()) -> api_binary().
-spec outbound_caller_id_number(req(), Default) -> ne_binary() | Default.
outbound_caller_id_number(Req) ->
    outbound_caller_id_number(Req, 'undefined').
outbound_caller_id_number(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NUMBER, JObj, Default).

-spec outbound_caller_id_name(req()) -> api_binary().
-spec outbound_caller_id_name(req(), Default) -> ne_binary() | Default.
outbound_caller_id_name(Req) ->
    outbound_caller_id_name(Req, 'undefined').
outbound_caller_id_name(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NAME, JObj, Default).

-spec emergency_caller_id_number(req()) -> api_binary().
-spec emergency_caller_id_number(req(), Default) -> ne_binary() | Default.
emergency_caller_id_number(Req) ->
    emergency_caller_id_number(Req, 'undefined').
emergency_caller_id_number(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_E_CALLER_ID_NUMBER, JObj, Default).

-spec emergency_caller_id_name(req()) -> api_binary().
-spec emergency_caller_id_name(req(), Default) -> ne_binary() | Default.
emergency_caller_id_name(Req) ->
    emergency_caller_id_name(Req, 'undefined').
emergency_caller_id_name(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_E_CALLER_ID_NAME, JObj, Default).

-spec to_did(req()) -> api_binary().
-spec to_did(req(), Default) -> ne_binary() | Default.
to_did(Req) ->
    to_did(Req, 'undefined').
to_did(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_TO_DID, JObj, Default).

-spec call_id(req()) -> api_binary().
-spec call_id(req(), Default) -> ne_binary() | Default.
call_id(Req) ->
    call_id(Req, 'undefined').
call_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_CALL_ID, JObj, Default).

-spec control_queue(req()) -> api_binary().
-spec control_queue(req(), Default) -> ne_binary() | Default.
control_queue(Req) ->
    control_queue(Req, 'undefined').
control_queue(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_CONTROL_QUEUE, JObj, Default).

-spec flags(req()) -> ne_binaries().
-spec flags(req(), Default) -> ne_binaries() | Default.
flags(Req) ->
    flags(Req, []).
flags(?REQ_TYPE(JObj), Default) ->
    wh_json:get_list_value(?KEY_FLAGS, JObj, Default).

-spec jobj_to_req(wh_json:object()) -> ?MODULE:req().
jobj_to_req(?JSON_WRAPPER(_)=JObj) ->
    ?REQ_TYPE(JObj).

-spec req_to_jobj(req()) -> wh_json:object().
req_to_jobj(?REQ_TYPE(JObj)) ->
    JObj.

-spec put_callid(req()) -> api_binary().
put_callid(?REQ_TYPE(JObj)) ->
    wh_util:put_callid(JObj).

-spec set_outbound_call_id(req(), ne_binary()) -> req().
set_outbound_call_id(?REQ_TYPE(JObj), CallId) ->
    ?REQ_TYPE(wh_json:insert_value(?KEY_OUTBOUND_CALL_ID, CallId, JObj)).

-spec custom_channel_vars(req()) -> api_object().
-spec custom_channel_vars(req(), Default) -> wh_json:object() | Default.
custom_channel_vars(Req) ->
    custom_channel_vars(Req, 'undefined').
custom_channel_vars(?REQ_TYPE(JObj), Default) ->
    wh_json:get_json_value(?KEY_CCVS, JObj, Default).

-spec custom_sip_headers(req()) -> api_object().
-spec custom_sip_headers(req(), Default) -> wh_json:object() | Default.
custom_sip_headers(Req) ->
    custom_sip_headers(Req, 'undefined').
custom_sip_headers(?REQ_TYPE(JObj), Default) ->
    wh_json:get_json_value(?KEY_CSHS, JObj, Default).

-spec timeout(req()) -> api_integer().
-spec timeout(req(), Default) -> integer() | Default.
timeout(Req) ->
    timeout(Req, 'undefined').
timeout(?REQ_TYPE(JObj), Default) ->
    wh_json:get_integer_value(?KEY_TIMEOUT, JObj, Default).

-spec ignore_early_media(req()) -> api_boolean().
-spec ignore_early_media(req(), Default) -> boolean() | Default.
ignore_early_media(Req) ->
    ignore_early_media(Req, 'undefined').
ignore_early_media(?REQ_TYPE(JObj), Default) ->
    wh_json:is_true(?KEY_IGNORE_EARLY_MEDIA, JObj, Default).

-spec media(req()) -> api_binary().
-spec media(req(), Default) -> ne_binary() | Default.
media(Req) ->
    media(Req, 'undefined').
media(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_MEDIA, JObj, Default).

-spec message_id(req()) -> api_binary().
-spec message_id(req(), Default) -> ne_binary() | Default.
message_id(Req) ->
    message_id(Req, 'undefined').
message_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_MESSAGE_ID, JObj, Default).

-spec hold_media(req()) -> api_binary().
-spec hold_media(req(), Default) -> ne_binary() | Default.
hold_media(Req) ->
    hold_media(Req, 'undefined').
hold_media(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_HOLD_MEDIA, JObj, Default).

-spec presence_id(req()) -> api_binary().
-spec presence_id(req(), Default) -> ne_binary() | Default.
presence_id(Req) ->
    presence_id(Req, 'undefined').
presence_id(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_PRESENCE_ID, JObj, Default).

-spec ringback(req()) -> api_binary().
-spec ringback(req(), Default) -> ne_binary() | Default.
ringback(Req) ->
    ringback(Req, 'undefined').
ringback(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_RINGBACK, JObj, Default).

-spec fax_identity_number(req()) -> api_binary().
-spec fax_identity_number(req(), Default) -> ne_binary() | Default.
fax_identity_number(Req) ->
    fax_identity_number(Req, 'undefined').
fax_identity_number(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_FAX_IDENTITY_NUMBER, JObj, Default).

-spec fax_identity_name(req()) -> api_binary().
-spec fax_identity_name(req(), Default) -> ne_binary() | Default.
fax_identity_name(Req) ->
    fax_identity_name(Req, 'undefined').
fax_identity_name(?REQ_TYPE(JObj), Default) ->
    wh_json:get_binary_value(?KEY_FAX_IDENTITY_NAME, JObj, Default).

-spec outbound_callee_id_number(req()) -> api_binary().
-spec outbound_callee_id_number(req(), Default) -> ne_binary() | Default.
outbound_callee_id_number(Req) ->
    outbound_callee_id_number(Req, 'undefined').
outbound_callee_id_number(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLEE_ID_NUMBER, JObj, Default).

-spec outbound_callee_id_name(req()) -> api_binary().
-spec outbound_callee_id_name(req(), Default) -> ne_binary() | Default.
outbound_callee_id_name(Req) ->
    outbound_callee_id_name(Req, 'undefined').
outbound_callee_id_name(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLEE_ID_NAME, JObj, Default).

-spec b_leg_events(req()) -> api_binaries().
-spec b_leg_events(req(), Default) -> ne_binaries() | Default.
b_leg_events(Req) ->
    b_leg_events(Req, 'undefined').
b_leg_events(?REQ_TYPE(JObj), Default) ->
    wh_json:get_list_value(?KEY_B_LEG_EVENTS, JObj, Default).

-spec from_uri_realm(req()) -> api_binary().
-spec from_uri_realm(req(), Default) -> ne_binary() | Default.
from_uri_realm(Req) ->
    from_uri_realm(Req, 'undefined').
from_uri_realm(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_FROM_URI_REALM, JObj, Default).

-spec account_realm(req()) -> api_binary().
-spec account_realm(req(), Default) -> ne_binary() | Default.
account_realm(Req) ->
    account_realm(Req, 'undefined').
account_realm(?REQ_TYPE(JObj), Default) ->
    wh_json:get_ne_value(?KEY_ACCOUNT_REALM, JObj, Default).

-spec format_from_uri(req()) -> boolean().
-spec format_from_uri(req(), Default) -> boolean() | Default.
format_from_uri(Req) ->
    format_from_uri(Req, 'false').
format_from_uri(?REQ_TYPE(JObj), Default) ->
    wh_json:is_true(?KEY_FORMAT_FROM_URI, JObj, Default).

-spec body(req()) -> api_binary().
-spec body(req(), Default) -> ne_binary() | Default.
body(Req) ->
    body(Req, 'undefined').
body(?REQ_TYPE(JObj), Default) ->
    wh_json:get_value(?KEY_BODY, JObj, Default).

-spec bypass_e164(req()) -> boolean().
-spec bypass_e164(req(), Default) -> boolean() | Default.
bypass_e164(Req) ->
    bypass_e164(Req, 'false').
bypass_e164(?REQ_TYPE(JObj), Default) ->
    wh_json:is_true(?KEY_BYPASS_E164, JObj, Default).

-spec msg_id(req()) -> api_binary().
msg_id(?REQ_TYPE(JObj)) ->
    wh_api:msg_id(JObj).

-spec server_id(req()) -> api_binary().
server_id(?REQ_TYPE(JObj)) ->
    wh_api:server_id(JObj).

-spec delete_keys(req(), ne_binaries()) -> req().
delete_keys(?REQ_TYPE(JObj), Keys) ->
    ?REQ_TYPE(wh_json:delete_keys(Keys, JObj)).

-spec set_values(req(), wh_proplist()) -> req().
set_values(?REQ_TYPE(JObj), Props) ->
    ?REQ_TYPE(wh_json:set_values(Props, JObj)).
