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

-export([force_outbound/1, force_outbound/2
         ,resource_type/1, resource_type/2
         ,account_id/1, account_id/2
         ,hunt_account_id/1, hunt_account_id/2
         ,outbound_call_id/1, outbound_call_id/2
         ,outbound_caller_id_number/1, outbound_caller_id_number/2
         ,outbound_caller_id_name/1, outbound_caller_id_name/2
         ,call_id/1, call_id/2
         ,control_queue/1, control_queue/2
         ,to_did/1, to_did/2
         ,flags/1, flags/2
        ]).

-include_lib("whistle/include/wh_api.hrl").
-include_lib("whistle/include/wh_amqp.hrl").

-type req() :: wh_json:object().
-type resp() :: wh_json:object().

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
-define(KEY_RESOURCE_TYPE, <<"Resource-Type">>).
-define(KEY_TO_DID, <<"To-DID">>).

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
          ,<<"Account-Realm">>
          ,<<"Application-Data">>
          ,<<"B-Leg-Events">>
          ,<<"Body">>
          ,<<"Bypass-E164">>
          ,?KEY_CALL_ID
          ,?KEY_CALL_ID
          ,?KEY_CONTROL_QUEUE
          ,<<"Custom-Channel-Vars">>
          ,<<"Custom-SIP-Headers">>
          ,<<"Emergency-Caller-ID-Name">>
          ,<<"Emergency-Caller-ID-Number">>
          ,<<"Enable-T38-Fax">>
          ,<<"Enable-T38-Fax-Request">>
          ,<<"Enable-T38-Gateway">>
          ,<<"Enable-T38-Passthrough">>
          ,<<"Fax-Identity-Name">>
          ,<<"Fax-Identity-Number">>
          ,<<"Fax-Timezone">>
          ,?KEY_FLAGS
          ,<<"Force-Fax">>
          ,?KEY_FORCE_OUTBOUND
          ,<<"Format-From-URI">>
          ,<<"From-URI-Realm">>
          ,<<"Group-ID">>
          ,<<"Hold-Media">>
          ,?KEY_HUNT_ACCOUNT_ID
          ,<<"Ignore-Early-Media">>
          ,<<"Inception">>
          ,<<"Media">>
          ,<<"Message-ID">>
          ,<<"Mode">>
          ,?KEY_OUTBOUND_CALL_ID
          ,?KEY_OUTBOUND_CALLER_ID_NAME
          ,?KEY_OUTBOUND_CALLER_ID_NUMBER
          ,<<"Presence-ID">>
          ,<<"Ringback">>
          ,<<"Timeout">>
         ]).
-define(OFFNET_RESOURCE_REQ_VALUES
        ,[{<<"Event-Category">>, <<"resource">>}
          ,{<<"Event-Name">>, <<"offnet_req">>}
          ,{?KEY_RESOURCE_TYPE, [?RESOURCE_TYPE_AUDIO, ?RESOURCE_TYPE_VIDEO, ?RESOURCE_TYPE_ORIGINATE, ?RESOURCE_TYPE_SMS]}
          ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>
                                     ,<<"fax">>, <<"eavesdrop">>, ?RESOURCE_TYPE_SMS
                                    ]}
          ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
          %% Eavesdrop
          ,{<<"Mode">>, [<<"full">>     % talk to both sides
                         ,<<"listen">>  % hear both sides - default
                         ,<<"whisper">> % talk to one side
                        ]}
         ]).
-define(OFFNET_RESOURCE_REQ_TYPES
        ,[{?KEY_ACCOUNT_ID, fun is_binary/1}
          ,{<<"B-Leg-Events">>, fun wapi_dialplan:b_leg_events_v/1}
          ,{?KEY_CALL_ID, fun is_binary/1}
          ,{?KEY_CONTROL_QUEUE, fun is_binary/1}
          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
          ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
          ,{<<"Enable-T38-Gateway">>, fun is_binary/1}
          ,{?KEY_FLAGS, fun is_list/1}
          ,{<<"Force-Fax">>, fun wh_util:is_boolean/1}
          ,{?KEY_FORCE_OUTBOUND, fun wh_util:is_boolean/1}
          ,{?KEY_TO_DID, fun is_binary/1}
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
-spec req(api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_req"}
    end;
req(JObj) -> req(wh_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES);
req_v(JObj) -> req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp(api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for offnet_resource_resp"}
    end;
resp(JObj) -> resp(wh_json:to_proplist(JObj)).

-spec resp_v(api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OFFNET_RESOURCE_RESP_VALUES, ?OFFNET_RESOURCE_RESP_TYPES);
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

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?OFFNET_RESOURCE_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:offnet_resource_publish(Payload, ContentType).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(TargetQ, JObj) -> publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?OFFNET_RESOURCE_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec force_outbound(req()) -> boolean().
-spec force_outbound(req(), Default) -> boolean() | Default.
force_outbound(JObj) ->
    force_outbound(JObj, 'false').
force_outbound(JObj, Default) ->
    wh_json:is_true(?KEY_FORCE_OUTBOUND, JObj, Default).

-spec resource_type(req()) -> ne_binary().
-spec resource_type(req(), Default) -> ne_binary() | Default.
resource_type(JObj) ->
    resource_type(JObj, ?RESOURCE_TYPE_AUDIO).
resource_type(JObj, Default) ->
    wh_json:get_ne_value(?KEY_RESOURCE_TYPE, JObj, Default).

-spec account_id(req()) -> api_binary().
-spec account_id(req(), Default) -> ne_binary() | Default.
account_id(JObj) ->
    account_id(JObj, 'undefined').
account_id(JObj, Default) ->
    wh_json:get_ne_value(?KEY_ACCOUNT_ID, JObj, Default).

-spec hunt_account_id(req()) -> api_binary().
-spec hunt_account_id(req(), Default) -> ne_binary() | Default.
hunt_account_id(JObj) ->
    hunt_account_id(JObj, 'undefined').
hunt_account_id(JObj, Default) ->
    wh_json:get_ne_value(?KEY_HUNT_ACCOUNT_ID, JObj, Default).

-spec outbound_call_id(req()) -> api_binary().
-spec outbound_call_id(req(), Default) -> ne_binary() | Default.
outbound_call_id(JObj) ->
    outbound_call_id(JObj, 'undefined').
outbound_call_id(JObj, Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALL_ID, JObj, Default).

-spec outbound_caller_id_number(req()) -> api_binary().
-spec outbound_caller_id_number(req(), Default) -> ne_binary() | Default.
outbound_caller_id_number(JObj) ->
    outbound_caller_id_number(JObj, 'undefined').
outbound_caller_id_number(JObj, Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NUMBER, JObj, Default).

-spec outbound_caller_id_name(req()) -> api_binary().
-spec outbound_caller_id_name(req(), Default) -> ne_binary() | Default.
outbound_caller_id_name(JObj) ->
    outbound_caller_id_name(JObj, 'undefined').
outbound_caller_id_name(JObj, Default) ->
    wh_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NAME, JObj, Default).

-spec to_did(req()) -> api_binary().
-spec to_did(req(), Default) -> ne_binary() | Default.
to_did(JObj) ->
    to_did(JObj, 'undefined').
to_did(JObj, Default) ->
    wh_json:get_ne_value(?KEY_TO_DID, JObj, Default).

-spec call_id(req()) -> api_binary().
-spec call_id(req(), Default) -> ne_binary() | Default.
call_id(JObj) ->
    call_id(JObj, 'undefined').
call_id(JObj, Default) ->
    wh_json:get_ne_value(?KEY_CALL_ID, JObj, Default).

-spec control_queue(req()) -> api_binary().
-spec control_queue(req(), Default) -> ne_binary() | Default.
control_queue(JObj) ->
    control_queue(JObj, 'undefined').
control_queue(JObj, Default) ->
    wh_json:get_ne_value(?KEY_CONTROL_QUEUE, JObj, Default).

-spec flags(req()) -> ne_binaries().
-spec flags(req(), Default) -> ne_binaries() | Default.
flags(JObj) ->
    flags(JObj, []).
flags(JObj, Default) ->
    wh_json:get_list_value(?KEY_FLAGS, JObj, Default).
