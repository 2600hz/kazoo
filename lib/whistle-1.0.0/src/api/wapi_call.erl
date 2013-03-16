%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Call-related messages, like switch events, status requests, etc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_call).

-export([optional_call_event_headers/0]).

-export([event/1, event_v/1]).
-export([channel_status_req/1, channel_status_req_v/1]).
-export([channel_status_resp/1, channel_status_resp_v/1]).
-export([call_status_req/1, call_status_req_v/1]).
-export([call_status_resp/1, call_status_resp_v/1]).
-export([query_auth_id_req/1, query_auth_id_req_v/1]).
-export([query_auth_id_resp/1, query_auth_id_resp_v/1]).
-export([cdr/1, cdr_v/1]).
-export([callid_update/1, callid_update_v/1]).
-export([control_transfer/1, control_transfer_v/1]).
-export([controller_queue/1, controller_queue_v/1]).
-export([usurp_control/1, usurp_control_v/1]).
-export([usurp_publisher/1, usurp_publisher_v/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_event/2, publish_event/3]).
-export([publish_channel_status_req/1 ,publish_channel_status_req/2, publish_channel_status_req/3]).
-export([publish_channel_status_resp/2, publish_channel_status_resp/3]).
-export([publish_call_status_req/1 ,publish_call_status_req/2, publish_call_status_req/3]).
-export([publish_call_status_resp/2, publish_call_status_resp/3]).
-export([publish_query_auth_id_req/1 ,publish_query_auth_id_req/2, publish_query_auth_id_req/3]).
-export([publish_query_auth_id_resp/2, publish_query_auth_id_resp/3]).
-export([publish_cdr/2, publish_cdr/3]).
-export([publish_callid_update/2, publish_callid_update/3]).
-export([publish_control_transfer/2, publish_control_transfer/3]).
-export([publish_controller_queue/2, publish_controller_queue/3]).
-export([publish_usurp_control/2, publish_usurp_control/3]).
-export([publish_usurp_publisher/2, publish_usurp_publisher/3]).

-export([get_status/1]).

-include_lib("whistle/include/wh_api.hrl").

%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>).

%% Call Events
-define(CALL_EVENT_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>
                                          ,<<"Custom-Channel-Vars">>, <<"Timestamp">>, <<"Channel-State">>
                                          ,<<"Call-Direction">>, <<"Transfer-History">>
                                          ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>
                                          ,<<"Other-Leg-Caller-ID-Number">>, <<"Other-Leg-Destination-Number">>
                                          ,<<"Other-Leg-Unique-ID">> %% BRIDGE
                                          ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                          ,<<"Terminator">>, <<"Disposition">>
                                          ,<<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
                                          ,<<"Raw-Application-Name">>, <<"Raw-Application-Data">>
                                          ,<<"Length">>, <<"Silence-Terminated">> %% Record-related
                                          ,<<"Channel-Call-State">>
                                          ,<<"Fax-Success">>, <<"Fax-Result-Code">>
                                          ,<<"Fax-Result-Text">>, <<"Fax-ECM-Used">>
                                          ,<<"Fax-Transferred-Pages">>, <<"Fax-Total-Pages">>
                                          ,<<"Fax-Bad-Rows">>, <<"Fax-Transfer-Rate">>
                                          ,<<"Switch-Hostname">>, <<"Group-ID">>
                                          ,<<"Control-Queue">>, <<"Channel-Moving">>
                                          ,<<"Conference-Name">>, <<"Conference-Config">>
                                     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Channel Status Request
-define(CHANNEL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CHANNEL_STATUS_REQ_HEADERS, []).
-define(CHANNEL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"channel_status_req">>}
                                   ]).
-define(CHANNEL_STATUS_REQ_TYPES, []).

%% Channel Status Response
-define(CHANNEL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CHANNEL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>
                                                   ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                              ]).
-define(CHANNEL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                     ,{<<"Event-Name">>, <<"channel_status_resp">>}
                                     ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                    ]).
-define(CHANNEL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Call Status Request
-define(CALL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_STATUS_REQ_HEADERS, []).
-define(CALL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                 ,{<<"Event-Name">>, <<"call_status_req">>}
                                ]).
-define(CALL_STATUS_REQ_TYPES, []).

%% Call Status Response
-define(CALL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CALL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>, <<"Switch-Hostname">>
                                                ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                                ,<<"Destination-Number">>, <<"Other-Leg-Unique-ID">>
                                                ,<<"Other-Leg-Caller-ID-Name">>, <<"Other-Leg-Caller-ID-Number">>
                                                ,<<"Other-Leg-Destination-Number">>, <<"Presence-ID">>
                                           ]).
-define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"call_status_resp">>}
                                  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                 ]).
-define(CALL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Query Auth ID Req
-define(QUERY_AUTH_ID_REQ_HEADERS, [<<"Auth-ID">>]).
-define(OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS, []).
-define(QUERY_AUTH_ID_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"query_auth_id_req">>}
                                  ]).
-define(QUERY_AUTH_ID_REQ_TYPES, []).

%% Query Auth ID Resp
-define(QUERY_AUTH_ID_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_AUTH_ID_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"query_auth_id_resp">>}
                                   ]).
-define(QUERY_AUTH_ID_RESP_TYPES, []).

%% Call CDR
-define(CALL_CDR_HEADERS, [ <<"Call-ID">>]).
-define(OPTIONAL_CALL_CDR_HEADERS, [<<"Hangup-Cause">>, <<"Handling-Server-Name">>, <<"Custom-Channel-Vars">>
                                        ,<<"Remote-SDP">>, <<"Local-SDP">>, <<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>, <<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                        ,<<"User-Agent">>, <<"Caller-ID-Type">>, <<"Other-Leg-Call-ID">>
                                        ,<<"Timestamp">>, <<"Request">>
                                        ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
                                        ,<<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
                                        ,<<"Digits-Dialed">>
                                   ]).
-define(CALL_CDR_VALUES, [{<<"Event-Category">>, <<"call_detail">>}
                          ,{<<"Event-Name">>, <<"cdr">>}
                          ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
                          ,{<<"Caller-ID-Type">>, [<<"pid">>, <<"rpid">>, <<"from">>]}
                         ]).
-define(CALL_CDR_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Call ID Update
-define(CALL_ID_UPDATE_HEADERS, [<<"Call-ID">>, <<"Replaces-Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_CALL_ID_UPDATE_HEADERS, []).
-define(CALL_ID_UPDATE_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                ,{<<"Event-Name">>, <<"call_id_update">>}
                                 ]).
-define(CALL_ID_UPDATE_TYPES, []).

%% Call Control Transfer
-define(CALL_CONTROL_TRANSFER_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_CONTROL_TRANSFER_HEADERS, []).
-define(CALL_CONTROL_TRANSFER_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                       ,{<<"Event-Name">>, <<"control_transfer">>}
                                 ]).
-define(CALL_CONTROL_TRANSFER_TYPES, []).

%% Call Usurp Control
-define(CALL_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Control-Queue">>, <<"Controller-Queue">>]).
-define(OPTIONAL_CALL_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(CALL_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"usurp_control">>}
                                   ]).
-define(CALL_USURP_CONTROL_TYPES, []).

%% Usurp Call Event Publisher
-define(PUBLISHER_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Reference">>]).
-define(OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(PUBLISHER_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"usurp_publisher">>}
                                        ]).
-define(PUBLISHER_USURP_CONTROL_TYPES, []).

%% Controller Queue Update
-define(CONTROLLER_QUEUE_HEADERS, [<<"Call-ID">>, <<"Controller-Queue">>]).
-define(OPTIONAL_CONTROLLER_QUEUE_HEADERS, []).
-define(CONTROLLER_QUEUE_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"controller_queue">>}
                                 ]).
-define(CONTROLLER_QUEUE_TYPES, []).

-spec optional_call_event_headers() -> ne_binaries().
optional_call_event_headers() ->
    ?OPTIONAL_CALL_EVENT_HEADERS.

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec event(api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
        false -> {error, "Proplist failed validation for call_event"}
    end;
event(JObj) ->
    event(wh_json:to_proplist(JObj)).

-spec event_v(api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES);
event_v(JObj) ->
    event_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a channel
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_req(Prop) when is_list(Prop) ->
    case channel_status_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?OPTIONAL_CHANNEL_STATUS_REQ_HEADERS);
        false -> {error, "Proplist failed validation for channel status req"}
    end;
channel_status_req(JObj) ->
    channel_status_req(wh_json:to_proplist(JObj)).

-spec channel_status_req_v(api_terms()) -> boolean().
channel_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?CHANNEL_STATUS_REQ_VALUES, ?CHANNEL_STATUS_REQ_TYPES);
channel_status_req_v(JObj) ->
    channel_status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a channel, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_resp(Prop) when is_list(Prop) ->
    case channel_status_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?OPTIONAL_CHANNEL_STATUS_RESP_HEADERS);
        false -> {error, "Proplist failed validation for channel status resp"}
    end;
channel_status_resp(JObj) ->
    channel_status_resp(wh_json:to_proplist(JObj)).

-spec channel_status_resp_v(api_terms()) -> boolean().
channel_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?CHANNEL_STATUS_RESP_VALUES, ?CHANNEL_STATUS_RESP_TYPES);
channel_status_resp_v(JObj) ->
    channel_status_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_req(Prop) when is_list(Prop) ->
    case call_status_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
        false -> {error, "Proplist failed validation for call status req"}
    end;
call_status_req(JObj) ->
    call_status_req(wh_json:to_proplist(JObj)).

-spec call_status_req_v(api_terms()) -> boolean().
call_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES);
call_status_req_v(JObj) ->
    call_status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_auth_id_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_req(Prop) when is_list(Prop) ->
    case query_auth_id_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS);
        false -> {error, "Proplist failed validation for auth id query req"}
    end;
query_auth_id_req(JObj) ->
    query_auth_id_req(wh_json:to_proplist(JObj)).

-spec query_auth_id_req_v(api_terms()) -> boolean().
query_auth_id_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?QUERY_AUTH_ID_REQ_VALUES, ?QUERY_AUTH_ID_REQ_TYPES);
query_auth_id_req_v(JObj) ->
    query_auth_id_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_auth_id_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_resp(Prop) when is_list(Prop) ->
    case query_auth_id_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS);
        false -> {error, "Proplist failed validation for auth id query resp"}
    end;
query_auth_id_resp(JObj) ->
    query_auth_id_resp(wh_json:to_proplist(JObj)).

-spec query_auth_id_resp_v(api_terms()) -> boolean().
query_auth_id_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?QUERY_AUTH_ID_RESP_VALUES, ?QUERY_AUTH_ID_RESP_TYPES);
query_auth_id_resp_v(JObj) ->
    query_auth_id_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_resp(Prop) when is_list(Prop) ->
    case call_status_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
        false -> {error, "Proplist failed validation for call status resp"}
    end;
call_status_resp(JObj) ->
    call_status_resp(wh_json:to_proplist(JObj)).

-spec call_status_resp_v(api_terms()) -> boolean().
call_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES);
call_status_resp_v(JObj) ->
    call_status_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a CDR for a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec cdr(api_terms()) -> {'ok', iolist()} | {'error', string()}.
cdr(Prop) when is_list(Prop) ->
    case cdr_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_CDR_HEADERS, ?OPTIONAL_CALL_CDR_HEADERS);
        false -> {error, "Proplist failed validation for call_cdr"}
    end;
cdr(JObj) ->
    cdr(wh_json:to_proplist(JObj)).

-spec cdr_v(api_terms()) -> boolean().
cdr_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_CDR_HEADERS, ?CALL_CDR_VALUES, ?CALL_CDR_TYPES);
cdr_v(JObj) ->
    cdr_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec callid_update(api_terms()) -> {'ok', iolist()} | {'error', string()}.
callid_update(Prop) when is_list(Prop) ->
    case callid_update_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_ID_UPDATE_HEADERS, ?OPTIONAL_CALL_ID_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for callid_update"}
    end;
callid_update(JObj) ->
    callid_update(wh_json:to_proplist(JObj)).

-spec callid_update_v(api_terms()) -> boolean().
callid_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_ID_UPDATE_HEADERS, ?CALL_ID_UPDATE_VALUES, ?CALL_ID_UPDATE_TYPES);
callid_update_v(JObj) ->
    callid_update_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec control_transfer(api_terms()) -> {'ok', iolist()} | {'error', string()}.
control_transfer(Prop) when is_list(Prop) ->
    case control_transfer_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_CONTROL_TRANSFER_HEADERS, ?OPTIONAL_CALL_CONTROL_TRANSFER_HEADERS);
        false -> {error, "Proplist failed validation for control_transfer"}
    end;
control_transfer(JObj) ->
    control_transfer(wh_json:to_proplist(JObj)).

-spec control_transfer_v(api_terms()) -> boolean().
control_transfer_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_CONTROL_TRANSFER_HEADERS, ?CALL_CONTROL_TRANSFER_VALUES, ?CALL_CONTROL_TRANSFER_TYPES);
control_transfer_v(JObj) ->
    control_transfer_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec usurp_control(api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_control(Prop) when is_list(Prop) ->
    case usurp_control_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_USURP_CONTROL_HEADERS, ?OPTIONAL_CALL_USURP_CONTROL_HEADERS);
        false -> {error, "Proplist failed validation for usurp_control"}
    end;
usurp_control(JObj) ->
    usurp_control(wh_json:to_proplist(JObj)).

-spec usurp_control_v(api_terms()) -> boolean().
usurp_control_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_USURP_CONTROL_HEADERS, ?CALL_USURP_CONTROL_VALUES, ?CALL_USURP_CONTROL_TYPES);
usurp_control_v(JObj) ->
    usurp_control_v(wh_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec usurp_publisher(api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_publisher(Prop) when is_list(Prop) ->
    case usurp_publisher_v(Prop) of
        true -> wh_api:build_message(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS);
        false -> {error, "Proplist failed validation for usurp_publisher"}
    end;
usurp_publisher(JObj) ->
    usurp_publisher(wh_json:to_proplist(JObj)).

-spec usurp_publisher_v(api_terms()) -> boolean().
usurp_publisher_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?PUBLISHER_USURP_CONTROL_VALUES, ?PUBLISHER_USURP_CONTROL_TYPES);
usurp_publisher_v(JObj) ->
    usurp_publisher_v(wh_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec controller_queue(api_terms()) -> {'ok', iolist()} | {'error', string()}.
controller_queue(Prop) when is_list(Prop) ->
    case controller_queue_v(Prop) of
        true -> wh_api:build_message(Prop, ?CONTROLLER_QUEUE_HEADERS, ?OPTIONAL_CONTROLLER_QUEUE_HEADERS);
        false -> {error, "Proplist failed validation for controller_queue"}
    end;
controller_queue(JObj) ->
    controller_queue(wh_json:to_proplist(JObj)).

-spec controller_queue_v(api_terms()) -> boolean().
controller_queue_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONTROLLER_QUEUE_HEADERS, ?CONTROLLER_QUEUE_VALUES, ?CONTROLLER_QUEUE_TYPES);
controller_queue_v(JObj) ->
    controller_queue_v(wh_json:to_proplist(JObj)).

-spec bind_q(binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallID = props:get_value(callid, Props, <<"*">>),
    amqp_util:callevt_exchange(),
    amqp_util:callmgr_exchange(),
    bind_q(Queue, props:get_value(restrict_to, Props), CallID).

bind_q(Q, undefined, CallID) ->
    ok = amqp_util:bind_q_to_callevt(Q, CallID),
    ok = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    ok = amqp_util:bind_q_to_callevt(Q, CallID, publisher_usurp);

bind_q(Q, [events|T], CallID) ->
    _ = amqp_util:bind_q_to_callevt(Q, CallID),
    bind_q(Q, T, CallID);
bind_q(Q, [cdr|T], CallID) ->
    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    bind_q(Q, T, CallID);
bind_q(Q, [status_req|T], CallID) ->
    ok = amqp_util:bind_q_to_callevt(Q, CallID, status_req),
    bind_q(Q, T, CallID);
bind_q(Q, [publisher_usurp|T], CallID) ->
    ok = amqp_util:bind_q_to_callevt(Q, CallID, publisher_usurp),
    bind_q(Q, T, CallID);
bind_q(Q, [_|T], CallID) ->
    bind_q(Q, T, CallID);
bind_q(_Q, [], _CallID) ->
    ok.

-spec unbind_q(ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallID = props:get_value(callid, Props, <<"*">>),
    unbind_q(Queue, props:get_value(restrict_to, Props), CallID).

unbind_q(Q, undefined, CallID) ->
    ok = amqp_util:unbind_q_from_callevt(Q, CallID),
    ok = amqp_util:unbind_q_from_callevt(Q, CallID, cdr),
    ok = amqp_util:unbind_q_from_callevt(Q, CallID, publisher_usurp);

unbind_q(Q, [events|T], CallID) ->
    ok = amqp_util:unbind_q_from_callevt(Q, CallID),
    unbind_q(Q, T, CallID);
unbind_q(Q, [cdr|T], CallID) ->
    ok = amqp_util:unbind_q_from_callevt(Q, CallID, cdr),
    unbind_q(Q, T, CallID);
unbind_q(Q, [status_req|T], CallID) ->
    ok = amqp_util:unbind_q_from_callevt(Q, CallID, status_req),
    unbind_q(Q, T, CallID);
unbind_q(Q, [publisher_usurp|T], CallID) ->
    ok = amqp_util:unbind_q_from_callevt(Q, CallID, publisher_usurp),
    unbind_q(Q, T, CallID);
unbind_q(Q, [_|T], CallID) ->
    unbind_q(Q, T, CallID);
unbind_q(_Q, [], _CallID) ->
    ok.

-spec publish_event(ne_binary(), api_terms()) -> 'ok'.
-spec publish_event(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_event(CallID, JObj) ->
    publish_event(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_event(CallID, Event, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Event, ?CALL_EVENT_VALUES, fun ?MODULE:event/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_channel_status_req(api_terms()) -> 'ok'.
-spec publish_channel_status_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_req(API) ->
    case is_list(API) of
        true -> publish_channel_status_req(props:get_value(<<"Call-ID">>, API), API);
        false -> publish_channel_status_req(wh_json:get_value(<<"Call-ID">>, API), API)
    end.
publish_channel_status_req(CallID, JObj) ->
    publish_channel_status_req(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_req(CallID, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CHANNEL_STATUS_REQ_VALUES, fun ?MODULE:channel_status_req/1),
    amqp_util:callevt_publish(CallID, Payload, status_req, ContentType).

-spec publish_channel_status_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_STATUS_RESP_VALUES, fun ?MODULE:channel_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_call_status_req(api_terms()) -> 'ok'.
-spec publish_call_status_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_call_status_req(API) ->
    case is_list(API) of
        true -> publish_call_status_req(props:get_value(<<"Call-ID">>, API), API);
        false -> publish_call_status_req(wh_json:get_value(<<"Call-ID">>, API), API)
    end.
publish_call_status_req(CallID, JObj) ->
    publish_call_status_req(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_call_status_req(CallID, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CALL_STATUS_REQ_VALUES, fun ?MODULE:call_status_req/1),
    amqp_util:callevt_publish(CallID, Payload, status_req, ContentType).

-spec publish_call_status_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_call_status_resp(RespQ, JObj) ->
    publish_call_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_call_status_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CALL_STATUS_RESP_VALUES, fun ?MODULE:call_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_auth_id_req(api_terms()) -> 'ok'.
-spec publish_query_auth_id_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_auth_id_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_auth_id_req(API) ->
    case is_list(API) of
        true -> publish_query_auth_id_req(props:get_value(<<"Auth-ID">>, API), API);
        false -> publish_query_auth_id_req(wh_json:get_value(<<"Auth-ID">>, API), API)
    end.
publish_query_auth_id_req(AuthId, JObj) ->
    publish_query_auth_id_req(AuthId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_auth_id_req(AuthId, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?QUERY_AUTH_ID_REQ_VALUES, fun ?MODULE:query_auth_id_req/1),
    amqp_util:callevt_publish(AuthId, Payload, status_req, ContentType).

-spec publish_query_auth_id_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_auth_id_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_auth_id_resp(RespQ, JObj) ->
    publish_query_auth_id_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_auth_id_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_AUTH_ID_RESP_VALUES, fun ?MODULE:query_auth_id_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_cdr(ne_binary(), api_terms()) -> 'ok'.
-spec publish_cdr(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_cdr(CallID, JObj) ->
    publish_cdr(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_cdr(CallID, CDR, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(CDR, ?CALL_CDR_VALUES, fun ?MODULE:cdr/1),
    amqp_util:callevt_publish(CallID, Payload, cdr, ContentType).

-spec publish_callid_update(ne_binary(), api_terms()) -> 'ok'.
-spec publish_callid_update(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_callid_update(CallID, JObj) ->
    publish_callid_update(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_callid_update(CallID, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_ID_UPDATE_VALUES, fun ?MODULE:callid_update/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_control_transfer(ne_binary(), api_terms()) -> 'ok'.
-spec publish_control_transfer(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_control_transfer(TargetQ, JObj) ->
    publish_control_transfer(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_control_transfer(TargetQ, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_CONTROL_TRANSFER_VALUES, fun ?MODULE:control_transfer/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_controller_queue(ne_binary(), api_terms()) -> 'ok'.
-spec publish_controller_queue(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_controller_queue(TargetQ, JObj) ->
    publish_controller_queue(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_controller_queue(TargetQ, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CONTROLLER_QUEUE_VALUES, fun ?MODULE:controller_queue/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_usurp_control(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_control(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_control(CallID, JObj) ->
    publish_usurp_control(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_control(CallID, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_USURP_CONTROL_VALUES, fun ?MODULE:usurp_control/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_usurp_publisher(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_publisher(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_publisher(CallID, JObj) ->
    publish_usurp_publisher(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_publisher(CallID, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?PUBLISHER_USURP_CONTROL_VALUES, fun ?MODULE:usurp_publisher/1),
    amqp_util:callevt_publish(CallID, Payload, publisher_usurp, ContentType).

-spec get_status(api_terms()) -> ne_binary().
get_status(API) when is_list(API) ->
    props:get_value(<<"Status">>, API);
get_status(API) ->
    wh_json:get_value(<<"Status">>, API).
