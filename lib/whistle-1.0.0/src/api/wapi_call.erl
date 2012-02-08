%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Call-related messages, like switch events, status requests, etc
%%% @end
%%% Created : 24 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_call).

-export([event/1, event_v/1]).
-export([channel_status_req/1, channel_status_req_v/1]).
-export([channel_status_resp/1, channel_status_resp_v/1]).
-export([call_status_req/1, call_status_req_v/1]).
-export([call_status_resp/1, call_status_resp_v/1]).
-export([channel_query_req/1, channel_query_req_v/1]).
-export([channel_query_resp/1, channel_query_resp_v/1]).
-export([cdr/1, cdr_v/1]).
-export([rate_req/1, rate_req_v/1, rate_resp/1, rate_resp_v/1
         ,rate_resp_rate/1, required_rate_resp_rate_headers/0
        ]).
-export([callid_update/1, callid_update_v/1]).
-export([control_transfer/1, control_transfer_v/1]).
-export([controller_queue/1, controller_queue_v/1]).
-export([usurp_control/1, usurp_control_v/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_event/2, publish_event/3]).
-export([publish_channel_status_req/1 ,publish_channel_status_req/2, publish_channel_status_req/3]).
-export([publish_channel_status_resp/2, publish_channel_status_resp/3]).
-export([publish_call_status_req/1 ,publish_call_status_req/2, publish_call_status_req/3]).
-export([publish_call_status_resp/2, publish_call_status_resp/3]).
-export([publish_cdr/2, publish_cdr/3]).
-export([publish_rate_req/1, publish_rate_req/2, publish_rate_req/3]).
-export([publish_rate_resp/2, publish_rate_resp/3]).
-export([publish_callid_update/2, publish_callid_update/3]).
-export([publish_control_transfer/2, publish_control_transfer/3]).
-export([publish_controller_queue/2, publish_controller_queue/3]).
-export([publish_usurp_control/2, publish_usurp_control/3]).

-export([publish_channel_query_req/2, publish_channel_query_req/3]).
-export([publish_channel_query_resp/2, publish_channel_query_resp/3]).

-export([optional_channel_headers/0]).

-export([get_status/1]).

-include("../wh_api.hrl").

%% Call Events
-define(CALL_EVENT_HEADERS, [<<"Timestamp">>, <<"Call-ID">>, <<"Channel-Call-State">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>, <<"Custom-Channel-Vars">>
                                          ,<<"Msg-ID">>, <<"Channel-State">>, <<"Call-Direction">>, <<"Transfer-History">>
                                          ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>
                                          ,<<"Other-Leg-Caller-ID-Number">>, <<"Other-Leg-Destination-Number">>
                                          ,<<"Other-Leg-Unique-ID">> %% BRIDGE
                                          ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                          ,<<"Terminator">>, <<"Disposition">>, <<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
                                          ,<<"Raw-Application-Name">>, <<"Length">>
                                     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% Channel Status Request
-define(CHANNEL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CHANNEL_STATUS_REQ_HEADERS, []).
-define(CHANNEL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                             ,{<<"Event-Name">>, <<"channel_status_req">>}
                            ]).
-define(CHANNEL_STATUS_REQ_TYPES, []).

%% Channel Status Response
-define(CHANNEL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CHANNEL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>, <<"Switch-Hostname">>]).
-define(CHANNEL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"channel_status_resp">>}
                                  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                 ]).
-define(CHANNEL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

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
                                                ,<<"Other-Leg-Destination-Number">>
                                           ]).
-define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"call_status_resp">>}
                                  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                 ]).
-define(CALL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% Channel Query Request
-define(CHANNEL_QUERY_REQ_HEADERS, []).
-define(OPTIONAL_CHANNEL_QUERY_REQ_HEADERS, [<<"Call-Direction">>, <<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                                 ,<<"IP-Address">>, <<"Destination-Number">>, <<"Switch-Hostname">>
                                            ]).
-define(CHANNEL_QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                   ,{<<"Event-Name">>, <<"channel_query_req">>}
                                   ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
                                     ]).
-define(CHANNEL_QUERY_REQ_TYPES, []).

%% Channel Query Response
-define(CHANNEL_QUERY_RESP_HEADERS, [<<"Active-Calls">>]).
-define(OPTIONAL_CHANNEL_QUERY_RESP_HEADERS, []).
-define(CHANNEL_QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"call">>}
                                    ,{<<"Event-Name">>, <<"channel_query_resp">>}
                                   ]).
-define(CHANNEL_QUERY_RESP_TYPES, []).

%% Call CDR
-define(CALL_CDR_HEADERS, [ <<"Call-ID">>]).
-define(OPTIONAL_CALL_CDR_HEADERS, [<<"Hangup-Cause">>, <<"Handling-Server-Name">>, <<"Custom-Channel-Vars">>
                                        ,<<"Remote-SDP">>, <<"Local-SDP">>, <<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>, <<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                        ,<<"User-Agent">>, <<"Caller-ID-Type">>, <<"Other-Leg-Call-ID">>
                                        ,<<"Timestamp">>
                                        ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
                                        ,<<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
                                        ,<<"Digits-Dialed">>
                                   ]).
-define(CALL_CDR_VALUES, [{<<"Event-Category">>, <<"call_detail">>}
                          ,{<<"Event-Name">>, <<"cdr">>}
                          ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
                          ,{<<"Caller-ID-Type">>, [<<"pid">>, <<"rpid">>, <<"from">>]}
                         ]).
-define(CALL_CDR_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

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
-define(OPTIONAL_CALL_USURP_CONTROL_HEADERS, [<<"Reason">>]).
-define(CALL_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"usurp_control">>}
                                   ]).
-define(CALL_USURP_CONTROL_TYPES, []).

%% Controller Queue Update
-define(CONTROLLER_QUEUE_HEADERS, [<<"Call-ID">>, <<"Controller-Queue">>]).
-define(OPTIONAL_CONTROLLER_QUEUE_HEADERS, []).
-define(CONTROLLER_QUEUE_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"controller_queue">>}
                                 ]).
-define(CONTROLLER_QUEUE_TYPES, []).


%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>). %% Routing key to bind with in AMQP

%% AMQP fields for Rating Request
-define(RATING_REQ_HEADERS, [<<"To-DID">>]).
-define(OPTIONAL_RATING_REQ_HEADERS, [<<"Call-ID">>, <<"Account-ID">>, <<"From-DID">>
                                          ,<<"Options">>, <<"Direction">>, <<"Control-Queue">>]).
-define(RATING_REQ_VALUES, [{<<"Event-Category">>, <<"call_mgmt">>}
                            ,{<<"Event-Name">>, <<"rating_req">>}
                            ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                           ]).
-define(RATING_REQ_TYPES, [
                           {<<"Options">>, fun is_list/1}
                          ]).

%% AMQP fields for Rating Response
-define(RATING_RESP_HEADERS, [<<"Rates">>]).
-define(OPTIONAL_RATING_RESP_HEADERS, []).
-define(RATING_RESP_VALUES, [{<<"Event-Category">>, <<"call_mgmt">>}
                             ,{<<"Event-Name">>, <<"rating_resp">>}
                            ]).
-define(RATING_RESP_TYPES, []).

-define(RATING_RESP_RATE_HEADERS, [<<"Rate">>, <<"Rate-Increment">>, <<"Rate-Minimum">>, <<"Surcharge">>, <<"Base-Cost">>]).
-define(OPTIONAL_RATING_RESP_RATE_HEADERS, [<<"Rate-Name">>]).
-define(RATING_RESP_RATE_VALUES, []).
-define(RATING_RESP_RATE_TYPES, []).

required_rate_resp_rate_headers() ->
    ?RATING_RESP_RATE_HEADERS.

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec event/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
        false -> {error, "Proplist failed validation for call_event"}
    end;
event(JObj) ->
    event(wh_json:to_proplist(JObj)).

-spec event_v/1 :: (api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES);
event_v(JObj) ->
    event_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a channel
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_req(Prop) when is_list(Prop) ->
    case channel_status_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?OPTIONAL_CHANNEL_STATUS_REQ_HEADERS);
        false -> {error, "Proplist failed validation for channel status req"}
    end;
channel_status_req(JObj) ->
    channel_status_req(wh_json:to_proplist(JObj)).

-spec channel_status_req_v/1 :: (api_terms()) -> boolean().
channel_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?CHANNEL_STATUS_REQ_VALUES, ?CHANNEL_STATUS_REQ_TYPES);
channel_status_req_v(JObj) ->
    channel_status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a channel, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_resp(Prop) when is_list(Prop) ->
    case channel_status_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?OPTIONAL_CHANNEL_STATUS_RESP_HEADERS);
        false -> {error, "Proplist failed validation for channel status resp"}
    end;
channel_status_resp(JObj) ->
    channel_status_resp(wh_json:to_proplist(JObj)).

-spec channel_status_resp_v/1 :: (api_terms()) -> boolean().
channel_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?CHANNEL_STATUS_RESP_VALUES, ?CHANNEL_STATUS_RESP_TYPES);
channel_status_resp_v(JObj) ->
    channel_status_resp_v(wh_json:to_proplist(JObj)).

optional_channel_headers() ->
    ?OPTIONAL_CALL_STATUS_RESP_HEADERS.

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_req(Prop) when is_list(Prop) ->
    case call_status_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
        false -> {error, "Proplist failed validation for call status req"}
    end;
call_status_req(JObj) ->
    call_status_req(wh_json:to_proplist(JObj)).

-spec call_status_req_v/1 :: (api_terms()) -> boolean().
call_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES);
call_status_req_v(JObj) ->
    call_status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_resp(Prop) when is_list(Prop) ->
    case call_status_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
        false -> {error, "Proplist failed validation for call status resp"}
    end;
call_status_resp(JObj) ->
    call_status_resp(wh_json:to_proplist(JObj)).

-spec call_status_resp_v/1 :: (api_terms()) -> boolean().
call_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES);
call_status_resp_v(JObj) ->
    call_status_resp_v(wh_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Channel Query Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_query_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_query_req(Prop) when is_list(Prop) ->    
    case channel_query_req_v(Prop) of
        true ->
            wh_api:build_message(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS);
        false -> {error, "Proplist failed validation for channel_query_req_req"}
    end;
channel_query_req(JObj) ->
    channel_query_req(wh_json:to_proplist(JObj)).

-spec channel_query_req_v/1 :: (api_terms()) -> boolean().
channel_query_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?CHANNEL_QUERY_REQ_VALUES, ?CHANNEL_QUERY_REQ_TYPES);
channel_query_req_v(JObj) ->
    channel_query_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Channel Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_query_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_query_resp(Prop) when is_list(Prop) ->
    case channel_query_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?OPTIONAL_CHANNEL_QUERY_RESP_HEADERS);
        false -> {error, "Proplist failed validation for resource_resp"}
    end;
channel_query_resp(JObj) ->
    channel_query_resp(wh_json:to_proplist(JObj)).

-spec channel_query_resp_v/1 :: (api_terms()) -> boolean().
channel_query_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?CHANNEL_QUERY_RESP_VALUES, ?CHANNEL_QUERY_RESP_TYPES);
channel_query_resp_v(JObj) ->
    channel_query_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a CDR for a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec cdr/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
cdr(Prop) when is_list(Prop) ->
    case cdr_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_CDR_HEADERS, ?OPTIONAL_CALL_CDR_HEADERS);
        false -> {error, "Proplist failed validation for call_cdr"}
    end;
cdr(JObj) ->
    cdr(wh_json:to_proplist(JObj)).

-spec cdr_v/1 :: (api_terms()) -> boolean().
cdr_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_CDR_HEADERS, ?CALL_CDR_VALUES, ?CALL_CDR_TYPES);
cdr_v(JObj) ->
    cdr_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec callid_update/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
callid_update(Prop) when is_list(Prop) ->
    case callid_update_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_ID_UPDATE_HEADERS, ?OPTIONAL_CALL_ID_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for callid_update"}
    end;
callid_update(JObj) ->
    callid_update(wh_json:to_proplist(JObj)).

-spec callid_update_v/1 :: (api_terms()) -> boolean().
callid_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_ID_UPDATE_HEADERS, ?CALL_ID_UPDATE_VALUES, ?CALL_ID_UPDATE_TYPES);
callid_update_v(JObj) ->
    callid_update_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec control_transfer/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
control_transfer(Prop) when is_list(Prop) ->
    case control_transfer_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_CONTROL_TRANSFER_HEADERS, ?OPTIONAL_CALL_CONTROL_TRANSFER_HEADERS);
        false -> {error, "Proplist failed validation for control_transfer"}
    end;
control_transfer(JObj) ->
    control_transfer(wh_json:to_proplist(JObj)).

-spec control_transfer_v/1 :: (api_terms()) -> boolean().
control_transfer_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_CONTROL_TRANSFER_HEADERS, ?CALL_CONTROL_TRANSFER_VALUES, ?CALL_CONTROL_TRANSFER_TYPES);
control_transfer_v(JObj) ->
    control_transfer_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec usurp_control/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_control(Prop) when is_list(Prop) ->
    case usurp_control_v(Prop) of
        true -> wh_api:build_message(Prop, ?CALL_USURP_CONTROL_HEADERS, ?OPTIONAL_CALL_USURP_CONTROL_HEADERS);
        false -> {error, "Proplist failed validation for usurp_control"}
    end;
usurp_control(JObj) ->
    usurp_control(wh_json:to_proplist(JObj)).

-spec usurp_control_v/1 :: (api_terms()) -> boolean().
usurp_control_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_USURP_CONTROL_HEADERS, ?CALL_USURP_CONTROL_VALUES, ?CALL_USURP_CONTROL_TYPES);
usurp_control_v(JObj) ->
    usurp_control_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec controller_queue/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
controller_queue(Prop) when is_list(Prop) ->
    case controller_queue_v(Prop) of
        true -> wh_api:build_message(Prop, ?CONTROLLER_QUEUE_HEADERS, ?OPTIONAL_CONTROLLER_QUEUE_HEADERS);
        false -> {error, "Proplist failed validation for controller_queue"}
    end;
controller_queue(JObj) ->
    controller_queue(wh_json:to_proplist(JObj)).

-spec controller_queue_v/1 :: (api_terms()) -> boolean().
controller_queue_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONTROLLER_QUEUE_HEADERS, ?CONTROLLER_QUEUE_VALUES, ?CONTROLLER_QUEUE_TYPES);
controller_queue_v(JObj) ->
    controller_queue_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Rating request
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rate_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', iolist()} | {'error', string()}.
rate_req(Prop) when is_list(Prop) ->
    case rate_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?RATING_REQ_HEADERS, ?OPTIONAL_RATING_REQ_HEADERS);
        false -> {error, "Proplist failed validation for rate_req"}
    end;
rate_req(JObj) ->
    rate_req(wh_json:to_proplist(JObj)).

-spec rate_req_v/1 :: (proplist() | wh_json:json_object()) -> boolean().
rate_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATING_REQ_HEADERS, ?RATING_REQ_VALUES, ?RATING_REQ_TYPES);
rate_req_v(JObj) ->
    rate_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Rating response
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rate_resp/1 :: (proplist() | wh_json:json_object()) -> {'ok', iolist()} | {'error', string()}.
rate_resp(Prop) when is_list(Prop) ->
    Rates = [ begin
                  {ok, RateProp} = rate_resp_rate(Rate),
                  wh_json:from_list(RateProp)
              end || Rate <- props:get_value(<<"Rates">>, Prop, [])
            ],
    Prop1 = [{<<"Rates">>, Rates} | props:delete(<<"Rates">>, Prop)],

    case rate_resp_v(Prop1) of
        true -> wh_api:build_message(Prop1, ?RATING_RESP_HEADERS, ?OPTIONAL_RATING_RESP_HEADERS);
        false -> {error, "Proplist failed validation for rate_resp"}
    end;
rate_resp(JObj) ->
    rate_resp(wh_json:to_proplist(JObj)).

-spec rate_resp_v/1 :: (proplist() | wh_json:json_object()) -> boolean().
rate_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATING_RESP_HEADERS, ?RATING_RESP_VALUES, ?RATING_RESP_TYPES) andalso
        lists:all(fun rate_resp_rate_v/1, props:get_value(<<"Rates">>, Prop, []));
rate_resp_v(JObj) ->
    rate_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Rating response
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rate_resp_rate/1 :: (proplist() | wh_json:json_object()) -> {'ok', proplist()} | {'error', string()}.
rate_resp_rate(Prop) when is_list(Prop) ->
    case rate_resp_rate_v(Prop) of
        true -> wh_api:build_message_specific_headers(Prop, ?RATING_RESP_RATE_HEADERS, ?OPTIONAL_RATING_RESP_RATE_HEADERS);
        false -> {error, "Proplist failed validation for rate_resp_rate"}
    end;
rate_resp_rate(JObj) ->
    rate_resp_rate(wh_json:to_proplist(JObj)).

-spec rate_resp_rate_v/1 :: (proplist() | wh_json:json_object()) -> boolean().
rate_resp_rate_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?RATING_RESP_RATE_HEADERS, ?RATING_RESP_RATE_VALUES, ?RATING_RESP_RATE_TYPES);
rate_resp_rate_v(JObj) ->
    rate_resp_rate_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallID = props:get_value(callid, Props, <<"*">>),
    amqp_util:callevt_exchange(),
    amqp_util:callmgr_exchange(),
    amqp_util:resource_exchange(),
    bind_q(Queue, props:get_value(restrict_to, Props), CallID).

bind_q(Q, undefined, CallID) ->
    amqp_util:bind_q_to_callevt(Q, CallID),
    amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    amqp_util:bind_q_to_resource(Q, ?KEY_CHANNEL_QUERY),
    amqp_util:bind_q_to_callmgr(Q, rating_key(CallID));

bind_q(Q, [events|T], CallID) ->
    amqp_util:bind_q_to_callevt(Q, CallID),
    bind_q(Q, T, CallID);
bind_q(Q, [cdr|T], CallID) ->
    amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    bind_q(Q, T, CallID);
bind_q(Q, [rating|T], CallID) ->
    amqp_util:bind_q_to_callmgr(Q, rating_key(CallID)),
    bind_q(Q, T, CallID);
bind_q(Q, [status_req|T], CallID) ->
    amqp_util:bind_q_to_callevt(Q, CallID, status_req),
    bind_q(Q, T, CallID);
bind_q(Q, [query_req|T], CallID) ->
    amqp_util:bind_q_to_resource(Q, ?KEY_CHANNEL_QUERY),
    bind_q(Q, T, CallID);
bind_q(Q, [_|T], CallID) ->
    bind_q(Q, T, CallID);
bind_q(_Q, [], _CallID) ->
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallID = props:get_value(callid, Props, <<"*">>),
    unbind_q(Queue, props:get_value(restrict_to, Props), CallID).

unbind_q(Q, undefined, CallID) ->
    amqp_util:unbind_q_from_callevt(Q, CallID),
    amqp_util:unbind_q_from_callevt(Q, CallID, cdr),
    amqp_util:unbind_q_from_resource(Q, ?KEY_CHANNEL_QUERY),
    amqp_util:unbind_q_from_callmgr(Q, rating_key(CallID));

unbind_q(Q, [events|T], CallID) ->
    amqp_util:unbind_q_from_callevt(Q, CallID),
    unbind_q(Q, T, CallID);
unbind_q(Q, [cdr|T], CallID) ->
    amqp_util:unbind_q_from_callevt(Q, CallID, cdr),
    unbind_q(Q, T, CallID);
unbind_q(Q, [rating|T], CallID) ->
    amqp_util:unbind_q_from_callmgr(Q, rating_key(CallID)),
    unbind_q(Q, T, CallID);
unbind_q(Q, [status_req|T], CallID) ->
    amqp_util:unbind_q_from_callevt(Q, CallID, status_req),
    unbind_q(Q, T, CallID);
unbind_q(Q, [query_req|T], CallID) ->
    amqp_util:unbind_q_from_resource(Q, ?KEY_CHANNEL_QUERY),
    unbind_q(Q, T, CallID);
unbind_q(Q, [_|T], CallID) ->
    unbind_q(Q, T, CallID);
unbind_q(_Q, [], _CallID) ->
    ok.

-spec publish_event/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_event/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_event(CallID, JObj) ->
    publish_event(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_event(CallID, Event, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Event, ?CALL_EVENT_VALUES, fun ?MODULE:event/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_channel_status_req/1 :: (api_terms()) -> 'ok'.
-spec publish_channel_status_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
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

-spec publish_channel_status_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_STATUS_RESP_VALUES, fun ?MODULE:channel_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_call_status_req/1 :: (api_terms()) -> 'ok'.
-spec publish_call_status_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
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

-spec publish_call_status_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_call_status_resp(RespQ, JObj) ->
    publish_call_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_call_status_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CALL_STATUS_RESP_VALUES, fun ?MODULE:call_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_cdr/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_cdr/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_cdr(CallID, JObj) ->
    publish_cdr(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_cdr(CallID, CDR, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(CDR, ?CALL_CDR_VALUES, fun ?MODULE:cdr/1),
    amqp_util:callevt_publish(CallID, Payload, cdr, ContentType).

-spec publish_callid_update/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_callid_update/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_callid_update(CallID, JObj) ->
    publish_callid_update(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_callid_update(CallID, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_ID_UPDATE_VALUES, fun ?MODULE:callid_update/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_control_transfer/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_control_transfer/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_control_transfer(TargetQ, JObj) ->
    publish_control_transfer(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_control_transfer(TargetQ, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_CONTROL_TRANSFER_VALUES, fun ?MODULE:control_transfer/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_controller_queue/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_controller_queue/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_controller_queue(TargetQ, JObj) ->
    publish_controller_queue(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_controller_queue(TargetQ, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CONTROLLER_QUEUE_VALUES, fun ?MODULE:controller_queue/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec publish_usurp_control/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_control/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_control(CallID, JObj) ->
    publish_usurp_control(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_control(CallID, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?CALL_USURP_CONTROL_VALUES, fun ?MODULE:usurp_control/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_rate_req/1 :: (api_terms()) -> 'ok'.
-spec publish_rate_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_rate_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_rate_req(API) ->
    case is_list(API) of
        true -> publish_rate_req(props:get_value(<<"Call-ID">>, API, <<"0000000000">>), API);
        false -> publish_rate_req(wh_json:get_value(<<"Call-ID">>, API, <<"0000000000">>), API)
    end.
publish_rate_req(CallID, API) ->
    publish_rate_req(CallID, API, ?DEFAULT_CONTENT_TYPE).
publish_rate_req(CallID, API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?RATING_REQ_VALUES, fun ?MODULE:rate_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, rating_key(CallID)).

-spec publish_rate_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_rate_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_rate_resp(Queue, JObj) ->
    publish_rate_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_rate_resp(Queue, API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?RATING_RESP_VALUES, fun ?MODULE:rate_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_channel_query_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_query_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_query_req(CallID, JObj) ->
    publish_channel_query_req(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_query_req(CallID, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CHANNEL_QUERY_REQ_VALUES, fun ?MODULE:channel_query_req/1),
    amqp_util:callevt_publish(CallID, Payload, ContentType).

-spec publish_channel_query_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_query_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_query_resp(RespQ, JObj) ->
    
    publish_channel_query_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_query_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_QUERY_RESP_VALUES, fun ?MODULE:channel_query_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec get_status/1 :: (api_terms()) -> ne_binary().
get_status(API) when is_list(API) ->
    props:get_value(<<"Status">>, API);
get_status(API) ->
    wh_json:get_value(<<"Status">>, API).

-spec rating_key/1 :: ('undefined' | ne_binary()) -> ne_binary().
rating_key(undefined) ->
    list_to_binary([?KEY_RATING_REQ, ".*"]);
rating_key(CallID) when is_binary(CallID) ->
    list_to_binary([?KEY_RATING_REQ, ".", amqp_util:encode(CallID)]).
