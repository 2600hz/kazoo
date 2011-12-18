%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Call-related messages, like switch events, status requests, etc
%%% @end
%%% Created : 24 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_call).

-export([event/1, event_v/1, status_req/1, status_req_v/1
	 ,status_resp/1, status_resp_v/1, cdr/1, cdr_v/1
	 ,rate_req/1, rate_req_v/1, rate_resp/1, rate_resp_v/1
	 ,rate_resp_rate/1, rate_resp_rate_v/1
	 ,channel_req/1, channel_resp/1, channel_req_v/1, channel_resp_v/1
	]).

-export([optional_channel_headers/0, required_rate_resp_rate_headers/0]).

-export([bind_q/2, unbind_q/2]).

-export([publish_event/2, publish_event/3
	 ,publish_status_req/1 ,publish_status_req/2, publish_status_req/3
	 ,publish_status_resp/2, publish_status_resp/3
	 ,publish_cdr/2, publish_cdr/3
	 ,publish_rate_req/1, publish_rate_req/2, publish_rate_req/3
	 ,publish_rate_resp/2, publish_rate_resp/3
	 ,publish_channel_req/2, publish_channel_req/3
	 ,publish_channel_resp/2, publish_channel_resp/3
	]).

-export([get_status/1]).

-include("../wh_api.hrl").

%% Call Events
-define(CALL_EVENT_HEADERS, [<<"Timestamp">>, <<"Call-ID">>, <<"Channel-Call-State">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>, <<"Custom-Channel-Vars">>
					  ,<<"Msg-ID">>, <<"Channel-State">>, <<"Call-Direction">>
					  ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>, <<"Other-Leg-Caller-ID-Number">> %% BRIDGE
					  ,<<"Other-Leg-Destination-Number">>,<<"Other-Leg-Unique-ID">> %% BRIDGE
					  ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                          ,<<"Terminator">>, <<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
				     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% Call Status Request
-define(CALL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_STATUS_REQ_HEADERS, []).
-define(CALL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
			     ,{<<"Event-Name">>, <<"status_req">>}
			    ]).
-define(CALL_STATUS_REQ_TYPES, []).

%% Call Status Response
-define(CALL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CALL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>, <<"Switch-Hostname">>]).
-define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
				  ,{<<"Event-Name">>, <<"status_resp">>}
				  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>]}
				 ]).
-define(CALL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

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

%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>). %% Routing key to bind with in AMQP

%% AMQP fields for Rating Request
-define(RATING_REQ_HEADERS, [<<"To-DID">>]).
-define(OPTIONAL_RATING_REQ_HEADERS, [<<"Call-ID">>, <<"From-DID">>, <<"Account-ID">>, <<"Options">>, <<"Direction">>]).
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
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec status_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
status_req(Prop) when is_list(Prop) ->
    case status_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_status req"}
    end;
status_req(JObj) ->
    status_req(wh_json:to_proplist(JObj)).

-spec status_req_v/1 :: (api_terms()) -> boolean().
status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES);
status_req_v(JObj) ->
    status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec status_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
status_resp(Prop) when is_list(Prop) ->
    case status_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
	false -> {error, "Proplist failed validation for call_status_resp"}
    end;
status_resp(JObj) ->
    status_resp(wh_json:to_proplist(JObj)).

-spec status_resp_v/1 :: (api_terms()) -> boolean().
status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES);
status_resp_v(JObj) ->
    status_resp_v(wh_json:to_proplist(JObj)).

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
%% @doc Rating request
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rate_req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
rate_req(Prop) when is_list(Prop) ->
    case rate_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?RATING_REQ_HEADERS, ?OPTIONAL_RATING_REQ_HEADERS);
	false -> {error, "Proplist failed validation for rate_req"}
    end;
rate_req(JObj) ->
    rate_req(wh_json:to_proplist(JObj)).

-spec rate_req_v/1 :: (proplist() | json_object()) -> boolean().
rate_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATING_REQ_HEADERS, ?RATING_REQ_VALUES, ?RATING_REQ_TYPES);
rate_req_v(JObj) ->
    rate_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Rating response
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rate_resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
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

-spec rate_resp_v/1 :: (proplist() | json_object()) -> boolean().
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
-spec rate_resp_rate/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
rate_resp_rate(Prop) when is_list(Prop) ->
    case rate_resp_rate_v(Prop) of
	true -> wh_api:build_message_specific_headers(Prop, ?RATING_RESP_RATE_HEADERS, ?OPTIONAL_RATING_RESP_RATE_HEADERS);
	false -> {error, "Proplist failed validation for rate_resp_rate"}
    end;
rate_resp_rate(JObj) ->
    rate_resp_rate(wh_json:to_proplist(JObj)).

-spec rate_resp_rate_v/1 :: (proplist() | json_object()) -> boolean().
rate_resp_rate_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?RATING_RESP_RATE_HEADERS, ?RATING_RESP_RATE_VALUES, ?RATING_RESP_RATE_TYPES);
rate_resp_rate_v(JObj) ->
    rate_resp_rate_v(wh_json:to_proplist(JObj)).

%% Channel Query Request
-define(CHANNEL_QUERY_REQ_HEADERS, []).
-define(OPTIONAL_CHANNEL_QUERY_REQ_HEADERS, [<<"Call-Direction">>, <<"Caller-ID-Name">>, <<"Caller-ID-Number">>
						 ,<<"IP-Address">>, <<"Destination-Number">>, <<"Switch-Hostname">>
					    ]).
-define(CHANNEL_QUERY_REQ_VALUES, [{<<"Event-Category">>, <<"locate">>}
				   ,{<<"Event-Name">>, <<"channel_req">>}
				   ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
				  ]).
-define(CHANNEL_QUERY_REQ_TYPES, []).

%% Channel Query Response
-define(CHANNEL_QUERY_RESP_HEADERS, [<<"Active-Calls">>]).
-define(OPTIONAL_CHANNEL_QUERY_RESP_HEADERS, []).
-define(CHANNEL_QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"locate">>}
				    ,{<<"Event-Name">>, <<"channel_resp">>}
				   ]).
-define(CHANNEL_QUERY_RESP_TYPES, []).

optional_channel_headers() ->
    ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS.

required_rate_resp_rate_headers() ->
    ?RATING_RESP_RATE_HEADERS.

%%--------------------------------------------------------------------
%% @doc Channel Query Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_req(Prop) when is_list(Prop) ->
    case channel_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for channel_query_req"}
    end;
channel_req(JObj) ->
    channel_req(wh_json:to_proplist(JObj)).

-spec channel_req_v/1 :: (api_terms()) -> boolean().
channel_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?CHANNEL_QUERY_REQ_VALUES, ?CHANNEL_QUERY_REQ_TYPES);
channel_req_v(JObj) ->
    channel_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Channel Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_resp(Prop) when is_list(Prop) ->
    case channel_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?OPTIONAL_CHANNEL_QUERY_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end;
channel_resp(JObj) ->
    channel_resp(wh_json:to_proplist(JObj)).

-spec channel_resp_v/1 :: (api_terms()) -> boolean().
channel_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?CHANNEL_QUERY_RESP_VALUES, ?CHANNEL_QUERY_RESP_TYPES);
channel_resp_v(JObj) ->
    channel_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
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
bind_q(Q, [call_status|T], CallID) ->
    amqp_util:bind_q_to_callevt(Q, CallID, status_req),
    bind_q(Q, T, CallID);
bind_q(Q, [switch_lookups|T], CallID) ->
    amqp_util:bind_q_to_resource(Q, ?KEY_CHANNEL_QUERY),
    bind_q(Q, T, CallID);
bind_q(Q, [_|T], CallID) ->
    bind_q(Q, T, CallID);
bind_q(_Q, [], _CallID) ->
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallID = props:get_value(callid, Props, <<"*">>),

    amqp_util:unbind_q_from_callevt(Queue, CallID),
    amqp_util:bind_q_to_callmgr(Queue, rating_key(CallID)),
    amqp_util:unbind_q_from_resource(Queue, ?KEY_CHANNEL_QUERY),
    amqp_util:bind_q_to_callevt(Queue, CallID, status_req),
    amqp_util:unbind_q_from_callevt(Queue, CallID, cdr).

-spec publish_event/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_event/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_event(CallID, JObj) ->
    publish_event(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_event(CallID, Event, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Event, ?CALL_EVENT_VALUES, fun ?MODULE:event/1),
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

-spec publish_status_req/1 :: (api_terms()) -> 'ok'.
-spec publish_status_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_status_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_status_req(API) ->
    case is_list(API) of
	true -> publish_status_req(props:get_value(<<"Call-ID">>, API), API);
	false -> publish_status_req(wh_json:get_value(<<"Call-ID">>, API), API)
    end.
publish_status_req(CallID, JObj) ->
    publish_status_req(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_status_req(CallID, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CALL_STATUS_REQ_VALUES, fun ?MODULE:status_req/1),
    amqp_util:callevt_publish(CallID, Payload, status_req, ContentType).

-spec publish_status_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_status_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_status_resp(RespQ, JObj) ->
    publish_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_status_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CALL_STATUS_RESP_VALUES, fun ?MODULE:status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_cdr/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_cdr/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_cdr(CallID, JObj) ->
    publish_cdr(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_cdr(CallID, CDR, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(CDR, ?CALL_CDR_VALUES, fun ?MODULE:cdr/1),
    amqp_util:callevt_publish(CallID, Payload, cdr, ContentType).

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

-spec publish_channel_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_req(CallID, JObj) ->
    publish_channel_req(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_req(CallID, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CHANNEL_QUERY_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callevt_publish(CallID, Payload, ContentType).

-spec publish_channel_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_resp(RespQ, JObj) ->
    publish_channel_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_resp(RespQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_QUERY_RESP_VALUES, fun ?MODULE:resp/1),
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
