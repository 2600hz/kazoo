%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(wapi_channel_query).

-export([req/1, req_v/1, resp/1, resp_v/1]).

-export([bind_q/2, unbind_q/1, unbind_q/2]).

-export([publish_req/2, publish_req/3, publish_resp/2, publish_resp/3]).

-export([optional_headers/0]).

-include("../wh_api.hrl").

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

optional_headers() ->
    ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS.

%%--------------------------------------------------------------------
%% @doc Channel Query Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for channel_query_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (proplist() | json_object()) -> boolean().
req_v([_|_]=Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?CHANNEL_QUERY_REQ_VALUES, ?CHANNEL_QUERY_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Channel Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?OPTIONAL_CHANNEL_QUERY_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (proplist() | json_object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?CHANNEL_QUERY_RESP_VALUES, ?CHANNEL_QUERY_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallID = get_callid(Props),
    amqp_util:bind_q_to_callevt(Queue, CallID, status_req),
    ok.

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue) ->
    unbind_q(Queue, []).
unbind_q(Queue, Props) ->
    CallID = get_callid(Props),
    amqp_util:unbind_q_from_callevt(Queue, CallID, status_req).

-spec publish_req/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_req/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
publish_req(CallID, JSON) ->
    publish_req(CallID, JSON, ?DEFAULT_CONTENT_TYPE).
publish_req(CallID, Payload, ContentType) ->
    amqp_util:callevt_publish(CallID, Payload, ContentType).

-spec publish_resp/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
publish_resp(RespQ, JSON) ->
    publish_resp(RespQ, JSON, ?DEFAULT_CONTENT_TYPE).
publish_resp(RespQ, Payload, ContentType) ->
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec get_callid/1 :: (proplist()) -> ne_binary().
get_callid(Props) ->
    case props:get_value(callid, Props) of
	undefined -> <<"*">>;
	CID -> CID
    end.
