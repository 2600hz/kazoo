%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% API functions for Jonny5 WhApps to inter-communicate. For use only
%%% only with other Jonny5 WhApps (though you could write on compatible,
%%% but why bother).
%%% @end
%%% Created :  6 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_jonny5).

-export([status_req/1, status_req_v/1]). %% Status requests to other J5 instances
-export([status_resp/1, status_resp_v/1]). %% Status responses from other J5 instances

-export([sync_req/1, sync_req_v/1]).
-export([sync_resp/1, sync_resp_v/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_status_req/1, publish_status_req/2]).
-export([publish_status_resp/2, publish_status_resp/3]).
-export([publish_sync_req/1, publish_sync_req/2]).
-export([publish_sync_resp/2, publish_sync_resp/3]).

-include("jonny5.hrl").

-define(KEY_JONNY5_STATUS, <<"jonny5.status">>).
-define(KEY_JONNY5_SYNC, <<"jonny5.sync">>).

%% For remote status collection
-define(STATUS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_STATUS_REQ_HEADERS, []).
-define(STATUS_REQ_VALUES, [
			    {<<"Event-Category">>, <<"jonny5">>}
			    ,{<<"Event-Name">>, <<"status_req">>}
			   ]).
-define(STATUS_REQ_TYPES, []).

-define(STATUS_RESP_HEADERS, [<<"Account-ID">>, <<"Uptime">>]).
-define(OPTIONAL_STATUS_RESP_HEADERS, [<<"Prepay">>, <<"Max-Two-Way">>, <<"Max-Inbound">>
					   ,<<"Two-Way">>, <<"Inbound">>
				      ]).
-define(STATUS_RESP_VALUES, [
			     {<<"Event-Category">>, <<"jonny5">>}
			     ,{<<"Event-Name">>, <<"status_resp">>}
			    ]).
-define(STATUS_RESP_TYPES, []).

%% For whapp sync across the cluster
-define(SYNC_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_SYNC_REQ_HEADERS, []).
-define(SYNC_REQ_VALUES, [
			  {<<"Event-Category">>, <<"jonny5">>}
			  ,{<<"Event-Name">>, <<"sync_req">>}
			 ]).
-define(SYNC_REQ_TYPES, []).

-define(SYNC_RESP_HEADERS, [<<"Account-ID">>, <<"Uptime">>]).
-define(OPTIONAL_SYNC_RESP_HEADERS, [<<"Prepay">>, <<"Max-Two-Way">>, <<"Max-Inbound">>
					 ,<<"Two-Way">>, <<"Inbound">>
				    ]).
-define(SYNC_RESP_VALUES, [
			   {<<"Event-Category">>, <<"jonny5">>}
			   ,{<<"Event-Name">>, <<"sync_resp">>}
			  ]).
-define(SYNC_RESP_TYPES, []).

-spec status_req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
status_req(Prop) when is_list(Prop) ->
    case status_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?STATUS_REQ_HEADERS, ?OPTIONAL_STATUS_REQ_HEADERS);
	false -> {error, "validation of status_req failed"}
    end;
status_req(JObj) ->
    status_req(wh_json:to_proplist(JObj)).

-spec status_req_v/1 :: (proplist() | json_object()) -> boolean().
status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STATUS_REQ_HEADERS, ?STATUS_REQ_TYPES, ?STATUS_REQ_VALUES);
status_req_v(JObj) ->
    status_req_v(wh_json:to_proplist(JObj)).


-spec status_resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
status_resp(Prop) when is_list(Prop) ->
    case status_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?STATUS_RESP_HEADERS, ?OPTIONAL_STATUS_RESP_HEADERS);
	false -> {error, "validation of status_resp failed"}
    end;
status_resp(JObj) ->
    status_resp(wh_json:to_proplist(JObj)).

-spec status_resp_v/1 :: (proplist() | json_object()) -> boolean().
status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STATUS_RESP_HEADERS, ?STATUS_RESP_TYPES, ?STATUS_RESP_VALUES);
status_resp_v(JObj) ->
    status_resp_v(wh_json:to_proplist(JObj)).

-spec sync_req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
sync_req(Prop) when is_list(Prop) ->
    case sync_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYNC_REQ_HEADERS, ?OPTIONAL_SYNC_REQ_HEADERS);
	false -> {error, "validation of sync_req failed"}
    end;
sync_req(JObj) ->
    sync_req(wh_json:to_proplist(JObj)).

-spec sync_req_v/1 :: (proplist() | json_object()) -> boolean().
sync_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_REQ_HEADERS, ?SYNC_REQ_TYPES, ?SYNC_REQ_VALUES);
sync_req_v(JObj) ->
    sync_req_v(wh_json:to_proplist(JObj)).


-spec sync_resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
sync_resp(Prop) when is_list(Prop) ->
    case sync_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYNC_RESP_HEADERS, ?OPTIONAL_SYNC_RESP_HEADERS);
	false -> {error, "validation of sync_resp failed"}
    end;
sync_resp(JObj) ->
    sync_resp(wh_json:to_proplist(JObj)).

-spec sync_resp_v/1 :: (proplist() | json_object()) -> boolean().
sync_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_RESP_HEADERS, ?SYNC_RESP_TYPES, ?SYNC_RESP_VALUES);
sync_resp_v(JObj) ->
    sync_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:resource_exchange(),
    _ = amqp_util:bind_q_to_resource(Queue, ?KEY_JONNY5_STATUS),
    _ = amqp_util:bind_q_to_resource(Queue, ?KEY_JONNY5_SYNC),
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_resource(Queue, ?KEY_JONNY5_STATUS),
    amqp_util:unbind_q_from_resource(Queue, ?KEY_JONNY5_SYNC).

-spec publish_status_req/1 :: (ne_binary()) -> 'ok'.
-spec publish_status_req/2 :: (ne_binary(), ne_binary()) -> 'ok'.
publish_status_req(JSON) ->
    publish_status_req(JSON, ?DEFAULT_CONTENT_TYPE).
publish_status_req(Payload, ContentType) ->
    amqp_util:resource_publish(Payload, ?KEY_JONNY5_STATUS, ContentType).

-spec publish_status_resp/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec publish_status_resp/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish_status_resp(Queue, JObj) ->
    publish_status_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_status_resp(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_sync_req/1 :: (ne_binary()) -> 'ok'.
-spec publish_sync_req/2 :: (ne_binary(), ne_binary()) -> 'ok'.
publish_sync_req(JSON) ->
    publish_sync_req(JSON, ?DEFAULT_CONTENT_TYPE).
publish_sync_req(Payload, ContentType) ->
    amqp_util:resource_publish(Payload, ?KEY_JONNY5_SYNC, ContentType).

-spec publish_sync_resp/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec publish_sync_resp/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish_sync_resp(Queue, JObj) ->
    publish_sync_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync_resp(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).
