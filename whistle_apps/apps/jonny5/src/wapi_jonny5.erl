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

-export([publish_status_req/1, publish_status_resp/2]).
-export([publish_sync_req/1, publish_sync_resp/2]).

-include("jonny5.hrl").

-define(KEY_JONNY5_STATUS, <<"jonny5.status">>).
-define(KEY_JONNY5_SYNC, <<"jonny5.sync">>).

%% For remote status collection
-define(STATUS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_STATUS_REQ_HEADERS, [<<"Uptime">>]).
-define(STATUS_REQ_VALUES, [
			    {<<"Event-Category">>, <<"jonny5">>}
			    ,{<<"Event-Name">>, <<"status_req">>}
			   ]).
-define(STATUS_REQ_TYPES, []).

-define(STATUS_RESP_HEADERS, [<<"Account-ID">>, <<"Uptime">>]).
-define(OPTIONAL_STATUS_RESP_HEADERS, [<<"Prepay">>, <<"Max-Two-Way">>, <<"Max-Inbound">>
					   ,<<"Two-Way">>, <<"Inbound">>, <<"Trunks">>
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
					 ,<<"Two-Way">>, <<"Inbound">>, <<"Trunks">>
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
    wh_api:validate(Prop, ?STATUS_REQ_HEADERS, ?STATUS_REQ_VALUES, ?STATUS_REQ_TYPES);
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
    wh_api:validate(Prop, ?STATUS_RESP_HEADERS, ?STATUS_RESP_VALUES, ?STATUS_RESP_TYPES);
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
    wh_api:validate(Prop, ?SYNC_REQ_HEADERS, ?SYNC_REQ_VALUES, ?SYNC_REQ_TYPES);
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
    wh_api:validate(Prop, ?SYNC_RESP_HEADERS, ?SYNC_RESP_VALUES, ?SYNC_RESP_TYPES);
sync_resp_v(JObj) ->
    sync_resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    AcctId = get_acct_id(Props),

    amqp_util:resource_exchange(),
    _ = amqp_util:bind_q_to_resource(Queue, <<?KEY_JONNY5_STATUS/binary, ".", AcctId/binary>>),
    _ = amqp_util:bind_q_to_resource(Queue, <<?KEY_JONNY5_SYNC/binary, ".", AcctId/binary>>),
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    AcctId = get_acct_id(Props),

    amqp_util:unbind_q_from_resource(Queue, status_req_routing_key(AcctId)),
    amqp_util:unbind_q_from_resource(Queue, sync_req_routing_key(AcctId)).

-spec get_acct_id/1 :: (proplist()) -> ne_binary().
get_acct_id(Prop) when is_list(Prop) ->
    case props:get_value(account_id, Prop) of
	undefined ->
	    case props:get_value(<<"Account-ID">>, Prop) of
		undefined -> <<"*">>;
		AID -> AID
	    end;
	AID -> AID
    end;
get_acct_id(JObj) ->
    wh_json:get_value(<<"Account-ID">>, JObj, <<>>).

-spec publish_status_req/1 :: (api_terms()) -> 'ok'.
publish_status_req(Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?STATUS_REQ_VALUES, fun ?MODULE:status_req/1),
    amqp_util:resource_publish(Payload, status_req_routing_key(get_acct_id(Req)), ?DEFAULT_CONTENT_TYPE).

-spec publish_status_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
publish_status_resp(Queue, Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?STATUS_RESP_VALUES, fun ?MODULE:status_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync_req/1 :: (api_terms()) -> 'ok'.
publish_sync_req(Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?SYNC_REQ_VALUES, fun ?MODULE:sync_req/1),
    amqp_util:resource_publish(Payload, sync_req_routing_key(get_acct_id(Req)), ?DEFAULT_CONTENT_TYPE).

-spec publish_sync_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
publish_sync_resp(Queue, Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?SYNC_RESP_VALUES, fun ?MODULE:sync_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ?DEFAULT_CONTENT_TYPE).

status_req_routing_key(<<>>) ->
    ?KEY_JONNY5_STATUS;
status_req_routing_key(AcctId) when is_binary(AcctId) ->
    <<?KEY_JONNY5_STATUS/binary, ".", AcctId/binary>>.

sync_req_routing_key(<<>>) ->
    ?KEY_JONNY5_SYNC;
sync_req_routing_key(AcctId) when is_binary(AcctId) ->
    <<?KEY_JONNY5_SYNC/binary, ".", AcctId/binary>>.
