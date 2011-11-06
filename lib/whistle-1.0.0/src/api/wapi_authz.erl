%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% Created : 14 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_authz).

-export([req/1, resp/1, req_v/1, resp_v/1, bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3]).

-include("../wh_api.hrl").

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>
				,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
			   ]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>, <<"Request">>]).
-define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
			   ,{<<"Event-Name">>, <<"authz_req">>}
			  ]).
-define(AUTHZ_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"To">>, fun is_binary/1}
			  ,{<<"From">>, fun is_binary/1}
			  ,{<<"Call-ID">>, fun is_binary/1}
			  ,{<<"Caller-ID-Name">>, fun is_binary/1}
			  ,{<<"Caller-ID-Number">>, fun is_binary/1}
			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
			 ]).

%% Authorization Responses
-define(AUTHZ_RESP_HEADERS, [<<"Msg-ID">>, <<"Call-ID">>, <<"Is-Authorized">>]).
-define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Custom-Channel-Vars">>]).
-define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
			    ,{<<"Event-Name">>, <<"authz_resp">>}
			    ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
			   ]).
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
        case req_v(Prop) of
	    true -> wh_api:build_message(Prop, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
	    false -> {error, "Proplist failed validation for authz_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (proplist() | json_object()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_REQ_HEADERS, ?AUTHZ_REQ_VALUES, ?AUTHZ_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
	false -> {error, "Proplist failed validation for authz_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (proplist() | json_object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    ok.

-spec unbind_q/1 :: (binary()) -> 'ok'.
unbind_q(Q) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_req/1 :: (iolist()) -> 'ok'.
-spec publish_req/2 :: (iolist(), binary()) -> 'ok'.
publish_req(JSON) ->
    publish_req(JSON, ?DEFAULT_CONTENT_TYPE).
publish_req(Payload, ContentType) ->
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ).

-spec publish_resp/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), iolist(), binary()) -> 'ok'.
publish_resp(Queue, JSON) ->
    publish_resp(Queue, JSON, ?DEFAULT_CONTENT_TYPE).

publish_resp(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).
