%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_resource).

-compile({no_auto_import, [error/1]}).

-export([req/1, resp/1, error/1, req_v/1, resp_v/1, error_v/1]).

-export([bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3
	 ,publish_error/2, publish_error/3]).

-include("../wh_api.hrl").

%% Resource Request
-define(RESOURCE_REQ_HEADERS, [<<"Msg-ID">>, <<"Resource-Type">>, <<"Invite-Format">>]).
-define(OPTIONAL_RESOURCE_REQ_HEADERS, [<<"Resource-Minimum">>, <<"Resource-Maximum">>, <<"Geo-Location">>
                                        ,<<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
					,<<"Application-Name">>, <<"Application-Data">>, <<"SIP-Headers">>
                                        ,<<"Custom-Channel-Vars">>
				       ]).
-define(RESOURCE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
			      ,{<<"Event-Name">>, <<"originate_req">>}
			      ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>]}
			      ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>]}
			      ,?INVITE_FORMAT_TUPLE
			     ]).
-define(RESOURCE_REQ_TYPES, [{<<"Invite-Format">>, fun is_binary/1}
			     ,{<<"Route">>, fun is_binary/1}
			     ,{<<"To-User">>, fun is_binary/1}
			     ,{<<"To-Realm">>, fun is_binary/1}
			     ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                             ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
			    ]).

%% Resource Response
-define(RESOURCE_RESP_HEADERS, [<<"Msg-ID">>, <<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_RESOURCE_RESP_HEADERS, [<<"To">>, <<"Timestamp">>, <<"Channel-Call-State">>
                                             ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                             ,<<"Custom-Channel-Vars">>
                                        ]).
-define(RESOURCE_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
			       ,{<<"Event-Name">>, [<<"offnet_resp">>, <<"originate_resp">>]}
			      ]).
-define(RESOURCE_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% Resource Error
-define(RESOURCE_ERROR_HEADERS, [<<"Msg-ID">>]).
-define(OPTIONAL_RESOURCE_ERROR_HEADERS, [<<"Failed-Attempts">>, <<"Failed-Route">>, <<"Failure-Message">>
                                              ,<<"Failure-Code">>, <<"Hangup-Cause">>, <<"Hangup-Code">>]).
-define(RESOURCE_ERROR_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                ,{<<"Event-Name">>, [<<"originate_error">>, <<"resource_error">>]}
                               ]).
-define(RESOURCE_ERROR_TYPES, []).


%%--------------------------------------------------------------------
%% @doc Resource Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?RESOURCE_REQ_HEADERS, ?OPTIONAL_RESOURCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for resource_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (proplist() | json_object()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESOURCE_REQ_HEADERS, ?RESOURCE_REQ_VALUES, ?RESOURCE_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Resource Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?RESOURCE_RESP_HEADERS, ?OPTIONAL_RESOURCE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (proplist() | json_object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESOURCE_RESP_HEADERS, ?RESOURCE_RESP_VALUES, ?RESOURCE_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Resource Error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
	true -> wh_api:build_message(Prop, ?RESOURCE_ERROR_HEADERS, ?OPTIONAL_RESOURCE_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for resource_error"}
    end;
error(JObj) ->
    error(wh_json:to_proplist(JObj)).

-spec error_v/1 :: (proplist() | json_object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESOURCE_ERROR_HEADERS, ?RESOURCE_ERROR_VALUES, ?RESOURCE_ERROR_TYPES);
error_v(JObj) ->
    error_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Prop) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Queue, ?KEY_RESOURCE_REQ),
    ok.

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_callmgr(Queue).

-spec publish_req/1 :: (iolist()) -> 'ok'.
-spec publish_req/2 :: (iolist(), ne_binary()) -> 'ok'.
publish_req(JSON) ->
    publish_req(JSON, ?DEFAULT_CONTENT_TYPE).
publish_req(Payload, ContentType) ->
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RESOURCE_REQ).

-spec publish_resp/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
publish_resp(Queue, JSON) ->
    publish_resp(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_error/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_error/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
publish_error(Queue, JSON) ->
    publish_error(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_error(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).
