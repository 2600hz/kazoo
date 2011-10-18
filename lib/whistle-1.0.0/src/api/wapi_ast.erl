%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% AST requests, responses, and errors
%%% @end
%%% Created : 17 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_ast).

-compile({no_auto_import, [error/1]}).

-export([req/1, resp/1, error/1, req_v/1, resp_v/1, error_v/1]).

-export([bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3
	 ,publish_error/2, publish_error/3]).

-include("../wh_api.hrl").

%% AST Request - when Speech to text is desired
-define(AST_REQ_HEADERS, [<<"AST-Endpoint">>, <<"AST-Account-ID">>, <<"AST-Account-Password">>, <<"Call-ID">>
			      ,<<"Control-Queue">>
			 ]).
-define(OPTIONAL_AST_REQ_HEADERS, [<<"Language">>, <<"Stream-Response">>]).
-define(AST_REQ_VALUES, [{<<"Event-Category">>, <<"ast">>}
			 ,{<<"Event-Name">>, <<"ast_req">>}
			]).
-define(AST_REQ_TYPES, [{<<"Stream-Response">>, fun(V) -> is_boolean(wh_util:to_boolean(V)) end}
		       ]).

%% Ast Response
-define(AST_RESP_HEADERS, [<<"Response-Text">>]).
-define(OPTIONAL_AST_RESP_HEADERS, []).
-define(AST_RESP_VALUES, [{<<"Event-Category">>, <<"ast">>}
			  ,{<<"Event-Name">>, <<"ast_resp">>}
			 ]).
-define(AST_RESP_TYPES, []).

%% Ast Error
-define(AST_ERROR_HEADERS, [<<"Error-Code">>, <<"Error-Msg">>]).
-define(OPTIONAL_AST_ERROR_HEADERS, []).
-define(AST_ERROR_VALUES, [{<<"Event-Category">>, <<"ast">>}
			   ,{<<"Event-Name">>, <<"ast_error">>}
			  ]).
-define(AST_ERROR_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Request ast - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (json_object() | proplist()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?AST_REQ_HEADERS, ?OPTIONAL_AST_REQ_HEADERS);
	false -> {error, "Proplist failed validation for ast_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (json_object() | proplist()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AST_REQ_HEADERS, ?AST_REQ_VALUES, ?AST_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Response with ast - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (json_object() | proplist()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?AST_RESP_HEADERS, ?OPTIONAL_AST_RESP_HEADERS);
	false -> {error, "Proplist failed validation for ast_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (proplist() | json_object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AST_RESP_HEADERS, ?AST_RESP_VALUES, ?AST_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Ast error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
	true -> wh_api:build_message(Prop, ?AST_ERROR_HEADERS, ?OPTIONAL_AST_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for ast_error"}
    end;
error(JObj) ->
    error(wh_json:to_proplist(JObj)).

-spec error_v/1 :: (proplist() | json_object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AST_ERROR_HEADERS, ?AST_ERROR_VALUES, ?AST_ERROR_TYPES);
error_v(JObj) ->
    error_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:callctl_exchange(),
    amqp_util:bind_q_to_callctl(Queue),
    ok.

-spec unbind_q/1 :: (binary()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_callctk(Queue).

-spec publish_req/1 :: (iolist()) -> 'ok'.
-spec publish_req/2 :: (iolist(), ne_binary()) -> 'ok'.
publish_req(JSON) ->
    publish_req(JSON, ?DEFAULT_CONTENT_TYPE).
publish_req(Payload, ContentType) ->
    amqp_util:callctl_publish(Payload, ContentType).

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
