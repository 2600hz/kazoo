%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% ASR requests, responses, and errors
%%% @end
%%% Created : 17 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_asr).

-compile({no_auto_import, [error/1]}).

-export([req/1, resp/1, error/1, req_v/1, resp_v/1, error_v/1]).

-export([bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3
	 ,publish_error/2, publish_error/3]).

-include("../wh_api.hrl").

%% ASR Request - when Speech to text is desired
-define(ASR_REQ_HEADERS, [<<"ASR-Endpoint">>, <<"ASR-Account-ID">>, <<"ASR-Account-Password">>, <<"Call-ID">>
			      ,<<"Control-Queue">>
			 ]).
-define(OPTIONAL_ASR_REQ_HEADERS, [<<"Language">>, <<"Stream-Response">>]).
-define(ASR_REQ_VALUES, [{<<"Event-Category">>, <<"asr">>}
			 ,{<<"Event-Name">>, <<"req">>}
			]).
-define(ASR_REQ_TYPES, [{<<"Stream-Response">>, fun(V) -> is_boolean(wh_util:to_boolean(V)) end}
		       ]).

%% Asr Response
-define(ASR_RESP_HEADERS, [<<"Response-Text">>]).
-define(OPTIONAL_ASR_RESP_HEADERS, []).
-define(ASR_RESP_VALUES, [{<<"Event-Category">>, <<"asr">>}
			  ,{<<"Event-Name">>, <<"resp">>}
			 ]).
-define(ASR_RESP_TYPES, []).

%% Asr Error
-define(ASR_ERROR_HEADERS, [<<"Error-Code">>, <<"Error-Msg">>]).
-define(OPTIONAL_ASR_ERROR_HEADERS, []).
-define(ASR_ERROR_VALUES, [{<<"Event-Category">>, <<"asr">>}
			   ,{<<"Event-Name">>, <<"error">>}
			  ]).
-define(ASR_ERROR_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Request asr - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?ASR_REQ_HEADERS, ?OPTIONAL_ASR_REQ_HEADERS);
	false -> {error, "Proplist failed validation for asr_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ASR_REQ_HEADERS, ?ASR_REQ_VALUES, ?ASR_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Response with asr - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?ASR_RESP_HEADERS, ?OPTIONAL_ASR_RESP_HEADERS);
	false -> {error, "Proplist failed validation for asr_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (proplist() | json_object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ASR_RESP_HEADERS, ?ASR_RESP_VALUES, ?ASR_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Asr error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
	true -> wh_api:build_message(Prop, ?ASR_ERROR_HEADERS, ?OPTIONAL_ASR_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for asr_error"}
    end;
error(JObj) ->
    error(wh_json:to_proplist(JObj)).

-spec error_v/1 :: (proplist() | json_object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ASR_ERROR_HEADERS, ?ASR_ERROR_VALUES, ?ASR_ERROR_TYPES);
error_v(JObj) ->
    error_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% bind to a queue to the asr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:callctl_exchange(),
    amqp_util:bind_q_to_callctl(Queue, ?KEY_ASR_REQ),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% unbind to a queue to the asr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec unbind_q/1 :: (binary()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_callctk(Queue).

%%--------------------------------------------------------------------
%% @doc
%% prepare and publish an asr request
%% @end
%%--------------------------------------------------------------------
-spec publish_req/1 :: (api_terms()) -> 'ok'.
-spec publish_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?ASR_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callctl_publish(?KEY_ASR_REQ, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% prepare and publish an asr response
%% @end
%%--------------------------------------------------------------------
-spec publish_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?ASR_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% prepare and publish an asr error
%% @end
%%--------------------------------------------------------------------
-spec publish_error/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_error/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_error(Queue, Error, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Error, ?ASR_ERROR_VALUES, fun ?MODULE:error/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
