%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handles authentication requests, responses, queue bindings
%%% @end
%%% Created : 14 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_authn).

-export([req/1, resp/1, req_v/1, resp_v/1, bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3]).

-include("../wh_api.hrl").

-define(AUTHN_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
				, <<"Auth-User">>, <<"Auth-Realm">>]).
-define(OPTIONAL_AUTHN_REQ_HEADERS, [<<"Method">>]).
-define(AUTHN_REQ_VALUES, [{<<"Event-Category">>, <<"directory">>}
			   ,{<<"Event-Name">>, <<"authn_req">>}
			  ]).
-define(AUTHN_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"To">>, fun is_binary/1}
			  ,{<<"From">>, fun is_binary/1}
			  ,{<<"Orig-IP">>, fun is_binary/1}
			  ,{<<"Auth-User">>, fun is_binary/1}
			  ,{<<"Auth-Realm">>, fun is_binary/1}
			 ]).

%% Authentication Responses
-define(AUTHN_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Password">>]).
-define(OPTIONAL_AUTHN_RESP_HEADERS, [<<"Tenant-ID">>, <<"Access-Group">>, <<"Custom-Channel-Vars">>]).
-define(AUTHN_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
			   ,{<<"Event-Name">>, <<"authn_resp">>}
			   ,{<<"Auth-Method">>, [<<"password">>, <<"ip">>, <<"a1-hash">>, <<"error">>]}
			 ]).
-define(AUTHN_RESP_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"Auth-Password">>, fun is_binary/1}
			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
			  ,{<<"Access-Group">>, fun is_binary/1}
			  ,{<<"Tenant-ID">>, fun is_binary/1}
			 ]).

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
        case req_v(Prop) of
	    true -> wh_api:build_message(Prop, ?AUTHN_REQ_HEADERS, ?OPTIONAL_AUTHN_REQ_HEADERS);
	    false -> {error, "Proplist failed validation for authn_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHN_REQ_HEADERS, ?AUTHN_REQ_VALUES, ?AUTHN_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?AUTHN_RESP_HEADERS, ?OPTIONAL_AUTHN_RESP_HEADERS);
	false -> {error, "Proplist failed validation for authn_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHN_RESP_HEADERS, ?AUTHN_RESP_VALUES, ?AUTHN_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHN_REQ),
    ok.

-spec unbind_q/1 :: (binary()) -> 'ok'.
unbind_q(Q) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHN_REQ).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_req/1 :: (api_terms()) -> 'ok'.
-spec publish_req/2 :: (api_terms(), binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?AUTHN_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHN_REQ).

-spec publish_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), api_terms(), binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHN_RESP_VALUES, fun resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
