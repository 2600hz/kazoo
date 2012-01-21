%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% Created : 14 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_authz).

-export([req/1, resp/1, req_v/1, resp_v/1, win/1, win_v/1, bind_q/2, unbind_q/1]).

-export([publish_req/1, publish_req/2, publish_resp/2, publish_resp/3, publish_win/2, publish_win/3]).

-export([get_auth_realm/1]).

-include("../wh_api.hrl").

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>
                                ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                ,<<"Request">>
                           ]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>]).
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

%% Authorization Requests
-define(AUTHZ_WIN_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_AUTHZ_WIN_HEADERS, []).
-define(AUTHZ_WIN_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
                           ,{<<"Event-Name">>, <<"authz_win">>}
                          ]).
-define(AUTHZ_WIN_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
        case req_v(Prop) of
            true -> wh_api:build_message(Prop, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
            false -> {error, "Proplist failed validation for authz_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_REQ_HEADERS, ?AUTHZ_REQ_VALUES, ?AUTHZ_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
        false -> {error, "Proplist failed validation for authz_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Win - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec win/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
win(Prop) when is_list(Prop) ->
        case win_v(Prop) of
            true -> wh_api:build_message(Prop, ?AUTHZ_WIN_HEADERS, ?OPTIONAL_AUTHZ_WIN_HEADERS);
            false -> {error, "Proplist failed validation for authz_win"}
    end;
win(JObj) ->
    win(wh_json:to_proplist(JObj)).

-spec win_v/1 :: (api_terms()) -> boolean().
win_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_WIN_HEADERS, ?AUTHZ_WIN_VALUES, ?AUTHZ_WIN_TYPES);
win_v(JObj) ->
    win_v(wh_json:to_proplist(JObj)).

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
-spec publish_req/1 :: (api_terms()) -> 'ok'.
-spec publish_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?AUTHZ_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ).

-spec publish_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_win/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_win/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_win(Queue, JObj) ->
    publish_win(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_win(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_WIN_VALUES, fun ?MODULE:win/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_realm/1  :: (wh_json:json_object()) -> ne_binary().
get_auth_realm(ApiJObj) ->
    [_ReqUser, ReqDomain] = binary:split(wh_json:get_value(<<"Request">>, ApiJObj), <<"@">>),
    ReqDomain.
