%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_authz).

-export([req/1, req_v/1
         ,resp/1, resp_v/1
         ,identify_req/1, identify_req_v/1
         ,identify_resp/1, identify_resp_v/1
         ,win/1, win_v/1
         ,update/1, update_v/1
         ,bind_q/2, unbind_q/2
         ,publish_req/1, publish_req/2
         ,publish_resp/2, publish_resp/3
         ,publish_identify_req/1, publish_identify_req/2
         ,publish_identify_resp/2, publish_identify_resp/3
         ,publish_win/2, publish_win/3
         ,publish_update/1, publish_update/2
        ]).

-include_lib("wh_api.hrl").

-define(EVENT_CATEGORY, <<"authz">>).
-define(KEY_AUTHZ_REQ, <<"authz.req">>).
-define(KEY_AUTHZ_UPDATE, <<"authz.update">>).
-define(KEY_AUTHZ_CALL_COMMAND, <<"authz.call_command">>).
-define(KEY_AUTHZ_IDENT_REQ, <<"authz.ident_req">>).

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"To">>, <<"From">>, <<"Call-ID">>
                                ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                ,<<"Account-ID">>, <<"Request">>, <<"Usage">>
                                ,<<"Call-Direction">>
                           ]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>, <<"Switch-Hostname">>]).
-define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"req">>}
                          ]).
-define(AUTHZ_REQ_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Call-ID">>, fun is_binary/1}
                          ,{<<"Account-ID">>, fun is_binary/1}
                          ,{<<"Caller-ID-Name">>, fun is_binary/1}
                          ,{<<"Caller-ID-Number">>, fun is_binary/1}
                          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                          ,{<<"Usage">>, fun wh_json:is_json_object/1}
                         ]).

%% Authorization Responses
-define(AUTHZ_RESP_HEADERS, [<<"Call-ID">>, <<"Is-Authorized">>]).
-define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Type">>, <<"Global-Resource">>]).
-define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, <<"resp">>}
                            ,{<<"Type">>, [<<"flat_rate">>, <<"per_minute">>]}
                            ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
                            ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                           ]).
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Authorization Identify Requests
-define(AUTHZ_IDENT_REQ_HEADERS, [<<"To">>, <<"From">>, <<"Request">>, <<"Call-ID">>
                                      ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                 ]).
-define(OPTIONAL_AUTHZ_IDENT_REQ_HEADERS, [<<"Custom-Channel-Vars">>]).
-define(AUTHZ_IDENT_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                 ,{<<"Event-Name">>, <<"identify_req">>}
                                ]).
-define(AUTHZ_IDENT_REQ_TYPES, [{<<"To">>, fun is_binary/1}
                                ,{<<"From">>, fun is_binary/1}
                                ,{<<"Call-ID">>, fun is_binary/1}
                                ,{<<"Caller-ID-Name">>, fun is_binary/1}
                                ,{<<"Caller-ID-Number">>, fun is_binary/1}
                                ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                               ]).

%% Authorization Identify Responses
-define(AUTHZ_IDENT_RESP_HEADERS, [<<"Call-ID">>, <<"Account-ID">>]).
-define(OPTIONAL_AUTHZ_IDENT_RESP_HEADERS, [<<"Reseller-ID">>, <<"Global-Resource">>]).
-define(AUTHZ_IDENT_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                  ,{<<"Event-Name">>, <<"identify_resp">>}
                                  ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                                 ]).
-define(AUTHZ_IDENT_RESP_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                                 ,{<<"Account-ID">>, fun is_binary/1}
                                 ,{<<"Reseller-ID">>, fun is_binary/1}
                                ]).

%% Authorization Wins
-define(AUTHZ_WIN_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_AUTHZ_WIN_HEADERS, [<<"Switch-Hostname">>]).
-define(AUTHZ_WIN_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"win">>}
                          ]).
-define(AUTHZ_WIN_TYPES, []).

%% Authorization Update
-define(AUTHZ_UPDATE_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_AUTHZ_UPDATE_HEADERS, [<<"Handling-Server-Name">>, <<"Custom-Channel-Vars">>
                                            ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                            ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                            ,<<"Other-Leg-Call-ID">>, <<"Timestamp">>
                                            ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
                                            ,<<"Created-Time">>, <<"Answered-Time">>, <<"Progress-Time">>
                                            ,<<"Progress-Media-Time">>, <<"Hangup-Time">>
                                            ,<<"Transfer-Time">>
                                       ]).
-define(AUTHZ_UPDATE_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"update">>}
                              ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
                             ]).
-define(AUTHZ_UPDATE_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

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
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        true -> wh_api:build_message(Prop, ?AUTHZ_UPDATE_HEADERS, ?OPTIONAL_AUTHZ_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for authz_update"}
    end;
update(JObj) ->
    update(wh_json:to_proplist(JObj)).

-spec update_v/1 :: (api_terms()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_UPDATE_HEADERS, ?AUTHZ_UPDATE_VALUES, ?AUTHZ_UPDATE_TYPES);
update_v(JObj) ->
    update_v(wh_json:to_proplist(JObj)).

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
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec identify_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
identify_req(Prop) when is_list(Prop) ->
    case identify_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?AUTHZ_IDENT_REQ_HEADERS, ?OPTIONAL_AUTHZ_IDENT_REQ_HEADERS);
        false -> {error, "Proplist failed validation for authz_identify_req"}
    end;
identify_req(JObj) ->
    identify_req(wh_json:to_proplist(JObj)).

-spec identify_req_v/1 :: (api_terms()) -> boolean().
identify_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_IDENT_REQ_HEADERS, ?AUTHZ_IDENT_REQ_VALUES, ?AUTHZ_IDENT_REQ_TYPES);
identify_req_v(JObj) ->
    identify_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec identify_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
identify_resp(Prop) when is_list(Prop) ->
    case identify_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?AUTHZ_IDENT_RESP_HEADERS, ?OPTIONAL_AUTHZ_IDENT_RESP_HEADERS);
        false -> {error, "Proplist failed validation for authz_identify_resp"}
    end;
identify_resp(JObj) ->
    identify_resp(wh_json:to_proplist(JObj)).

-spec identify_resp_v/1 :: (api_terms()) -> boolean().
identify_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHZ_IDENT_RESP_HEADERS, ?AUTHZ_IDENT_RESP_VALUES, ?AUTHZ_IDENT_RESP_TYPES);
identify_resp_v(JObj) ->
    identify_resp_v(wh_json:to_proplist(JObj)).


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
-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.

bind_q(Queue, Props) ->
    amqp_util:callmgr_exchange(),
    bind_to_q(Queue, props:get_value(restrict_to, Props)).

bind_to_q(Q, undefined) ->
    ok = amqp_util:bind_q_to_callmgr(Q, <<"authz.*">>);
bind_to_q(Q, [reqs|T]) ->
    ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, [updates|T]) ->
    ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_UPDATE),
    bind_to_q(Q, T);
bind_to_q(Q, [call_command|T]) ->
    ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_CALL_COMMAND),
    bind_to_q(Q, T);
bind_to_q(Q, [identify|T]) ->
    ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_IDENT_REQ),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value(restrict_to, Props)).

unbind_q_from(Q, undefined) ->
    ok = amqp_util:unbind_q_from_callmgr(Q, <<"authz.*">>);
unbind_q_from(Q, [reqs|T]) ->
    ok = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, [updates|T]) ->
    ok = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_UPDATE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [call_command|T]) ->
    ok = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_CALL_COMMAND),
    unbind_q_from(Q, T);
unbind_q_from(Q, [identify|T]) ->
    ok = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_IDENT_REQ),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    ok.

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

-spec publish_update/1 :: (api_terms()) -> 'ok'.
-spec publish_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_update(JObj) ->
    publish_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Update, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Update, ?AUTHZ_UPDATE_VALUES, fun ?MODULE:update/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_UPDATE).

-spec publish_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_identify_req/1 :: (api_terms()) -> 'ok'.
-spec publish_identify_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_identify_req(JObj) ->
    publish_identify_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_identify_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?AUTHZ_IDENT_REQ_VALUES, fun ?MODULE:identify_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_IDENT_REQ).

-spec publish_identify_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_identify_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_identify_resp(Queue, JObj) ->
    publish_identify_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_identify_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_IDENT_RESP_VALUES, fun ?MODULE:identify_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_win/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_win/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_win(Queue, JObj) ->
    publish_win(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_win(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_WIN_VALUES, fun ?MODULE:win/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
