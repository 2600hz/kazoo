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

-export([originate_req/1, originate_req_v/1]).
-export([publish_originate_req/1, publish_originate_req/2]).
-export([bind_q/2]).
-export([unbind_q/1, unbind_q/2]).
 
-include("../wh_api.hrl").

-define(ORIGINATE_REQ_HEADERS, [<<"Endpoints">>, <<"Application-Name">>]).
-define(OPTIONAL_ORIGINATE_REQ_HEADERS, [<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
                                             ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
                                             ,<<"Outgoing-Callee-ID-Name">>, <<"Outgoing-Callee-ID-Number">>
                                             ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                             ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                             ,<<"Ringback">>, <<"Dial-Endpoint-Method">>
                                             ,<<"Media">>, <<"Hold-Media">>, <<"SIP-Headers">>
                                             ,<<"Custom-Channel-Vars">>, <<"Application-Data">>
                                        ]).
-define(ORIGINATE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_req">>}
                               ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                               ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                               ,{<<"Continue-On-Fail">>, [<<"true">>, <<"false">>]}
                               ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>, <<"fax">>]}
                              ]).
-define(ORIGINATE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                              ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                              ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
                             ]).

%% Originate Endpoints
-define(ORIGINATE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_ORIGINATE_REQ_ENDPOINT_HEADERS, [<<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
                                                      ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
                                                      ,<<"Outgoing-Callee-ID-Name">>, <<"Outgoing-Callee-ID-Number">>
                                                      ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                                      ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                                      ,<<"Ignore-Early-Media">>, <<"Bypass-Media">>, <<"Hold-Media">>
                                                      ,<<"Endpoint-Timeout">>, <<"Endpoint-Progress-Timeout">>
                                                      ,<<"Endpoint-Delay">>, <<"Codecs">>, <<"SIP-Headers">>, <<"Presence-ID">>
                                                      ,<<"Custom-Channel-Vars">>, <<"Auth-User">>, <<"Auth-Password">>
                                                      ,<<"Endpoint-Type">>, <<"Endpoint-Options">>
                                                 ]).
-define(ORIGINATE_REQ_ENDPOINT_VALUES, [{<<"Ignore-Early-Media">>, [<<"true">>, <<"false">>]}
                                        ,{<<"Bypass-Media">>, [<<"true">>, <<"false">>]}
                                        ,{<<"Endpoint-Type">>, [<<"sip">>, <<"freetdm">>]}
                                       ]).
-define(ORIGINATE_REQ_ENDPOINT_TYPES, [{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                                       ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
                                       ,{<<"Endpoint-Options">>, ?IS_JSON_OBJECT}
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
-spec originate_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', iolist()} | {'error', string()}.
originate_req(Prop) when is_list(Prop) ->
    case originate_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?ORIGINATE_REQ_HEADERS, ?OPTIONAL_ORIGINATE_REQ_HEADERS);
        false -> {error, "Proplist failed validation for originate request"}
    end;
originate_req(JObj) ->
    originate_req(wh_json:to_proplist(JObj)).

-spec originate_req_v/1 :: (proplist() | wh_json:json_object()) -> boolean().
originate_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ORIGINATE_REQ_HEADERS, ?ORIGINATE_REQ_VALUES, ?ORIGINATE_REQ_TYPES);
originate_req_v(JObj) ->
    originate_req_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Prop) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Queue, ?KEY_RESOURCE_REQ).

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
-spec unbind_q/2 :: (ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue) ->
    unbind_q(Queue, []).
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_callmgr(Queue, ?KEY_RESOURCE_REQ).

-spec publish_originate_req/1 :: (api_terms()) -> 'ok'.
-spec publish_originate_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_originate_req(JObj) ->
    publish_originate_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_originate_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?ORIGINATE_REQ_VALUES, fun ?MODULE:originate_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RESOURCE_REQ).
