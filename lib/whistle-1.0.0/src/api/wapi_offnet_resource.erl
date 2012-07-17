%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_offnet_resource).

-export([req/1, req_v/1]).
-export([resp/1, resp_v/1]).
-export([publish_req/1, publish_req/2]).
-export([publish_resp/2, publish_resp/3]).
-export([bind_q/2]).
-export([unbind_q/1, unbind_q/2]).

-include("../wh_api.hrl").

%% Offnet Resource Request
-define(OFFNET_RESOURCE_REQ_HEADERS, [<<"Resource-Type">>, <<"To-DID">>
                                          ,<<"Application-Name">>
                                     ]).
-define(OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS, [<<"Timeout">>, <<"Ignore-Early-Media">>, <<"Flags">>, <<"Media">>
                                                   ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
                                                   ,<<"Emergency-Caller-ID-Name">>, <<"Emergency-Caller-ID-Number">>
                                                   ,<<"Ringback">>, <<"SIP-Headers">>, <<"Custom-Channel-Vars">>
                                                   ,<<"Hold-Media">>, <<"Presence-ID">>, <<"Account-Realm">>
                                                   ,<<"Control-Queue">>, <<"Call-ID">>, <<"Application-Data">>
                                                   ,<<"Account-ID">>
                                              ]).
-define(OFFNET_RESOURCE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                     ,{<<"Event-Name">>, <<"offnet_req">>}
                                     ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>, <<"originate">>]}
                                     ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>, <<"fax">>]}
                                     ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                                    ]).
-define(OFFNET_RESOURCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                                    ,{<<"Account-ID">>, fun is_binary/1}
                                    ,{<<"Control-Queue">>, fun is_binary/1}
                                    ,{<<"To-DID">>, fun is_binary/1}
                                    ,{<<"SIP-Headers">>, fun wh_json:is_json_object/1}
                                    ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                                    ,{<<"Flags">>, fun is_list/1}
                                   ]).

%% Offnet Resource Response
-define(OFFNET_RESOURCE_RESP_HEADERS, [<<"Response-Message">>]).
-define(OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS, [<<"Error-Message">>, <<"Response-Code">>
                                                    ,<<"Call-ID">>, <<"Resource-Response">>
                                               ]).
-define(OFFNET_RESOURCE_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
                                      ,{<<"Event-Name">>, <<"offnet_resp">>}
                                     ]).
-define(OFFNET_RESOURCE_RESP_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        true -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
        false -> {error, "Proplist failed validation for offnet_resource_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_RESP_HEADERS);
        false -> {error, "Proplist failed validation for offnet_resource_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v/1 :: (api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?OFFNET_RESOURCE_RESP_HEADERS, ?OFFNET_RESOURCE_RESP_VALUES, ?OFFNET_RESOURCE_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    _ = amqp_util:resource_exchange(),
    amqp_util:bind_q_to_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_resource(Queue, ?KEY_OFFNET_RESOURCE_REQ).

-spec publish_req/1 :: (api_terms()) -> 'ok'.
-spec publish_req/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?OFFNET_RESOURCE_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:offnet_resource_publish(Payload, ContentType).

-spec publish_resp/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(TargetQ, JObj) ->
    publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(TargetQ, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?OFFNET_RESOURCE_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).
