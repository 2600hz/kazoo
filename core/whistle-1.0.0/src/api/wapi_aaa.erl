%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Handles AAA (authentication, authorization, accounting) requests, responses, queue bindings
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(wapi_aaa).

-export([req/1, req_v/1
         ,resp/1, resp_v/1
         ,custom_req/1, custom_req_v/1
         ,custom_resp/1, custom_resp_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_req/1, publish_req/2
         ,publish_resp/2, publish_resp/3
         ,publish_custom_req/1, publish_custom_req/2
         ,publish_custom_resp/2, publish_custom_resp/3
         ,get_auth_user/1, get_auth_realm/1
         ,req_event_type/0, resp_event_type/0
         ,custom_req_event_type/0, custom_resp_event_type/0
         ,get_authn_req_routing/1]).

-include_lib("whistle/include/wh_api.hrl").

-define(KEY_AUTHN_REQ, <<"aaa.authn.req">>). %% corresponds to the aaa_authn_req/1 api call

-define(EVENT_CATEGORY, <<"aaa">>).
-define(AUTHN_REQ_EVENT_NAME, <<"aaa_authn_req">>).
-define(AUTHN_RESP_EVENT_NAME, <<"aaa_authn_resp">>).
-define(CUSTOM_REQ_EVENT_NAME, <<"aaa_custom_req">>).
-define(CUSTOM_RESP_EVENT_NAME, <<"aaa_custom_resp">>).

-define(AAA_QUEUE, <<"circlemaker_listener">>). %% correspond to the cm_listener:?QUEUE_NAME

% Authn Request headers

-define(AUTHN_REQ_HEADERS, [<<"To">>, <<"From">>
                            ,<<"Auth-User">>, <<"Auth-Realm">>
                            ,<<"Method">>
                           ]).
-define(OPTIONAL_AUTHN_REQ_HEADERS, [<<"Orig-IP">>, <<"Orig-Port">>, <<"Call-ID">>
                                     ,<<"Switch-Hostname">>
                                     ,<<"NAS-IP-Address">>, <<"NAS-Port">>
                                     ,<<"Auth-Nonce">>, <<"Auth-Response">>
                                     ,<<"User-Agent">>, <<"Expires">>
                                     ,<<"Custom-SIP-Headers">>, <<"Account-ID">>
                                     ,<<"User-Name">>, <<"User-Password">>
                                     ,<<"Response-Queue">>, <<"AAA-Result">>
                                    ]).
-define(AUTHN_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?AUTHN_REQ_EVENT_NAME}
                          ]).
-define(AUTHN_REQ_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Orig-IP">>, fun is_binary/1}
                          ,{<<"Orig-Port">>, fun is_binary/1}
                          ,{<<"Auth-User">>, fun is_binary/1}
                          ,{<<"Auth-Realm">>, fun is_binary/1}
                          ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                         ]).

% Authn Response headers

-define(AUTHN_RESP_HEADERS, [<<"Auth-Password">>, <<"Response-Queue">>, <<"AAA-Result">>]).
-define(OPTIONAL_AUTHN_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                      ,<<"Auth-Username">>, <<"Auth-Nonce">>
                                      , <<"NAS-IP-Address">>, <<"NAS-Port">>
                                      ,<<"Access-Group">>, <<"Tenant-ID">>, <<"Expires">>
                                      ,<<"Suppress-Unregister-Notifications">>
                                      ,<<"Register-Overwrite-Notify">>
                                      ,<<"User-Name">>, <<"User-Password">>
                                     ]).
-define(AUTHN_RESP_VALUES, [{<<"Event-Category">>, <<"aaa">>}
                           ,{<<"Event-Name">>, <<"aaa_authn_resp">>}
                           ]).
-define(AUTHN_RESP_TYPES, [{<<"Auth-Password">>, fun is_binary/1}
                           ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                           ,{<<"Access-Group">>, fun is_binary/1}
                           ,{<<"Tenant-ID">>, fun is_binary/1}
                           ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                          ]).

%% Authn Failure Response headers

-define(AUTHN_ERR_HEADERS, []).
-define(OPTIONAL_AUTHN_ERR_HEADERS, []).
-define(AUTHN_ERR_VALUES, [{<<"Event-Category">>, <<"aaa">>}
                            ,{<<"Event-Name">>, <<"aaa_authn_err">>}
                         ]).
-define(AUTHN_ERR_TYPES, []).

% Custom Request headers

-define(CUSTOM_REQ_HEADERS, [<<"Custom-Msg">>]).
-define(OPTIONAL_CUSTOM_REQ_HEADERS, [<<"Response-Queue">>, <<"Server-ID">>]).
-define(CUSTOM_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                             ,{<<"Event-Name">>, ?CUSTOM_REQ_EVENT_NAME}
                           ]).
-define(CUSTOM_REQ_TYPES, [{<<"Response-Queue">>, fun is_binary/1}
                           ,{<<"Server-ID">>, fun is_binary/1}
                           ,{<<"Custom-Msg">>, fun wh_json:is_json_object/1}
                          ]).

% Custom Response headers

-define(CUSTOM_RESP_HEADERS, ?CUSTOM_REQ_HEADERS).
-define(OPTIONAL_CUSTOM_RESP_HEADERS, ?OPTIONAL_CUSTOM_REQ_HEADERS).
-define(CUSTOM_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                             ,{<<"Event-Name">>, ?CUSTOM_RESP_EVENT_NAME}
                            ]).
-define(CUSTOM_RESP_TYPES, ?CUSTOM_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
        case req_v(Prop) of
            true -> wh_api:build_message(Prop, ?AUTHN_REQ_HEADERS, ?OPTIONAL_AUTHN_REQ_HEADERS);
            false -> {error, "Proplist failed validation for AAA authn_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHN_REQ_HEADERS, ?AUTHN_REQ_VALUES, ?AUTHN_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

-spec req_event_type() -> {ne_binary(), ne_binary()}.
req_event_type() ->
    {?EVENT_CATEGORY, ?AUTHN_REQ_EVENT_NAME}.

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?AUTHN_RESP_HEADERS, ?OPTIONAL_AUTHN_RESP_HEADERS);
        false -> {error, "Proplist failed validation for AAA authn_resp"}
    end;
resp(JObj) ->
    resp(wh_json:to_proplist(JObj)).

-spec resp_v(api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHN_RESP_HEADERS, ?AUTHN_RESP_VALUES, ?AUTHN_RESP_TYPES);
resp_v(JObj) ->
    resp_v(wh_json:to_proplist(JObj)).

-spec resp_event_type() -> {ne_binary(), ne_binary()}.
resp_event_type() ->
    {?EVENT_CATEGORY, ?AUTHN_RESP_EVENT_NAME}.

%%--------------------------------------------------------------------
%% @doc Custom AAA Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec custom_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
custom_req(Prop) when is_list(Prop) ->
    case custom_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?CUSTOM_REQ_HEADERS, ?OPTIONAL_CUSTOM_REQ_HEADERS);
        false -> {error, "Proplist failed validation for aaa_custom_req"}
    end;
custom_req(JObj) ->
    custom_req(wh_json:to_proplist(JObj)).

-spec custom_req_v(api_terms()) -> boolean().
custom_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CUSTOM_REQ_HEADERS, ?CUSTOM_REQ_VALUES, ?CUSTOM_REQ_TYPES);
custom_req_v(JObj) ->
    custom_req_v(wh_json:to_proplist(JObj)).

-spec custom_req_event_type() -> {ne_binary(), ne_binary()}.
custom_req_event_type() ->
    {?EVENT_CATEGORY, ?CUSTOM_REQ_EVENT_NAME}.

%%--------------------------------------------------------------------
%% @doc Custom AAA Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec custom_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
custom_resp(Prop) when is_list(Prop) ->
    case custom_resp_v(Prop) of
        true -> wh_api:build_message(Prop, ?CUSTOM_RESP_HEADERS, ?OPTIONAL_CUSTOM_RESP_HEADERS);
        false -> {error, "Proplist failed validation for aaa_custom_resp"}
    end;
custom_resp(JObj) ->
    custom_resp(wh_json:to_proplist(JObj)).

-spec custom_resp_v(api_terms()) -> boolean().
custom_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CUSTOM_RESP_HEADERS, ?CUSTOM_RESP_VALUES, ?CUSTOM_RESP_TYPES);
custom_resp_v(JObj) ->
    custom_resp_v(wh_json:to_proplist(JObj)).

-spec custom_resp_event_type() -> {ne_binary(), ne_binary()}.
custom_resp_event_type() ->
    {?EVENT_CATEGORY, ?CUSTOM_RESP_EVENT_NAME}.

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:bind_q_to_targeted(Q, ?KEY_AUTHN_REQ).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_targeted(Q).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:targeted_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON Authn AAA Message
%% @end
%%--------------------------------------------------------------------
-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?AUTHN_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:targeted_publish(?AAA_QUEUE, Payload, ContentType).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?AUTHN_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc Publish the JSON Custom AAA Message
%% @end
%%--------------------------------------------------------------------
-spec publish_custom_req(api_terms()) -> 'ok'.
-spec publish_custom_req(api_terms(), binary()) -> 'ok'.
publish_custom_req(JObj) ->
    publish_custom_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_custom_req(Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?CUSTOM_REQ_VALUES, fun ?MODULE:custom_req/1),
    amqp_util:targeted_publish(?AAA_QUEUE, Payload, ContentType).

-spec publish_custom_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_custom_resp(ne_binary(), api_terms(), binary()) -> 'ok'.
publish_custom_resp(Queue, JObj) ->
    publish_custom_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_custom_resp(Queue, Resp, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Resp, ?CUSTOM_RESP_VALUES, fun ?MODULE:custom_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% creating the routing key for either binding queues or publishing messages
%% @end
%%-----------------------------------------------------------------------------
-spec get_authn_req_routing(ne_binary() | api_terms()) -> ne_binary().
get_authn_req_routing(Realm) when is_binary(Realm) ->
    list_to_binary([?KEY_AUTHN_REQ, ".", amqp_util:encode(Realm)]);
get_authn_req_routing(Req) ->
    get_authn_req_routing(get_auth_realm(Req)).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth user from the API request
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_user(wh_json:object()) -> api_binary().
get_auth_user(ApiJObj) ->
    case wh_json:get_value(<<"Auth-User">>, ApiJObj) of
        'undefined' -> 'undefined';
         Username -> wh_util:to_lower_binary(Username)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_realm(wh_json:object() | wh_proplist()) -> ne_binary().
get_auth_realm(ApiProp) when is_list(ApiProp) ->
    AuthRealm = props:get_value(<<"Auth-Realm">>, ApiProp, <<"missing.realm">>),
    case wh_network_utils:is_ipv4(AuthRealm)
        orelse wh_network_utils:is_ipv6(AuthRealm)
    of
        'false' -> wh_util:to_lower_binary(AuthRealm);
        'true' ->
            [_ToUser, ToDomain] = binary:split(props:get_value(<<"To">>, ApiProp), <<"@">>),
            wh_util:to_lower_binary(ToDomain)
    end;
get_auth_realm(ApiJObj) -> get_auth_realm(wh_json:to_proplist(ApiJObj)).
