%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Routing requests, responses, and wins!
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_sms).

-include_lib("whistle/include/wh_api.hrl").

-export([message/1, message_v/1
         ,delivery/1, delivery_v/1
         ,resume/1, resume_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_message/1, publish_message/2
         ,publish_delivery/1, publish_delivery/2
         ,publish_resume/1, publish_resume/2
        ]).

-define(SMS_EXCHANGE, <<"sms">>).
-define(EVENT_CATEGORY, <<"message">>).

-define(SMS_REQ_EVENT_NAME, <<"route">>).
-define(SMS_REQ_HEADERS, [<<"Call-ID">>, <<"Endpoints">>, <<"Application-Name">>]).
-define(OPTIONAL_SMS_REQ_HEADERS
        ,[<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
          ,<<"Application-Data">>, <<"Message-ID">>, <<"Body">>
          ,<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>
          ,<<"Outbound-Callee-ID-Name">>, <<"Outbound-Callee-ID-Number">>
          ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
          ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
          ,<<"From-User">>, <<"From-Realm">>, <<"From-URI">>
          ,<<"To-User">>, <<"To-Realm">>, <<"To-URI">>
          ,<<"Dial-Endpoint-Method">>
          ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
          ,<<"SIP-Transport">>, <<"SIP-Headers">>
          | wapi_dialplan:optional_bridge_req_headers()
         ]).
-define(SMS_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                         ,{<<"Event-Name">>, ?SMS_REQ_EVENT_NAME}
                         ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                         ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>]}
                         ,{<<"Application-Name">>, [<<"send">>]}
                        ]).
-define(SMS_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                        ,{<<"SIP-Headers">>, fun wh_json:is_json_object/1}
                        ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                        ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                        ,{<<"Continue-On-Fail">>, fun wh_util:is_boolean/1}
                       ]).
-define(SMS_ROUTING_KEY(CallId), <<"message.route.", (amqp_util:encode(CallId))/binary>>).

%% SMS Endpoints
-define(SMS_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_SMS_REQ_ENDPOINT_HEADERS, wapi_dialplan:optional_bridge_req_endpoint_headers()).
-define(SMS_REQ_ENDPOINT_VALUES, [{<<"Endpoint-Type">>
                                   ,[<<"sip">>, <<"xmpp">>, <<"smpp">>, <<"http">>]}
                                 ]).
-define(SMS_REQ_ENDPOINT_TYPES, [{<<"SIP-Headers">>, fun wh_json:is_json_object/1}
                                 ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                                 ,{<<"Endpoint-Options">>, fun wh_json:is_json_object/1}
                                ]).

%% Delivery
-define(DELIVERY_REQ_EVENT_NAME, <<"delivery">>).
-define(DELIVERY_HEADERS, [<<"Call-ID">>, <<"Message-ID">>]).
-define(OPTIONAL_DELIVERY_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>
                                    ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                    ,<<"From-Network-Addr">>
                                    ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                    ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                    ,<<"Contact">>, <<"User-Agent">>
                                    ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                    ,<<"To">>, <<"From">>, <<"Request">>
                                    ,<<"Body">>, <<"Account-ID">>
                                    ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                   ]).
-define(DELIVERY_TYPES, [{<<"To">>, fun is_binary/1}
                         ,{<<"From">>, fun is_binary/1}
                         ,{<<"Request">>, fun is_binary/1}
                         ,{<<"Message-ID">>, fun is_binary/1}
                         ,{<<"Event-Queue">>, fun is_binary/1}
                         ,{<<"Caller-ID-Name">>, fun is_binary/1}
                         ,{<<"Caller-ID-Number">>, fun is_binary/1}
                         ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                         ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                        ]).
-define(DELIVERY_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, ?DELIVERY_REQ_EVENT_NAME}
                             ]).
-define(DELIVERY_ROUTING_KEY(CallId), <<"message.delivery.", (amqp_util:encode(CallId))/binary>>).

%% SMS Resume
-define(RESUME_REQ_EVENT_NAME, <<"resume">>).
-define(RESUME_REQ_HEADERS, [<<"SMS-ID">>]).
-define(OPTIONAL_RESUME_REQ_HEADERS, []).
-define(RESUME_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, ?RESUME_REQ_EVENT_NAME}
                           ]).
-define(RESUME_REQ_TYPES, []).
-define(RESUME_ROUTING_KEY(CallId), <<"message.resume.", (amqp_util:encode(CallId))/binary>>).

-spec message(api_terms()) -> api_formatter_return().
message(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = message_endpoint_headers(EP),
               wh_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              message_endpoint_v(EP)
          ],
    Prop1 = [{<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case message_v(Prop1) of
        'true' -> wh_api:build_message(Prop1, ?SMS_REQ_HEADERS, ?OPTIONAL_SMS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for message"}
    end;
message(JObj) ->
    message(wh_json:to_proplist(JObj)).

-spec message_v(api_terms()) -> boolean().
message_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SMS_REQ_HEADERS, ?SMS_REQ_VALUES, ?SMS_REQ_TYPES);
message_v(JObj) ->
    message_v(wh_json:to_proplist(JObj)).

-spec message_endpoint_headers(api_terms()) -> {'ok', wh_proplist()} |
                                               {'error', string()}.
message_endpoint_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?SMS_REQ_ENDPOINT_HEADERS, ?OPTIONAL_SMS_REQ_ENDPOINT_HEADERS);
message_endpoint_headers(JObj) ->
    message_endpoint_headers(wh_json:to_proplist(JObj)).

-spec message_endpoint_v(api_terms()) -> boolean().
message_endpoint_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?SMS_REQ_ENDPOINT_HEADERS, ?SMS_REQ_ENDPOINT_VALUES, ?SMS_REQ_ENDPOINT_TYPES);
message_endpoint_v(JObj) ->
    message_endpoint_v(wh_json:to_proplist(JObj)).

-spec delivery(api_terms()) -> {'ok', iolist()} |
                               {'error', string()}.
delivery(Prop) when is_list(Prop) ->
    case delivery_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?DELIVERY_HEADERS, ?OPTIONAL_DELIVERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_delivery"}
    end;
delivery(JObj) -> delivery(wh_json:to_proplist(JObj)).

-spec delivery_v(api_terms()) -> boolean().
delivery_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DELIVERY_HEADERS, ?DELIVERY_REQ_VALUES, ?DELIVERY_TYPES);
delivery_v(JObj) -> delivery_v(wh_json:to_proplist(JObj)).


-spec resume(api_terms()) -> {'ok', iolist()} |
                             {'error', string()}.
resume(Prop) when is_list(Prop) ->
    case resume_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RESUME_REQ_HEADERS, ?OPTIONAL_RESUME_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_delivery"}
    end;
resume(JObj) -> resume(wh_json:to_proplist(JObj)).

-spec resume_v(api_terms()) -> boolean().
resume_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESUME_REQ_HEADERS, ?RESUME_REQ_VALUES, ?RESUME_REQ_TYPES);
resume_v(JObj) -> resume_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('call_id', Props, <<"*">>),
    bind_q(Queue, CallId, props:get_value('restrict_to', Props)).

bind_q(Queue, CallId, 'undefined') ->
    amqp_util:bind_q_to_exchange(Queue, ?SMS_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    amqp_util:bind_q_to_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    amqp_util:bind_q_to_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), ?SMS_EXCHANGE);
bind_q(Queue, CallId, ['routing'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, ?SMS_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    bind_q(Queue, CallId, Restrict);
bind_q(Queue, CallId, ['delivery'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    bind_q(Queue, CallId, Restrict);
bind_q(Queue, CallId, ['resume'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    bind_q(Queue, CallId, Restrict);
bind_q(_, _, []) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('call_id', Props, <<"*">>),
    unbind_q(Queue, CallId, props:get_value('restrict_to', Props)).

unbind_q(Queue, CallId, 'undefined') ->
    amqp_util:unbind_q_from_exchange(Queue, ?SMS_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    amqp_util:unbind_q_from_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    amqp_util:unbind_q_from_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), ?SMS_EXCHANGE);
unbind_q(Queue, CallId, ['routing'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, ?SMS_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    unbind_q(Queue, CallId, Restrict);
unbind_q(Queue, CallId, ['delivery'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    unbind_q(Queue, CallId, Restrict);
unbind_q(Queue, CallId, ['resume'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), ?SMS_EXCHANGE),
    unbind_q(Queue, CallId, Restrict);
unbind_q(_, _, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?SMS_EXCHANGE, <<"fanout">>).

-spec publish_message(api_terms()) -> 'ok'.
-spec publish_message(api_terms(), binary()) -> 'ok'.
publish_message(JObj) ->
    publish_message(JObj, ?DEFAULT_CONTENT_TYPE).
publish_message(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?SMS_REQ_VALUES, fun message/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    amqp_util:basic_publish(?SMS_EXCHANGE, ?SMS_ROUTING_KEY(CallId), Payload, ContentType).

-spec publish_delivery(api_terms()) -> 'ok'.
-spec publish_delivery(api_terms(), binary()) -> 'ok'.
publish_delivery(JObj) ->
    publish_delivery(JObj, ?DEFAULT_CONTENT_TYPE).
publish_delivery(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?DELIVERY_REQ_VALUES, fun delivery/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    amqp_util:basic_publish(?SMS_EXCHANGE, ?DELIVERY_ROUTING_KEY(CallId), Payload, ContentType).

-spec publish_resume(api_terms() | ne_binary()) -> 'ok'.
-spec publish_resume(api_terms(), binary()) -> 'ok'.
publish_resume(SMS) when is_binary(SMS) ->
    Payload = [{<<"SMS-ID">>, SMS}
               | wh_api:default_headers(<<"API">>, <<"0.9.7">>)
              ],
    publish_resume(Payload, ?DEFAULT_CONTENT_TYPE);
publish_resume(JObj) ->
    publish_resume(JObj, ?DEFAULT_CONTENT_TYPE).
publish_resume(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RESUME_REQ_VALUES, fun resume/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    amqp_util:basic_publish(?SMS_EXCHANGE, ?RESUME_ROUTING_KEY(CallId), Payload, ContentType).
