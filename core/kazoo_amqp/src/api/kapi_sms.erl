%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_sms).

-export([message/1, message_v/1
        ,delivery/1, delivery_v/1
        ,resume/1, resume_v/1
        ,inbound/1, inbound_v/1
        ,outbound/1, outbound_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_message/1, publish_message/2
        ,publish_delivery/1, publish_delivery/2
        ,publish_targeted_delivery/2, publish_targeted_delivery/3
        ,publish_resume/1, publish_resume/2
        ,publish_inbound/1, publish_inbound/2
        ,publish_outbound/1, publish_outbound/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(LOWER(X), kz_term:to_lower_binary(X)).

-define(SMS_EXCHANGE, <<"sms">>).
-define(EVENT_CATEGORY, <<"message">>).

-define(SMS_REQ_EVENT_NAME, <<"route">>).
-define(SMS_REQ_HEADERS, [<<"Call-ID">>, <<"Endpoints">>, <<"Application-Name">>, <<"Body">>]).
-define(OPTIONAL_SMS_REQ_HEADERS
       ,[<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
        ,<<"Application-Data">>, <<"Message-ID">>
        ,<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>
        ,<<"Outbound-Callee-ID-Name">>, <<"Outbound-Callee-ID-Number">>
        ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
        ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
        ,<<"From-User">>, <<"From-Realm">>, <<"From-URI">>
        ,<<"To-User">>, <<"To-Realm">>, <<"To-URI">>
        ,<<"Dial-Endpoint-Method">>
        ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
        ,<<"SIP-Transport">>, <<"SIP-Headers">>
        ,<<"Route-ID">>, <<"Route-Type">>
             | kapi_dialplan:optional_bridge_req_headers()
        ]).
-define(SMS_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                        ,{<<"Event-Name">>, ?SMS_REQ_EVENT_NAME}
                        ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                        ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>]}
                        ,{<<"Application-Name">>, [<<"send">>]}
                        ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                        ]).
-define(SMS_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                       ,{<<"SIP-Headers">>, fun kz_json:is_json_object/1}
                       ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                       ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                       ,{<<"Continue-On-Fail">>, fun kz_term:is_boolean/1}
                       ,{<<"Message-ID">>, fun is_binary/1}
                       ,{<<"Body">>, fun is_binary/1}
                       ]).
-define(SMS_ROUTING_KEY(RouteId, CallId)
       ,list_to_binary(["message.route."
                       ,kz_amqp_util:encode(RouteId), "."
                       ,kz_amqp_util:encode(CallId)
                       ])
       ).

%% SMS Endpoints
-define(SMS_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_SMS_REQ_ENDPOINT_HEADERS, kapi_dialplan:optional_bridge_req_endpoint_headers()).
-define(SMS_REQ_ENDPOINT_VALUES, [{<<"Endpoint-Type">>
                                  ,[<<"sip">>, <<"xmpp">>, <<"smpp">>, <<"http">>, <<"amqp">>]}
                                 ]).
-define(SMS_REQ_ENDPOINT_TYPES, [{<<"SIP-Headers">>, fun kz_json:is_json_object/1}
                                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                ,{<<"Endpoint-Options">>, fun kz_json:is_json_object/1}
                                ]).

%% Delivery
-define(DELIVERY_REQ_EVENT_NAME, <<"delivery">>).
-define(DELIVERY_HEADERS, [<<"Call-ID">>, <<"Message-ID">>]).
-define(OPTIONAL_DELIVERY_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Orig-Port">>
                                   ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                   ,<<"From-Network-Addr">>
                                   ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                   ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                   ,<<"Contact">>, <<"User-Agent">>
                                   ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                   ,<<"To">>, <<"From">>, <<"Request">>
                                   ,<<"Body">>, <<"Account-ID">>
                                   ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                   ,<<"Delivery-Result-Text">>, <<"Error-Code">>, <<"Error-Message">>
                                   ]).
-define(DELIVERY_TYPES, [{<<"To">>, fun is_binary/1}
                        ,{<<"From">>, fun is_binary/1}
                        ,{<<"Request">>, fun is_binary/1}
                        ,{<<"Message-ID">>, fun is_binary/1}
                        ,{<<"Event-Queue">>, fun is_binary/1}
                        ,{<<"Caller-ID-Name">>, fun is_binary/1}
                        ,{<<"Caller-ID-Number">>, fun is_binary/1}
                        ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                        ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                        ]).
-define(DELIVERY_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                             ,{<<"Event-Name">>, ?DELIVERY_REQ_EVENT_NAME}
                             ]).
-define(DELIVERY_ROUTING_KEY(CallId), <<"message.delivery.", (kz_amqp_util:encode(CallId))/binary>>).

%% SMS Resume
-define(RESUME_REQ_EVENT_NAME, <<"resume">>).
-define(RESUME_REQ_HEADERS, [<<"SMS-ID">>]).
-define(OPTIONAL_RESUME_REQ_HEADERS, []).
-define(RESUME_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?RESUME_REQ_EVENT_NAME}
                           ]).
-define(RESUME_REQ_TYPES, []).
-define(RESUME_ROUTING_KEY(CallId), <<"message.resume.", (kz_amqp_util:encode(CallId))/binary>>).

%% Inbound
-define(INBOUND_REQ_EVENT_NAME, <<"inbound">>).
-define(INBOUND_HEADERS, [<<"Message-ID">>, <<"Body">>, <<"Route-ID">>
                         ,<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                         ]).
-define(OPTIONAL_INBOUND_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Orig-Port">>
                                  ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                  ,<<"From-Network-Addr">>
                                  ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                  ,<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                  ,<<"Contact">>, <<"User-Agent">>
                                  ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                  ,<<"To">>, <<"From">>, <<"Request">>
                                  ,<<"Account-ID">>
                                  ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                  ,<<"Route-Type">>, <<"System-ID">>
                                  ]).
-define(INBOUND_TYPES, [{<<"To">>, fun is_binary/1}
                       ,{<<"From">>, fun is_binary/1}
                       ,{<<"Request">>, fun is_binary/1}
                       ,{<<"Message-ID">>, fun is_binary/1}
                       ,{<<"System-ID">>, fun is_binary/1}
                       ,{<<"Event-Queue">>, fun is_binary/1}
                       ,{<<"Caller-ID-Name">>, fun is_binary/1}
                       ,{<<"Caller-ID-Number">>, fun is_binary/1}
                       ,{<<"Callee-ID-Name">>, fun is_binary/1}
                       ,{<<"Callee-ID-Number">>, fun is_binary/1}
                       ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                       ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                       ,{<<"Body">>, fun is_binary/1}
                       ]).
-define(INBOUND_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, ?INBOUND_REQ_EVENT_NAME}
                            ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                            ]).
-define(INBOUND_ROUTING_KEY(RouteId, CallId), <<"message.inbound."
                                               ,(kz_amqp_util:encode(?LOWER(RouteId)))/binary, "."
                                               ,(kz_amqp_util:encode(CallId))/binary
                                              >>).

%% Outbound
-define(OUTBOUND_REQ_EVENT_NAME, <<"outbound">>).
-define(OUTBOUND_HEADERS, [<<"Message-ID">>, <<"Body">>, <<"Route-ID">>
                          ,<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                          ]).
-define(OPTIONAL_OUTBOUND_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Orig-Port">>
                                   ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                   ,<<"From-Network-Addr">>
                                   ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                   ,<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                   ,<<"Contact">>, <<"User-Agent">>
                                   ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                   ,<<"To">>, <<"From">>, <<"Request">>
                                   ,<<"Account-ID">>
                                   ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                   ,<<"Route-Type">>, <<"System-ID">>
                                   ]).
-define(OUTBOUND_TYPES, [{<<"To">>, fun is_binary/1}
                        ,{<<"From">>, fun is_binary/1}
                        ,{<<"Request">>, fun is_binary/1}
                        ,{<<"Message-ID">>, fun is_binary/1}
                        ,{<<"System-ID">>, fun is_binary/1}
                        ,{<<"Event-Queue">>, fun is_binary/1}
                        ,{<<"Caller-ID-Name">>, fun is_binary/1}
                        ,{<<"Caller-ID-Number">>, fun is_binary/1}
                        ,{<<"Callee-ID-Name">>, fun is_binary/1}
                        ,{<<"Callee-ID-Number">>, fun is_binary/1}
                        ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                        ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                        ,{<<"Body">>, fun is_binary/1}
                        ]).
-define(OUTBOUND_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                             ,{<<"Event-Name">>, ?OUTBOUND_REQ_EVENT_NAME}
                             ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                             ]).
-define(OUTBOUND_ROUTING_KEY(RouteId, CallId)
       ,list_to_binary(["message.outbound."
                       ,kz_amqp_util:encode(?LOWER(RouteId)), "."
                       ,kz_amqp_util:encode(CallId)
                       ])
       ).

-spec message(kz_term:api_terms()) -> api_formatter_return().
message(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = message_endpoint_headers(EP),
               kz_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              message_endpoint_v(EP)
          ],
    Prop1 = [{<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case message_v(Prop1) of
        'true' -> kz_api:build_message(Prop1, ?SMS_REQ_HEADERS, ?OPTIONAL_SMS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for message"}
    end;
message(JObj) ->
    message(kz_json:to_proplist(JObj)).

-spec message_v(kz_term:api_terms()) -> boolean().
message_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SMS_REQ_HEADERS, ?SMS_REQ_VALUES, ?SMS_REQ_TYPES);
message_v(JObj) ->
    message_v(kz_json:to_proplist(JObj)).

-spec message_endpoint_headers(kz_term:api_terms()) -> {'ok', kz_term:proplist()} |
                                                       {'error', string()}.
message_endpoint_headers(Prop) when is_list(Prop) ->
    kz_api:build_message_specific_headers(Prop, ?SMS_REQ_ENDPOINT_HEADERS, ?OPTIONAL_SMS_REQ_ENDPOINT_HEADERS);
message_endpoint_headers(JObj) ->
    message_endpoint_headers(kz_json:to_proplist(JObj)).

-spec message_endpoint_v(kz_term:api_terms()) -> boolean().
message_endpoint_v(Prop) when is_list(Prop) ->
    kz_api:validate_message(Prop, ?SMS_REQ_ENDPOINT_HEADERS, ?SMS_REQ_ENDPOINT_VALUES, ?SMS_REQ_ENDPOINT_TYPES);
message_endpoint_v(JObj) ->
    message_endpoint_v(kz_json:to_proplist(JObj)).

-spec delivery(kz_term:api_terms()) -> {'ok', iolist()} |
                                       {'error', string()}.
delivery(Prop) when is_list(Prop) ->
    case delivery_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DELIVERY_HEADERS, ?OPTIONAL_DELIVERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_delivery"}
    end;
delivery(JObj) -> delivery(kz_json:to_proplist(JObj)).

-spec delivery_v(kz_term:api_terms()) -> boolean().
delivery_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DELIVERY_HEADERS, ?DELIVERY_REQ_VALUES, ?DELIVERY_TYPES);
delivery_v(JObj) -> delivery_v(kz_json:to_proplist(JObj)).

-spec inbound(kz_term:api_terms()) -> {'ok', iolist()} |
                                      {'error', string()}.
inbound(Prop) when is_list(Prop) ->
    case inbound_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?INBOUND_HEADERS, ?OPTIONAL_INBOUND_HEADERS);
        'false' -> {'error', "Proplist failed validation for inbound"}
    end;
inbound(JObj) -> inbound(kz_json:to_proplist(JObj)).

-spec inbound_v(kz_term:api_terms()) -> boolean().
inbound_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?INBOUND_HEADERS, ?INBOUND_REQ_VALUES, ?INBOUND_TYPES);
inbound_v(JObj) -> inbound_v(kz_json:to_proplist(JObj)).

-spec outbound(kz_term:api_terms()) -> {'ok', iolist()} |
                                       {'error', string()}.
outbound(Prop) when is_list(Prop) ->
    case outbound_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?OUTBOUND_HEADERS, ?OPTIONAL_OUTBOUND_HEADERS);
        'false' -> {'error', "Proplist failed validation for outbound"}
    end;
outbound(JObj) -> outbound(kz_json:to_proplist(JObj)).

-spec outbound_v(kz_term:api_terms()) -> boolean().
outbound_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?OUTBOUND_HEADERS, ?OUTBOUND_REQ_VALUES, ?OUTBOUND_TYPES);
outbound_v(JObj) -> outbound_v(kz_json:to_proplist(JObj)).

-spec resume(kz_term:api_terms()) -> {'ok', iolist()} |
                                     {'error', string()}.
resume(Prop) when is_list(Prop) ->
    case resume_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESUME_REQ_HEADERS, ?OPTIONAL_RESUME_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_delivery"}
    end;
resume(JObj) -> resume(kz_json:to_proplist(JObj)).

-spec resume_v(kz_term:api_terms()) -> boolean().
resume_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESUME_REQ_HEADERS, ?RESUME_REQ_VALUES, ?RESUME_REQ_TYPES);
resume_v(JObj) -> resume_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('call_id', Props, props:get_value('message_id', Props, <<"*">>)),
    RouteId = ?LOWER(props:get_value('route_id', Props, <<"*">>)),
    Exchange = props:get_value('exchange', Props, ?SMS_EXCHANGE),
    bind_q(Exchange, Queue, CallId, RouteId, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Exchange, Queue, CallId, RouteId, 'undefined') ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?SMS_ROUTING_KEY(RouteId,CallId), Exchange),
    kz_amqp_util:bind_q_to_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), Exchange),
    kz_amqp_util:bind_q_to_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), Exchange),
    kz_amqp_util:bind_q_to_exchange(Queue, ?INBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    kz_amqp_util:bind_q_to_exchange(Queue, ?OUTBOUND_ROUTING_KEY(RouteId, CallId), Exchange);
bind_q(Exchange, Queue, CallId, RouteId, ['route'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?SMS_ROUTING_KEY(RouteId,CallId), Exchange),
    bind_q(Exchange, Queue, CallId, RouteId, Restrict);
bind_q(Exchange, Queue, CallId, RouteId, ['delivery'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), Exchange),
    bind_q(Exchange, Queue, CallId, RouteId, Restrict);
bind_q(Exchange, Queue, CallId, RouteId, ['resume'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), Exchange),
    bind_q(Exchange, Queue, CallId, RouteId, Restrict);
bind_q(Exchange, Queue, CallId, RouteId, ['inbound'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?INBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    bind_q(Exchange, Queue, CallId, RouteId, Restrict);
bind_q(Exchange, Queue, CallId, RouteId, ['outbound'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?OUTBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    bind_q(Exchange, Queue, CallId, RouteId, Restrict);
bind_q(_, _, _, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('call_id', Props, props:get_value('message_id', Props, <<"*">>)),
    RouteId = ?LOWER(props:get_value('route_id', Props, <<"*">>)),
    Exchange = props:get_value('exchange', Props, ?SMS_EXCHANGE),
    unbind_q(Exchange, Queue, CallId, RouteId, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Exchange, Queue, CallId, RouteId, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?SMS_ROUTING_KEY(RouteId,CallId), Exchange),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), Exchange),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), Exchange),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?INBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    kz_amqp_util:unbind_q_from_exchange(Queue, ?OUTBOUND_ROUTING_KEY(RouteId, CallId), Exchange);
unbind_q(Exchange, Queue, CallId, RouteId, ['route'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?SMS_ROUTING_KEY(RouteId,CallId), Exchange),
    unbind_q(Exchange, Queue, CallId, RouteId, Restrict);
unbind_q(Exchange, Queue, CallId, RouteId, ['delivery'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?DELIVERY_ROUTING_KEY(CallId), Exchange),
    unbind_q(Exchange, Queue, CallId, RouteId, Restrict);
unbind_q(Exchange, Queue, CallId, RouteId, ['resume'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?RESUME_ROUTING_KEY(CallId), Exchange),
    unbind_q(Exchange, Queue, CallId, RouteId, Restrict);
unbind_q(Exchange, Queue, CallId, RouteId, ['inbound'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?INBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    unbind_q(Exchange, Queue, CallId, RouteId, Restrict);
unbind_q(Exchange, Queue, CallId, RouteId, ['outbound'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, ?OUTBOUND_ROUTING_KEY(RouteId, CallId), Exchange),
    unbind_q(Exchange, Queue, CallId, RouteId, Restrict);
unbind_q(_, _, _, _, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?SMS_EXCHANGE, <<"topic">>).

-spec publish_message(kz_term:api_terms()) -> 'ok'.
publish_message(JObj) ->
    publish_message(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_message(kz_term:api_terms(), binary()) -> 'ok'.
publish_message(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SMS_REQ_VALUES, fun message/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    RouteId = props:get_value(<<"Route-ID">>, Req, <<"*">>),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?SMS_ROUTING_KEY(RouteId, CallId), Payload, ContentType).

-spec publish_inbound(kz_term:api_terms()) -> 'ok'.
publish_inbound(JObj) ->
    publish_inbound(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_inbound(kz_term:api_terms(), binary()) -> 'ok'.
publish_inbound(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?INBOUND_REQ_VALUES, fun inbound/1),
    MessageId = props:get_value(<<"Message-ID">>, Req),
    RouteId = props:get_value(<<"Route-ID">>, Req, <<"*">>),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?INBOUND_ROUTING_KEY(RouteId, MessageId), Payload, ContentType).

-spec publish_outbound(kz_term:api_terms()) -> 'ok'.
publish_outbound(JObj) ->
    publish_outbound(JObj, ?DEFAULT_CONTENT_TYPE, []).

-spec publish_outbound(kz_term:api_terms(), binary(), list()) -> 'ok'.
publish_outbound(Req, ContentType, AMQPOptions) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?OUTBOUND_REQ_VALUES, fun outbound/1),
    MessageId = props:get_value(<<"Message-ID">>, Req),
    RouteId = props:get_value(<<"Route-ID">>, Req, <<"*">>),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    RK = ?OUTBOUND_ROUTING_KEY(RouteId, MessageId),
    kz_amqp_util:basic_publish(Exchange, RK, Payload, ContentType, AMQPOptions).

-spec publish_delivery(kz_term:api_terms()) -> 'ok'.
publish_delivery(JObj) ->
    publish_delivery(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_delivery(kz_term:api_terms(), binary()) -> 'ok'.
publish_delivery(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DELIVERY_REQ_VALUES, fun delivery/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?DELIVERY_ROUTING_KEY(CallId), Payload, ContentType).

-spec publish_targeted_delivery(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_delivery(RespQ, JObj) ->
    publish_targeted_delivery(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_delivery(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_targeted_delivery(RespQ, JObj, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?DELIVERY_REQ_VALUES, fun delivery/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_resume(kz_term:api_terms() | kz_term:ne_binary()) -> 'ok'.
publish_resume(SMS) when is_binary(SMS) ->
    Payload = [{<<"SMS-ID">>, SMS}
               | kz_api:default_headers(<<"API">>, <<"0.9.7">>)
              ],
    publish_resume(Payload, ?DEFAULT_CONTENT_TYPE);
publish_resume(JObj) ->
    publish_resume(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resume(kz_term:api_terms(), binary()) -> 'ok'.
publish_resume(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RESUME_REQ_VALUES, fun resume/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?RESUME_ROUTING_KEY(CallId), Payload, ContentType).
