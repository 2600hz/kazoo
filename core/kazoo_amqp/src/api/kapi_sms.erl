%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author James Aimonetti
%%% @author Karl Anderson
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
        ,publish_message/1
        ,publish_delivery/1
        ,publish_targeted_delivery/2
        ,publish_resume/1
        ,publish_inbound/1, publish_inbound/2
        ,publish_outbound/1, publish_outbound/2
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
        ,<<"From">>, <<"From-User">>, <<"From-Realm">>, <<"From-URI">>
        ,<<"To">>, <<"To-User">>, <<"To-Realm">>, <<"To-URI">>
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
-define(BIND_SMS_ROUTING_KEY(Props)
       ,?SMS_ROUTING_KEY(bind_route_id(Props), bind_call_id(Props))).
-define(PUBLISH_SMS_ROUTING_KEY(Props)
       ,?SMS_ROUTING_KEY(route_id(Props), call_id(Props))).

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
-define(BIND_DELIVERY_ROUTING_KEY(Props)
       ,?DELIVERY_ROUTING_KEY(bind_call_id(Props))).
-define(PUBLISH_DELIVERY_ROUTING_KEY(Props)
       ,?DELIVERY_ROUTING_KEY(call_id(Props))).

%% SMS Resume
-define(RESUME_REQ_EVENT_NAME, <<"resume">>).
-define(RESUME_REQ_HEADERS, [<<"SMS-ID">>]).
-define(OPTIONAL_RESUME_REQ_HEADERS, []).
-define(RESUME_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?RESUME_REQ_EVENT_NAME}
                           ]).
-define(RESUME_REQ_TYPES, []).
-define(RESUME_ROUTING_KEY(CallId), <<"message.resume.", (kz_amqp_util:encode(CallId))/binary>>).
-define(BIND_RESUME_ROUTING_KEY(Props)
       ,?RESUME_ROUTING_KEY(bind_call_id(Props))).
-define(PUBLISH_RESUME_ROUTING_KEY(Props)
       ,?RESUME_ROUTING_KEY(call_id(Props))).

%% Inbound
-define(INBOUND_REQ_EVENT_NAME, <<"inbound">>).
-define(INBOUND_HEADERS, [<<"Message-ID">>, <<"Body">>
                         ,<<"To">>, <<"From">>
                         ]).
-define(OPTIONAL_INBOUND_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Orig-Port">>
                                  ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                  ,<<"From-Network-Addr">>
                                  ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                  ,<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                                  ,<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                  ,<<"Contact">>, <<"User-Agent">>
                                  ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                  ,<<"To">>, <<"From">>, <<"Request">>
                                  ,<<"Account-ID">>
                                  ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                  ,<<"Route-Type">>, <<"System-ID">>, <<"Route-ID">>
                                  ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                  ,<<"Charges">>
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
                            ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>, <<"api">>]}
                            ]).
-define(INBOUND_ROUTING_KEY(RouteType, CallId), <<"message.inbound."
                                                 ,(kz_amqp_util:encode(?LOWER(RouteType)))/binary, "."
                                                 ,(kz_amqp_util:encode(CallId))/binary
                                                >>).
-define(BIND_INBOUND_ROUTING_KEY(Props)
       ,?INBOUND_ROUTING_KEY(bind_route_type(Props), bind_call_id(Props))).
-define(PUBLISH_INBOUND_ROUTING_KEY(Props)
       ,?INBOUND_ROUTING_KEY(route_type(Props), call_id(Props))).

%% Outbound
-define(OUTBOUND_REQ_EVENT_NAME, <<"outbound">>).
-define(OUTBOUND_HEADERS, [<<"Message-ID">>
                          ,<<"Body">>
                          ,<<"From">>
                          ,<<"To">>
                          ]).
-define(OPTIONAL_OUTBOUND_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Orig-Port">>
                                   ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                   ,<<"From-Network-Addr">>
                                   ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                   ,<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                                   ,<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                   ,<<"Contact">>, <<"User-Agent">>
                                   ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                   ,<<"To">>, <<"From">>, <<"Request">>
                                   ,<<"Account-ID">>, <<"Application-ID">>
                                   ,<<"Delivery-Result-Code">>, <<"Delivery-Failure">>, <<"Status">>
                                   ,<<"Route-Type">>, <<"System-ID">>, <<"Route-ID">>
                                   ,<<"Originator-Properties">>, <<"Target-Properties">>
                                   ,<<"Originator-Flags">>, <<"Target-Flags">>
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
-define(OUTBOUND_ROUTING_KEY(RouteId, CallId), outbound_routing_key(RouteId, CallId)).
-define(BIND_OUTBOUND_ROUTING_KEY(Props)
       ,?OUTBOUND_ROUTING_KEY(bind_route_id(Props), bind_call_id(Props))).
-define(PUBLISH_OUTBOUND_ROUTING_KEY(Props)
       ,?OUTBOUND_ROUTING_KEY(route_id(Props), call_id(Props))).

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
    bind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props, 'undefined') ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_SMS_ROUTING_KEY(Props), bind_exchange_id(Props)),
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_DELIVERY_ROUTING_KEY(Props), bind_exchange_id(Props)),
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_RESUME_ROUTING_KEY(Props), bind_exchange_id(Props)),
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_INBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_OUTBOUND_ROUTING_KEY(Props), bind_exchange_id(Props));
bind_q(Queue, Props, ['route'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_SMS_ROUTING_KEY(Props), bind_exchange_id(Props)),
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['delivery'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_DELIVERY_ROUTING_KEY(Props), bind_exchange_id(Props)),
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['resume'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_RESUME_ROUTING_KEY(Props), bind_exchange_id(Props)),
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['inbound'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_INBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['outbound'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_OUTBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    bind_q(Queue, Props, Restrict);
bind_q( _, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props, 'undefined') ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_SMS_ROUTING_KEY(Props), bind_exchange_id(Props)),
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_DELIVERY_ROUTING_KEY(Props), bind_exchange_id(Props)),
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_RESUME_ROUTING_KEY(Props), bind_exchange_id(Props)),
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_INBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_OUTBOUND_ROUTING_KEY(Props), bind_exchange_id(Props));
unbind_q(Queue, Props, ['route'|Restrict]) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_SMS_ROUTING_KEY(Props), bind_exchange_id(Props)),
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['delivery'|Restrict]) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_DELIVERY_ROUTING_KEY(Props), bind_exchange_id(Props)),
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['resume'|Restrict]) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_RESUME_ROUTING_KEY(Props), bind_exchange_id(Props)),
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['inbound'|Restrict]) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_INBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['outbound'|Restrict]) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_OUTBOUND_ROUTING_KEY(Props), bind_exchange_id(Props)),
    unbind_q(Queue, Props, Restrict);
unbind_q( _, _, []) -> 'ok'.

-spec bind_exchange_id(kz_term:api_terms()) -> kz_term:ne_binary().
bind_exchange_id(Props) ->
    props:get_value('exchange', Props, ?SMS_EXCHANGE).

-spec bind_call_id(kz_term:api_terms()) -> kz_term:ne_binary().
bind_call_id(Props) ->
    props:get_value('call_id', Props, props:get_value('message_id', Props, <<"*">>)).

-spec bind_route_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
bind_route_id(Props) ->
    props:get_value('route_id', Props).

-spec bind_route_type(kz_term:api_terms()) -> kz_term:ne_binary().
bind_route_type(Props) ->
    ?LOWER(props:get_value('route_type', Props, <<"*">>)).

-spec exchange_id(kz_term:api_terms()) -> kz_term:ne_binary().
exchange_id(Props)
  when is_list(Props) ->
    props:get_value(<<"Exchange-ID">>, Props, ?SMS_EXCHANGE);
exchange_id(JObj) ->
    exchange_id(kz_json:to_proplist(JObj)).

-spec call_id(kz_term:api_terms()) -> kz_term:ne_binary().
call_id(Props)
  when is_list(Props) ->
    props:get_value(<<"Call-ID">>, Props, props:get_value(<<"Message-ID">>, Props));
call_id(JObj) ->
    call_id(kz_json:to_proplist(JObj)).

-spec route_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
route_id(Props)
  when is_list(Props) ->
    ?LOWER(props:get_value(<<"Route-ID">>, Props));
route_id(JObj) ->
    route_id(kz_json:to_proplist(JObj)).

-spec route_type(kz_term:api_terms()) -> kz_term:ne_binary().
route_type(Props)
  when is_list(Props) ->
    ?LOWER(props:get_value(<<"Route-Type">>, Props));
route_type(JObj) ->
    route_type(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?SMS_EXCHANGE, <<"topic">>).

-spec publish_message(kz_term:api_terms()) -> 'ok'.
publish_message(Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SMS_REQ_VALUES, fun message/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    RouteId = props:get_value(<<"Route-ID">>, Req, <<"*">>),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?SMS_ROUTING_KEY(RouteId, CallId), Payload).

-spec publish_inbound(kz_term:api_terms()) -> 'ok'.
publish_inbound(Req) ->
    publish_inbound(Req, []).

-spec publish_inbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_inbound(Req, AMQPOptions) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?INBOUND_REQ_VALUES, fun inbound/1),
    Exchange = exchange_id(Req),
    kz_amqp_util:basic_publish(Exchange, ?PUBLISH_INBOUND_ROUTING_KEY(Req), Payload, ?DEFAULT_CONTENT_TYPE, AMQPOptions).

-spec publish_outbound(kz_term:api_terms()) -> 'ok'.
publish_outbound(JObj) ->
    publish_outbound(JObj, []).

-spec publish_outbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_outbound(Req, AMQPOptions) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?OUTBOUND_REQ_VALUES, fun outbound/1),
    Exchange = exchange_id(Req),
    RK = ?PUBLISH_OUTBOUND_ROUTING_KEY(Req),
    kz_amqp_util:basic_publish(Exchange, RK, Payload, ?DEFAULT_CONTENT_TYPE, AMQPOptions).

-spec publish_delivery(kz_term:api_terms()) -> 'ok'.
publish_delivery(Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DELIVERY_REQ_VALUES, fun delivery/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?DELIVERY_ROUTING_KEY(CallId), Payload).

-spec publish_targeted_delivery(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_delivery(RespQ, JObj) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?DELIVERY_REQ_VALUES, fun delivery/1),
    kz_amqp_util:targeted_publish(RespQ, Payload).

-spec publish_resume(kz_term:api_terms() | kz_term:ne_binary()) -> 'ok'.
publish_resume(SMS) when is_binary(SMS) ->
    Payload = [{<<"SMS-ID">>, SMS}
               | kz_api:default_headers(<<"API">>, <<"0.9.7">>)
              ],
    publish_resume(Payload);
publish_resume(Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RESUME_REQ_VALUES, fun resume/1),
    CallId = props:get_value(<<"Call-ID">>, Req),
    Exchange = props:get_value(<<"Exchange-ID">>, Req, ?SMS_EXCHANGE),
    kz_amqp_util:basic_publish(Exchange, ?RESUME_ROUTING_KEY(CallId), Payload).

-spec outbound_routing_key(kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
outbound_routing_key(RouteId, CallId) ->
    Parts = ["message"
            ,"outbound"
            ,to_lower(RouteId)
            ,kz_amqp_util:encode(CallId)
            ],
    kz_binary:join(lists:filter(fun(Part) -> Part =/= 'undefined' end, Parts), <<".">>).

-spec to_lower(term()) -> kz_term:api_ne_binary().
to_lower('undefined') -> 'undefined';
to_lower(Term) -> kz_term:to_lower_binary(Term).
