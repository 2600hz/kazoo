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

-export([ 
          message/1, message_v/1
         ,delivery/1, delivery_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_message/1, publish_message/2
         ,publish_delivery/1, publish_delivery/2
        ]).

-define(SMS_EXCHANGE, <<"sms">>).
-define(EVENT_CATEGORY, <<"message">>).

%% Message
-define(MESSAGE_REQ_EVENT_NAME, <<"route">>).
-define(MESSAGE_HEADERS, [<<"To">>, <<"From">>, <<"Call-ID">>, <<"Message-ID">>, <<"Body">>]).
-define(OPTIONAL_MESSAGE_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>
                                  ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                  ,<<"From-Network-Addr">>
                                  ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                  ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                  ,<<"Contact">>, <<"User-Agent">>
                                  ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
                                  ,<<"Request">>, <<"Account-ID">>
                                    ]).
-define(MESSAGE_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Request">>, fun is_binary/1}
                          ,{<<"Message-ID">>, fun is_binary/1}
                          ,{<<"Event-Queue">>, fun is_binary/1}
                          ,{<<"Caller-ID-Name">>, fun is_binary/1}
                          ,{<<"Caller-ID-Number">>, fun is_binary/1}
                          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                          ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                         ]).
-define(MESSAGE_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                        ,{<<"Event-Name">>, ?MESSAGE_REQ_EVENT_NAME}
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


-spec message(api_terms()) -> {'ok', iolist()} | {'error', string()}.
message(Prop) when is_list(Prop) ->
    case message_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MESSAGE_HEADERS, ?OPTIONAL_MESSAGE_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_message"}
    end;
message(JObj) -> message(wh_json:to_proplist(JObj)).

-spec message_v(api_terms()) -> boolean().
message_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MESSAGE_HEADERS, ?MESSAGE_REQ_VALUES, ?MESSAGE_TYPES);
message_v(JObj) -> message_v(wh_json:to_proplist(JObj)).


-spec delivery(api_terms()) -> {'ok', iolist()} | {'error', string()}.
delivery(Prop) when is_list(Prop) ->
    case delivery_v(Prop) of
        'true' -> wh_api:build_delivery(Prop, ?DELIVERY_HEADERS, ?OPTIONAL_DELIVERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_delivery"}
    end;
delivery(JObj) -> delivery(wh_json:to_proplist(JObj)).

-spec delivery_v(api_terms()) -> boolean().
delivery_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DELIVERY_HEADERS, ?DELIVERY_REQ_VALUES, ?DELIVERY_TYPES);
delivery_v(JObj) -> delivery_v(wh_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    MessageId = props:get_value('message_id', Props, <<"*">>),
    amqp_util:bind_q_to_exchange(Queue, message_routing_key(AccountId, MessageId), ?SMS_EXCHANGE).

   
-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    MessageId = props:get_value('message_id', Props, <<"*">>),
    amqp_util:unbind_q_from_exchange(Queue, message_routing_key(AccountId, MessageId), ?SMS_EXCHANGE).

message_routing_key('undefined', 'undefined') -> message_routing_key(<<"*">>, <<"*">>);
message_routing_key('undefined', MessageId) -> message_routing_key(<<"*">>, MessageId);
message_routing_key(AccountId, 'undefined') -> message_routing_key(AccountId, <<"*">>);
message_routing_key(AccountId, MessageId) ->
    list_to_binary(["message.", amqp_util:encode(AccountId), ".", amqp_util:encode(MessageId) ]).

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
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?MESSAGE_REQ_VALUES, fun message/1),
    MessageId = props:get_first_defined([<<"Message-ID">>,<<"Call-ID">>], Req ,<<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, Req, <<"*">>),
    amqp_util:basic_publish(?SMS_EXCHANGE, message_routing_key(AccountId, MessageId), Payload, ContentType).

-spec publish_delivery(api_terms()) -> 'ok'.
-spec publish_delivery(api_terms(), binary()) -> 'ok'.
publish_delivery(JObj) ->
    publish_delivery(JObj, ?DEFAULT_CONTENT_TYPE).
publish_delivery(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?DELIVERY_REQ_VALUES, fun delivery/1),
    MessageId = props:get_first_defined([<<"Message-ID">>,<<"Call-ID">>], Req ,<<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, Req, <<"*">>),
    amqp_util:basic_publish(?SMS_EXCHANGE, message_routing_key(AccountId, MessageId), Payload, ContentType).




