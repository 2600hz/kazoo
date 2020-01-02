%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_im).

-export([inbound/1, inbound_v/1
        ,outbound/1, outbound_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_inbound/1, publish_inbound/2
        ,publish_outbound/1, publish_outbound/2
        ]).

-include_lib("kz_amqp_util.hrl").

-define(LOWER(X), kz_term:to_lower_binary(X)).

-define(IM_EXCHANGE, <<"im">>).
-define(EVENT_CATEGORIES, [<<"sms">>, <<"mms">>]).
-define(IM_CONTENT_TYPE, <<"application/json">>).

%% Inbound
-define(INBOUND_REQ_EVENT_NAME, <<"inbound">>).
-define(INBOUND_HEADERS, [<<"Message-ID">>
                         ,<<"Body">>
                         ,<<"From">>
                         ,<<"To">>
                         ]).
-define(OPTIONAL_INBOUND_HEADERS, [<<"Custom-Vars">>
                                  ,<<"Custom-SIP-Headers">>
                                  ,<<"Request">>
                                  ,<<"Account-ID">>
                                  ,<<"Route-Type">>
                                  ,<<"System-ID">>
                                  ,<<"Route-ID">>
                                  ,<<"Transaction-ID">>
                                  ]).
-define(INBOUND_TYPES, [{<<"To">>, fun is_binary/1}
                       ,{<<"From">>, fun is_binary/1}
                       ,{<<"Request">>, fun is_binary/1}
                       ,{<<"Message-ID">>, fun is_binary/1}
                       ,{<<"System-ID">>, fun is_binary/1}
                       ,{<<"Custom-Vars">>, fun kz_json:is_json_object/1}
                       ,{<<"Body">>, fun is_binary/1}
                       ]).
-define(INBOUND_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORIES}
                            ,{<<"Event-Name">>, ?INBOUND_REQ_EVENT_NAME}
                            ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                            ]).

-define(INBOUND_RK(Category, RouteType, MsgId), inbound_routing_key(Category, RouteType, MsgId)).

-define(BIND_INBOUND_RK(Category, Props)
       ,?INBOUND_RK(Category
                   ,bind_route_type(Props)
                   ,bind_message_id(Props)
                   )).

-define(PUBLISH_INBOUND_RK(Props)
       ,?INBOUND_RK(kz_api:event_category(Props)
                   ,route_type(Props)
                   ,message_id(Props)
                   )).

%% Outbound
-define(OUTBOUND_REQ_EVENT_NAME, <<"outbound">>).
-define(OUTBOUND_HEADERS, [<<"Message-ID">>
                          ,<<"Body">>
                          ,<<"From">>
                          ,<<"To">>
                          ]).
-define(OPTIONAL_OUTBOUND_HEADERS, [<<"Custom-Vars">>
                                   ,<<"Request">>
                                   ,<<"Account-ID">>, <<"Application-ID">>
                                   ,<<"Route-Type">>, <<"System-ID">>, <<"Route-ID">>
                                   ,<<"Originator-Properties">>, <<"Target-Properties">>
                                   ,<<"Originator-Flags">>, <<"Target-Flags">>
                                   ,<<"Endpoints">>
                                   ,<<"Transaction-ID">>
                                   ]).
-define(OUTBOUND_TYPES, [{<<"To">>, fun is_binary/1}
                        ,{<<"From">>, fun is_binary/1}
                        ,{<<"Request">>, fun is_binary/1}
                        ,{<<"Message-ID">>, fun is_binary/1}
                        ,{<<"System-ID">>, fun is_binary/1}
                        ,{<<"Custom-Vars">>, fun kz_json:is_json_object/1}
                        ,{<<"Body">>, fun is_binary/1}
                        ,{<<"Endpoints">>, fun is_list/1}
                        ]).
-define(OUTBOUND_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORIES}
                             ,{<<"Event-Name">>, ?OUTBOUND_REQ_EVENT_NAME}
                             ,{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                             ]).

-define(OUTBOUND_RK(Category, RouteId, MsgId), outbound_routing_key(Category, RouteId, MsgId)).

-define(BIND_OUTBOUND_RK(Category, Props)
       ,?OUTBOUND_RK(Category
                    ,bind_route_id(Props)
                    ,bind_message_id(Props)
                    )).

-define(PUBLISH_OUTBOUND_RK(Props)
       ,?OUTBOUND_RK(kz_api:event_category(Props)
                    ,route_id(Props)
                    ,message_id(Props)
                    )).

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

%%------------------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props, 'undefined') ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_INBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    _ = [kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_OUTBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    'ok';
bind_q(Queue, Props, ['inbound'|Restrict]) ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_INBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['outbound'|Restrict]) ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue, ?BIND_OUTBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    bind_q(Queue, Props, Restrict);
bind_q( _, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props, 'undefined') ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_INBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_OUTBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    'ok';
unbind_q(Queue, Props, ['inbound'|Restrict]) ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_INBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['outbound'|Restrict]) ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue, ?BIND_OUTBOUND_RK(Category, Props), bind_exchange_id(Props))
         || Category <- bind_categories(Props)
        ],
    unbind_q(Queue, Props, Restrict);
unbind_q( _, _, []) -> 'ok'.

-spec bind_exchange_id(kz_term:api_terms()) -> kz_term:ne_binary().
bind_exchange_id(Props) ->
    props:get_value('exchange', Props, ?IM_EXCHANGE).

-spec bind_categories(kz_term:api_terms()) -> kz_term:ne_binaries().
bind_categories(Props) ->
    props:get_value('im_types', Props, ?EVENT_CATEGORIES).

-spec bind_message_id(kz_term:api_terms()) -> kz_term:ne_binary().
bind_message_id(Props) ->
    props:get_value('message_id', Props, <<"*">>).

-spec bind_route_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
bind_route_id(Props) ->
    props:get_value('route_id', Props).

-spec bind_route_type(kz_term:api_terms()) -> kz_term:ne_binary().
bind_route_type(Props) ->
    ?LOWER(props:get_value('route_type', Props, <<"*">>)).

-spec exchange_id(kz_term:api_terms()) -> kz_term:ne_binary().
exchange_id(Props)
  when is_list(Props) ->
    props:get_value(<<"Exchange-ID">>, Props, ?IM_EXCHANGE);
exchange_id(JObj) ->
    exchange_id(kz_json:to_proplist(JObj)).

-spec message_id(kz_term:api_terms()) -> kz_term:ne_binary().
message_id(Props)
  when is_list(Props) ->
    props:get_value(<<"Message-ID">>, Props);
message_id(JObj) ->
    message_id(kz_json:to_proplist(JObj)).

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
    kz_amqp_util:new_exchange(?IM_EXCHANGE, <<"topic">>).

-spec publish_inbound(kz_term:api_terms()) -> 'ok'.
publish_inbound(Req) ->
    publish_inbound(Req, []).

-spec publish_inbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_inbound(Req, AMQPOptions) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?INBOUND_REQ_VALUES, fun inbound/1),
    Exchange = exchange_id(Req),
    RK = ?PUBLISH_INBOUND_RK(Req),
    kz_amqp_util:basic_publish(Exchange, RK, Payload, ?IM_CONTENT_TYPE, maybe_gzip(Req, AMQPOptions)).

-spec publish_outbound(kz_term:api_terms()) -> 'ok'.
publish_outbound(JObj) ->
    publish_outbound(JObj, []).

-spec publish_outbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_outbound(Req, AMQPOptions) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?OUTBOUND_REQ_VALUES, fun outbound/1),
    Exchange = exchange_id(Req),
    RK = ?PUBLISH_OUTBOUND_RK(Req),
    kz_amqp_util:basic_publish(Exchange, RK, Payload, ?IM_CONTENT_TYPE, maybe_gzip(Req, AMQPOptions)).

-spec outbound_routing_key(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
outbound_routing_key(Category, RouteId, MsgId) ->
    routing_key(Category, <<"outbound">>, RouteId, MsgId).

-spec inbound_routing_key(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
inbound_routing_key(Category, RouteType, MsgId) ->
    routing_key(Category, <<"inbound">>, RouteType, MsgId).

-spec routing_key(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
routing_key(Category, Direction, RouteType, MsgId) ->
    Parts = [kz_term:to_binary(Category)
            ,Direction
            ,encode(to_lower(RouteType))
            ,kz_amqp_util:encode(MsgId)
            ],
    kz_binary:join(lists:filter(fun(Part) -> Part =/= 'undefined' end, Parts), <<".">>).

-spec to_lower(term()) -> kz_term:api_ne_binary().
to_lower('undefined') -> 'undefined';
to_lower(Term) -> kz_term:to_lower_binary(Term).

-spec encode(term()) -> kz_term:api_ne_binary().
encode('undefined') -> 'undefined';
encode(Term) -> kz_amqp_util:encode(Term).

-spec maybe_gzip(kz_term:api_terms(), kz_term:proplist()) -> kz_term:proplist().
maybe_gzip(Req, AMQPOptions) ->
    case kz_api:event_category(Req) of
        <<"mms">> -> [{content_encoding, <<"gzip">>} | AMQPOptions];
        <<"sms">> -> AMQPOptions
    end.
