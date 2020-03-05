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

-export([api_definitions/0, api_definition/1]).

-export([inbound/1
        ,inbound_v/1
        ,publish_inbound/1
        ,publish_inbound/2
        ]).
-export([outbound/1
        ,outbound_v/1
        ,publish_outbound/1
        ,publish_outbound/2
        ]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ]).

-include_lib("kz_amqp_util.hrl").

-define(IM_EXCHANGE, <<"im">>).
-define(EVENT_CATEGORIES, [<<"sms">>, <<"mms">>]).
-define(IM_CONTENT_TYPE, <<"application/json">>).

-ifdef(TEST).
-export([routing_key/4
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [inbound_definition()
    ,outbound_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"inbound">>) ->
    inbound_definition();
api_definition(<<"outbound">>) ->
    outbound_definition().

-spec inbound_definition() -> kapi_definition:api().
inbound_definition() ->
    EventName = <<"inbound">>,
    Category = ?EVENT_CATEGORIES,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Inbound">>}
              ,{fun kapi_definition:set_description/2, <<"SMS MMS Inbound">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun inbound/1}
              ,{fun kapi_definition:set_validate_fun/2, fun inbound_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_inbound/1}
              ,{fun kapi_definition:set_binding/2, fun publish_inbound_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Body">>
                                                            ,<<"From">>
                                                            ,<<"Message-ID">>
                                                            ,<<"To">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Charges">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"Custom-Vars">>
                                                            ,<<"Request">>
                                                            ,<<"Route-ID">>
                                                            ,<<"Route-Type">>
                                                            ,<<"System-ID">>
                                                            ,<<"Transaction-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Body">>, fun is_binary/1}
                ,{<<"Custom-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"From">>, fun is_binary/1}
                ,{<<"Message-ID">>, fun is_binary/1}
                ,{<<"Request">>, fun is_binary/1}
                ,{<<"System-ID">>, fun is_binary/1}
                ,{<<"To">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec outbound_definition() -> kapi_definition:api().
outbound_definition() ->
    EventName = <<"outbound">>,
    Category = ?EVENT_CATEGORIES,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Outbound">>}
              ,{fun kapi_definition:set_description/2, <<"SMS MMS Outbound">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun outbound/1}
              ,{fun kapi_definition:set_validate_fun/2, fun outbound_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_outbound/1}
              ,{fun kapi_definition:set_binding/2, fun publish_outbound_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Body">>
                                                            ,<<"From">>
                                                            ,<<"Message-ID">>
                                                            ,<<"To">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Application-ID">>
                                                            ,<<"Custom-Vars">>
                                                            ,<<"Endpoints">>
                                                            ,<<"Originator-Flags">>
                                                            ,<<"Originator-Properties">>
                                                            ,<<"Request">>
                                                            ,<<"Route-ID">>
                                                            ,<<"Route-Type">>
                                                            ,<<"System-ID">>
                                                            ,<<"Target-Flags">>
                                                            ,<<"Target-Properties">>
                                                            ,<<"Transaction-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Route-Type">>, [<<"onnet">>, <<"offnet">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Body">>, fun is_binary/1}
                ,{<<"Custom-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Endpoints">>, fun is_list/1}
                ,{<<"From">>, fun is_binary/1}
                ,{<<"Message-ID">>, fun is_binary/1}
                ,{<<"Request">>, fun is_binary/1}
                ,{<<"System-ID">>, fun is_binary/1}
                ,{<<"To">>, fun is_binary/1}
                ]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc SMS MMS Inbound
%% @end
%%------------------------------------------------------------------------------
-spec inbound(kz_term:api_terms()) -> kz_api:api_formatter_return().
inbound(Req) ->
    kapi_definition:build_message(Req, inbound_definition()).

-spec inbound_v(kz_term:api_terms()) -> boolean().
inbound_v(Req) ->
    kapi_definition:validate(Req, inbound_definition()).

-spec publish_inbound(kz_term:api_terms()) -> 'ok'.
publish_inbound(Req) ->
    publish_inbound(Req, []).

-spec publish_inbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_inbound(Req, AMQPOptions) ->
    Definition = inbound_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(exchange_id(Req)
                              ,(kapi_definition:binding(Definition))(Req)
                              ,Payload
                              ,?IM_CONTENT_TYPE
                              ,maybe_gzip(Req, AMQPOptions)
                              ).

%%------------------------------------------------------------------------------
%% @doc SMS MMS Inbound
%% @end
%%------------------------------------------------------------------------------
-spec outbound(kz_term:api_terms()) -> kz_api:api_formatter_return().
outbound(Req) ->
    kapi_definition:build_message(Req, outbound_definition()).

-spec outbound_v(kz_term:api_terms()) -> boolean().
outbound_v(Req) ->
    kapi_definition:validate(Req, outbound_definition()).

-spec publish_outbound(kz_term:api_terms()) -> 'ok'.
publish_outbound(JObj) ->
    publish_outbound(JObj, []).

-spec publish_outbound(kz_term:api_terms(), kz_term:proplist()) -> 'ok'.
publish_outbound(Req, AMQPOptions) ->
    Definition = outbound_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(exchange_id(Req)
                              ,(kapi_definition:binding(Definition))(Req)
                              ,Payload
                              ,?IM_CONTENT_TYPE
                              ,maybe_gzip(Req, AMQPOptions)
                              ).

%%------------------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props, 'undefined') ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue
                                        ,bind_inbound_routing_key(Category, Props)
                                        ,bind_exchange_id(Props)
                                        )
         || Category <- bind_categories(Props)
        ],
    _ = [kz_amqp_util:bind_q_to_exchange(Queue
                                        ,bind_outbound_routing_key(Category, Props)
                                        ,bind_exchange_id(Props)
                                        )
         || Category <- bind_categories(Props)
        ],
    'ok';
bind_q(Queue, Props, ['inbound'|Restrict]) ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue
                                        ,bind_inbound_routing_key(Category, Props)
                                        ,bind_exchange_id(Props)
                                        )
         || Category <- bind_categories(Props)
        ],
    bind_q(Queue, Props, Restrict);
bind_q(Queue, Props, ['outbound'|Restrict]) ->
    _ = [kz_amqp_util:bind_q_to_exchange(Queue
                                        ,bind_outbound_routing_key(Category, Props)
                                        ,bind_exchange_id(Props)
                                        )
         || Category <- bind_categories(Props)
        ],
    bind_q(Queue, Props, Restrict);
bind_q( _, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, Props, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props, 'undefined') ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue
                                            ,bind_inbound_routing_key(Category, Props)
                                            ,bind_exchange_id(Props)
                                            )
         || Category <- bind_categories(Props)
        ],
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue
                                            ,bind_outbound_routing_key(Category, Props)
                                            ,bind_exchange_id(Props)
                                            )
         || Category <- bind_categories(Props)
        ],
    'ok';
unbind_q(Queue, Props, ['inbound'|Restrict]) ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue
                                            ,bind_inbound_routing_key(Category, Props)
                                            ,bind_exchange_id(Props)
                                            )
         || Category <- bind_categories(Props)
        ],
    unbind_q(Queue, Props, Restrict);
unbind_q(Queue, Props, ['outbound'|Restrict]) ->
    _ = [kz_amqp_util:unbind_q_from_exchange(Queue
                                            ,bind_outbound_routing_key(Category, Props)
                                            ,bind_exchange_id(Props)
                                            )
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
    to_lower(props:get_value('route_type', Props, <<"*">>)).

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
    to_lower(props:get_value(<<"Route-ID">>, Props));
route_id(JObj) ->
    route_id(kz_json:to_proplist(JObj)).

-spec route_type(kz_term:api_terms()) -> kz_term:ne_binary().
route_type(Props)
  when is_list(Props) ->
    to_lower(props:get_value(<<"Route-Type">>, Props));
route_type(JObj) ->
    route_type(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?IM_EXCHANGE, <<"topic">>).

-spec publish_outbound_routing_key(kz_term:proplist()) -> kz_term:ne_binary().
publish_outbound_routing_key(Props) ->
    outbound_routing_key(kz_api:event_category(Props), route_id(Props), message_id(Props)).

-spec bind_outbound_routing_key(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary().
bind_outbound_routing_key(Category, Props) ->
    outbound_routing_key(Category, bind_route_id(Props), bind_message_id(Props)).

-spec outbound_routing_key(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
outbound_routing_key(Category, RouteId, MsgId) ->
    routing_key(Category, <<"outbound">>, RouteId, MsgId).

-spec publish_inbound_routing_key(kz_term:proplist()) -> kz_term:ne_binary().
publish_inbound_routing_key(Props) ->
    inbound_routing_key(kz_api:event_category(Props), route_type(Props), message_id(Props)).

-spec bind_inbound_routing_key(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary().
bind_inbound_routing_key(Category, Props) ->
    inbound_routing_key(Category, bind_route_type(Props), bind_message_id(Props)).

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
