%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_inspector).

-export([api_definitions/0, api_definition/1]).

-export([lookup_req/1
        ,lookup_req_v/1
        ,publish_lookup_req/1
        ,publish_lookup_req/2
        ]).
-export([lookup_resp/1
        ,lookup_resp_v/1
        ,publish_lookup_resp/2
        ,publish_lookup_resp/3
        ]).
-export([filter_req/1
        ,filter_req_v/1
        ,publish_filter_req/1
        ,publish_filter_req/2
        ]).
-export([filter_resp/1
        ,filter_resp_v/1
        ,publish_filter_resp/2
        ,publish_filter_resp/3
        ]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(CI_AMQP_KEY(SubKey), <<"call_inspector.", SubKey/binary>>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [lookup_req_definition()
    ,lookup_resp_definition()
    ,filter_req_definition()
    ,filter_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"lookup_req">>) ->
    lookup_req_definition();
api_definition(<<"lookup_resp">>) ->
    lookup_resp_definition();
api_definition(<<"filter_req">>) ->
    filter_req_definition();
api_definition(<<"filter_resp">>) ->
    filter_resp_definition().

-spec lookup_req_definition() -> kapi_definition:api().
lookup_req_definition() ->
    EventName = <<"lookup_req">>,
    Category = <<"call_inspector">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Inspector Lookup Request">>}
              ,{fun kapi_definition:set_description/2, <<"Call Inspector Lookup Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun lookup_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun lookup_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_lookup_req/1}
              ,{fun kapi_definition:set_binding/2, ?CI_AMQP_KEY(<<"lookup">>)}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec lookup_resp_definition() -> kapi_definition:api().
lookup_resp_definition() ->
    EventName = <<"lookup_resp">>,
    Category = <<"call_inspector">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Inspector Lookup Response">>}
              ,{fun kapi_definition:set_description/2, <<"Call Inspector Lookup Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun lookup_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun lookup_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_lookup_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Chunks">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Chunks">>
                                                            ,<<"Analysis">>
                                                            ,<<"Dialog-Entities">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec filter_req_definition() -> kapi_definition:api().
filter_req_definition() ->
    EventName = <<"filter_req">>,
    Category = <<"call_inspector">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Inspector Filter Request">>}
              ,{fun kapi_definition:set_description/2, <<"Call Inspector Filter Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun filter_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun filter_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_filter_req/1}
              ,{fun kapi_definition:set_binding/2, ?CI_AMQP_KEY(<<"filter">>)}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-IDs">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec filter_resp_definition() -> kapi_definition:api().
filter_resp_definition() ->
    EventName = <<"filter_resp">>,
    Category = <<"call_inspector">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Inspector Filter Response">>}
              ,{fun kapi_definition:set_description/2, <<"Call Inspector Filter Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun filter_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun filter_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_filter_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-IDs">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%% Lookup Req
-spec lookup_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
lookup_req(Req) ->
    kapi_definition:build_message(Req, lookup_req_definition()).

-spec lookup_req_v(kz_term:api_terms()) -> boolean().
lookup_req_v(Req) ->
    kapi_definition:validate(Req, lookup_req_definition()).

-spec publish_lookup_req(kz_term:api_terms()) -> 'ok'.
publish_lookup_req(JObj) ->
    publish_lookup_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_req(Req, ContentType) ->
    Definition = lookup_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:monitor_publish(Payload, ContentType, kapi_definition:binding(Definition)).

%% Lookup Resp
-spec lookup_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
lookup_resp(Req) ->
    kapi_definition:build_message(Req, lookup_resp_definition()).

-spec lookup_resp_v(kz_term:api_terms()) -> boolean().
lookup_resp_v(Req) ->
    kapi_definition:validate(Req, lookup_resp_definition()).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_lookup_resp(RespQ, JObj) ->
    publish_lookup_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lookup_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_lookup_resp(RespQ, JObj, ContentType) ->
    Definition = lookup_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%% Filter Req
-spec filter_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
filter_req(Req) ->
    kapi_definition:build_message(Req, filter_req_definition()).

-spec filter_req_v(kz_term:api_terms()) -> boolean().
filter_req_v(Req) ->
    kapi_definition:validate(Req, filter_req_definition()).

-spec publish_filter_req(kz_term:api_terms()) -> 'ok'.
publish_filter_req(JObj) ->
    publish_filter_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_filter_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_filter_req(Req, ContentType) ->
    Definition = filter_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:monitor_publish(Payload, ContentType, kapi_definition:binding(Definition)).

%% Filter Resp
-spec filter_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
filter_resp(Req) ->
    kapi_definition:build_message(Req, filter_resp_definition()).

-spec filter_resp_v(kz_term:api_terms()) -> boolean().
filter_resp_v(Req) ->
    kapi_definition:validate(Req, filter_resp_definition()).

-spec publish_filter_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_filter_resp(RespQ, JObj) ->
    publish_filter_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_filter_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_filter_resp(RespQ, JObj, ContentType) ->
    Definition = filter_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%% Bind and UnBind
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_monitor(Q, ?CI_AMQP_KEY(<<"*">>)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:monitor_exchange().
