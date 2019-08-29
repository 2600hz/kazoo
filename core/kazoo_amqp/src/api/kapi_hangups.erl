%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_hangups).

-export([api_definitions/0, api_definition/1]).

-export([query_req/1
        ,query_req_v/1
        ,publish_query_req/1
        ,publish_query_req/2
        ]).

-export([query_resp/1
        ,query_resp_v/1
        ,publish_query_resp/2
        ,publish_query_resp/3
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include_lib("kz_amqp_util.hrl").

-define(QUERY_REQ_ROUTING_KEY, <<"hangups.query">>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [query_req_definition()
    ,query_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"query_req">>) ->
    query_req_definition();
api_definition(<<"query_resp">>) ->
    query_resp_definition().

-spec query_req_definition() -> kapi_definition:api().
query_req_definition() ->
    EventName = <<"query_req">>,
    Category = <<"hangups">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Hangups Query Request">>}
              ,{fun kapi_definition:set_description/2, <<"Hangups Query Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_req/1}
              ,{fun kapi_definition:set_binding/2, ?QUERY_REQ_ROUTING_KEY}
              ,{fun kapi_definition:set_required_headers/2, [<<"Hangup-Cause">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Raw-Data">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, [{<<"Raw-Data">>, fun kz_term:is_boolean/1}]}
              ],
    kapi_definition:setters(Setters).

-spec query_resp_definition() -> kapi_definition:api().
query_resp_definition() ->
    EventName = <<"query_resp">>,
    Category = <<"hangups">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Hangups Query Response">>}
              ,{fun kapi_definition:set_description/2, <<"Hangups Query Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"mean">>
                ,<<"one_to_five">>, <<"five_to_fifteen">>, <<"one_to_fifteen">>
                ,<<"start_time">>
                ,<<"account_id">>
                ,<<"hangup_cause">>
                ,<<"one">>, <<"five">>, <<"fifteen">>, <<"day">>
                ,<<"count">>
                ,<<"meters">>
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_req(Req) ->
    kapi_definition:build_message(Req, query_req_definition()).

-spec query_req_v(kz_term:api_terms()) -> boolean().
query_req_v(Req) ->
    kapi_definition:validate(Req, query_req_definition()).

-spec publish_query_req(kz_term:api_terms()) -> 'ok'.
publish_query_req(JObj) ->
    publish_query_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_req(API, ContentType) ->
    Definition = query_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:kapps_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_resp(Req) ->
    kapi_definition:build_message(Req, query_resp_definition()).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Req) ->
    kapi_definition:validate(Req, query_resp_definition()).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(RespQ, JObj) ->
    publish_query_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(RespQ, API, ContentType) ->
    Definition = query_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    'ok' = kz_amqp_util:bind_q_to_kapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    'ok' = kz_amqp_util:unbind_q_from_kapps(Queue, ?QUERY_REQ_ROUTING_KEY).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().
