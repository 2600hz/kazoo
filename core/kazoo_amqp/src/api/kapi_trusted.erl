%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_trusted).

-include("kz_amqp_util.hrl").

-export([api_definitions/0, api_definition/1]).

-export([query/1
        ,query_v/1
        ,publish_query/0, publish_query/1
        ]).

-export([reply/1
        ,reply_v/1
        ,publish_reply/2
        ]).

-export([reload/1
        ,reload_v/1
        ,publish_reload/0, publish_reload/1
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-define(EXCHANGE_TRUSTED, <<"trusted">>).
-define(TYPE_TRUSTED, <<"topic">>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [query_definition()
    ,reply_definition()
    ,reload_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"query">>) ->
    query_definition();
api_definition(<<"reply">>) ->
    reply_definition();
api_definition(<<"reload">>) ->
    reload_definition().

-spec query_definition() -> kapi_definition:api().
query_definition() ->
    EventName = <<"query">>,
    Category = <<"trusted">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Trusted Query Request">>}
              ,{fun kapi_definition:set_description/2, <<"Trusted Query Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query/1}
              ,{fun kapi_definition:set_binding/2, <<"trusted.query">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Server-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec reply_definition() -> kapi_definition:api().
reply_definition() ->
    EventName = <<"reply">>,
    Category = <<"trusted">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Trusted Reply">>}
              ,{fun kapi_definition:set_description/2, <<"Trusted Reply">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reply/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reply_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reply/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Trusted">>]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, [{<<"Trusted">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

-spec reload_definition() -> kapi_definition:api().
reload_definition() ->
    EventName = <<"reload">>,
    Category = <<"trusted">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Trusted Reload">>}
              ,{fun kapi_definition:set_description/2, <<"Trusted Reload">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reload/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reload_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reload/1}
              ,{fun kapi_definition:set_binding/2, <<"trusted.reload">>}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%% QUERY
-spec query(kz_term:api_terms()) -> kz_api:api_formatter_return().
query(Req) ->
    kapi_definition:build_message(Req, query_definition()).

-spec query_v(kz_term:api_terms()) -> boolean().
query_v(Req) ->
    kapi_definition:validate(Req, query_definition()).

-spec publish_query() -> 'ok'.
publish_query() ->
    publish_query(kz_json:new()).

-spec publish_query(kz_term:api_terms()) -> 'ok'.
publish_query(Req) ->
    Definition = query_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?EXCHANGE_TRUSTED, <<"trusted.query">>, Payload).

%% REPLY
-spec reply(kz_term:api_terms()) -> kz_api:api_formatter_return().
reply(Req) ->
    kapi_definition:build_message(Req, reply_definition()).

-spec reply_v(kz_term:api_terms()) -> boolean().
reply_v(Req) ->
    kapi_definition:validate(Req, reply_definition()).

-spec publish_reply(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_reply(Target, JObj) ->
    Definition = reply_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Target, Payload).

%% RELOAD
-spec reload(kz_term:api_terms()) -> kz_api:api_formatter_return().
reload(Req) ->
    kapi_definition:build_message(Req, reload_definition()).

-spec reload_v(kz_term:api_terms()) -> boolean().
reload_v(Req) ->
    kapi_definition:validate(Req, reload_definition()).

-spec publish_reload() -> 'ok'.
publish_reload() ->
    publish_reload(kz_json:new()).

-spec publish_reload(kz_term:api_terms()) -> 'ok'.
publish_reload(Req) ->
    Definition = reload_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?EXCHANGE_TRUSTED, <<"trusted.reload">>, Payload).

%% Bind and UnBind
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, <<"trusted.*">>);
bind_to_q(Q, ['reload'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, <<"trusted.reload">>, ?EXCHANGE_TRUSTED),
    bind_to_q(Q, T);
bind_to_q(Q, ['query'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, <<"trusted.query">>, ?EXCHANGE_TRUSTED),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, <<"trusted.*">>, ?EXCHANGE_TRUSTED);
unbind_q_from(Q, ['reload'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, <<"trusted.reload">>, ?EXCHANGE_TRUSTED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['query'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, <<"trusted.query">>, ?EXCHANGE_TRUSTED),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?EXCHANGE_TRUSTED, ?TYPE_TRUSTED).
