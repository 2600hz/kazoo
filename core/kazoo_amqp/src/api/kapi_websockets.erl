%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_websockets).

-export([api_definitions/0, api_definition/1]).

-export([get_req/1
        ,get_req_v/1
        ,publish_get_req/1
        ,publish_get_req/2
        ]).
-export([get_resp/1
        ,get_resp_v/1
        ,publish_get_resp/2
        ,publish_get_resp/3
        ]).
-export([module_req/1
        ,module_req_v/1
        ,publish_module_req/1
        ,publish_module_req/2
        ]).
-export([module_resp/1
        ,module_resp_v/1
        ,publish_module_resp/2
        ,publish_module_resp/3
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include("kapi_websockets.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type restriction() :: 'get' | 'module_req'.
-type restrictions() :: [restriction()].
-type bind_prop() :: {'restrict_to', restrictions()}.
-type bind_props() :: [bind_prop()].

-define(DEFAULT_RESTRICTIONS, ['get', 'module_req']).

-export_type([bind_props/0]).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [get_req_definition()
    ,get_resp_definition()
    ,module_req_definition()
    ,module_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"get_req">>) ->
    get_req_definition();
api_definition(<<"get_resp">>) ->
    get_resp_definition();
api_definition(<<"module_req">>) ->
    module_req_definition();
api_definition(<<"module_resp">>) ->
    module_resp_definition().

-spec get_req_definition() -> kapi_definition:api().
get_req_definition() ->
    EventName = <<"get_req">>,
    Category = <<"websockets">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"WebSockets Get Request">>}
              ,{fun kapi_definition:set_description/2, <<"WebSockets Request to Read">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun get_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun get_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_get_req/1}
              ,{fun kapi_definition:set_binding/2, ?KEY_WEBSOCKETS_GET_REQ}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [?KEY_API_ACCOUNT_ID
                                                            ,<<"Socket-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec get_resp_definition() -> kapi_definition:api().
get_resp_definition() ->
    EventName = <<"get_resp">>,
    Category = <<"websockets">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"WebSockets Get Response">>}
              ,{fun kapi_definition:set_description/2, <<"WebSockets Answer to Read Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun get_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun get_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_get_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Data">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec module_req_definition() -> kapi_definition:api().
module_req_definition() ->
    EventName = <<"module_req">>,
    Category = <<"websockets">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"WebSockets Module Request">>}
              ,{fun kapi_definition:set_description/2, <<"WebSockets Module Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun module_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun module_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_module_req/1}
              ,{fun kapi_definition:set_binding/2, ?MODULE_REQ_ROUTING_KEY}
              ,{fun kapi_definition:set_required_headers/2, [<<"Module">>
                                                            ,<<"Action">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Persist">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Event-Category">>, Category}
                ,{<<"Event-Name">>, EventName}
                ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Persist">>, fun kz_term:is_boolean/1}
                ,{<<"Module">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec module_resp_definition() -> kapi_definition:api().
module_resp_definition() ->
    EventName = <<"module_resp">>,
    Category = <<"websockets">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"WebSockets Module Response">>}
              ,{fun kapi_definition:set_description/2, <<"WebSockets Module Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun module_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun module_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_module_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Persisted">>
                                                            ,<<"Started">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Error">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Persisted">>, fun kz_term:is_boolean/1}
                ,{<<"Started">>, fun kz_term:is_boolean/1}
                ,{<<"Error">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%% request to read
-spec get_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
get_req(Req) ->
    kapi_definition:build_message(Req, get_req_definition()).

-spec get_req_v(kz_term:api_terms()) -> boolean().
get_req_v(Req) ->
    kapi_definition:validate(Req, get_req_definition()).

-spec publish_get_req(kz_term:api_terms()) -> 'ok'.
publish_get_req(JObj) ->
    publish_get_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_req(Api, ContentType) ->
    Definition = get_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Api
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:sysconf_publish(kapi_definition:binding(Definition), Payload, ContentType).

%% answer to a read request
-spec get_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
get_resp(Req) ->
    kapi_definition:build_message(Req, get_resp_definition()).

-spec get_resp_v(kz_term:api_terms()) -> boolean().
get_resp_v(Req) ->
    kapi_definition:validate(Req, get_resp_definition()).

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_get_resp(RespQ, JObj) ->
    publish_get_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_resp(RespQ, Api, ContentType) ->
    Definition = get_resp_definition(),
    PrepareOptions = [{'formatter', kapi_definition:build_fun(Definition)}
                     ,{'remove_recursive', 'false'}
                     ],
    {'ok', Payload} = kz_api:prepare_api_payload(Api
                                                ,kapi_definition:values(Definition)
                                                ,PrepareOptions
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%% Module request
-spec module_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
module_req(Req) ->
    kapi_definition:build_message(Req, module_req_definition()).

-spec module_req_v(kz_term:api_terms()) -> boolean().
module_req_v(Req) ->
    kapi_definition:validate(Req, module_req_definition()).

-spec publish_module_req(kz_term:api_terms()) -> 'ok'.
publish_module_req(API) ->
    publish_module_req(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_module_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_module_req(API, ContentType) ->
    Definition = module_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:kapps_publish(kapi_definition:binding(Definition), Payload, ContentType).

%% Module response
-spec module_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
module_resp(Req) ->
    kapi_definition:build_message(Req, module_resp_definition()).

-spec module_resp_v(kz_term:api_terms()) -> boolean().
module_resp_v(Req) ->
    kapi_definition:validate(Req, module_resp_definition()).

-spec publish_module_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_module_resp(ServerId, API) ->
    publish_module_resp(ServerId, API, ?DEFAULT_CONTENT_TYPE).

-spec publish_module_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_module_resp(ServerId, API, ContentType) ->
    Definition = module_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), bind_props()) -> 'ok'.
bind_q(Queue, Props) ->
    lists:foreach(fun(Restriction) -> add_restriction(Queue, Restriction) end
                 ,props:get_value('restrict_to', Props, ?DEFAULT_RESTRICTIONS)
                 ).

-spec add_restriction(kz_term:ne_binary(), restriction()) -> 'ok'.
add_restriction(Queue, 'get') ->
    kz_amqp_util:bind_q_to_sysconf(Queue, ?KEY_WEBSOCKETS_GET_REQ);
add_restriction(Queue, 'module_req') ->
    kz_amqp_util:bind_q_to_kapps(Queue, ?MODULE_REQ_ROUTING_KEY).


-spec unbind_q(kz_term:ne_binary(), bind_props()) -> 'ok'.
unbind_q(Queue, Props) ->
    lists:foreach(fun(Restriction) -> remove_restriction(Queue, Restriction) end
                 ,props:get_value('restrict_to', Props, ?DEFAULT_RESTRICTIONS)
                 ).

-spec remove_restriction(kz_term:ne_binary(), restriction()) -> 'ok'.
remove_restriction(Queue, 'get') ->
    kz_amqp_util:unbind_q_from_sysconf(Queue, ?KEY_WEBSOCKETS_GET_REQ);
remove_restriction(Queue, 'module_req') ->
    kz_amqp_util:unbind_q_from_kapps(Queue, ?MODULE_REQ_ROUTING_KEY).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange(),
    kz_amqp_util:targeted_exchange(),
    kz_amqp_util:sysconf_exchange().
