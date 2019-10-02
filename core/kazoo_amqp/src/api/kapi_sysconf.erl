%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Expose system configuration data.
%%% System configuration data is stored as key/values in a namespace
%%% (a doc) in `system_config' DB.
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_sysconf).

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
-export([set_req/1
        ,set_req_v/1
        ,publish_set_req/1
        ,publish_set_req/2
        ]).
-export([set_resp/1
        ,set_resp_v/1
        ,publish_set_resp/2
        ,publish_set_resp/3
        ]).
-export([flush_req/1
        ,flush_req_v/1
        ,publish_flush_req/1
        ,publish_flush_req/2
        ]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,get_category/1, get_category/2
        ,get_key/1, get_key/2
        ,get_value/1, get_value/2
        ]).

-include_lib("kz_amqp_util.hrl").

-define(CAT_KEY, <<"Category">>).
-define(KEY_KEY, <<"Key">>).
-define(VALUE_KEY, <<"Value">>).

-define(EVENT_CATEGORY, <<"sysconf">>).

-define(SYSCONF_TYPES, [{?CAT_KEY, fun is_binary/1}
                       ,{<<"Node">>, fun is_binary/1}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [get_req_definition()
    ,get_resp_definition()
    ,set_req_definition()
    ,set_resp_definition()
    ,flush_req_definition()
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
api_definition(<<"set_req">>) ->
    set_req_definition();
api_definition(<<"set_resp">>) ->
    set_resp_definition();
api_definition(<<"flush_req">>) ->
    flush_req_definition().

-spec get_req_definition() -> kapi_definition:api().
get_req_definition() ->
    EventName = <<"get_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Request To Read">>}
              ,{fun kapi_definition:set_description/2
               , <<"Configuration Document Update - Request To Read">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun get_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun get_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_get_req/1}
              ,{fun kapi_definition:set_binding/2, <<"sysconf.get">>}
              ,{fun kapi_definition:set_required_headers/2, [?CAT_KEY
                                                            ,?KEY_KEY
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Default">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, ?SYSCONF_TYPES}
              ],
    kapi_definition:setters(Setters).

-spec get_resp_definition() -> kapi_definition:api().
get_resp_definition() ->
    EventName = <<"get_resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Answer to Read Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Configuration Document Update - Answer to Read Request">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun get_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun get_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_get_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [?CAT_KEY
                                                            ,?KEY_KEY
                                                            ,?VALUE_KEY
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, ?SYSCONF_TYPES}
              ],
    kapi_definition:setters(Setters).

-spec set_req_definition() -> kapi_definition:api().
set_req_definition() ->
    EventName = <<"set_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Request to Write">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Configuration Document Update - Request to Write">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun set_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun set_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_set_req/1}
              ,{fun kapi_definition:set_binding/2, <<"sysconf.set">>}
              ,{fun kapi_definition:set_required_headers/2, [?CAT_KEY
                                                            ,?KEY_KEY
                                                            ,?VALUE_KEY
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Node-Specific">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, ?SYSCONF_TYPES}
              ],
    kapi_definition:setters(Setters).

-spec set_resp_definition() -> kapi_definition:api().
set_resp_definition() ->
    EventName = <<"set_resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Answer to Write Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Configuration Document Update - Answer to Write Request">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun set_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun set_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_set_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [?CAT_KEY
                                                            ,?KEY_KEY
                                                            ,?VALUE_KEY
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Status">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, ?SYSCONF_TYPES}
              ],
    kapi_definition:setters(Setters).

-spec flush_req_definition() -> kapi_definition:api().
flush_req_definition() ->
    EventName = <<"flush_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Flush Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Configuration Document Update - Flush a given key">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun flush_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun flush_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_flush_req/1}
              ,{fun kapi_definition:set_binding/2, <<"sysconf.flush">>}
              ,{fun kapi_definition:set_required_headers/2, [?CAT_KEY]}
              ,{fun kapi_definition:set_optional_headers/2, [?KEY_KEY]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, ?SYSCONF_TYPES}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Request to READ.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Answer to a READ request.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Request to WRITE.
%% @end
%%------------------------------------------------------------------------------
-spec set_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
set_req(Req) ->
    kapi_definition:build_message(Req, set_req_definition()).

-spec set_req_v(kz_term:api_terms()) -> boolean().
set_req_v(Req) ->
    kapi_definition:validate(Req, set_req_definition()).

-spec publish_set_req(kz_term:api_terms()) -> 'ok'.
publish_set_req(JObj) ->
    publish_set_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_set_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_set_req(Api, ContentType) ->
    Definition = set_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Api
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:sysconf_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Answer to Request to WRITE.
%% @end
%%------------------------------------------------------------------------------
-spec set_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
set_resp(Req) ->
    kapi_definition:build_message(Req, set_resp_definition()).

-spec set_resp_v(kz_term:api_terms()) -> boolean().
set_resp_v(Req) ->
    kapi_definition:validate(Req, set_resp_definition()).

-spec publish_set_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_set_resp(RespQ, JObj) ->
    publish_set_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_set_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_set_resp(RespQ, Api, ContentType) ->
    Definition = set_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Api
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Flush a given key.
%% @end
%%------------------------------------------------------------------------------
-spec flush_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
flush_req(Req) ->
    kapi_definition:build_message(Req, flush_req_definition()).

-spec flush_req_v(kz_term:api_terms()) -> boolean().
flush_req_v(Req) ->
    kapi_definition:validate(Req, flush_req_definition()).

-spec publish_flush_req(kz_term:api_terms()) -> 'ok'.
publish_flush_req(JObj) ->
    publish_flush_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush_req(Api, ContentType) ->
    Definition = flush_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Api
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:sysconf_publish(kapi_definition:binding(Definition), Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Prop) ->
    add_bindings(Q, props:get_value('restrict_to', Prop)).

add_bindings(Q, 'undefined') ->
    add_bindings(Q, ['get', 'set', 'flush']);
add_bindings(Q, ['get'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, kapi_definition:binding(get_req_definition())),
    add_bindings(Q, T);
add_bindings(Q, ['set'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, kapi_definition:binding(set_req_definition())),
    add_bindings(Q, T);
add_bindings(Q, ['flush'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, kapi_definition:binding(flush_req_definition())),
    add_bindings(Q, T);
add_bindings(Q, [_|T]) ->
    add_bindings(Q, T);
add_bindings(_, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Prop) ->
    rm_bindings(Q, props:get_value('restrict_to', Prop)).

rm_bindings(Q, 'undefined') ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(get_req_definition())),
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(set_req_definition())),
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(flush_req_definition()));
rm_bindings(Q, ['get'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(get_req_definition())),
    rm_bindings(Q, T);
rm_bindings(Q, ['set'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(set_req_definition())),
    rm_bindings(Q, T);
rm_bindings(Q, ['flush'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, kapi_definition:binding(flush_req_definition())),
    rm_bindings(Q, T);
rm_bindings(Q, [_|T]) ->
    rm_bindings(Q, T);
rm_bindings(_, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:sysconf_exchange().

-spec get_category(kz_json:object()) -> kz_term:api_binary().
get_category(JObj) ->
    get_category(JObj, 'undefined').

-spec get_category(kz_json:object(), Default) -> kz_term:ne_binary() | Default.
get_category(JObj, Default) ->
    kz_json:get_value(?CAT_KEY, JObj, Default).

-spec get_key(kz_json:object()) -> kz_term:api_binary().
get_key(JObj) ->
    get_key(JObj, 'undefined').

-spec get_key(kz_json:object(), Default) -> kz_term:ne_binary() | Default.
get_key(JObj, Default) ->
    kz_json:get_value(?KEY_KEY, JObj, Default).

-spec get_value(kz_json:object()) -> kz_term:api_object().
get_value(JObj) ->
    get_value(JObj, 'undefined').

-spec get_value(kz_json:object(), Default) -> kz_json:object() | Default.
get_value(JObj, Default) ->
    kz_json:get_value(?VALUE_KEY, JObj, Default).
