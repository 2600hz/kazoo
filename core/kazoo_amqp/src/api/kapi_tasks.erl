%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_tasks).

-export([api_definitions/0, api_definition/1]).

-export([category/1
        ,action/1
        ,task_id/1
        ,reply/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

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
-export([start_req/1
        ,start_req_v/1
        ,publish_start_req/1
        ,publish_start_req/2
        ]).
-export([start_resp/1
        ,start_resp_v/1
        ,publish_start_resp/2
        ,publish_start_resp/3
        ]).
-export([stop_req/1
        ,stop_req_v/1
        ,publish_stop_req/1
        ,publish_stop_req/2
        ]).
-export([stop_resp/1
        ,stop_resp_v/1
        ,publish_stop_resp/2
        ,publish_stop_resp/3
        ]).
-export([remove_req/1
        ,remove_req_v/1
        ,publish_remove_req/1
        ,publish_remove_req/2
        ]).
-export([remove_resp/1
        ,remove_resp_v/1
        ,publish_remove_resp/2
        ,publish_remove_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(TASKS_AMQP_KEY(SubKey), <<"tasks.", SubKey>>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [lookup_req_definition()
    ,lookup_resp_definition()
    ,start_req_definition()
    ,start_resp_definition()
    ,stop_req_definition()
    ,stop_resp_definition()
    ,remove_req_definition()
    ,remove_resp_definition()
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
api_definition(<<"start_req">>) ->
    start_req_definition();
api_definition(<<"start_resp">>) ->
    start_resp_definition();
api_definition(<<"stop_req">>) ->
    stop_req_definition();
api_definition(<<"stop_resp">>) ->
    stop_resp_definition();
api_definition(<<"remove_req">>) ->
    remove_req_definition();
api_definition(<<"remove_resp">>) ->
    remove_resp_definition().

-spec lookup_req_definition() -> kapi_definition:api().
lookup_req_definition() ->
    EventName = <<"lookup_req">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Lookup Request">>}
              ,{fun kapi_definition:set_description/2, <<"Lookup Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun lookup_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun lookup_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_lookup_req/1}
              ,{fun kapi_definition:set_binding/2, ?TASKS_AMQP_KEY("lookup")}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Category">>
                                                            ,<<"Action">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec lookup_resp_definition() -> kapi_definition:api().
lookup_resp_definition() ->
    EventName = <<"lookup_resp">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Lookup Response">>}
              ,{fun kapi_definition:set_description/2, <<"Lookup Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun lookup_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun lookup_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_lookup_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Help">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec start_req_definition() -> kapi_definition:api().
start_req_definition() ->
    EventName = <<"start_req">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Start Request">>}
              ,{fun kapi_definition:set_description/2, <<"Start Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun start_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun start_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_start_req/1}
              ,{fun kapi_definition:set_binding/2, ?TASKS_AMQP_KEY("start")}
              ,{fun kapi_definition:set_required_headers/2, [<<"Task-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec start_resp_definition() -> kapi_definition:api().
start_resp_definition() ->
    EventName = <<"start_resp">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Start Response">>}
              ,{fun kapi_definition:set_description/2, <<"Start Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun start_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun start_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_start_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Reply">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec stop_req_definition() -> kapi_definition:api().
stop_req_definition() ->
    EventName = <<"stop_req">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Stop Request">>}
              ,{fun kapi_definition:set_description/2, <<"Stop Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun stop_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun stop_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_stop_req/1}
              ,{fun kapi_definition:set_binding/2, ?TASKS_AMQP_KEY("stop")}
              ,{fun kapi_definition:set_required_headers/2, [<<"Task-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec stop_resp_definition() -> kapi_definition:api().
stop_resp_definition() ->
    EventName = <<"stop_resp">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Stop Response">>}
              ,{fun kapi_definition:set_description/2, <<"Stop Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun stop_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun stop_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_stop_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Reply">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec remove_req_definition() -> kapi_definition:api().
remove_req_definition() ->
    EventName = <<"remove_req">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Remove Request">>}
              ,{fun kapi_definition:set_description/2, <<"Remove Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun remove_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun remove_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_remove_req/1}
              ,{fun kapi_definition:set_binding/2, ?TASKS_AMQP_KEY("remove")}
              ,{fun kapi_definition:set_required_headers/2, [<<"Task-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec remove_resp_definition() -> kapi_definition:api().
remove_resp_definition() ->
    EventName = <<"remove_resp">>,
    Category = <<"tasks">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Remove Response">>}
              ,{fun kapi_definition:set_description/2, <<"Remove Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun remove_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun remove_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_remove_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Reply">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec category(kz_json:object()) -> kz_term:api_binary().
category(JObj) ->
    kz_json:get_value(<<"Category">>, JObj).

-spec action(kz_json:object()) -> kz_term:api_binary().
action(JObj) ->
    kz_json:get_value(<<"Action">>, JObj).

-spec task_id(kz_json:object()) -> kz_tasks:id().
task_id(JObj) ->
    kz_json:get_value(<<"Task-ID">>, JObj).

-spec reply(kz_json:object()) -> kz_json:object() | kz_term:ne_binary().
reply(JObj) ->
    kz_json:get_value(<<"Reply">>, JObj).


-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_to_q(Q, props:get_value('restrict_to', Props)).

-spec bind_to_q(kz_term:ne_binary(), kz_term:atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_tasks(Q, ?TASKS_AMQP_KEY("*"));
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_from_q(Q, props:get_value('restrict_to', Props)).

-spec unbind_from_q(kz_term:ne_binary(), kz_term:atoms()) -> 'ok'.
unbind_from_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_tasks(Q, ?TASKS_AMQP_KEY("*"));
unbind_from_q(_Q, []) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:tasks_exchange().

%%------------------------------------------------------------------------------
%% @doc Lookup Request
%% @end
%%------------------------------------------------------------------------------
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
    kz_amqp_util:tasks_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Lookup Response
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Start Request
%% @end
%%------------------------------------------------------------------------------
-spec start_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
start_req(Req) ->
    kapi_definition:build_message(Req, start_req_definition()).

-spec start_req_v(kz_term:api_terms()) -> boolean().
start_req_v(Req) ->
    kapi_definition:validate(Req, start_req_definition()).

-spec publish_start_req(kz_term:api_terms()) -> 'ok'.
publish_start_req(JObj) ->
    publish_start_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_start_req(Req, ContentType) ->
    Definition = start_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:tasks_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Start Response
%% @end
%%------------------------------------------------------------------------------
-spec start_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
start_resp(Req) ->
    kapi_definition:build_message(Req, start_resp_definition()).

-spec start_resp_v(kz_term:api_terms()) -> boolean().
start_resp_v(Req) ->
    kapi_definition:validate(Req, start_resp_definition()).

-spec publish_start_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_start_resp(RespQ, JObj) ->
    publish_start_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_start_resp(RespQ, JObj, ContentType) ->
    Definition = start_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Stop Request
%% @end
%%------------------------------------------------------------------------------
-spec stop_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
stop_req(Req) ->
    kapi_definition:build_message(Req, stop_req_definition()).

-spec stop_req_v(kz_term:api_terms()) -> boolean().
stop_req_v(Req) ->
    kapi_definition:validate(Req, stop_req_definition()).

-spec publish_stop_req(kz_term:api_terms()) -> 'ok'.
publish_stop_req(JObj) ->
    publish_stop_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stop_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_stop_req(Req, ContentType) ->
    Definition = stop_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:tasks_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Stop Response
%% @end
%%------------------------------------------------------------------------------
-spec stop_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
stop_resp(Req) ->
    kapi_definition:build_message(Req, stop_resp_definition()).

-spec stop_resp_v(kz_term:api_terms()) -> boolean().
stop_resp_v(Req) ->
    kapi_definition:validate(Req, stop_resp_definition()).

-spec publish_stop_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_stop_resp(RespQ, JObj) ->
    publish_stop_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stop_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_stop_resp(RespQ, JObj, ContentType) ->
    Definition = stop_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Remove Request
%% @end
%%------------------------------------------------------------------------------
-spec remove_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
remove_req(Req) ->
    kapi_definition:build_message(Req, remove_req_definition()).

-spec remove_req_v(kz_term:api_terms()) -> boolean().
remove_req_v(Req) ->
    kapi_definition:validate(Req, remove_req_definition()).

-spec publish_remove_req(kz_term:api_terms()) -> 'ok'.
publish_remove_req(JObj) ->
    publish_remove_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_remove_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_remove_req(Req, ContentType) ->
    Definition = remove_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:tasks_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Remove Response
%% @end
%%------------------------------------------------------------------------------
-spec remove_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
remove_resp(Req) ->
    kapi_definition:build_message(Req, remove_resp_definition()).

-spec remove_resp_v(kz_term:api_terms()) -> boolean().
remove_resp_v(Req) ->
    kapi_definition:validate(Req, remove_resp_definition()).

-spec publish_remove_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_remove_resp(RespQ, JObj) ->
    publish_remove_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_remove_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_remove_resp(RespQ, JObj, ContentType) ->
    Definition = remove_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
