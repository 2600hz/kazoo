%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle registration-related APIs, like `reg_success' and `reg_lookup'.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_registration).

-export([api_definitions/0, api_definition/1]).

-export([success/1
        ,success_v/1
        ,publish_success/1
        ,publish_success/2
        ]).
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
-export([query_err/1
        ,query_err_v/1
        ,publish_query_err/2
        ,publish_query_err/3
        ]).
-export([flush/1
        ,flush_v/1
        ,publish_flush/1
        ,publish_flush/2
        ]).
-export([sync/1
        ,sync_v/1
        ,publish_sync/1
        ,publish_sync/2
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([success_keys/0]).

-include("kz_amqp_util.hrl").

-ifdef(TEST).
-export([get_success_routing/2
        ,get_query_routing/2
        ,get_flush_routing/1
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [success_definition()
    ,query_req_definition()
    ,query_resp_definition()
    ,query_err_definition()
    ,flush_definition()
    ,sync_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"success">>) ->
    success_definition();
api_definition(<<"query_req">>) ->
    query_req_definition();
api_definition(<<"query_resp">>) ->
    query_resp_definition();
api_definition(<<"query_err">>) ->
    query_err_definition();
api_definition(<<"flush">>) ->
    flush_definition();
api_definition(<<"sync">>) ->
    sync_definition().

-spec success_definition() -> kapi_definition:api().
success_definition() ->
    EventName = <<"reg_success">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Success">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Success">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun success/1}
              ,{fun kapi_definition:set_validate_fun/2, fun success_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_success/1}
              ,{fun kapi_definition:set_binding/2, fun get_success_routing/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Contact">>
                                                            ,<<"Event-Timestamp">>
                                                            ,<<"Expires">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-DB">>
                                                            ,<<"AOR">>
                                                            ,<<"Authorizing-ID">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"First-Registration">>
                                                            ,<<"FreeSWITCH-Hostname">>
                                                            ,<<"FreeSWITCH-Nodename">>
                                                            ,<<"From-Host">>
                                                            ,<<"From-User">>
                                                            ,?KEY_API_ACCOUNT_ID
                                                            ,?KEY_API_CALL_ID
                                                            ,<<"Network-IP">>
                                                            ,<<"Network-Port">>
                                                            ,<<"Original-Contact">>
                                                            ,<<"Presence-Hosts">>
                                                            ,<<"Profile-Name">>
                                                            ,<<"Proxy-IP">>
                                                            ,<<"Proxy-Path">>
                                                            ,<<"Proxy-Port">>
                                                            ,<<"Proxy-Protocol">>
                                                            ,<<"Register-Overwrite-Notify">>
                                                            ,<<"Registrar-Node">>
                                                            ,<<"RPid">>
                                                            ,<<"RUID">>
                                                            ,<<"Source-IP">>
                                                            ,<<"Source-Port">>
                                                            ,<<"Status">>
                                                            ,<<"Suppress-Unregister-Notify">>
                                                            ,<<"To-Host">>
                                                            ,<<"To-User">>
                                                            ,<<"User-Agent">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_req_definition() -> kapi_definition:api().
query_req_definition() ->
    EventName = <<"reg_query">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Query">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Query">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_req/1}
              ,{fun kapi_definition:set_binding/2, fun get_query_routing/1}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Count-Only">>
                                                            ,<<"Fields">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Fields">>, fun(Fs) when is_list(Fs) ->
                                        SuccessDef = success_definition(),
                                        Allowed = kapi_definition:optional_headers(SuccessDef) ++
                                            kapi_definition:required_headers(SuccessDef) ++
                                            [<<"Bridge-RURI">>],
                                        lists:all(fun(F) -> lists:member(F, Allowed) end, Fs);
                                   (_) -> 'false'
                                end}
                ,{<<"Count-Only">>, fun(N) -> kz_term:to_boolean(N) end}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_resp_definition() -> kapi_definition:api().
query_resp_definition() ->
    EventName = <<"reg_query_resp">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Query Response">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Query Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Count">>
                                                            ,<<"Fields">>
                                                            ,<<"Registrar-Age">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_err_definition() -> kapi_definition:api().
query_err_definition() ->
    EventName = <<"reg_query_error">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Query Error">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Query Error">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_err/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_err_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_err/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Registrar-Age">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec flush_definition() -> kapi_definition:api().
flush_definition() ->
    EventName = <<"reg_flush">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Flush">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Flush">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun flush/1}
              ,{fun kapi_definition:set_validate_fun/2, fun flush_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_flush/1}
              ,{fun kapi_definition:set_binding/2, fun get_flush_routing/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Realm">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec sync_definition() -> kapi_definition:api().
sync_definition() ->
    EventName = <<"reg_sync">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration Sync">>}
              ,{fun kapi_definition:set_description/2, <<"Directory Registration Sync">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sync/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sync_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_sync/1}
              ,{fun kapi_definition:set_binding/2, <<"registration.sync">>}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Registration Success.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec success(kz_term:api_terms()) -> kz_api:api_formatter_return().
success(Req) ->
    kapi_definition:build_message(Req, success_definition()).

-spec success_v(kz_term:api_terms()) -> boolean().
success_v(Req) ->
    kapi_definition:validate(Req, success_definition()).

-spec publish_success(kz_term:api_terms()) -> 'ok'.
publish_success(JObj) ->
    publish_success(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_success(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_success(Success, ContentType) ->
    Definition = success_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Success
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:registrar_publish((kapi_definition:binding(Definition))(Success)
                                  ,Payload
                                  ,ContentType
                                  ).

%%------------------------------------------------------------------------------
%% @doc Registration Query.
%% Takes proplist, creates JSON string or error.
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
publish_query_req(Req, ContentType) ->
    Definition = query_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:registrar_publish((kapi_definition:binding(Definition))(Req)
                                  ,Payload
                                  ,ContentType
                                  ).

%%------------------------------------------------------------------------------
%% @doc Registration Query Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_resp(Req) ->
    kapi_definition:build_message(Req, query_resp_definition()).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Req) ->
    kapi_definition:validate(Req, query_resp_definition()).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(Queue, JObj) ->
    publish_query_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(Queue, Resp, ContentType) ->
    Definition = query_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Registration Query Error Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_err(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_err(Req) ->
    kapi_definition:build_message(Req, query_err_definition()).

-spec query_err_v(kz_term:api_terms()) -> boolean().
query_err_v(Req) ->
    kapi_definition:validate(Req, query_err_definition()).

-spec publish_query_err(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_err(Queue, JObj) ->
    publish_query_err(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_err(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_err(Queue, Resp, ContentType) ->
    Definition = query_err_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Registration Flush
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:api_terms()) -> kz_api:api_formatter_return().
flush(Req) ->
    kapi_definition:build_message(Req, flush_definition()).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Req) ->
    kapi_definition:validate(Req, flush_definition()).

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush(API, ContentType) ->
    Definition = flush_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:registrar_publish((kapi_definition:binding(Definition))(API)
                                  ,Payload
                                  ,ContentType
                                  ).

%%------------------------------------------------------------------------------
%% @doc Registration Sync
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:api_terms()) -> kz_api:api_formatter_return().
sync(Req) ->
    kapi_definition:build_message(Req, sync_definition()).

-spec sync_v(kz_term:api_terms()) -> boolean().
sync_v(Req) ->
    kapi_definition:validate(Req, sync_definition()).

-spec publish_sync(kz_term:api_terms()) -> 'ok'.
publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync(API, ContentType) ->
    Definition = sync_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:registrar_publish(kapi_definition:binding(Definition)
                                  ,Payload
                                  ,ContentType
                                  ).

%%------------------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn `gen_listeners'.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q(Q, props:get_value('restrict_to', Props), Props).

bind_q(Q, 'undefined', Props) ->
    _ = kz_amqp_util:bind_q_to_registrar(Q, get_success_binding(Props)),
    kz_amqp_util:bind_q_to_registrar(Q, get_query_binding(Props));
bind_q(Q, ['reg_success'|T], Props) ->
    _ = kz_amqp_util:bind_q_to_registrar(Q, get_success_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_query'|T], Props) ->
    _ = kz_amqp_util:bind_q_to_registrar(Q, get_query_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = kz_amqp_util:bind_q_to_registrar(Q, (kapi_definition:binding(flush_definition()))(Realm)),
    bind_q(Q, T, Props);
bind_q(Q, [_|T], Props) -> bind_q(Q, T, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q(Q, props:get_value('restrict_to', Props), Props).

unbind_q(Q, 'undefined', Props) ->
    _ = kz_amqp_util:unbind_q_from_registrar(Q, get_success_binding(Props)),
    kz_amqp_util:unbind_q_from_registrar(Q, get_query_binding(Props));
unbind_q(Q, ['reg_success'|T], Props) ->
    _ = kz_amqp_util:unbind_q_from_registrar(Q, get_success_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_query'|T], Props) ->
    _ = kz_amqp_util:unbind_q_from_registrar(Q, get_query_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = kz_amqp_util:unbind_q_from_registrar(Q
                                            ,(kapi_definition:binding(flush_definition()))(Realm)
                                            ),
    unbind_q(Q, T, Props);
unbind_q(Q, [_|T], Props) -> unbind_q(Q, T, Props);
unbind_q(_, [], _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:registrar_exchange().

%%------------------------------------------------------------------------------
%% @doc Special access to the API keys.
%% @end
%%------------------------------------------------------------------------------
-spec success_keys() -> kz_term:ne_binaries().
success_keys() ->
    SuccessDef = success_definition(),
    kapi_definition:optional_headers(SuccessDef) ++ kapi_definition:required_headers(SuccessDef).

-spec get_success_routing(kz_term:api_terms()) -> kz_term:ne_binary().
get_success_routing(Prop) when is_list(Prop) ->
    get_success_routing(props:get_value(<<"Realm">>, Prop), props:get_value(<<"Username">>, Prop));
get_success_routing(JObj) ->
    get_success_routing(kz_json:get_value(<<"Realm">>, JObj)
                       ,kz_json:get_value(<<"Username">>, JObj)
                       ).

-spec get_success_routing(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_success_routing(Realm, User) ->
    list_to_binary([<<"registration.success.">>
                   ,kz_amqp_util:encode(Realm)
                   ,".", kz_amqp_util:encode(User)
                   ]).

-spec get_query_routing(kz_term:api_terms()) -> kz_term:ne_binary().
get_query_routing(Prop) when is_list(Prop) ->
    get_query_routing(props:get_value(<<"Realm">>, Prop), props:get_value(<<"Username">>, Prop));
get_query_routing(JObj) ->
    get_query_routing(kz_json:get_value(<<"Realm">>, JObj)
                     ,kz_json:get_value(<<"Username">>, JObj)
                     ).

-spec get_query_routing(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_query_routing(Realm, 'undefined') ->
    get_query_routing(Realm, <<"*">>);
get_query_routing(Realm, User) ->
    list_to_binary([<<"registration.query.">>
                   ,kz_amqp_util:encode(Realm)
                   ,".", kz_amqp_util:encode(User)
                   ]).

%%------------------------------------------------------------------------------
%% @doc Allow Queues to be bound for specific realms, and even users within those realms.
%% The resulting binding will be `reg.success.{realm | *}.{user | *}'.
%% The `*' matches one segment only, which means all `reg_success' messages will be published to
%% `key.success.realm.user'.
%% @end
%%------------------------------------------------------------------------------
get_success_binding(Props) ->
    get_success_routing(props:get_value('realm', Props, <<"*">>)
                       ,props:get_value('user', Props, <<"*">>)
                       ).

get_query_binding(Props) ->
    get_query_routing(props:get_value('realm', Props, <<"*">>)
                     ,props:get_value('user', Props, <<"*">>)
                     ).

-spec get_flush_routing(kz_term:ne_binary() | kz_term:proplist() | kz_json:object()) -> kz_term:ne_binary().
get_flush_routing(Realm) when is_binary(Realm) ->
    <<"registration.flush.", (kz_amqp_util:encode(Realm))/binary>>;
get_flush_routing(Prop) when is_list(Prop) ->
    get_flush_routing(props:get_value(<<"Realm">>, Prop));
get_flush_routing(JObj) ->
    get_flush_routing(kz_json:get_value(<<"Realm">>, JObj)).
