%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Switch events messages.
%%% @author Edouard Swiac
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_switch).

-export([api_definitions/0, api_definition/1]).

-export([reload_acls/1
        ,reload_acls_v/1
        ,publish_reload_acls/0
        ]).
-export([reload_gateways/1
        ,reload_gateways_v/1
        ,publish_reload_gateways/0
        ]).
-export([fs_xml_flush/1
        ,fs_xml_flush_v/1
        ,publish_fs_xml_flush/1
        ]).
-export([notify/1
        ,notify_v/1
        ,publish_notify/1
        ,publish_notify/2
        ]).
-export([fs_command/1
        ,fs_command_v/1
        ,publish_fs_command/1
        ,publish_fs_command/2
        ]).
-export([fs_reply/1
        ,fs_reply_v/1
        ,publish_fs_reply/2
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([notify_realm/1, notify_username/1]).

-include_lib("kz_amqp_util.hrl").

-define(SWITCH_EXCHANGE, <<"switch">>).
-define(SWITCH_EXCHANGE_TYPE, <<"topic">>).

-ifdef(TEST).
-export([notify_routing_key/2
        ,fs_command_routing_key/1
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [reload_acls_definition()
    ,reload_gateways_definition()
    ,fs_xml_flush_definition()
    ,notify_definition()
    ,fs_command_definition()
    ,fs_reply_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"reload_acls">>) ->
    reload_acls_definition();
api_definition(<<"reload_gateways">>) ->
    reload_gateways_definition();
api_definition(<<"fs_xml_flush">>) ->
    fs_xml_flush_definition();
api_definition(<<"notify">>) ->
    notify_definition();
api_definition(<<"fs_command">>) ->
    fs_command_definition();
api_definition(<<"fs_reply">>) ->
    fs_reply_definition().

-spec reload_acls_definition() -> kapi_definition:api().
reload_acls_definition() ->
    EventName = <<"reload_acls">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Reload ACLs">>}
              ,{fun kapi_definition:set_description/2, <<"Request reload of FreeSWITCH ACLs">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reload_acls/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reload_acls_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reload_acls/0}
              ,{fun kapi_definition:set_binding/2, <<"switch.reload_acls">>}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec reload_gateways_definition() -> kapi_definition:api().
reload_gateways_definition() ->
    EventName = <<"reload_gateways">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Reload Gateways">>}
              ,{fun kapi_definition:set_description/2, <<"Request reload of FreeSWITCH gateways">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reload_gateways/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reload_gateways_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reload_gateways/0}
              ,{fun kapi_definition:set_binding/2, <<"switch.reload_gateways">>}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec fs_xml_flush_definition() -> kapi_definition:api().
fs_xml_flush_definition() ->
    EventName = <<"fs_xml_flush">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Flush fs_xml_flush">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Request flush of FreeSWITCH fs_xml_flush">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fs_xml_flush/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fs_xml_flush_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fs_xml_flush/1}
              ,{fun kapi_definition:set_binding/2, <<"switch.fs_xml_flush">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Username">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Realm">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec notify_definition() -> kapi_definition:api().
notify_definition() ->
    EventName = <<"notify">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Request NOTIFY message">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Request that FreeSWITCH send a NOTIFY message">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun notify/1}
              ,{fun kapi_definition:set_validate_fun/2, fun notify_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_notify/1}
              ,{fun kapi_definition:set_binding/2, fun notify_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Event">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Body">>
                                                            ,<<"Content-Type">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec fs_command_definition() -> kapi_definition:api().
fs_command_definition() ->
    EventName = <<"command">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Request Command">>}
              ,{fun kapi_definition:set_description/2, <<"Request a FreeSWITCH command">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fs_command/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fs_command_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fs_command/1}
              ,{fun kapi_definition:set_binding/2, fun fs_command_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Args">>
                                                            ,<<"Command">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"FreeSWITCH-Node">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec fs_reply_definition() -> kapi_definition:api().
fs_reply_definition() ->
    EventName = <<"reply">>,
    Category = <<"switch_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Request Command's Reply">>}
              ,{fun kapi_definition:set_description/2, <<"Reply to a FreeSWITCH command">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fs_reply/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fs_reply_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fs_reply/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Command">>
                                                            ,<<"Result">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Error">>
                                                            ,<<"Event-Data">>
                                                            ,<<"FreeSWITCH-Node">>
                                                            ,<<"Response">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Request reload of FreeSWITCH ACLs.
%% @end
%%------------------------------------------------------------------------------
-spec reload_acls(kz_term:api_terms()) -> kz_api:api_formatter_return().
reload_acls(Req) ->
    kapi_definition:build_message(Req, reload_acls_definition()).

-spec reload_acls_v(kz_term:api_terms()) -> boolean().
reload_acls_v(Req) ->
    kapi_definition:validate(Req, reload_acls_definition()).

-spec publish_reload_acls() -> 'ok'.
publish_reload_acls() ->
    Definition = reload_acls_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(kz_api:default_headers(<<"switch_event">>
                                                                       ,kz_term:to_binary(?MODULE)
                                                                       )
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,kapi_definition:binding(Definition)
                              ,Payload
                              ,?DEFAULT_CONTENT_TYPE
                              ).

%%------------------------------------------------------------------------------
%% @doc Request reload of FreeSWITCH gateways.
%% @end
%%------------------------------------------------------------------------------
-spec reload_gateways(kz_term:api_terms()) -> kz_api:api_formatter_return().
reload_gateways(Req) ->
    kapi_definition:build_message(Req, reload_gateways_definition()).

-spec reload_gateways_v(kz_term:api_terms()) -> boolean().
reload_gateways_v(Req) ->
    kapi_definition:validate(Req, reload_gateways_definition()).

-spec publish_reload_gateways() -> 'ok'.
publish_reload_gateways() ->
    Definition = reload_gateways_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(kz_api:default_headers(<<"switch_event">>
                                                                       ,kz_term:to_binary(?MODULE)
                                                                       )
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,kapi_definition:binding(Definition)
                              ,Payload
                              ,?DEFAULT_CONTENT_TYPE
                              ).

%%------------------------------------------------------------------------------
%% @doc Request flush of FreeSWITCH `fs_xml_flush'.
%% @end
%%------------------------------------------------------------------------------
-spec fs_xml_flush(kz_term:api_terms()) -> kz_api:api_formatter_return().
fs_xml_flush(Req) ->
    kapi_definition:build_message(Req, fs_xml_flush_definition()).

-spec fs_xml_flush_v(kz_term:api_terms()) -> boolean().
fs_xml_flush_v(Req) ->
    kapi_definition:validate(Req, fs_xml_flush_definition()).

-spec publish_fs_xml_flush(kz_term:api_terms()) -> 'ok'.
publish_fs_xml_flush(JObj) ->
    publish_fs_xml_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fs_xml_flush(kz_term:api_terms(), binary()) -> 'ok'.
publish_fs_xml_flush(Req, ContentType) ->
    Definition = fs_xml_flush_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,kapi_definition:binding(Definition)
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Request that FreeSWITCH send a `NOTIFY' message.
%% @end
%%------------------------------------------------------------------------------
-spec notify(kz_term:api_terms()) -> kz_api:api_formatter_return().
notify(Req) ->
    kapi_definition:build_message(Req, notify_definition()).

-spec notify_v(kz_term:api_terms()) -> boolean().
notify_v(Req) ->
    kapi_definition:validate(Req, notify_definition()).

-spec publish_notify(kz_term:api_terms()) -> 'ok'.
publish_notify(JObj) ->
    publish_notify(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_notify(kz_term:api_terms(), binary()) -> 'ok'.
publish_notify(Req, ContentType) ->
    Definition = notify_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    Realm = notify_realm(Req),
    Username = notify_username(Req),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,(kapi_definition:binding(Definition))(Realm, Username)
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Request a FreeSWITCH command.
%% @end
%%------------------------------------------------------------------------------
-spec fs_command(kz_term:api_terms()) -> kz_api:api_formatter_return().
fs_command(Req) ->
    kapi_definition:build_message(Req, fs_command_definition()).

-spec fs_command_v(kz_term:api_terms()) -> boolean().
fs_command_v(Req) ->
    kapi_definition:validate(Req, fs_command_definition()).

-spec publish_fs_command(kz_term:api_terms()) -> 'ok'.
publish_fs_command(JObj) ->
    publish_fs_command(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fs_command(kz_term:api_terms(), binary()) -> 'ok'.
publish_fs_command(Req, ContentType) ->
    Definition = fs_command_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,(kapi_definition:binding(Definition))(check_fs_node(Req))
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Reply to a FreeSWITCH command.
%% @end
%%------------------------------------------------------------------------------
-spec fs_reply(kz_term:api_terms()) -> kz_api:api_formatter_return().
fs_reply(Req) ->
    kapi_definition:build_message(Req, fs_reply_definition()).

-spec fs_reply_v(kz_term:api_terms()) -> boolean().
fs_reply_v(Req) ->
    kapi_definition:validate(Req, fs_reply_definition()).

-spec publish_fs_reply(binary(), kz_term:api_terms()) -> 'ok'.
publish_fs_reply(Queue, Req) ->
    Definition = fs_reply_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Q, 'undefined', _Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, <<"switch.*">>, ?SWITCH_EXCHANGE);
bind_to_q(Q, ['reload_acls'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q
                                          ,kapi_definition:binding(reload_acls_definition())
                                          ,?SWITCH_EXCHANGE
                                          ),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['reload_gateways'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q
                                          ,kapi_definition:binding(reload_gateways_definition())
                                          ,?SWITCH_EXCHANGE
                                          ),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q
                                          ,kapi_definition:binding(fs_xml_flush_definition())
                                          ,?SWITCH_EXCHANGE
                                          ),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['notify'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = kz_amqp_util:bind_q_to_exchange(Q
                                          ,(kapi_definition:binding(notify_definition()))(Realm, Username)
                                          ,?SWITCH_EXCHANGE
                                          ),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['command'|T], Props) ->
    Node = kz_term:to_binary(props:get_value('node', Props, <<"*">>)),
    'ok' = kz_amqp_util:bind_q_to_exchange(Q
                                          ,(kapi_definition:binding(fs_command_definition()))(Node)
                                          ,?SWITCH_EXCHANGE
                                          ),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _Props) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props), Props).

unbind_q_from(Q, 'undefined', _Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, <<"switch.*">>, ?SWITCH_EXCHANGE);
unbind_q_from(Q, ['reload_acls'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q
                                              ,kapi_definition:binding(reload_acls_definition())
                                              ,?SWITCH_EXCHANGE
                                              ),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['reload_gateways'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q
                                              ,kapi_definition:binding(reload_gateways_definition())
                                              ,?SWITCH_EXCHANGE
                                              ),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q
                                              ,kapi_definition:binding(fs_xml_flush_definition())
                                              ,?SWITCH_EXCHANGE
                                              ),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['notify'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q
                                              ,(kapi_definition:binding(notify_definition()))(Realm, Username)
                                              ,?SWITCH_EXCHANGE
                                              ),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['command'|T], Props) ->
    Node = kz_term:to_binary(props:get_value('node', Props, <<"*">>)),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q
                                              ,(kapi_definition:binding(fs_command_definition()))(Node)
                                              ,?SWITCH_EXCHANGE
                                              ),
    unbind_q_from(Q, T, Props);
unbind_q_from(_Q, [], _Props) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?SWITCH_EXCHANGE, ?SWITCH_EXCHANGE_TYPE).

-spec notify_realm(kz_term:api_terms()) -> kz_term:api_ne_binary().
notify_realm(API) ->
    get_value(API, <<"Realm">>, 'undefined').

-spec notify_username(kz_term:api_terms()) -> kz_term:api_ne_binary().
notify_username(API) ->
    get_value(API, <<"Username">>, 'undefined').

-spec check_fs_node(kz_term:api_terms()) -> kz_term:api_ne_binary().
check_fs_node(API) ->
    get_value(API, <<"FreeSWITCH-Node">>, <<"*">>).

-spec get_value(kz_term:api_terms(), kz_json:key(), Default) -> kz_term:ne_binary() | Default.
get_value(Props, Key, Default) when is_list(Props) ->
    get_value(Props, Key, Default, fun props:get_value/3);
get_value(JObj, Key, Default) ->
    get_value(JObj, Key, Default, fun kz_json:get_value/3).

-spec get_value(kz_term:api_terms(), kz_json:key(), Default, fun()) -> kz_term:ne_binary() | Default.
get_value(API, Key, Default, Get) ->
    Get(Key, API, Default).

-spec notify_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
notify_routing_key(Realm, Username) ->
    kz_binary:join([<<"switch.notify">>
                   ,kz_amqp_util:encode(Realm)
                   ,kz_amqp_util:encode(Username)
                   ]
                  ,<<".">>
                  ).

-spec fs_command_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
fs_command_routing_key(Node) ->
    <<"switch.command.", (kz_amqp_util:encode(Node))/binary>>.
