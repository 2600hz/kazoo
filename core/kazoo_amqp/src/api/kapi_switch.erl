%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Switch events messages.
%%% @author Edouard Swiac
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_switch).

-export([reload_acls/1, reload_acls_v/1]).
-export([reload_gateways/1, reload_gateways_v/1]).
-export([fs_xml_flush/1, fs_xml_flush_v/1]).
-export([notify/1, notify_v/1
        ,notify_realm/1, notify_username/1
        ]).
-export([fs_command/1, fs_command_v/1]).
-export([fs_reply/1, fs_reply_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_reload_acls/0]).
-export([publish_reload_gateways/0]).
-export([publish_fs_xml_flush/1]).
-export([publish_notify/1, publish_notify/2]).
-export([publish_command/1, publish_command/2]).
-export([publish_reply/2]).

-include_lib("kz_amqp_util.hrl").

-define(SWITCH_EXCHANGE, <<"switch">>).
-define(SWITCH_EXCHANGE_TYPE, <<"topic">>).

%% request to reload acl
-define(RELOAD_ACLS_HEADERS, []).
-define(OPTIONAL_RELOAD_ACLS_HEADERS, []).
-define(RELOAD_ACLS_VALUES, [{<<"Event-Name">>, <<"reload_acls">>}
                            ,{<<"Event-Category">>, <<"switch_event">>}
                            ]).
-define(RELOAD_ACLS_TYPES, []).
-define(RELOAD_ACLS_KEY, <<"switch.reload_acls">>).

%% request to reload gateways
-define(RELOAD_GATEWAYS_HEADERS, []).
-define(OPTIONAL_RELOAD_GATEWAYS_HEADERS, []).
-define(RELOAD_GATEWAYS_VALUES, [{<<"Event-Name">>, <<"reload_gateways">>}
                                ,{<<"Event-Category">>, <<"switch_event">>}
                                ]).
-define(RELOAD_GATEWAYS_TYPES, []).
-define(RELOAD_GATEWAYS_KEY, <<"switch.reload_gateways">>).

%% request to flush fs xml cache
-define(FS_XML_FLUSH_HEADERS, [<<"Username">>]).
-define(OPTIONAL_FS_XML_FLUSH_HEADERS, [<<"Realm">>]).
-define(FS_XML_FLUSH_VALUES, [{<<"Event-Name">>, <<"fs_xml_flush">>}
                             ,{<<"Event-Category">>, <<"switch_event">>}
                             ]).
-define(FS_XML_FLUSH_TYPES, []).
-define(FS_XML_FLUSH_KEY, <<"switch.fs_xml_flush">>).

-define(NOTIFY_HEADERS, [<<"Username">>, <<"Realm">>, <<"Event">>]).
-define(OPTIONAL_NOTIFY_HEADERS, [<<"Body">>, <<"Content-Type">>]).
-define(NOTIFY_VALUES, [{<<"Event-Category">>, <<"switch_event">>}
                       ,{<<"Event-Name">>, <<"notify">>}
                       ]).
-define(NOTIFY_TYPES, []).
-define(NOTIFY_KEY(Realm, Username)
       ,kz_binary:join([<<"switch.notify">>
                       ,kz_amqp_util:encode(Realm)
                       ,kz_amqp_util:encode(Username)
                       ]
                      ,<<".">>
                      )
       ).

%% request fs command
-define(FS_COMMAND_HEADERS, [<<"Command">>, <<"Args">>]).
-define(OPTIONAL_FS_COMMAND_HEADERS, [<<"FreeSWITCH-Node">>]).
-define(FS_COMMAND_VALUES, [{<<"Event-Name">>, <<"command">>}
                           ,{<<"Event-Category">>, <<"switch_event">>}
                           ]).
-define(FS_COMMAND_TYPES, []).
-define(FS_COMMAND_KEY(N), <<"switch.command.", (kz_amqp_util:encode(N))/binary>>).

%% reply fs command
-define(FSREPLY_COMMAND_HEADERS, [<<"Command">>, <<"Result">>]).
-define(OPTIONAL_FSREPLY_COMMAND_HEADERS, [<<"FreeSWITCH-Node">>
                                          ,<<"Error">>
                                          ,<<"Response">>
                                          ,<<"Event-Data">>
                                          ]).
-define(FSREPLY_COMMAND_VALUES, [{<<"Event-Name">>, <<"reply">>}
                                ,{<<"Event-Category">>, <<"switch_event">>}
                                ]).
-define(FSREPLY_COMMAND_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Request reload of FreeSWITCH ACLs.
%% @end
%%------------------------------------------------------------------------------
-spec reload_acls(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_acls(Prop) when is_list(Prop) ->
    case reload_acls_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RELOAD_ACLS_HEADERS, ?OPTIONAL_RELOAD_ACLS_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event reload_acls req"}
    end;
reload_acls(JObj) ->
    reload_acls(kz_json:to_proplist(JObj)).

-spec reload_acls_v(kz_term:api_terms()) -> boolean().
reload_acls_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RELOAD_ACLS_HEADERS, ?RELOAD_ACLS_VALUES, ?RELOAD_ACLS_TYPES);
reload_acls_v(JObj) ->
    reload_acls_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Request reload of FreeSWITCH gateways.
%% @end
%%------------------------------------------------------------------------------
-spec reload_gateways(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_gateways(Prop) when is_list(Prop) ->
    case reload_gateways_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RELOAD_GATEWAYS_HEADERS, ?OPTIONAL_RELOAD_GATEWAYS_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event reload_gateways req"}
    end;
reload_gateways(JObj) ->
    reload_gateways(kz_json:to_proplist(JObj)).

-spec reload_gateways_v(kz_term:api_terms()) -> boolean().
reload_gateways_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RELOAD_GATEWAYS_HEADERS, ?RELOAD_GATEWAYS_VALUES, ?RELOAD_GATEWAYS_TYPES);
reload_gateways_v(JObj) ->
    reload_gateways_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Request flush of FreeSWITCH `fs_xml_flush'.
%% @end
%%------------------------------------------------------------------------------
-spec fs_xml_flush(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
fs_xml_flush(Prop) when is_list(Prop) ->
    case fs_xml_flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FS_XML_FLUSH_HEADERS, ?OPTIONAL_FS_XML_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event fs_xml_flush req"}
    end;
fs_xml_flush(JObj) ->
    fs_xml_flush(kz_json:to_proplist(JObj)).

-spec fs_xml_flush_v(kz_term:api_terms()) -> boolean().
fs_xml_flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FS_XML_FLUSH_HEADERS, ?FS_XML_FLUSH_VALUES, ?FS_XML_FLUSH_TYPES);
fs_xml_flush_v(JObj) ->
    fs_xml_flush_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Request that FreeSWITCH send a `NOTIFY' message.
%% @end
%%------------------------------------------------------------------------------
-spec notify(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
notify(Prop) when is_list(Prop) ->
    case notify_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NOTIFY_HEADERS, ?OPTIONAL_NOTIFY_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event notify req"}
    end;
notify(JObj) ->
    notify(kz_json:to_proplist(JObj)).

-spec notify_v(kz_term:api_terms()) -> boolean().
notify_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NOTIFY_HEADERS, ?NOTIFY_VALUES, ?NOTIFY_TYPES);
notify_v(JObj) ->
    notify_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Request a FreeSWITCH command.
%% @end
%%------------------------------------------------------------------------------
-spec fs_command(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
fs_command(Prop) when is_list(Prop) ->
    case fs_command_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FS_COMMAND_HEADERS, ?OPTIONAL_FS_COMMAND_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch command"}
    end;
fs_command(JObj) ->
    fs_command(kz_json:to_proplist(JObj)).

-spec fs_command_v(kz_term:api_terms()) -> boolean().
fs_command_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FS_COMMAND_HEADERS, ?FS_COMMAND_VALUES, ?FS_COMMAND_TYPES);
fs_command_v(JObj) ->
    fs_command_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Reply to a FreeSWITCH command.
%% @end
%%------------------------------------------------------------------------------
-spec fs_reply(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
fs_reply(Prop) when is_list(Prop) ->
    case fs_reply_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FSREPLY_COMMAND_HEADERS, ?OPTIONAL_FSREPLY_COMMAND_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch command"}
    end;
fs_reply(JObj) ->
    fs_reply(kz_json:to_proplist(JObj)).

-spec fs_reply_v(kz_term:api_terms()) -> boolean().
fs_reply_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FSREPLY_COMMAND_HEADERS, ?FSREPLY_COMMAND_VALUES, ?FSREPLY_COMMAND_TYPES);
fs_reply_v(JObj) ->
    fs_reply_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Q, 'undefined', _Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, <<"switch.*">>, ?SWITCH_EXCHANGE);
bind_to_q(Q, ['reload_acls'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, ?RELOAD_ACLS_KEY, ?SWITCH_EXCHANGE),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['reload_gateways'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, ?RELOAD_GATEWAYS_KEY, ?SWITCH_EXCHANGE),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, ?FS_XML_FLUSH_KEY, ?SWITCH_EXCHANGE),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['notify'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, ?NOTIFY_KEY(Realm, Username), ?SWITCH_EXCHANGE),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['command'|T], Props) ->
    Node = props:get_value('node', Props, <<"*">>),
    'ok' = kz_amqp_util:bind_q_to_exchange(Q, ?FS_COMMAND_KEY(kz_term:to_binary(Node)), ?SWITCH_EXCHANGE),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _Props) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props), Props).

unbind_q_from(Q, 'undefined', _Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, <<"switch.*">>, ?SWITCH_EXCHANGE);
unbind_q_from(Q, ['reload_acls'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, ?RELOAD_ACLS_KEY, ?SWITCH_EXCHANGE),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['reload_gateways'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, ?RELOAD_GATEWAYS_KEY, ?SWITCH_EXCHANGE),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, ?FS_XML_FLUSH_KEY, ?SWITCH_EXCHANGE),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['notify'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, ?NOTIFY_KEY(Realm, Username), ?SWITCH_EXCHANGE),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['command'|T], Props) ->
    Node = props:get_value('node', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Q, ?FS_COMMAND_KEY(kz_term:to_binary(Node)), ?SWITCH_EXCHANGE),
    unbind_q_from(Q, T, Props);
unbind_q_from(_Q, [], _Props) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?SWITCH_EXCHANGE, ?SWITCH_EXCHANGE_TYPE).

-spec publish_reload_acls() -> 'ok'.
publish_reload_acls() ->
    Defaults = kz_api:default_headers(<<"switch_event">>, kz_term:to_binary(?MODULE)),
    {'ok', Payload} = kz_api:prepare_api_payload(Defaults, ?RELOAD_ACLS_VALUES, fun reload_acls/1),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE, ?RELOAD_ACLS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_reload_gateways() -> 'ok'.
publish_reload_gateways() ->
    Defaults = kz_api:default_headers(<<"switch_event">>, kz_term:to_binary(?MODULE)),
    {'ok', Payload} = kz_api:prepare_api_payload(Defaults, ?RELOAD_GATEWAYS_VALUES, fun reload_gateways/1),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE, ?RELOAD_GATEWAYS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_fs_xml_flush(kz_term:api_terms()) -> 'ok'.
publish_fs_xml_flush(JObj) ->
    publish_fs_xml_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fs_xml_flush(kz_term:api_terms(), binary()) -> 'ok'.
publish_fs_xml_flush(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FS_XML_FLUSH_VALUES, fun fs_xml_flush/1),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE, ?FS_XML_FLUSH_KEY, Payload, ContentType).

-spec publish_notify(kz_term:api_terms()) -> 'ok'.
publish_notify(JObj) ->
    publish_notify(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_notify(kz_term:api_terms(), binary()) -> 'ok'.
publish_notify(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?NOTIFY_VALUES, fun notify/1),

    Realm = notify_realm(Req),
    Username = notify_username(Req),

    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE
                              ,?NOTIFY_KEY(Realm, Username)
                              ,Payload
                              ,ContentType
                              ).

-spec notify_realm(kz_term:api_terms()) -> kz_term:api_ne_binary().
notify_realm(API) ->
    get_value(API, <<"Realm">>, 'undefined').

-spec notify_username(kz_term:api_terms()) -> kz_term:api_ne_binary().
notify_username(API) ->
    get_value(API, <<"Username">>, 'undefined').

-spec publish_command(kz_term:api_terms()) -> 'ok'.
publish_command(JObj) ->
    publish_command(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_command(kz_term:api_terms(), binary()) -> 'ok'.
publish_command(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FS_COMMAND_VALUES, fun fs_command/1),
    N = check_fs_node(Req),
    kz_amqp_util:basic_publish(?SWITCH_EXCHANGE, ?FS_COMMAND_KEY(N), Payload, ContentType).

-spec publish_reply(binary(), kz_term:api_terms()) -> 'ok'.
publish_reply(Queue, Req) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?FSREPLY_COMMAND_VALUES, fun fs_reply/1),
    kz_amqp_util:targeted_publish(Queue, Payload).

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
