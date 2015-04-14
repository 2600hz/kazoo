%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Switch events messages
%%% @end
%%% @contributors
%%%   Eduoard Swiac
%%%-------------------------------------------------------------------
-module(wapi_switch).

-export([reload_acls/1, reload_acls_v/1]).
-export([reload_gateways/1, reload_gateways_v/1]).
-export([fs_xml_flush/1, fs_xml_flush_v/1]).
-export([check_sync/1, check_sync_v/1
         ,check_sync_realm/1, check_sync_username/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_reload_acls/0]).
-export([publish_reload_gateways/0]).
-export([publish_fs_xml_flush/1]).
-export([publish_check_sync/1, publish_check_sync/2]).

-include_lib("whistle/include/wh_api.hrl").

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

-define(CHECK_SYNC_HEADERS, [<<"Username">>, <<"Realm">>]).
-define(OPTIONAL_CHECK_SYNC_HEADERS, []).
-define(CHECK_SYNC_VALUES, [{<<"Event-Category">>, <<"switch_event">>}
                            ,{<<"Event-Name">>, <<"check_sync">>}
                           ]).
-define(CHECK_SYNC_TYPES, []).
-define(CHECK_SYNC_KEY(Realm, Username)
        ,wh_util:join_binary([<<"switch.check_sync">>
                              ,amqp_util:encode(Realm)
                              ,amqp_util:encode(Username)
                             ]
                             ,<<".">>
                            )
       ).

%% Request a reload_acls
-spec reload_acls(api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_acls(Prop) when is_list(Prop) ->
    case reload_acls_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RELOAD_ACLS_HEADERS, ?OPTIONAL_RELOAD_ACLS_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event reload_acls req"}
    end;
reload_acls(JObj) ->
    reload_acls(wh_json:to_proplist(JObj)).

-spec reload_acls_v(api_terms()) -> boolean().
reload_acls_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RELOAD_ACLS_HEADERS, ?RELOAD_ACLS_VALUES, ?RELOAD_ACLS_TYPES);
reload_acls_v(JObj) ->
    reload_acls_v(wh_json:to_proplist(JObj)).

%% Request a reload_gateways
-spec reload_gateways(api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_gateways(Prop) when is_list(Prop) ->
    case reload_gateways_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RELOAD_GATEWAYS_HEADERS, ?OPTIONAL_RELOAD_GATEWAYS_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event reload_gateways req"}
    end;
reload_gateways(JObj) ->
    reload_gateways(wh_json:to_proplist(JObj)).

-spec reload_gateways_v(api_terms()) -> boolean().
reload_gateways_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RELOAD_GATEWAYS_HEADERS, ?RELOAD_GATEWAYS_VALUES, ?RELOAD_GATEWAYS_TYPES);
reload_gateways_v(JObj) ->
    reload_gateways_v(wh_json:to_proplist(JObj)).

%% Request a fs_xml_flush
-spec fs_xml_flush(api_terms()) -> {'ok', iolist()} | {'error', string()}.
fs_xml_flush(Prop) when is_list(Prop) ->
    case fs_xml_flush_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?FS_XML_FLUSH_HEADERS, ?OPTIONAL_FS_XML_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event fs_xml_flush req"}
    end;
fs_xml_flush(JObj) ->
    fs_xml_flush(wh_json:to_proplist(JObj)).

-spec fs_xml_flush_v(api_terms()) -> boolean().
fs_xml_flush_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FS_XML_FLUSH_HEADERS, ?FS_XML_FLUSH_VALUES, ?FS_XML_FLUSH_TYPES);
fs_xml_flush_v(JObj) ->
    fs_xml_flush_v(wh_json:to_proplist(JObj)).

%% Request a check_sync
-spec check_sync(api_terms()) -> {'ok', iolist()} | {'error', string()}.
check_sync(Prop) when is_list(Prop) ->
    case check_sync_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CHECK_SYNC_HEADERS, ?OPTIONAL_CHECK_SYNC_HEADERS);
        'false' -> {'error', "Proplist failed validation for switch event check_sync req"}
    end;
check_sync(JObj) ->
    check_sync(wh_json:to_proplist(JObj)).

-spec check_sync_v(api_terms()) -> boolean().
check_sync_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHECK_SYNC_HEADERS, ?CHECK_SYNC_VALUES, ?CHECK_SYNC_TYPES);
check_sync_v(JObj) ->
    check_sync_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Q, 'undefined', _Props) ->
    'ok' = amqp_util:bind_q_to_configuration(Q, <<"switch.*">>);
bind_to_q(Q, ['reload_acls'|T], Props) ->
    'ok' = amqp_util:bind_q_to_configuration(Q, ?RELOAD_ACLS_KEY),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['reload_gateways'|T], Props) ->
    'ok' = amqp_util:bind_q_to_configuration(Q, ?RELOAD_GATEWAYS_KEY),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = amqp_util:bind_q_to_configuration(Q, ?FS_XML_FLUSH_KEY),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['check_sync'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = amqp_util:bind_q_to_configuration(Q, ?CHECK_SYNC_KEY(Realm, Username)),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _Props) -> 'ok'.

-spec unbind_q(ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props), Props).

unbind_q_from(Q, 'undefined', _Props) ->
    'ok' = amqp_util:unbind_q_from_configuration(Q, <<"switch.*">>);
unbind_q_from(Q, ['reload_acls'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_configuration(Q, ?RELOAD_ACLS_KEY),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['reload_gateways'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_configuration(Q, ?RELOAD_GATEWAYS_KEY),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['fs_xml_flush'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_configuration(Q, ?FS_XML_FLUSH_KEY),
    unbind_q_from(Q, T, Props);
unbind_q_from(Q, ['check_sync'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    Username = props:get_value('username', Props, <<"*">>),
    'ok' = amqp_util:unbind_q_from_configuration(Q, ?CHECK_SYNC_KEY(Realm, Username)),
    unbind_q_from(Q, T, Props);
unbind_q_from(_Q, [], _Props) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:configuration_exchange().

-spec publish_reload_acls() -> 'ok'.
publish_reload_acls() ->
    Defaults = wh_api:default_headers(<<"switch_event">>, wh_util:to_binary(?MODULE)),
    {'ok', Payload} = wh_api:prepare_api_payload(Defaults, ?RELOAD_ACLS_VALUES, fun ?MODULE:reload_acls/1),
    amqp_util:configuration_publish(?RELOAD_ACLS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_reload_gateways() -> 'ok'.
publish_reload_gateways() ->
    Defaults = wh_api:default_headers(<<"switch_event">>, wh_util:to_binary(?MODULE)),
    {'ok', Payload} = wh_api:prepare_api_payload(Defaults, ?RELOAD_GATEWAYS_VALUES, fun ?MODULE:reload_gateways/1),
    amqp_util:configuration_publish(?RELOAD_GATEWAYS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_fs_xml_flush(api_terms()) -> 'ok'.
-spec publish_fs_xml_flush(api_terms(), binary()) -> 'ok'.
publish_fs_xml_flush(JObj) ->
    publish_fs_xml_flush(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fs_xml_flush(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?FS_XML_FLUSH_VALUES, fun ?MODULE:fs_xml_flush/1),
    amqp_util:configuration_publish(?FS_XML_FLUSH_KEY, Payload, ContentType).

-spec publish_check_sync(api_terms()) -> 'ok'.
-spec publish_check_sync(api_terms(), binary()) -> 'ok'.
publish_check_sync(JObj) ->
    publish_check_sync(JObj, ?DEFAULT_CONTENT_TYPE).
publish_check_sync(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CHECK_SYNC_VALUES, fun ?MODULE:check_sync/1),

    Realm = check_sync_realm(Req),
    Username = check_sync_username(Req),

    amqp_util:configuration_publish(?CHECK_SYNC_KEY(Realm, Username)
                                    ,Payload
                                    ,ContentType
                                   ).

-spec check_sync_realm(api_terms()) -> api_binary().
check_sync_realm(Props) when is_list(Props) ->
    check_sync_value(Props, <<"Realm">>, fun props:get_value/2);
check_sync_realm(JObj) ->
    check_sync_value(JObj, <<"Realm">>, fun wh_json:get_value/2).

-spec check_sync_username(api_terms()) -> api_binary().
check_sync_username(Props) when is_list(Props) ->
    check_sync_value(Props, <<"Username">>, fun props:get_value/2);
check_sync_username(JObj) ->
    check_sync_value(JObj, <<"Username">>, fun wh_json:get_value/2).

-spec check_sync_value(api_terms(), ne_binary(), fun()) -> api_binary().
check_sync_value(API, Key, Get) ->
    Get(Key, API).
