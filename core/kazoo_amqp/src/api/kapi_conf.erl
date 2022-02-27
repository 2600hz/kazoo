%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Configuration updates (like DB doc changes) can be communicated across
%%% the AMQP bus so WhApps can flush cache entries, update settings, etc.
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_conf).

-export([doc_update/1, doc_update_v/1
        ,doc_type_update/1, doc_type_update_v/1

        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0

        ,publish_doc_update/5, publish_doc_update/6
        ,publish_doc_type_update/1, publish_doc_type_update/2

        ,publish_db_update/3, publish_db_update/4

        ,get_database/1
        ,get_account_id/1, get_account_db/1
        ,get_type/1, get_doc/1, get_id/1
        ,get_action/1, get_is_soft_deleted/1
        ]).

-type action() :: 'created' | 'edited' | 'deleted'.
-export_type([action/0]).

-include_lib("kz_amqp_util.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(CONF_DOC_UPDATE_HEADERS, [<<"ID">>, <<"Database">>]).
-define(OPTIONAL_CONF_DOC_UPDATE_HEADERS, [<<"Account-ID">>
                                          ,<<"Date-Created">>
                                          ,<<"Date-Modified">>
                                          ,<<"Doc">>
                                          ,<<"Is-Soft-Deleted">>
                                          ,<<"Rev">>
                                          ,<<"Type">>
                                          ,<<"Version">>
                                          ,<<"Origin-Cache">>
                                          ]).
-define(CONF_DOC_UPDATE_VALUES, [{<<"Event-Category">>, ?KAPI_CONF_CATEGORY}
                                ,{<<"Event-Name">>, [?DOC_EDITED
                                                    ,?DOC_CREATED
                                                    ,?DOC_DELETED
                                                    ,?DB_EDITED
                                                    ,?DB_CREATED
                                                    ,?DB_DELETED
                                                    ,?DB_VIEWS_UPDATED
                                                    ]}
                                ]).
-define(CONF_DOC_UPDATE_TYPES, [{<<"ID">>, fun is_binary/1}
                               ,{<<"Rev">>, fun is_binary/1}
                               ,{<<"Is-Soft-Deleted">>, fun kz_term:is_boolean/1}
                               ]).

-define(DOC_TYPE_UPDATE_HEADERS, [<<"Type">>]).
-define(OPTIONAL_DOC_TYPE_UPDATE_HEADERS
       ,[<<"Action">>
        ,<<"Account-ID">>
        ]
       ).
-define(DOC_TYPE_UPDATE_VALUES, [{<<"Event-Category">>, ?KAPI_CONF_CATEGORY}
                                ,{<<"Event-Name">>, <<"doc_type_update">>}
                                ]).
-define(DOC_TYPE_UPDATE_TYPES, []).

-spec get_account_id(kz_term:api_terms()) -> kz_term:api_binary().
get_account_id(API) ->
    get_value(API, <<"Account-ID">>).

-spec get_action(kz_term:api_terms()) -> kz_term:api_binary().
get_action(API) ->
    get_value(API, <<"Action">>).

-spec get_account_db(kz_term:api_terms()) -> kz_term:api_binary().
get_account_db(API) ->
    get_value(API, <<"Account-DB">>).

-spec get_database(kz_term:api_terms()) -> kz_term:ne_binary().
get_database(API) ->
    get_value(API, <<"Database">>).

%% returns the public fields of the document
-spec get_doc(kz_term:api_terms()) -> kz_term:api_object().
get_doc(API) ->
    get_value(API, <<"Doc">>).

-spec get_id(kz_term:api_terms()) -> kz_term:api_binary().
get_id(API) ->
    get_value(API, <<"ID">>).

%% returns the pvt_type field
-spec get_type(kz_term:api_terms()) -> kz_term:api_binary().
get_type(API) ->
    get_value(API, <<"Type">>).

-spec get_is_soft_deleted(kz_term:api_terms()) -> boolean().
get_is_soft_deleted(API) ->
    kz_term:is_true(get_value(API, <<"Is-Soft-Deleted">>)).

-spec get_value(kz_term:api_terms(), kz_term:ne_binary()) -> any().
get_value(Prop, Key) when is_list(Prop) ->
    props:get_value(Key, Prop);
get_value(JObj, Key) ->
    kz_json:get_value(Key, JObj).

%%------------------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec doc_update(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
doc_update(Prop) when is_list(Prop) ->
    case doc_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONF_DOC_UPDATE_HEADERS, ?OPTIONAL_CONF_DOC_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for document_change"}
    end;
doc_update(JObj) ->
    doc_update(kz_json:to_proplist(JObj)).

-spec doc_update_v(kz_term:api_terms()) -> boolean().
doc_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONF_DOC_UPDATE_HEADERS, ?CONF_DOC_UPDATE_VALUES, ?CONF_DOC_UPDATE_TYPES);
doc_update_v(JObj) ->
    doc_update_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec doc_type_update(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
doc_type_update(Prop) when is_list(Prop) ->
    case doc_type_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DOC_TYPE_UPDATE_HEADERS, ?OPTIONAL_DOC_TYPE_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for document_change"}
    end;
doc_type_update(JObj) ->
    doc_type_update(kz_json:to_proplist(JObj)).

-spec doc_type_update_v(kz_term:api_terms()) -> boolean().
doc_type_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DOC_TYPE_UPDATE_HEADERS, ?DOC_TYPE_UPDATE_VALUES, ?DOC_TYPE_UPDATE_TYPES);
doc_type_update_v(JObj) ->
    doc_type_update_v(kz_json:to_proplist(JObj)).

-spec bind_q(binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q(Q, Props, props:get_value('restrict_to', Props)).

-spec bind_q(binary(), kz_term:proplist(), kz_term:api_atoms()) -> 'ok'.
bind_q(Q, Props, 'undefined') ->
    bind_for_doc_changes(Q, Props);
bind_q(Q, Props, ['doc_updates'|Restrict]) ->
    bind_for_doc_changes(Q, Props),
    bind_q(Q, Props, Restrict);
bind_q(Q, Props, ['doc_type_updates'|Restrict]) ->
    bind_for_doc_type_changes(Q, Props),
    bind_q(Q, Props, Restrict);
bind_q(Q, Props, [_|Restrict]) ->
    bind_q(Q, Props, Restrict);
bind_q(_Q, _Props, []) -> 'ok'.

-spec bind_for_doc_changes(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_for_doc_changes(Q, Props) ->
    case props:get_value('keys', Props) of
        'undefined' ->
            kz_amqp_util:bind_q_to_configuration(Q, get_routing_key(Props));
        List ->
            _ = [kz_amqp_util:bind_q_to_configuration(Q, get_routing_key(KeyProps))
                 || KeyProps <- List
                ],
            'ok'
    end.

-spec bind_for_doc_type_changes(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_for_doc_type_changes(Q, Props) ->
    case props:get_value('type', Props) of
        'undefined' -> bind_for_doc_types(Q, Props);
        Type ->
            kz_amqp_util:bind_q_to_configuration(Q, doc_type_update_routing_key(Type))
    end.

-spec bind_for_doc_types(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_for_doc_types(Q, Props) ->
    case props:get_value('types', Props) of
        'undefined' ->
            lager:warning("binding for doc type changes without supplying a type");
        Types ->
            _ = [kz_amqp_util:bind_q_to_configuration(Q, doc_type_update_routing_key(Type))
                 || Type <- Types
                ],
            'ok'
    end.

-spec unbind_q(binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q(Q, Props, props:get_value('restrict_to', Props)).

-spec unbind_q(binary(), kz_term:proplist(), kz_term:api_atoms()) -> 'ok'.
unbind_q(Q, Props, 'undefined') ->
    unbind_for_doc_changes(Q, Props);
unbind_q(Q, Props, ['doc_updates'|Restrict]) ->
    unbind_for_doc_changes(Q, Props),
    unbind_q(Q, Props, Restrict);
unbind_q(Q, Props, ['doc_type_updates'|Restrict]) ->
    unbind_for_doc_type_changes(Q, Props),
    unbind_q(Q, Props, Restrict);
unbind_q(Q, Props, [_|Restrict]) ->
    unbind_q(Q, Props, Restrict);
unbind_q(_Q, _Props, []) -> 'ok'.

-spec unbind_for_doc_changes(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_for_doc_changes(Q, Props) ->
    case props:get_value('keys', Props) of
        'undefined' ->
            kz_amqp_util:unbind_q_from_configuration(Q, get_routing_key(Props));
        List ->
            _ = [kz_amqp_util:unbind_q_from_configuration(Q, get_routing_key(KeyProps))
                 || KeyProps <- List
                ],
            'ok'
    end.

-spec unbind_for_doc_type_changes(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_for_doc_type_changes(Q, Props) ->
    case props:get_value('type', Props) of
        'undefined' -> unbind_for_doc_types(Q, Props);
        Type ->
            kz_amqp_util:unbind_q_from_configuration(Q, doc_type_update_routing_key(Type))
    end.

-spec unbind_for_doc_types(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_for_doc_types(Q, Props) ->
    case props:get_value('types', Props) of
        'undefined' -> 'ok';
        Types ->
            [kz_amqp_util:unbind_q_from_configuration(Q, doc_type_update_routing_key(Type))
             || Type <- Types
            ]
    end.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:configuration_exchange().

-spec get_routing_key(kz_term:proplist()) -> binary().
get_routing_key(Props) ->
    Action = props:get_binary_value('action', Props, <<"*">>),
    Db = props:get_binary_value('db', Props, <<"*">>),
    Type = props:get_binary_value('doc_type', Props
                                 ,props:get_value('type', Props, <<"*">>)
                                 ),
    Id = props:get_binary_value('doc_id', Props
                               ,props:get_value('id', Props, <<"*">>)
                               ),
    case kz_amqp_util:document_routing_key(Action, Db, Type, Id) of
        <<"*.*.*.*">> -> <<"#">>;
        RK -> RK
    end.

-spec publish_doc_update(action(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_doc_update(Action, Db, Type, Id, JObj) ->
    publish_doc_update(Action, Db, Type, Id, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_doc_update(action(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_doc_update(Action, Db, Type, Id, Change, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Change, ?CONF_DOC_UPDATE_VALUES, fun doc_update/1),
    kz_amqp_util:document_change_publish(Action, Db, Type, Id, Payload, ContentType).

-spec publish_db_update(action(), kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_db_update(Action, Db, JObj) ->
    publish_db_update(Action, Db, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_db_update(action(), kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_db_update(Action, Db, Change, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Change, ?CONF_DOC_UPDATE_VALUES, fun doc_update/1),
    kz_amqp_util:document_change_publish(Action, Db, <<"database">>, Db, Payload, ContentType).

-spec publish_doc_type_update(kz_term:api_terms()) -> 'ok'.
publish_doc_type_update(JObj) ->
    publish_doc_type_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_doc_type_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_doc_type_update(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DOC_TYPE_UPDATE_VALUES, fun doc_type_update/1),
    kz_amqp_util:configuration_publish(doc_type_update_routing_key(API), Payload, ContentType, [{'mandatory', 'true'}]).

-spec doc_type_update_routing_key(kz_term:api_terms() | kz_term:ne_binary()) -> kz_term:ne_binary().
doc_type_update_routing_key(<<_/binary>> = Type) ->
    <<"configuration.doc_type_update.", Type/binary>>;
doc_type_update_routing_key(API) ->
    doc_type_update_routing_key(get_type(API)).
