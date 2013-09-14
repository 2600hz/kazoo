%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Configuration updates (like DB doc changes) can be communicated across
%%% the AMQP bus so WhApps can flush cache entries, update settings, etc.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_conf).

-export([doc_update/1, doc_update_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_doc_update/5, publish_doc_update/6
         ,get_account_id/1, get_account_db/1
         ,get_type/1, get_doc/1, get_id/1
        ]).

-type action() :: 'created' | 'edited' | 'deleted'.
-export_type([action/0]).

-include_lib("whistle/include/wh_api.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(CONF_DOC_UPDATE_HEADERS, [<<"ID">>, <<"Rev">>, <<"Database">>]).
-define(OPTIONAL_CONF_DOC_UPDATE_HEADERS, [<<"Account-ID">>, <<"Type">>, <<"Version">>
                                               ,<<"Date-Modified">>, <<"Date-Created">>
                                               ,<<"Doc">>
                                          ]).
-define(CONF_DOC_UPDATE_VALUES, [{<<"Event-Category">>, <<"configuration">>}
                                 ,{<<"Event-Name">>, [<<"doc_edited">>
                                                          ,<<"doc_created">>
                                                          ,<<"doc_deleted">>
                                                     ]}
                                ]).
-define(CONF_DOC_UPDATE_TYPES, [{<<"ID">>, fun is_binary/1}
                                ,{<<"Rev">>, fun is_binary/1}
                               ]).

-spec get_account_id(api_terms()) -> wh_json:json_term() | 'undefined'.
get_account_id(Prop) when is_list(Prop) ->
    props:get_value(<<"Account-ID">>, Prop);
get_account_id(JObj) ->
    wh_json:get_value(<<"Account-ID">>, JObj).

-spec get_account_db(api_terms()) -> wh_json:json_term() | 'undefined'.
get_account_db(Prop) when is_list(Prop) ->
    props:get_value(<<"Account-DB">>, Prop);
get_account_db(JObj) ->
    wh_json:get_value(<<"Account-DB">>, JObj).

%% returns the public fields of the document
-spec get_doc(api_terms()) -> wh_json:json_term() | 'undefined'.
get_doc(Prop) when is_list(Prop) ->
    props:get_value(<<"Doc">>, Prop);
get_doc(JObj) ->
    wh_json:get_value(<<"Doc">>, JObj).

-spec get_id(api_terms()) -> wh_json:json_term() | 'undefined'.
get_id(Prop) when is_list(Prop) ->
    props:get_value(<<"ID">>, Prop);
get_id(JObj) ->
    wh_json:get_value(<<"ID">>, JObj).

%% returns the pvt_type field
-spec get_type(api_terms()) -> wh_json:json_term() | 'undefined'.
get_type(Prop) when is_list(Prop) ->
    props:get_value(<<"Type">>, Prop);
get_type(JObj) ->
    wh_json:get_value(<<"Type">>, JObj).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec doc_update(api_terms()) ->
                        {'ok', iolist()} |
                        {'error', string()}.
doc_update(Prop) when is_list(Prop) ->
    case doc_update_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONF_DOC_UPDATE_HEADERS, ?OPTIONAL_CONF_DOC_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for document_change"}
    end;
doc_update(JObj) ->
    doc_update(wh_json:to_proplist(JObj)).

-spec doc_update_v(api_terms()) -> boolean().
doc_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONF_DOC_UPDATE_HEADERS, ?CONF_DOC_UPDATE_VALUES, ?CONF_DOC_UPDATE_TYPES);
doc_update_v(JObj) ->
    doc_update_v(wh_json:to_proplist(JObj)).

-spec bind_q(binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    RoutingKey = get_routing_key(Props),
    amqp_util:bind_q_to_configuration(Q, RoutingKey).

-spec unbind_q(binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    RoutingKey = get_routing_key(Props),
    amqp_util:unbind_q_from_configuration(Q, RoutingKey).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:configuration_exchange().

-spec get_routing_key(wh_proplist()) -> binary().
get_routing_key(Props) ->
    Action = props:get_binary_value('action', Props, <<"*">>),
    Db = props:get_binary_value('db', Props, <<"*">>),
    Type = props:get_binary_value('doc_type', Props
                                  ,props:get_value('type', Props, <<"*">>)),
    Id = props:get_binary_value('doc_id', Props
                                ,props:get_value('id', Props, <<"*">>)),
    amqp_util:document_routing_key(Action, Db, Type, Id).

-spec publish_doc_update(action(), binary(), binary(), binary(), api_terms()) -> 'ok'.
-spec publish_doc_update(action(), binary(), binary(), binary(), api_terms(), binary()) -> 'ok'.
publish_doc_update(Action, Db, Type, Id, JObj) ->
    publish_doc_update(Action, Db, Type, Id, JObj, ?DEFAULT_CONTENT_TYPE).
publish_doc_update(Action, Db, Type, Id, Change, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Change, ?CONF_DOC_UPDATE_VALUES, fun ?MODULE:doc_update/1),
    amqp_util:document_change_publish(Action, Db, Type, Id, Payload, ContentType).
