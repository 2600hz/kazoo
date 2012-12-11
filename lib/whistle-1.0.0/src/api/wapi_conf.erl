%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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
         ,publish_doc_update/5, publish_doc_update/6
         ,get_account_id/1, get_account_db/1
         ,get_type/1, get_doc/1, get_id/1
        ]).

-type action() :: 'created' | 'edited' | 'deleted'.
-export_type([action/0]).

-include_lib("wh_api.hrl").

%% Configuration Document Update
-define(CONF_DOC_UPDATE_HEADERS, [<<"ID">>, <<"Rev">>, <<"Doc">>]).
-define(OPTIONAL_CONF_DOC_UPDATE_HEADERS, [<<"Account-DB">>, <<"Account-ID">>
                                               ,<<"Date-Modified">>, <<"Date-Created">>
                                               ,<<"Type">>, <<"Version">>
                                          ]).
-define(CONF_DOC_UPDATE_VALUES, [{<<"Event-Category">>, <<"configuration">>}
                                 ,{<<"Event-Name">>, [<<"doc_edited">>, <<"doc_created">>, <<"doc_deleted">>]}]).
-define(CONF_DOC_UPDATE_TYPES, [{<<"ID">>, fun is_binary/1}
                                ,{<<"Rev">>, fun is_binary/1}
                               ]).

-spec get_account_id/1 :: (api_terms()) -> wh_json:json_term() | 'undefined'.
get_account_id(Prop) when is_list(Prop) ->
    props:get_value(<<"Account-ID">>, Prop);
get_account_id(JObj) ->
    wh_json:get_value(<<"Account-ID">>, JObj).

-spec get_account_db/1 :: (api_terms()) -> wh_json:json_term() | 'undefined'.
get_account_db(Prop) when is_list(Prop) ->
    props:get_value(<<"Account-DB">>, Prop);
get_account_db(JObj) ->
    wh_json:get_value(<<"Account-DB">>, JObj).

%% returns the public fields of the document
-spec get_doc/1 :: (api_terms()) -> wh_json:json_term() | 'undefined'.
get_doc(Prop) when is_list(Prop) ->
    props:get_value(<<"Doc">>, Prop);
get_doc(JObj) ->
    wh_json:get_value(<<"Doc">>, JObj).

-spec get_id/1 :: (api_terms()) -> wh_json:json_term() | 'undefined'.
get_id(Prop) when is_list(Prop) ->
    props:get_value(<<"ID">>, Prop);
get_id(JObj) ->
    wh_json:get_value(<<"ID">>, JObj).

%% returns the pvt_type field
-spec get_type/1 :: (api_terms()) -> wh_json:json_term() | 'undefined'.
get_type(Prop) when is_list(Prop) ->
    props:get_value(<<"Type">>, Prop);
get_type(JObj) ->
    wh_json:get_value(<<"Type">>, JObj).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec doc_update/1 :: (api_terms()) ->
                              {'ok', iolist()} |
                              {'error', string()}.
doc_update(Prop) when is_list(Prop) ->
    case doc_update_v(Prop) of
        true -> wh_api:build_message(Prop, ?CONF_DOC_UPDATE_HEADERS, ?OPTIONAL_CONF_DOC_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for document_change"}
    end;
doc_update(JObj) ->
    doc_update(wh_json:to_proplist(JObj)).

-spec doc_update_v/1 :: (api_terms()) -> boolean().
doc_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONF_DOC_UPDATE_HEADERS, ?CONF_DOC_UPDATE_VALUES, ?CONF_DOC_UPDATE_TYPES);
doc_update_v(JObj) ->
    doc_update_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    amqp_util:configuration_exchange(),

    RoutingKey = get_routing_key(Props),
    amqp_util:bind_q_to_configuration(Q, RoutingKey).

-spec unbind_q/2 :: (binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    RoutingKey = get_routing_key(Props),
    amqp_util:unbind_q_from_configuration(Q, RoutingKey).

-spec get_routing_key/1 :: (wh_proplist()) -> binary().
get_routing_key(Props) ->
    Action = props:get_value(action, Props, <<"*">>), % see conf_action() type below
    Db = props:get_value(db, Props, <<"*">>),
    DocType = props:get_value(doc_type, Props, <<"*">>),
    DocId = props:get_value(doc_id, Props, <<"*">>),
    amqp_util:document_routing_key(Action, Db, DocType, DocId).

-spec publish_doc_update/5 :: (action(), binary(), binary(), binary(), api_terms()) -> 'ok'.
-spec publish_doc_update/6 :: (action(), binary(), binary(), binary(), api_terms(), binary()) -> 'ok'.
publish_doc_update(Action, Db, Type, Id, JObj) ->
    publish_doc_update(Action, Db, Type, Id, JObj, ?DEFAULT_CONTENT_TYPE).
publish_doc_update(Action, Db, Type, Id, Change, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Change, ?CONF_DOC_UPDATE_VALUES, fun ?MODULE:doc_update/1),
    amqp_util:document_change_publish(Action, Db, Type, Id, Payload, ContentType).
