%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Handle registration-related APIs, like reg_success and reg_lookup.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_registration).

-export([success/1, success_v/1
         ,query_req/1, query_req_v/1
         ,query_resp/1, query_resp_v/1
         ,query_err/1, query_err_v/1
         ,flush/1, flush_v/1
         ,sync/1, sync_v/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([success_keys/0]).

-export([publish_success/1, publish_success/2
         ,publish_query_req/1, publish_query_req/2
         ,publish_query_resp/2, publish_query_resp/3
         ,publish_query_err/2, publish_query_err/3
         ,publish_flush/1, publish_flush/2
         ,publish_sync/1, publish_sync/2
        ]).

-include_lib("whistle/include/wh_api.hrl").

-define(KEY_REG_SUCCESS, <<"registration.success">>).
-define(KEY_REG_QUERY, <<"registration.query">>).

%% Registration Success
-define(REG_SUCCESS_HEADERS, [<<"Event-Timestamp">>, <<"Contact">>
                              ,<<"Expires">>, <<"Username">>
                              ,<<"Realm">>
                             ]).
-define(OPTIONAL_REG_SUCCESS_HEADERS, [<<"Status">>, <<"User-Agent">>
                                       ,<<"Call-ID">>, <<"Profile-Name">>
                                       ,<<"Presence-Hosts">>
                                       ,<<"Account-ID">>, <<"Account-DB">>
                                       ,<<"From-User">>, <<"From-Host">>
                                       ,<<"To-User">>, <<"To-Host">>
                                       ,<<"RPid">>, <<"Authorizing-ID">>
                                       ,<<"FreeSWITCH-Hostname">>
                                       ,<<"FreeSWITCH-Nodename">>
                                       ,<<"Network-IP">>, <<"Network-Port">>
                                       ,<<"Suppress-Unregister-Notify">>
                                       ,<<"Register-Overwrite-Notify">>
                                       ,<<"Original-Contact">>
                                       ,<<"Registrar-Node">>
                                       ,<<"Proxy-Path">>, <<"AOR">>, <<"RUID">>
                                       ,<<"First-Registration">>
                                       ,<<"Custom-Channel-Vars">>
                                      ]).
-define(REG_SUCCESS_VALUES, [{<<"Event-Category">>, <<"directory">>}
                             ,{<<"Event-Name">>, <<"reg_success">>}
                            ]).
-define(REG_SUCCESS_TYPES, []).

%% Registration Flush
-define(REG_FLUSH_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_REG_FLUSH_HEADERS, [<<"Username">>]).
-define(REG_FLUSH_VALUES, [{<<"Event-Category">>, <<"directory">>}
                           ,{<<"Event-Name">>, <<"reg_flush">>}
                          ]).
-define(REG_FLUSH_TYPES, []).

%% Query Registrations
-define(REG_QUERY_HEADERS, []).
-define(OPTIONAL_REG_QUERY_HEADERS, [<<"Username">>, <<"Realm">>
                                     ,<<"Count-Only">>, <<"Fields">>
                                    ]).
-define(REG_QUERY_VALUES, [{<<"Event-Category">>, <<"directory">>}
                           ,{<<"Event-Name">>, <<"reg_query">>}
                          ]).
-define(REG_QUERY_TYPES, [{<<"Fields">>, fun(Fs) when is_list(Fs) ->
                                                 Allowed = ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS,
                                                 lists:foldl(fun(F, 'true') -> lists:member(F, Allowed);
                                                                (_, 'false') -> 'false'
                                                             end, 'true', Fs);
                                            (_) -> 'false'
                                         end}
                          ,{<<"Count-Only">>, fun(N) -> wh_util:to_boolean(N) end}
                         ]).

%% Registration Query Response
-define(REG_QUERY_RESP_HEADERS, []).
-define(OPTIONAL_REG_QUERY_RESP_HEADERS, [<<"Registrar-Age">>, <<"Count">>, <<"Fields">>]).
-define(REG_QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
                                ,{<<"Event-Name">>, <<"reg_query_resp">>}
                               ]).
-define(REG_QUERY_RESP_TYPES, []).

%% Registration Query Error
-define(REG_QUERY_ERR_HEADERS, []).
-define(OPTIONAL_REG_QUERY_ERR_HEADERS, [<<"Registrar-Age">>]).
-define(REG_QUERY_ERR_VALUES, [{<<"Event-Category">>, <<"directory">>}
                               ,{<<"Event-Name">>, <<"reg_query_error">>}
                              ]).
-define(REG_QUERY_ERR_TYPES, []).

%% Registration Sync
-define(REG_SYNC_HEADERS, []).
-define(OPTIONAL_REG_SYNC_HEADERS, []).
-define(REG_SYNC_VALUES, [{<<"Event-Category">>, <<"directory">>}
                           ,{<<"Event-Name">>, <<"reg_sync">>}
                          ]).
-define(REG_SYNC_TYPES, []).
-define(REG_SYNC_RK, <<"registration.sync">>).

%%--------------------------------------------------------------------
%% @doc Registration Success - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec success(api_terms()) ->
                     {'ok', iolist()} |
                     {'error', string()}.
success(Prop) when is_list(Prop) ->
    case success_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_SUCCESS_HEADERS, ?OPTIONAL_REG_SUCCESS_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_success"}
    end;
success(JObj) -> success(wh_json:to_proplist(JObj)).

-spec success_v(api_terms()) -> boolean().
success_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_SUCCESS_HEADERS, ?REG_SUCCESS_VALUES, ?REG_SUCCESS_TYPES);
success_v(JObj) -> success_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Registration Success - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec flush(api_terms()) ->
                   {'ok', iolist()} |
                   {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_FLUSH_HEADERS, ?OPTIONAL_REG_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_flush"}
    end;
flush(JObj) -> flush(wh_json:to_proplist(JObj)).

-spec flush_v(api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_FLUSH_HEADERS, ?REG_FLUSH_VALUES, ?REG_FLUSH_TYPES);
flush_v(JObj) -> flush_v(wh_json:to_proplist(JObj)).


-spec sync(api_terms()) ->
                   {'ok', iolist()} |
                   {'error', string()}.
sync(Prop) when is_list(Prop) ->
    case sync_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_SYNC_HEADERS, ?OPTIONAL_REG_SYNC_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_sync"}
    end;
sync(JObj) -> sync(wh_json:to_proplist(JObj)).

-spec sync_v(api_terms()) -> boolean().
sync_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_SYNC_HEADERS, ?REG_SYNC_VALUES, ?REG_SYNC_TYPES);
sync_v(JObj) -> sync_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Registration Query - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_req(api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_req(Prop) when is_list(Prop) ->
    case query_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_QUERY_HEADERS, ?OPTIONAL_REG_QUERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query"}
    end;
query_req(JObj) -> query_req(wh_json:to_proplist(JObj)).

-spec query_req_v(api_terms()) -> boolean().
query_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_QUERY_HEADERS, ?REG_QUERY_VALUES, ?REG_QUERY_TYPES);
query_req_v(JObj) -> query_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Registration Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_resp(api_terms()) ->
                        {'ok', iolist()} |
                        {'error', string()}.
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_QUERY_RESP_HEADERS, ?OPTIONAL_REG_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query_resp"}
    end;
query_resp(JObj) -> query_resp(wh_json:to_proplist(JObj)).

-spec query_resp_v(api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_QUERY_RESP_HEADERS, ?REG_QUERY_RESP_VALUES, ?REG_QUERY_RESP_TYPES);
query_resp_v(JObj) -> query_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Registration Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_err(api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_err(Prop) when is_list(Prop) ->
    case query_err_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REG_QUERY_ERR_HEADERS, ?OPTIONAL_REG_QUERY_ERR_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query_err"}
    end;
query_err(JObj) -> query_err(wh_json:to_proplist(JObj)).

-spec query_err_v(api_terms()) -> boolean().
query_err_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REG_QUERY_ERR_HEADERS, ?REG_QUERY_ERR_VALUES, ?REG_QUERY_ERR_TYPES);
query_err_v(JObj) -> query_err_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q(Q, props:get_value('restrict_to', Props), Props).

bind_q(Q, 'undefined', Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_success_binding(Props)),
    amqp_util:bind_q_to_callmgr(Q, get_query_binding(Props));
bind_q(Q, ['reg_success'|T], Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_success_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_query'|T], Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_query_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = amqp_util:bind_q_to_callmgr(Q, get_flush_routing(Realm)),
    bind_q(Q, T, Props);
bind_q(Q, [_|T], Props) -> bind_q(Q, T, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q(Q, props:get_value('restrict_to', Props), Props).

unbind_q(Q, 'undefined', Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_success_binding(Props)),
    amqp_util:unbind_q_from_callmgr(Q, get_query_binding(Props));
unbind_q(Q, ['reg_success'|T], Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_success_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_query'|T], Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_query_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = amqp_util:unbind_q_from_callmgr(Q, get_flush_routing(Realm)),
    unbind_q(Q, T, Props);
unbind_q(Q, [_|T], Props) -> unbind_q(Q, T, Props);
unbind_q(_, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_success(api_terms()) -> 'ok'.
-spec publish_success(api_terms(), ne_binary()) -> 'ok'.
publish_success(JObj) ->
    publish_success(JObj, ?DEFAULT_CONTENT_TYPE).
publish_success(Success, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Success, ?REG_SUCCESS_VALUES, fun ?MODULE:success/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_success_routing(Success)).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_flush(api_terms()) -> 'ok'.
-spec publish_flush(api_terms(), ne_binary()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).
publish_flush(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?REG_FLUSH_VALUES, fun ?MODULE:flush/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_flush_routing(API)).

-spec publish_query_req(api_terms()) -> 'ok'.
-spec publish_query_req(api_terms(), ne_binary()) -> 'ok'.
publish_query_req(JObj) ->
    publish_query_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?REG_QUERY_VALUES, fun ?MODULE:query_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_query_routing(Req)).

-spec publish_query_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_resp(Queue, JObj) ->
    publish_query_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?REG_QUERY_RESP_VALUES, fun ?MODULE:query_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_query_err(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_err(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_err(Queue, JObj) ->
    publish_query_err(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_err(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?REG_QUERY_ERR_VALUES, fun ?MODULE:query_err/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_sync(api_terms()) -> 'ok'.
-spec publish_sync(api_terms(), ne_binary()) -> 'ok'.
publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?REG_SYNC_VALUES, fun ?MODULE:sync/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?REG_SYNC_RK).

%%--------------------------------------------------------------------
%% @doc Special access to the API keys
%% @end
%%--------------------------------------------------------------------
-spec success_keys() -> ne_binaries().
success_keys() ->
    ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS.

-spec get_success_routing(api_terms()) -> ne_binary().
get_success_routing(Prop) when is_list(Prop) ->
    User = props:get_value(<<"Username">>, Prop),
    Realm = props:get_value(<<"Realm">>, Prop),
    get_success_routing(Realm, User);
get_success_routing(JObj) ->
    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    get_success_routing(Realm, User).

-spec get_success_routing(ne_binary(), ne_binary()) -> ne_binary().
get_success_routing(Realm, User) ->
    list_to_binary([?KEY_REG_SUCCESS, ".", amqp_util:encode(Realm), ".", amqp_util:encode(User)]).

-spec get_query_routing(api_terms()) -> ne_binary().
get_query_routing(Prop) when is_list(Prop) ->
    User = props:get_value(<<"Username">>, Prop),
    Realm = props:get_value(<<"Realm">>, Prop),
    get_query_routing(Realm, User);
get_query_routing(JObj) ->
    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    get_query_routing(Realm, User).

-spec get_query_routing(ne_binary(), ne_binary()) -> ne_binary().
get_query_routing(Realm, 'undefined') ->
    list_to_binary([?KEY_REG_QUERY, ".", amqp_util:encode(Realm), ".*"]);
get_query_routing(Realm, User) ->
    list_to_binary([?KEY_REG_QUERY, ".", amqp_util:encode(Realm), ".", amqp_util:encode(User)]).

%% Allow Queues to be bound for specific realms, and even users within those realms
%% the resulting binding will be reg.success.{realm | *}.{user | *}
%% * matches one segment only, which means all reg_success messages will be published to
%% "key.success.realm.user"
get_success_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_SUCCESS, Realm, User]).

get_query_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_QUERY, Realm, User]).

get_flush_routing(Realm) when is_binary(Realm) ->
    <<"registration.flush.", (amqp_util:encode(Realm))/binary>>;
get_flush_routing(Prop) when is_list(Prop) ->
    get_flush_routing(props:get_value(<<"Realm">>, Prop));
get_flush_routing(JObj) ->
    get_flush_routing(wh_json:get_value(<<"Realm">>, JObj)).
