%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
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

-include("kz_amqp_util.hrl").

-define(KEY_REG_SUCCESS, <<"registration.success">>).
-define(KEY_REG_QUERY, <<"registration.query">>).

%% Registration Success
-define(REG_SUCCESS_HEADERS, [<<"Event-Timestamp">>, <<"Contact">>
                             ,<<"Expires">>, <<"Username">>
                             ,<<"Realm">>
                             ]).
-define(OPTIONAL_REG_SUCCESS_HEADERS, [<<"Status">>, <<"User-Agent">>
                                      ,?KEY_API_CALL_ID, <<"Profile-Name">>
                                      ,<<"Presence-Hosts">>
                                      ,?KEY_API_ACCOUNT_ID, <<"Account-DB">>
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
                                      ,<<"Proxy-Protocol">>, <<"Proxy-IP">>, <<"Proxy-Port">>
                                      ,<<"Source-IP">>, <<"Source-Port">>
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
-define(OPTIONAL_REG_QUERY_FIELDS, [<<"Bridge-RURI">>]).
-define(OPTIONAL_REG_QUERY_HEADERS, [<<"Username">>, <<"Realm">>
                                    ,<<"Count-Only">>, <<"Fields">>
                                    ]).
-define(REG_QUERY_VALUES, [{<<"Event-Category">>, <<"directory">>}
                          ,{<<"Event-Name">>, <<"reg_query">>}
                          ]).
-define(REG_QUERY_TYPES, [{<<"Fields">>, fun(Fs) when is_list(Fs) ->
                                                 Allowed = ?OPTIONAL_REG_SUCCESS_HEADERS ++
                                                     ?REG_SUCCESS_HEADERS ++
                                                     ?OPTIONAL_REG_QUERY_FIELDS,
                                                 lists:foldl(fun(F, 'true') -> lists:member(F, Allowed);
                                                                (_, 'false') -> 'false'
                                                             end, 'true', Fs);
                                            (_) -> 'false'
                                         end}
                         ,{<<"Count-Only">>, fun(N) -> kz_term:to_boolean(N) end}
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

%%------------------------------------------------------------------------------
%% @doc Registration Success.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec success(kz_term:api_terms()) ->
                     {'ok', iolist()} |
                     {'error', string()}.
success(Prop) when is_list(Prop) ->
    case success_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_SUCCESS_HEADERS, ?OPTIONAL_REG_SUCCESS_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_success"}
    end;
success(JObj) -> success(kz_json:to_proplist(JObj)).

-spec success_v(kz_term:api_terms()) -> boolean().
success_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_SUCCESS_HEADERS, ?REG_SUCCESS_VALUES, ?REG_SUCCESS_TYPES);
success_v(JObj) -> success_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Registration Success.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:api_terms()) ->
                   {'ok', iolist()} |
                   {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_FLUSH_HEADERS, ?OPTIONAL_REG_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_flush"}
    end;
flush(JObj) -> flush(kz_json:to_proplist(JObj)).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_FLUSH_HEADERS, ?REG_FLUSH_VALUES, ?REG_FLUSH_TYPES);
flush_v(JObj) -> flush_v(kz_json:to_proplist(JObj)).


-spec sync(kz_term:api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
sync(Prop) when is_list(Prop) ->
    case sync_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_SYNC_HEADERS, ?OPTIONAL_REG_SYNC_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_sync"}
    end;
sync(JObj) -> sync(kz_json:to_proplist(JObj)).

-spec sync_v(kz_term:api_terms()) -> boolean().
sync_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_SYNC_HEADERS, ?REG_SYNC_VALUES, ?REG_SYNC_TYPES);
sync_v(JObj) -> sync_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Registration Query.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_req(kz_term:api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_req(Prop) when is_list(Prop) ->
    case query_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_QUERY_HEADERS, ?OPTIONAL_REG_QUERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query"}
    end;
query_req(JObj) -> query_req(kz_json:to_proplist(JObj)).

-spec query_req_v(kz_term:api_terms()) -> boolean().
query_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_QUERY_HEADERS, ?REG_QUERY_VALUES, ?REG_QUERY_TYPES);
query_req_v(JObj) -> query_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Registration Query Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_resp(kz_term:api_terms()) ->
                        {'ok', iolist()} |
                        {'error', string()}.
query_resp(Prop) when is_list(Prop) ->
    case query_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_QUERY_RESP_HEADERS, ?OPTIONAL_REG_QUERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query_resp"}
    end;
query_resp(JObj) -> query_resp(kz_json:to_proplist(JObj)).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_QUERY_RESP_HEADERS, ?REG_QUERY_RESP_VALUES, ?REG_QUERY_RESP_TYPES);
query_resp_v(JObj) -> query_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Registration Query Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_err(kz_term:api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
query_err(Prop) when is_list(Prop) ->
    case query_err_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REG_QUERY_ERR_HEADERS, ?OPTIONAL_REG_QUERY_ERR_HEADERS);
        'false' -> {'error', "Proplist failed validation for reg_query_err"}
    end;
query_err(JObj) -> query_err(kz_json:to_proplist(JObj)).

-spec query_err_v(kz_term:api_terms()) -> boolean().
query_err_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REG_QUERY_ERR_HEADERS, ?REG_QUERY_ERR_VALUES, ?REG_QUERY_ERR_TYPES);
query_err_v(JObj) -> query_err_v(kz_json:to_proplist(JObj)).

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
    _ = kz_amqp_util:bind_q_to_registrar(Q, get_flush_routing(Realm)),
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
    _ = kz_amqp_util:unbind_q_from_registrar(Q, get_flush_routing(Realm)),
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
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_success(kz_term:api_terms()) -> 'ok'.
publish_success(JObj) ->
    publish_success(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_success(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_success(Success, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Success, ?REG_SUCCESS_VALUES, fun success/1),
    kz_amqp_util:registrar_publish(get_success_routing(Success), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?REG_FLUSH_VALUES, fun flush/1),
    kz_amqp_util:registrar_publish(get_flush_routing(API), Payload, ContentType).

-spec publish_query_req(kz_term:api_terms()) -> 'ok'.
publish_query_req(JObj) ->
    publish_query_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REG_QUERY_VALUES, fun query_req/1),
    kz_amqp_util:registrar_publish(get_query_routing(Req), Payload, ContentType).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(Queue, JObj) ->
    publish_query_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?REG_QUERY_RESP_VALUES, fun query_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_query_err(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_err(Queue, JObj) ->
    publish_query_err(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_err(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_err(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?REG_QUERY_ERR_VALUES, fun query_err/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_sync(kz_term:api_terms()) -> 'ok'.
publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?REG_SYNC_VALUES, fun sync/1),
    kz_amqp_util:registrar_publish(?REG_SYNC_RK, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Special access to the API keys.
%% @end
%%------------------------------------------------------------------------------
-spec success_keys() -> kz_term:ne_binaries().
success_keys() ->
    ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS.

-spec get_success_routing(kz_term:api_terms()) -> kz_term:ne_binary().
get_success_routing(Prop) when is_list(Prop) ->
    User = props:get_value(<<"Username">>, Prop),
    Realm = props:get_value(<<"Realm">>, Prop),
    get_success_routing(Realm, User);
get_success_routing(JObj) ->
    User = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    get_success_routing(Realm, User).

-spec get_success_routing(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_success_routing(Realm, User) ->
    list_to_binary([?KEY_REG_SUCCESS, ".", kz_amqp_util:encode(Realm), ".", kz_amqp_util:encode(User)]).

-spec get_query_routing(kz_term:api_terms()) -> kz_term:ne_binary().
get_query_routing(Prop) when is_list(Prop) ->
    User = props:get_value(<<"Username">>, Prop),
    Realm = props:get_value(<<"Realm">>, Prop),
    get_query_routing(Realm, User);
get_query_routing(JObj) ->
    User = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    get_query_routing(Realm, User).

-spec get_query_routing(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_query_routing(Realm, 'undefined') ->
    list_to_binary([?KEY_REG_QUERY, ".", kz_amqp_util:encode(Realm), ".*"]);
get_query_routing(Realm, User) ->
    list_to_binary([?KEY_REG_QUERY, ".", kz_amqp_util:encode(Realm), ".", kz_amqp_util:encode(User)]).

%%------------------------------------------------------------------------------
%% @doc Allow Queues to be bound for specific realms, and even users within those realms.
%% The resulting binding will be `reg.success.{realm | *}.{user | *}'.
%% The `*' matches one segment only, which means all `reg_success' messages will be published to
%% `key.success.realm.user'.
%% @end
%%------------------------------------------------------------------------------
get_success_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", kz_amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", kz_amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_SUCCESS, Realm, User]).

get_query_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", kz_amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", kz_amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_QUERY, Realm, User]).

get_flush_routing(Realm) when is_binary(Realm) ->
    <<"registration.flush.", (kz_amqp_util:encode(Realm))/binary>>;
get_flush_routing(Prop) when is_list(Prop) ->
    get_flush_routing(props:get_value(<<"Realm">>, Prop));
get_flush_routing(JObj) ->
    get_flush_routing(kz_json:get_value(<<"Realm">>, JObj)).
