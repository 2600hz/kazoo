%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_frontier).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([ratelimits_req_v/1
        ,ratelimits_resp_v/1
        ,acls_req_v/1
        ,acls_resp_v/1
        ,flush/1, flush_v/1
        ]).
-export([publish_ratelimits_resp/2
        ,publish_acls_resp/2
        ,publish_flush/1, publish_flush/2
        ]).

-include("frontier.hrl").

-define(FRONTIER_EXCHANGE, <<"frontier">>).
-define(EXCHANGE_TYPE, <<"direct">>).
-define(ROUTE_KEY, <<"sbc_config">>).

-define(ACL_FRONTIER_EXCHANGE, <<"frontier_acl">>).
-define(ACL_EXCHANGE_TYPE, <<"topic">>).
-define(ACL_ROUTE_KEY, <<"flush">>).

-define(REQ_HEADERS, [<<"Entity">>]).
-define(OPTIONAL_REQ_HEADERS, [<<"With-Realm">>]).
-define(REQ_TYPES, [{<<"With-Realm">>, fun(V) -> is_boolean(kz_term:to_boolean(V)) end}]).
-define(RESP_HEADERS, []).
-define(OPTIONAL_RESP_HEADERS, [<<"Realm">>, <<"Device">>]).

-define(RATELIMITS_REQ_VALUES, [{<<"Event-Category">>, <<"rate_limit">>}
                               ,{<<"Event-Name">>, <<"query">>}
                               ]).
-define(RATELIMITS_RESP_VALUES, [{<<"Event-Category">>, <<"rate_limit">>}
                                ,{<<"Event-Name">>, <<"query_resp">>}
                                ]).
-define(RATELIMITS_RESP_TYPES, [{<<"Realm">>, fun kz_json:is_json_object/1}
                               ,{<<"Device">>, fun kz_json:is_json_object/1}
                               ]).

-define(ACL_REQ_VALUES, [{<<"Event-Category">>, <<"acl">>}
                        ,{<<"Event-Name">>, <<"query">>}
                        ]).
-define(ACL_RESP_VALUES, [{<<"Event-Category">>, <<"acl">>}
                         ,{<<"Event-Name">>, <<"query_resp">>}
                         ]).
-define(ACL_RESP_TYPES, [{<<"Realm">>, fun kz_json:is_json_object/1}
                        ,{<<"Device">>, fun kz_json:is_json_object/1}
                        ]).

%% ACL Flush
-define(ACL_FLUSH_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_ACL_FLUSH_HEADERS, [<<"Device">>]).
-define(ACL_FLUSH_VALUES, [{<<"Event-Category">>, <<"acl">>}
                          ,{<<"Event-Name">>, <<"acl_flush">>}
                          ]).
-define(ACL_FLUSH_TYPES, []).


-spec ratelimits_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
ratelimits_resp(Prop) when is_list(Prop) ->
    case ratelimits_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
ratelimits_resp(JObj) -> ratelimits_resp(kz_json:to_proplist(JObj)).

-spec publish_ratelimits_resp(kz_term:ne_binary(), kz_json:object()) -> ok.
publish_ratelimits_resp(Srv, JObj) -> publish_ratelimits_resp(Srv, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_ratelimits_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> ok.
publish_ratelimits_resp(Srv, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RATELIMITS_RESP_VALUES, fun ratelimits_resp/1),
    kz_amqp_util:targeted_publish(Srv, Payload, ContentType).

-spec ratelimits_req_v(kz_term:api_terms()) -> boolean().
ratelimits_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REQ_HEADERS, ?RATELIMITS_REQ_VALUES, ?REQ_TYPES);
ratelimits_req_v(JObj) -> ratelimits_req_v(kz_json:to_proplist(JObj)).

-spec ratelimits_resp_v(kz_term:api_terms()) -> boolean().
ratelimits_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?RATELIMITS_RESP_VALUES, ?RATELIMITS_RESP_TYPES);
ratelimits_resp_v(JObj) -> ratelimits_req_v(kz_json:to_proplist(JObj)).

-spec acls_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
acls_resp(Prop) when is_list(Prop) ->
    case acls_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for acl response"}
    end;
acls_resp(JObj) -> acls_resp(kz_json:to_proplist(JObj)).

-spec publish_acls_resp(kz_term:ne_binary(), kz_json:object()) -> ok.
publish_acls_resp(Srv, JObj) -> publish_acls_resp(Srv, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_acls_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> ok.
publish_acls_resp(Srv, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?ACL_RESP_VALUES, fun acls_resp/1),
    kz_amqp_util:targeted_publish(Srv, Payload, ContentType).

-spec acls_req_v(kz_term:api_terms()) -> boolean().
acls_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REQ_HEADERS, ?ACL_REQ_VALUES, ?REQ_TYPES);
acls_req_v(JObj) -> acls_req_v(kz_json:to_proplist(JObj)).

-spec acls_resp_v(kz_term:api_terms()) -> boolean().
acls_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?ACL_RESP_VALUES, ?ACL_RESP_TYPES);
acls_resp_v(JObj) -> acls_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_exchange(Q, ?ROUTE_KEY, ?FRONTIER_EXCHANGE).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_exchange(Q, ?ROUTE_KEY, ?FRONTIER_EXCHANGE).

-spec flush(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
flush(Prop) when is_list(Prop) ->
    case flush_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ACL_FLUSH_HEADERS, ?OPTIONAL_ACL_FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for acl_flush"}
    end;
flush(JObj) -> flush(kz_json:to_proplist(JObj)).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ACL_FLUSH_HEADERS, ?ACL_FLUSH_VALUES, ?ACL_FLUSH_TYPES);
flush_v(JObj) -> flush_v(kz_json:to_proplist(JObj)).

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?ACL_FLUSH_VALUES, fun flush/1),
    kz_amqp_util:basic_publish(?ACL_FRONTIER_EXCHANGE, ?ACL_ROUTE_KEY, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?FRONTIER_EXCHANGE, ?EXCHANGE_TYPE),
    kz_amqp_util:new_exchange(?ACL_FRONTIER_EXCHANGE, ?ACL_EXCHANGE_TYPE).
