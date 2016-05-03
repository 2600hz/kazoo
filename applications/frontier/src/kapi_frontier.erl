%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kapi_frontier).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([ratelimits_req_v/1
         ,ratelimits_resp_v/1
         ,acls_req_v/1
         ,acls_resp_v/1
        ]).
-export([publish_ratelimits_resp/2
         ,publish_acls_resp/2
        ]).

-include("frontier.hrl").
-include_lib("kazoo/include/kz_amqp.hrl").

-define(FRONTIER_EXCHANGE, <<"frontier">>).
-define(EXCHANGE_TYPE, <<"direct">>).

-define(ROUTE_KEY, <<"sbc_config">>).

-define(REQ_HEADERS, [<<"Entity">>]).
-define(OPTIONAL_REQ_HEADERS, [<<"With-Realm">>]).
-define(REQ_TYPES, [{<<"With-Realm">>, fun(V) -> is_boolean(kz_util:to_boolean(V)) end}]).
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

-spec ratelimits_resp(api(terms())) -> {'ok', iolist()} | {'error', string()}.
ratelimits_resp(Prop) when is_list(Prop) ->
    case ratelimits_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for subscription"}
    end;
ratelimits_resp(JObj) -> ratelimits_resp(kz_json:to_proplist(JObj)).

publish_ratelimits_resp(Srv, JObj) -> publish_ratelimits_resp(Srv, JObj, ?DEFAULT_CONTENT_TYPE).
publish_ratelimits_resp(Srv, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RATELIMITS_RESP_VALUES, fun ratelimits_resp/1),
    amqp_util:targeted_publish(Srv, Payload, ContentType).

-spec ratelimits_req_v(api(terms())) -> boolean().
ratelimits_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REQ_HEADERS, ?RATELIMITS_REQ_VALUES, ?REQ_TYPES);
ratelimits_req_v(JObj) -> ratelimits_req_v(kz_json:to_proplist(JObj)).

-spec ratelimits_resp_v(api(terms())) -> boolean().
ratelimits_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?RATELIMITS_RESP_VALUES, ?RATELIMITS_RESP_TYPES);
ratelimits_resp_v(JObj) -> ratelimits_req_v(kz_json:to_proplist(JObj)).

-spec acls_resp(api(terms())) -> {'ok', iolist()} | {'error', string()}.
acls_resp(Prop) when is_list(Prop) ->
    case acls_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for acl response"}
    end;
acls_resp(JObj) -> acls_resp(kz_json:to_proplist(JObj)).

publish_acls_resp(Srv, JObj) -> publish_acls_resp(Srv, JObj, ?DEFAULT_CONTENT_TYPE).
publish_acls_resp(Srv, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?ACL_RESP_VALUES, fun acls_resp/1),
    amqp_util:targeted_publish(Srv, Payload, ContentType).

-spec acls_req_v(api(terms())) -> boolean().
acls_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REQ_HEADERS, ?ACL_REQ_VALUES, ?REQ_TYPES);
acls_req_v(JObj) -> acls_req_v(kz_json:to_proplist(JObj)).

-spec acls_resp_v(api(terms())) -> boolean().
acls_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?ACL_RESP_VALUES, ?ACL_RESP_TYPES);
acls_resp_v(JObj) -> acls_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:bind_q_to_exchange(Q, ?ROUTE_KEY, ?FRONTIER_EXCHANGE).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_exchange(Q, ?ROUTE_KEY, ?FRONTIER_EXCHANGE).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?FRONTIER_EXCHANGE, ?EXCHANGE_TYPE).
