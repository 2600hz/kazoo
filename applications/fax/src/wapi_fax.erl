%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_fax).

-export([
          req/1, req_v/1
         ,query/1, query_v/1
         ,status/1, status_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_req/1, publish_req/2
         ,publish_query/2, publish_query/3
         ,publish_status/1, publish_status/2
         ,publish_targeted_status/2, publish_targeted_status/3
        ]).

-include("fax.hrl").

-define(FAX_EXCHANGE, <<"fax">>).

-define(FAX_REQ_HEADERS, [<<"Call">>, <<"Action">>]).
-define(OPTIONAL_FAX_REQ_HEADERS, [<<"Owner-ID">>]).
-define(FAX_REQ_VALUES, [{<<"Event-Category">>,<<"dialplan">>}
                         ,{<<"Event-Name">>, <<"fax_req">>}
                         ,{<<"Action">>, [<<"receive">>, <<"transmit">>]}
                        ]).
-define(FAX_REQ_TYPES, [{<<"Call">>, fun wh_json:is_json_object/1}]).



-define(FAX_QUERY_HEADERS, [<<"Job-ID">>]).
-define(OPTIONAL_FAX_QUERY_HEADERS, []).
-define(FAX_QUERY_VALUES, [{<<"Event-Category">>,<<"fax">>}
                         ,{<<"Event-Name">>, <<"query">>}
                        ]).
-define(FAX_QUERY_TYPES, []).


-define(FAX_STATUS_HEADERS, [<<"Job-ID">>]).
-define(OPTIONAL_FAX_STATUS_HEADERS, [<<"Status">>, <<"Fax-BoxId">>, <<"Account-ID">>, <<"Fax-Info">>]).
-define(FAX_STATUS_VALUES, [{<<"Event-Category">>,<<"fax">>}
                         ,{<<"Event-Name">>, <<"status">>}
                        ]).
-define(FAX_STATUS_TYPES, []).


req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_req"};
        'true' -> wh_api:build_message(Prop, ?FAX_REQ_HEADERS, ?OPTIONAL_FAX_REQ_HEADERS)
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_REQ_HEADERS, ?FAX_REQ_VALUES, ?FAX_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).


query(Prop) when is_list(Prop) ->
    case query_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query"};
        'true' -> wh_api:build_message(Prop, ?FAX_QUERY_HEADERS, ?OPTIONAL_FAX_QUERY_HEADERS)
    end;
query(JObj) ->
    query(wh_json:to_proplist(JObj)).

query_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_QUERY_HEADERS, ?FAX_QUERY_VALUES, ?FAX_QUERY_TYPES);
query_v(JObj) ->
    query_v(wh_json:to_proplist(JObj)).



status(Prop) when is_list(Prop) ->
    case status_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query"};
        'true' -> wh_api:build_message(Prop, ?FAX_STATUS_HEADERS, ?OPTIONAL_FAX_STATUS_HEADERS)
    end;
status(JObj) ->
    status(wh_json:to_proplist(JObj)).

status_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_STATUS_HEADERS, ?FAX_STATUS_VALUES, ?FAX_STATUS_TYPES);
status_v(JObj) ->
    status_v(wh_json:to_proplist(JObj)).





bind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    bind_q(Queue, AccountId, FaxId, props:get_value('restrict_to', Props)).    

bind_q(Queue, AccountId, FaxId, 'undefined') ->
    amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    amqp_util:bind_q_to_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    amqp_util:bind_q_to_targeted(Queue);
bind_q(Queue, AccountId, FaxId, ['status'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    bind_q(Queue, AccountId, FaxId, Restrict);
bind_q(Queue, AccountId, FaxId, ['query'|Restrict]) ->
    amqp_util:bind_q_to_targeted(Queue),
    bind_q(Queue, AccountId, FaxId, Restrict);
bind_q(Queue, AccountId, FaxId, ['req'|Restrict]) ->
    amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    bind_q(Queue, AccountId, FaxId, Restrict);
bind_q(_, _, _, []) -> 'ok'.


unbind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    unbind_q(Queue, AccountId, FaxId, props:get_value('restrict_to', Props)).    

unbind_q(Queue, AccountId, FaxId, 'undefined') ->
    amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    amqp_util:unbind_q_from_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    amqp_util:unbind_q_from_targeted(Queue);
unbind_q(Queue, AccountId, FaxId, ['status'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    unbind_q(Queue, AccountId, FaxId, Restrict);
unbind_q(Queue, AccountId, FaxId, ['query'|Restrict]) ->
    amqp_util:unbind_q_from_targeted(Queue),
    unbind_q(Queue, AccountId, FaxId, Restrict);
unbind_q(Queue, AccountId, FaxId, ['req'|Restrict]) ->
    amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    unbind_q(Queue, AccountId, FaxId, Restrict);
unbind_q(_, _, _, []) -> 'ok'.


%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?FAX_EXCHANGE, <<"fanout">>),
    amqp_util:targeted_exchange(),
    amqp_util:callmgr_exchange().

publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Api, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Api, ?FAX_REQ_VALUES, fun req/1),
    amqp_util:callmgr_publish(Payload, ContentType, fax_routing_key()).

publish_query(Q, JObj) ->
    publish_query(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?FAX_QUERY_VALUES, fun ?MODULE:query/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).


publish_status(API) ->
    publish_status(API, ?DEFAULT_CONTENT_TYPE).
publish_status(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?FAX_STATUS_VALUES, fun status/1),
    FaxId = props:get_first_defined([<<"Fax-ID">>,<<"Job-ID">>], API,<<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, API, <<"*">>),
    amqp_util:basic_publish(?FAX_EXCHANGE, status_routing_key(AccountId, FaxId), Payload, ContentType).

publish_targeted_status(Q, JObj) ->
    publish_targeted_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_targeted_status(Q, Api, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Api, ?FAX_STATUS_VALUES, fun status/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

fax_routing_key() ->
    <<"fax.req">>.
status_routing_key(AccountId, FaxId) ->
    list_to_binary(["fax.status.", amqp_util:encode(AccountId), ".", amqp_util:encode(FaxId) ]).

