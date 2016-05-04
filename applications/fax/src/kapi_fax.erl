%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_fax).

-export([req/1, req_v/1
         ,query_status/1, query_status_v/1
         ,status/1, status_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_req/1, publish_req/2
         ,publish_query_status/2, publish_query_status/3
         ,publish_status/1, publish_status/2
         ,publish_targeted_status/2, publish_targeted_status/3
        ]).

-include("fax.hrl").

-define(FAX_EXCHANGE, <<"fax">>).

-define(FAX_REQ_HEADERS, [<<"Call">>, <<"Action">>]).
-define(OPTIONAL_FAX_REQ_HEADERS, [<<"Owner-ID">>, <<"FaxBox-ID">>, <<"Fax-T38-Option">>]).
-define(FAX_REQ_VALUES, [{<<"Event-Category">>,<<"dialplan">>}
                         ,{<<"Event-Name">>, <<"fax_req">>}
                         ,{<<"Action">>, [<<"receive">>, <<"transmit">>]}
                        ]).
-define(FAX_REQ_TYPES, [{<<"Call">>, fun kz_json:is_json_object/1}]).

-define(FAX_QUERY_HEADERS, [<<"Job-ID">>]).
-define(OPTIONAL_FAX_QUERY_HEADERS, []).
-define(FAX_QUERY_VALUES, [{<<"Event-Category">>,<<"fax">>}
                           ,{<<"Event-Name">>, <<"query_status">>}
                          ]).
-define(FAX_QUERY_TYPES, []).

-define(FAX_STATUS_HEADERS, [<<"Job-ID">>]).
-define(OPTIONAL_FAX_STATUS_HEADERS, [<<"Status">>, <<"FaxBox-ID">>
                                      ,<<"Account-ID">>, <<"Fax-Info">>
                                      ,<<"Cloud-Job-ID">>, <<"Cloud-Printer-ID">>
                                      ,<<"Fax-State">>, <<"Direction">>, <<"Page">>
                                      ,<<"Caller-ID-Number">>, <<"Caller-ID-Name">>
                                      ,<<"Callee-ID-Number">>, <<"Callee-ID-Name">>
                                     ]).
-define(FAX_STATUS_VALUES, [{<<"Event-Category">>,<<"fax">>}
                            ,{<<"Event-Name">>, <<"status">>}
                            ,{<<"Fax-State">>, ?FAX_STATE_LIST}
                            ,{<<"Direction">>, [?FAX_INCOMING, ?FAX_OUTGOING]}
                           ]).
-define(FAX_STATUS_TYPES, []).

-spec req(maybe(terms())) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_req"};
        'true' -> kz_api:build_message(Prop, ?FAX_REQ_HEADERS, ?OPTIONAL_FAX_REQ_HEADERS)
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(maybe(terms())) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_REQ_HEADERS, ?FAX_REQ_VALUES, ?FAX_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

-spec query_status(maybe(terms())) -> {'ok', iolist()} | {'error', string()}.
query_status(Prop) when is_list(Prop) ->
    case query_status_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query_status"};
        'true' -> kz_api:build_message(Prop, ?FAX_QUERY_HEADERS, ?OPTIONAL_FAX_QUERY_HEADERS)
    end;
query_status(JObj) ->
    query_status(kz_json:to_proplist(JObj)).

-spec query_status_v(maybe(terms())) -> boolean().
query_status_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_QUERY_HEADERS, ?FAX_QUERY_VALUES, ?FAX_QUERY_TYPES);
query_status_v(JObj) ->
    query_status_v(kz_json:to_proplist(JObj)).

-spec status(maybe(terms())) -> {'ok', iolist()} | {'error', string()}.
status(Prop) when is_list(Prop) ->
    case status_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query_status"};
        'true' -> kz_api:build_message(Prop, ?FAX_STATUS_HEADERS, ?OPTIONAL_FAX_STATUS_HEADERS)
    end;
status(JObj) ->
    status(kz_json:to_proplist(JObj)).

-spec status_v(maybe(terms())) -> boolean().
status_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_STATUS_HEADERS, ?FAX_STATUS_VALUES, ?FAX_STATUS_TYPES);
status_v(JObj) ->
    status_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    bind_q(Queue, FaxId, props:get_value('restrict_to', Props)).

-spec bind_q(ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, FaxId, 'undefined') ->
    amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    amqp_util:bind_q_to_exchange(Queue, status_routing_key(FaxId), ?FAX_EXCHANGE);
bind_q(Queue, FaxId, ['status'|Restrict]) ->
    amqp_util:bind_q_to_exchange(Queue, status_routing_key(FaxId), ?FAX_EXCHANGE),
    bind_q(Queue, FaxId, Restrict);
bind_q(Queue, FaxId, ['query_status'|Restrict]) ->
    amqp_util:bind_q_to_targeted(Queue),
    bind_q(Queue, FaxId, Restrict);
bind_q(Queue, FaxId, ['req'|Restrict]) ->
    amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    bind_q(Queue, FaxId, Restrict);
bind_q(_, _, []) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    unbind_q(Queue, FaxId, props:get_value('restrict_to', Props)).

-spec unbind_q(ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, FaxId, 'undefined') ->
    amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    amqp_util:unbind_q_from_exchange(Queue, status_routing_key(FaxId), ?FAX_EXCHANGE);
unbind_q(Queue, FaxId, ['status'|Restrict]) ->
    amqp_util:unbind_q_from_exchange(Queue, status_routing_key(FaxId), ?FAX_EXCHANGE),
    unbind_q(Queue, FaxId, Restrict);
unbind_q(Queue, FaxId, ['query_status'|Restrict]) ->
    amqp_util:unbind_q_from_targeted(Queue),
    unbind_q(Queue, FaxId, Restrict);
unbind_q(Queue, FaxId, ['req'|Restrict]) ->
    amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    unbind_q(Queue, FaxId, Restrict);
unbind_q(_, _, []) -> 'ok'.

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

-spec publish_req(maybe(terms())) -> 'ok'.
-spec publish_req(maybe(terms()), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

publish_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?FAX_REQ_VALUES, fun req/1),
    amqp_util:callmgr_publish(Payload, ContentType, fax_routing_key()).

-spec publish_query_status(ne_binary(), maybe(terms())) -> 'ok'.
-spec publish_query_status(ne_binary(), maybe(terms()), ne_binary()) -> 'ok'.
publish_query_status(Q, JObj) ->
    publish_query_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

publish_query_status(Q, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_QUERY_VALUES, fun ?MODULE:query_status/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_status(maybe(terms())) -> 'ok'.
-spec publish_status(maybe(terms()), ne_binary()) -> 'ok'.
publish_status(API) ->
    publish_status(API, ?DEFAULT_CONTENT_TYPE).

publish_status(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_STATUS_VALUES, fun status/1),
    FaxId = props:get_first_defined([<<"Fax-ID">>,<<"Job-ID">>], API,<<"*">>),
    amqp_util:basic_publish(?FAX_EXCHANGE, status_routing_key(FaxId), Payload, ContentType).

-spec publish_targeted_status(ne_binary(), maybe(terms())) -> 'ok'.
-spec publish_targeted_status(ne_binary(), maybe(terms()), ne_binary()) -> 'ok'.
publish_targeted_status(Q, JObj) ->
    publish_targeted_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

publish_targeted_status(Q, Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?FAX_STATUS_VALUES, fun status/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

fax_routing_key() ->
    <<"fax.req">>.

status_routing_key(FaxId) ->
    list_to_binary(["fax.status.", amqp_util:encode(FaxId)]).
