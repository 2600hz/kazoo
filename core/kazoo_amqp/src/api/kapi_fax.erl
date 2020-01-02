%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_fax).

-export([account_id/1
        ,job_id/1
        ,to_number/1
        ,control_queue/1
        ,state/1
        ]).

-export([req/1, req_v/1
        ,query_status/1, query_status_v/1
        ,status/1, status_v/1
        ,start_account/1, start_account_v/1
        ,start_job/1, start_job_v/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_req/1, publish_req/2
        ,publish_query_status/2, publish_query_status/3
        ,publish_status/1, publish_status/2
        ,publish_targeted_status/2, publish_targeted_status/3
        ,publish_start_account/1, publish_start_account/2
        ,publish_start_job/1, publish_start_job/2
        ]).

-include_lib("kz_amqp_util.hrl").

-define(FAX_START, <<"start">>).
-define(FAX_ACQUIRE, <<"acquire">>).
-define(FAX_PREPARE, <<"prepare">>).
-define(FAX_ORIGINATE, <<"originate">>).
-define(FAX_NEGOTIATE, <<"negotiate">>).
-define(FAX_SEND, <<"send">>).
-define(FAX_RECEIVE, <<"receive">>).
-define(FAX_END, <<"end">>).
-define(FAX_ERROR, <<"error">>).

-define(FAX_STATE_LIST, [?FAX_START, ?FAX_PREPARE, ?FAX_SEND, ?FAX_RECEIVE, ?FAX_END, ?FAX_ERROR]).

-define(FAX_OUTGOING, <<"outgoing">>).
-define(FAX_INCOMING, <<"incoming">>).

-define(FAX_EXCHANGE, <<"faxes">>).

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
                                     ,<<"Stage">>
                                     ,<<"Caller-ID-Number">>, <<"Caller-ID-Name">>
                                     ,<<"Callee-ID-Number">>, <<"Callee-ID-Name">>
                                     ]).
-define(FAX_STATUS_VALUES, [{<<"Event-Category">>,<<"fax">>}
                           ,{<<"Event-Name">>, <<"status">>}
                           ,{<<"Fax-State">>, ?FAX_STATE_LIST}
                           ,{<<"Direction">>, [?FAX_INCOMING, ?FAX_OUTGOING]}
                           ]).
-define(FAX_STATUS_TYPES, []).

-define(FAX_START_ACCOUNT_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_FAX_START_ACCOUNT_HEADERS, []).
-define(FAX_START_ACCOUNT_VALUES, [{<<"Event-Category">>,<<"start">>}
                                  ,{<<"Event-Name">>, <<"account">>}
                                  ]).
-define(FAX_START_ACCOUNT_TYPES, []).

-define(FAX_START_JOB_HEADERS, [<<"Job-ID">>, <<"Account-ID">>, <<"To-Number">>, <<"Control-Queue">>]).
-define(OPTIONAL_FAX_START_JOB_HEADERS, []).
-define(FAX_START_JOB_VALUES, [{<<"Event-Category">>,<<"start">>}
                              ,{<<"Event-Name">>, <<"job">>}
                              ]).
-define(FAX_START_JOB_TYPES, []).

-spec req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_req"};
        'true' -> kz_api:build_message(Prop, ?FAX_REQ_HEADERS, ?OPTIONAL_FAX_REQ_HEADERS)
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_REQ_HEADERS, ?FAX_REQ_VALUES, ?FAX_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

-spec query_status(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_status(Prop) when is_list(Prop) ->
    case query_status_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query_status"};
        'true' -> kz_api:build_message(Prop, ?FAX_QUERY_HEADERS, ?OPTIONAL_FAX_QUERY_HEADERS)
    end;
query_status(JObj) ->
    query_status(kz_json:to_proplist(JObj)).

-spec query_status_v(kz_term:api_terms()) -> boolean().
query_status_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_QUERY_HEADERS, ?FAX_QUERY_VALUES, ?FAX_QUERY_TYPES);
query_status_v(JObj) ->
    query_status_v(kz_json:to_proplist(JObj)).

-spec status(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
status(Prop) when is_list(Prop) ->
    case status_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_query_status"};
        'true' -> kz_api:build_message(Prop, ?FAX_STATUS_HEADERS, ?OPTIONAL_FAX_STATUS_HEADERS)
    end;
status(JObj) ->
    status(kz_json:to_proplist(JObj)).

-spec status_v(kz_term:api_terms()) -> boolean().
status_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_STATUS_HEADERS, ?FAX_STATUS_VALUES, ?FAX_STATUS_TYPES);
status_v(JObj) ->
    status_v(kz_json:to_proplist(JObj)).

-spec start_account(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
start_account(Prop) when is_list(Prop) ->
    case start_account_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_start account"};
        'true' -> kz_api:build_message(Prop, ?FAX_START_ACCOUNT_HEADERS, ?OPTIONAL_FAX_START_ACCOUNT_HEADERS)
    end;
start_account(JObj) ->
    start_account(kz_json:to_proplist(JObj)).

-spec start_account_v(kz_term:api_terms()) -> boolean().
start_account_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_START_ACCOUNT_HEADERS, ?FAX_START_ACCOUNT_VALUES, ?FAX_START_ACCOUNT_TYPES);
start_account_v(JObj) ->
    start_account_v(kz_json:to_proplist(JObj)).

-spec start_job(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
start_job(Prop) when is_list(Prop) ->
    case start_job_v(Prop) of
        'false' -> {'error', "Proplist failed validation for fax_start account"};
        'true' -> kz_api:build_message(Prop, ?FAX_START_JOB_HEADERS, ?OPTIONAL_FAX_START_JOB_HEADERS)
    end;
start_job(JObj) ->
    start_job(kz_json:to_proplist(JObj)).

-spec start_job_v(kz_term:api_terms()) -> boolean().
start_job_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_START_JOB_HEADERS, ?FAX_START_JOB_VALUES, ?FAX_START_JOB_TYPES);
start_job_v(JObj) ->
    start_job_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    StartId = props:get_value('start_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    bind_q(Queue, AccountId, FaxId, StartId, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, AccountId, FaxId, StartId, 'undefined') ->
    kz_amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    kz_amqp_util:bind_q_to_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    kz_amqp_util:bind_q_to_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE);
bind_q(Queue, AccountId, FaxId, StartId, ['status'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(Queue, AccountId, FaxId, StartId, ['query_status'|Restrict]) ->
    kz_amqp_util:bind_q_to_targeted(Queue),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(Queue, AccountId, FaxId, StartId, ['req'|Restrict]) ->
    kz_amqp_util:bind_q_to_callmgr(Queue, fax_routing_key()),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(Queue, AccountId, FaxId, StartId, ['start'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(_, _, _, _, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    StartId = props:get_value('start_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    unbind_q(Queue, AccountId, FaxId, StartId, props:get_value('restrict_to', Props)).

-spec unbind_q(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, AccountId, FaxId, StartId, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    kz_amqp_util:unbind_q_from_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE);
unbind_q(Queue, AccountId,  FaxId, StartId, ['status'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, status_routing_key(AccountId, FaxId), ?FAX_EXCHANGE),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(Queue, AccountId, FaxId, StartId, ['query_status'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_targeted(Queue),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(Queue, AccountId, FaxId, StartId, ['req'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, fax_routing_key()),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(Queue, AccountId, FaxId, StartId, ['start'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(_, _, _, _, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?FAX_EXCHANGE, <<"topic">>),
    kz_amqp_util:targeted_exchange(),
    kz_amqp_util:callmgr_exchange().

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?FAX_REQ_VALUES, fun req/1),
    kz_amqp_util:callmgr_publish(Payload, ContentType, fax_routing_key()).

-spec publish_query_status(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_status(Q, JObj) ->
    publish_query_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_status(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_status(Q, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_QUERY_VALUES, fun query_status/1),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_status(kz_term:api_terms()) -> 'ok'.
publish_status(API) ->
    publish_status(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_status(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_status(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_STATUS_VALUES, fun status/1),
    FaxId = props:get_first_defined([<<"Fax-ID">>,<<"Job-ID">>], API,<<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, API, <<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE, status_routing_key(AccountId, FaxId), Payload, ContentType).

-spec publish_targeted_status(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_status(Q, JObj) ->
    publish_targeted_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_status(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_status(Q, Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?FAX_STATUS_VALUES, fun status/1),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_start_account(kz_term:api_terms()) -> 'ok'.
publish_start_account(API) ->
    publish_start_account(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_account(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_start_account(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_START_ACCOUNT_VALUES, fun start_account/1),
    AccountId = props:get_value(<<"Account-ID">>, API,<<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE, fax_start_key(<<"account">>, AccountId), Payload, ContentType).

-spec publish_start_job(kz_term:api_terms()) -> 'ok'.
publish_start_job(API) ->
    publish_start_job(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_job(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_start_job(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_START_JOB_VALUES, fun start_job/1),
    JobId = props:get_value(<<"Job-ID">>, API,<<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE, fax_start_key(<<"job">>, JobId), Payload, ContentType).

fax_routing_key() ->
    <<"fax.req">>.

fax_start_key(Id) ->
    fax_start_key(<<"*">>, Id).

fax_start_key(Type, Id) ->
    list_to_binary(["fax.start.", Type, ".", kz_amqp_util:encode(Id)]).

status_routing_key(AccountId, FaxId) ->
    list_to_binary(["fax.status.", AccountId, ".", kz_amqp_util:encode(FaxId)]).

-spec account_id(kz_json:object()) -> kz_term:ne_binary().
account_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Account-ID">>, JObj).

-spec control_queue(kz_json:object()) -> kz_term:ne_binary().
control_queue(JObj) ->
    case kz_amqp_util:split_routing_key(kz_api:server_id(JObj)) of
        {'undefined', _} -> kz_json:get_ne_binary_value(<<"Control-Queue">>, JObj);
        {Pid, _} -> list_to_binary(["consumer://"
                                   ,kz_term:to_binary(Pid)
                                   ,"/"
                                   ,kz_json:get_ne_binary_value(<<"Control-Queue">>, JObj)
                                   ])
    end.

-spec job_id(kz_json:object()) -> kz_term:ne_binary().
job_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Job-ID">>, JObj).

-spec to_number(kz_json:object()) -> kz_term:ne_binary().
to_number(JObj) ->
    kz_json:get_ne_binary_value(<<"To-Number">>, JObj).

-spec state(kz_json:object()) -> kz_term:ne_binary().
state(JObj) ->
    kz_json:get_ne_binary_value(<<"Fax-State">>, JObj).
