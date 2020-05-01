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

-export([api_definitions/0, api_definition/1]).

-export([account_id/1
        ,job_id/1
        ,to_number/1
        ,control_queue/1
        ,state/1
        ]).

-export([req/1
        ,req_v/1
        ,publish_req/1
        ,publish_req/2
        ]).
-export([query_status/1
        ,query_status_v/1
        ,publish_query_status/2
        ,publish_query_status/3
        ]).
-export([status/1
        ,status_v/1
        ,publish_status/1
        ,publish_status/2
        ]).
-export([start_account/1
        ,start_account_v/1
        ,publish_start_account/1
        ,publish_start_account/2
        ]).
-export([start_job/1
        ,start_job_v/1
        ,publish_start_job/1
        ,publish_start_job/2
        ]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,publish_targeted_status/2, publish_targeted_status/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(FAX_EXCHANGE, <<"faxes">>).

-ifdef(TEST).
-export([fax_start_key/2
        ,status_routing_key/2
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,query_status_definition()
    ,status_definition()
    ,start_account_definition()
    ,start_job_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"req">>) ->
    req_definition();
api_definition(<<"query_status">>) ->
    query_status_definition();
api_definition(<<"status">>) ->
    status_definition();
api_definition(<<"start_account">>) ->
    start_account_definition();
api_definition(<<"start_job">>) ->
    start_job_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"fax_req">>,
    Category = <<"dialplan">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Dialplan Fax Request">>}
              ,{fun kapi_definition:set_description/2, <<"Dialplan Fax Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, <<"fax.req">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call">>
                                                            ,<<"Action">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Owner-ID">>
                                                            ,<<"FaxBox-ID">>
                                                            ,<<"Fax-T38-Option">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Action">>, [<<"receive">>, <<"transmit">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_status_definition() -> kapi_definition:api().
query_status_definition() ->
    EventName = <<"query_status">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fax Query Status">>}
              ,{fun kapi_definition:set_description/2, <<"Fax Query Status">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_status/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_status_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_status/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Job-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec status_definition() -> kapi_definition:api().
status_definition() ->
    EventName = <<"status">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fax Status">>}
              ,{fun kapi_definition:set_description/2, <<"Fax Status">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun status/1}
              ,{fun kapi_definition:set_validate_fun/2, fun status_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_status/1}
              ,{fun kapi_definition:set_binding/2, fun status_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Job-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Status">>
                                                            ,<<"FaxBox-ID">>
                                                            ,<<"Account-ID">>
                                                            ,<<"Fax-Info">>
                                                            ,<<"Cloud-Job-ID">>
                                                            ,<<"Cloud-Printer-ID">>
                                                            ,<<"Fax-State">>
                                                            ,<<"Direction">>
                                                            ,<<"Page">>
                                                            ,<<"Stage">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Callee-ID-Name">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Fax-State">>, [<<"start">>
                                   ,<<"prepare">>
                                   ,<<"send">>
                                   ,<<"receive">>
                                   ,<<"end">>
                                   ,<<"error">>
                                   ]}
                ,{<<"Direction">>, [<<"incoming">>
                                   ,<<"outgoing">>
                                   ]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec start_account_definition() -> kapi_definition:api().
start_account_definition() ->
    EventName = <<"account">>,
    Category = <<"start">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Start Account">>}
              ,{fun kapi_definition:set_description/2, <<"Start Account">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun start_account/1}
              ,{fun kapi_definition:set_validate_fun/2, fun start_account_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_start_account/1}
              ,{fun kapi_definition:set_binding/2, fun fax_start_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec start_job_definition() -> kapi_definition:api().
start_job_definition() ->
    EventName = <<"job">>,
    Category = <<"start">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Start Job">>}
              ,{fun kapi_definition:set_description/2, <<"Start Job">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun start_job/1}
              ,{fun kapi_definition:set_validate_fun/2, fun start_job_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_start_job/1}
              ,{fun kapi_definition:set_binding/2, fun fax_start_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Job-ID">>
                                                            ,<<"Account-ID">>
                                                            ,<<"To-Number">>
                                                            ,<<"Control-Queue">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Dialplan Fax Request
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) -> kz_api:api_formatter_return().
req(Req) ->
    kapi_definition:build_message(Req, req_definition()).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Req) ->
    kapi_definition:validate(Req, req_definition()).

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(API, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, kapi_definition:binding(Definition)).

%%------------------------------------------------------------------------------
%% @doc Fax Query Status
%% @end
%%------------------------------------------------------------------------------
-spec query_status(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_status(Req) ->
    kapi_definition:build_message(Req, query_status_definition()).

-spec query_status_v(kz_term:api_terms()) -> boolean().
query_status_v(Req) ->
    kapi_definition:validate(Req, query_status_definition()).

-spec publish_query_status(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_status(Q, JObj) ->
    publish_query_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_status(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_status(Q, API, ContentType) ->
    Definition = query_status_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Fax Status
%% @end
%%------------------------------------------------------------------------------
-spec status(kz_term:api_terms()) -> kz_api:api_formatter_return().
status(Req) ->
    kapi_definition:build_message(Req, status_definition()).

-spec status_v(kz_term:api_terms()) -> boolean().
status_v(Req) ->
    kapi_definition:validate(Req, status_definition()).

-spec publish_status(kz_term:api_terms()) -> 'ok'.
publish_status(API) ->
    publish_status(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_status(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_status(API, ContentType) ->
    Definition = status_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    FaxId = props:get_first_defined([<<"Fax-ID">>,<<"Job-ID">>], API,<<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, API, <<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE
                              ,(kapi_definition:binding(Definition))(AccountId, FaxId)
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Start Account
%% @end
%%------------------------------------------------------------------------------
-spec start_account(kz_term:api_terms()) -> kz_api:api_formatter_return().
start_account(Req) ->
    kapi_definition:build_message(Req, start_account_definition()).

-spec start_account_v(kz_term:api_terms()) -> boolean().
start_account_v(Req) ->
    kapi_definition:validate(Req, start_account_definition()).

-spec publish_start_account(kz_term:api_terms()) -> 'ok'.
publish_start_account(API) ->
    publish_start_account(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_account(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_start_account(API, ContentType) ->
    Definition = start_account_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    AccountId = props:get_value(<<"Account-ID">>, API, <<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE
                              ,(kapi_definition:binding(Definition))(<<"account">>, AccountId)
                              ,Payload
                              ,ContentType
                              ).

%%------------------------------------------------------------------------------
%% @doc Start Job
%% @end
%%------------------------------------------------------------------------------
-spec start_job(kz_term:api_terms()) -> kz_api:api_formatter_return().
start_job(Req) ->
    kapi_definition:build_message(Req, start_job_definition()).

-spec start_job_v(kz_term:api_terms()) -> boolean().
start_job_v(Req) ->
    kapi_definition:validate(Req, start_job_definition()).

-spec publish_start_job(kz_term:api_terms()) -> 'ok'.
publish_start_job(API) ->
    publish_start_job(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_start_job(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_start_job(API, ContentType) ->
    Definition = start_job_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    JobId = props:get_value(<<"Job-ID">>, API, <<"*">>),
    kz_amqp_util:basic_publish(?FAX_EXCHANGE
                              ,(kapi_definition:binding(Definition))(<<"job">>, JobId)
                              ,Payload
                              ,ContentType
                              ).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    FaxId = props:get_value('fax_id', Props, <<"*">>),
    StartId = props:get_value('start_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    bind_q(Queue, AccountId, FaxId, StartId, props:get_value('restrict_to', Props)).

-spec bind_q(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, AccountId, FaxId, StartId, 'undefined') ->
    kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(req_definition())),
    kz_amqp_util:bind_q_to_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(status_definition()))(AccountId, FaxId)
                                   ,?FAX_EXCHANGE
                                   );
bind_q(Queue, AccountId, FaxId, StartId, ['status'|Restrict]) ->
    kz_amqp_util:bind_q_to_exchange(Queue
                                   ,(kapi_definition:binding(status_definition()))(AccountId, FaxId)
                                   ,?FAX_EXCHANGE
                                   ),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(Queue, AccountId, FaxId, StartId, ['query_status'|Restrict]) ->
    kz_amqp_util:bind_q_to_targeted(Queue),
    bind_q(Queue, AccountId, FaxId, StartId, Restrict);
bind_q(Queue, AccountId, FaxId, StartId, ['req'|Restrict]) ->
    kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(req_definition())),
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
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(req_definition())),
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue, fax_start_key(StartId), ?FAX_EXCHANGE),
    kz_amqp_util:unbind_q_from_exchange(Queue
                                       ,(kapi_definition:binding(status_definition()))(AccountId, FaxId)
                                       ,?FAX_EXCHANGE
                                       );
unbind_q(Queue, AccountId,  FaxId, StartId, ['status'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_exchange(Queue
                                              ,(kapi_definition:binding(status_definition()))(AccountId, FaxId)
                                              ,?FAX_EXCHANGE
                                              ),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(Queue, AccountId, FaxId, StartId, ['query_status'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_targeted(Queue),
    unbind_q(Queue, AccountId, FaxId, StartId, Restrict);
unbind_q(Queue, AccountId, FaxId, StartId, ['req'|Restrict]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(req_definition())),
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

-spec publish_targeted_status(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_status(Q, JObj) ->
    publish_targeted_status(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_status(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_status(Q, API, ContentType) ->
    Definition = status_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

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
