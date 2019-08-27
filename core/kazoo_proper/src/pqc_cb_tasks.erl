%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_tasks).

-export([create/2, create/3
        ,create_account/3, create_account/4
        ,execute/2, execute/3
        ,fetch/2, fetch/3
        ,fetch_csv/3, fetch_csv/4
        ,delete/2, delete/3
        ,query/3
        ]).

-include("kazoo_proper.hrl").

%%------------------------------------------------------------------------------
%% @doc Craete a noinput task
%% @end
%%------------------------------------------------------------------------------
-spec create(pqc_cb_api:state(), string()) -> pqc_cb_api:response().
create(API, QueryString) ->
    TaskURL = tasks_url(QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [201, 404, 409]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,<<>>
                           ).

%%------------------------------------------------------------------------------
%% @doc Craete an input task with CSV request body
%% @end
%%------------------------------------------------------------------------------
-spec create(pqc_cb_api:state(), string(), iolist()) -> pqc_cb_api:response().
create(API, QueryString, CSV) ->
    TaskURL = tasks_url(QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, "text/csv"}]),
    Expectations = [#expectation{response_codes = [201, 404, 409]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,CSV
                           ).

%%------------------------------------------------------------------------------
%% @doc Craete a noinput task for an account
%% @end
%%------------------------------------------------------------------------------
-spec create_account(pqc_cb_api:state(), kz_term:ne_binary(), string()) -> pqc_cb_api:response().
create_account(API, AccountId, QueryString) ->
    TaskURL = tasks_url(AccountId, QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [201, 404, 409]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,<<>>
                           ).

%%------------------------------------------------------------------------------
%% @doc Craete an input task with CSV request body for an account
%% @end
%%------------------------------------------------------------------------------
-spec create_account(pqc_cb_api:state(), kz_term:ne_binary(), string(), iolist()) -> pqc_cb_api:response().
create_account(API, AccountId, QueryString, CSV) ->
    TaskURL = tasks_url(AccountId, QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, "text/csv"}]),
    Expectations = [#expectation{response_codes = [201, 404, 409]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,CSV
                           ).

-spec execute(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
execute(API, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ,<<>>
                           ).

-spec execute(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
execute(API, AccountId, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,task_url(AccountId, TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ,<<>>
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,task_url(AccountId, TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_csv(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_csv(API, TaskId, CSV) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "text/csv"}]
                                }
                   ,#expectation{response_codes = [204]}
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,task_csv_url(TaskId, CSV)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, "text/csv"}])
                           ).

-spec fetch_csv(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_csv(API, AccountId, TaskId, CSV) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "text/csv"}]
                                }
                   ,#expectation{response_codes = [204]}
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,task_csv_url(AccountId, TaskId, CSV)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, "text/csv"}])
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, TaskId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,task_url(AccountId, TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec query(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
query(API, Category, Action) ->
    TaskURL = tasks_url(["category=", kz_term:to_list(Category)
                        ,"&action=", kz_term:to_list(Action)
                        ]
                       ),
    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,TaskURL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec task_csv_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
task_csv_url(TaskId, CSV) ->
    task_url(TaskId) ++ "?csv_name=" ++ kz_term:to_list(CSV).

-spec task_csv_url(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> string().
task_csv_url(AccountId, TaskId, CSV) ->
    task_url(AccountId, TaskId) ++ "?csv_name=" ++ kz_term:to_list(CSV).

-spec task_url(kz_term:ne_binary()) -> string().
task_url(TaskId) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks", kz_term:to_list(TaskId)], "/").

-spec task_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
task_url(AccountId, TaskId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "tasks", kz_term:to_list(TaskId)], "/").

-spec tasks_url(iolist()) -> iolist().
tasks_url(QueryString) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks"], "/") ++ [$? | QueryString].

-spec tasks_url(kz_term:ne_binary(), iolist()) -> iolist().
tasks_url(AccountId, QueryString) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "tasks"], "/") ++ [$? | QueryString].
