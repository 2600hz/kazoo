%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_tasks).

-export([create/3
        ,execute/2
        ,fetch/2, fetch_csv/3
        ,delete/2
        ,query/3
        ]).

-include("kazoo_proper.hrl").

-spec create(pqc_cb_api:state(), string(), iolist()) -> pqc_cb_api:response().
create(API, QueryString, CSV) ->
    TaskURL = tasks_url(QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"text/csv">>}]),

    pqc_cb_api:make_request([201, 404, 409]
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,CSV
                           ).

-spec execute(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
execute(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:patch/3
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ,<<>>
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_csv(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_csv(API, TaskId, CSV) ->
    Expectations = [#{'response_codes' => [200]
                     ,'response_headers' => [{"content-type", "text/csv"}]
                     }
                   ,#{'response_codes' => [204]}
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,task_csv_url(TaskId, CSV)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, <<"text/csv">>}])
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:delete/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec query(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
query(API, Category, Action) ->
    TaskURL = tasks_url(["category=", kz_term:to_list(Category)
                        ,"&action=", kz_term:to_list(Action)
                        ]
                       ),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,TaskURL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec task_csv_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
task_csv_url(TaskId, CSV) ->
    task_url(TaskId) ++ "?csv_name=" ++ kz_term:to_list(CSV).

-spec task_url(kz_term:ne_binary()) -> string().
task_url(TaskId) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks", kz_term:to_list(TaskId)], "/").

-spec tasks_url(iolist()) -> iolist().
tasks_url(QueryString) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks"], "/") ++ [$? | QueryString].
