-module(pqc_cb_tasks).

-export([create/3
        ,execute/2
        ,fetch/2
        ,delete/2
        ]).

-include("kazoo_proper.hrl").

-spec create(pqc_cb_api:state(), api_ne_binary(), ne_binary()) -> pqc_cb_api:response().
create(API, QueryString, CSV) ->
    TaskURL = tasks_url(QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"text/csv">>}]),

    pqc_cb_api:make_request([201, 404, 409]
                           ,fun kz_http:put/3
                           ,TaskURL
                           ,RequestHeaders
                           ,CSV
                           ).

-spec execute(pqc_cb_api:state(), ne_binary()) -> pqc_cb_api:response().
execute(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:patch/3
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ,<<>>
                           ).

-spec fetch(pqc_cb_api:state(), ne_binary()) -> pqc_cb_api:response().
fetch(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec delete(pqc_cb_api:state(), ne_binary()) -> pqc_cb_api:response().
delete(API, TaskId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:delete/2
                           ,task_url(TaskId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec task_url(string()) -> string().

task_url(TaskId) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks", kz_term:to_list(TaskId)], "/").

-spec tasks_url(string()) -> string().
tasks_url(QueryString) ->
    string:join([pqc_cb_api:v2_base_url(), "tasks"], "/") ++ [$? | QueryString].
