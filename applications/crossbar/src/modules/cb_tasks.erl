%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_tasks).

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_accepted/1, content_types_accepted/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,put/1
        ,patch/2, patch/3
        ,delete/2

        ,to_csv/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(SCHEMA_TASKS, <<"tasks">>).

-define(QS_CATEGORY, <<"category">>).
-define(QS_ACTION, <<"action">>).
-define(RV_FILENAME, <<"file_name">>).

-define(PATH_STOP, <<"stop">>).
-define(PATH_OUTPUT, <<"output">>).
-define(PATH_INPUT, <<"input">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate.tasks">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.tasks">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.tasks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.tasks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.tasks">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.tasks">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.tasks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.tasks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.tasks">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.tasks">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.tasks">>, ?MODULE, 'to_csv'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    case {cb_context:req_verb(Context), cb_context:req_nouns(Context)} of
        {?HTTP_GET, [{<<"tasks">>, []}]} -> 'true';
        {?HTTP_PUT, [{<<"tasks">>, []}]} ->
            cb_context:is_superduper_admin(Context);
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    case {cb_context:req_verb(Context)
         ,cb_context:req_nouns(Context)
         } of
        {?HTTP_GET, [{<<"tasks">>, []}]} -> 'true';
        {?HTTP_PUT, [{<<"tasks">>, []}]} ->
            cb_context:is_superduper_admin(Context);
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_TaskId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_TaskId, ?PATH_STOP) ->
    [?HTTP_PATCH];
allowed_methods(_TaskId, ?PATH_INPUT) ->
    [?HTTP_GET];
allowed_methods(_TaskId, ?PATH_OUTPUT) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /tasks => []
%%    /tasks/task_id => [<<"task_id">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_TaskId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_TaskId, ?PATH_STOP) -> 'true';
resource_exists(_TaskId, ?PATH_INPUT) -> 'true';
resource_exists(_TaskId, ?PATH_OUTPUT) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be requiring (matched to the client's
%% Content-Type header.
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    cta(Context, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, _TaskId) ->
    cta(Context, cb_context:req_verb(Context)).

-spec cta(cb_context:context(), http_method()) -> cb_context:context().
cta(Context, ?HTTP_PUT) ->
    CTA = [{'from_binary', ?CSV_CONTENT_TYPES ++ ?JSON_CONTENT_TYPES}],
    cb_context:add_content_types_accepted(Context, CTA);
cta(Context, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's Accept header).
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------

-spec content_types_provided(cb_context:context()) ->
          cb_context:context().
content_types_provided(Context) ->
    ctp(Context).

-spec content_types_provided(cb_context:context(), path_token()) ->
          cb_context:context().
content_types_provided(Context, _TaskId) ->
    ctp(Context).

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, _TaskId, _CSV) ->
    cb_context:add_content_types_provided(Context, [{'to_csv', ?CSV_CONTENT_TYPES}]).

-spec ctp(cb_context:context()) -> cb_context:context().
ctp(Context) ->
    cb_context:add_content_types_provided(Context, [{'to_json', ?JSON_CONTENT_TYPES}
                                                   ,{'to_csv', ?CSV_CONTENT_TYPES}
                                                   ]).

-spec to_csv({cowboy_req:req(), cb_context:context()}) ->
          {cowboy_req:req(), cb_context:context()}.
to_csv({Req, Context}) ->
    Filename = download_filename(Context, requested_attachment_name(Context)),
    lager:debug("download named ~s", [Filename]),

    Headers0 = cowboy_req:resp_headers(Req),
    Headers = maps:merge(#{<<"content-type">> => <<"text/csv">>
                          ,<<"content-disposition">> => <<"attachment; filename=\"", Filename/binary, "\"">>
                          }
                        ,Headers0
                        ),
    {Req, cb_context:set_resp_headers(Context, Headers)}.

-spec download_filename(cb_context:context(), kz_term:ne_binary()) -> kz_term:ne_binary().
download_filename(Context, ?KZ_TASKS_ANAME_OUT) ->
    TaskJObj = cb_context:doc(Context),

    Category = kzd_task:category(TaskJObj),
    Action = kzd_task:action(TaskJObj),
    TaskId = kz_doc:id(TaskJObj),

    list_to_binary([Category, "_"
                   ,Action, "_"
                   ,TaskId, "_out.csv"
                   ]);
download_filename(Context, ?KZ_TASKS_ANAME_IN) ->
    TaskJObj = cb_context:doc(Context),

    Category = kzd_task:category(TaskJObj),
    Action = kzd_task:action(TaskJObj),
    TaskId = kz_doc:id(TaskJObj),

    list_to_binary([Category, "_"
                   ,Action, "_"
                   ,TaskId, "_in.csv"
                   ]);
download_filename(_Context, Name) ->
    Name.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /tasks might load a list of task objects
%% /tasks/123 might load the task object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_tasks(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_tasks(Context, PathToken, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, TaskId, ?PATH_STOP) ->
    validate_tasks(Context, TaskId, cb_context:req_verb(Context));
validate(Context, TaskId, CSV) ->
    CSVFile = csv_path_to_file(CSV),
    QS = cb_context:query_string(Context),
    Values = [{<<"csv_name">>, CSVFile}
             ,{<<"accept">>, <<"text/csv">>}
             ],
    AdjustedQS = kz_json:set_values(Values, QS),
    AdjustedContext = cb_context:set_query_string(Context, AdjustedQS),
    validate(AdjustedContext, TaskId).

-spec validate_tasks(cb_context:context(), http_method()) -> cb_context:context().
validate_tasks(Context, ?HTTP_GET) ->
    case cb_context:account_id(Context) of
        'undefined' -> help(Context);
        _AccountId -> summary(Context)
    end;
validate_tasks(Context, ?HTTP_PUT) ->
    QS = cb_context:query_string(Context),
    case {kz_json:get_ne_binary_value(?QS_CATEGORY, QS)
         ,kz_json:get_ne_binary_value(?QS_ACTION, QS)
         }
    of
        {?NE_BINARY=_Cat, ?NE_BINARY=_Act} ->
            lager:debug("validating doing '~s' on cat '~s'", [_Cat, _Act]),
            validate_new_attachment(Context, is_content_type_csv(Context));
        {_, _} -> cb_context:add_system_error('invalid request', Context)
    end.

-spec validate_tasks(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_tasks(Context, TaskId, ?HTTP_GET) ->
    read(TaskId, Context);
validate_tasks(Context, TaskId, ?HTTP_PATCH) ->
    read(TaskId, Context);
validate_tasks(Context, TaskId, ?HTTP_DELETE) ->
    read(TaskId, Context).

-spec validate_new_attachment(cb_context:context(), boolean()) -> cb_context:context().
validate_new_attachment(Context, 'true') ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    CSVBinary = kz_json:get_value(<<"contents">>, FileJObj),
    try kz_csv:count_rows(CSVBinary) of
        0 ->
            lager:debug("failed to count rows in the CSV"),
            Msg = kz_json:from_list([{<<"message">>, <<"Empty CSV or some row(s) longer than others or header missing">>}
                                    ]),
            cb_context:add_validation_error(<<"csv">>, <<"format">>, Msg, Context);
        TotalRows ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:store/3, 'total_rows', TotalRows}
                                        ])
    catch
        _E:_R ->
            lager:error("malformed CSV: ~s: ~p", [_E, _R]),
            Msg = kz_json:from_list([{<<"message">>, <<"Malformed CSV - unable to process the file due to errors in the CSV">>}]),
            cb_context:add_validation_error(<<"csv">>, <<"format">>, Msg, Context)
    end;
validate_new_attachment(Context, 'false') ->
    Records = kzd_tasks:records(cb_context:req_data(Context)),
    case kz_term:is_empty(Records) of
        'true' ->
            %% For tasks without input data.
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            Ctx = cb_context:validate_request_data(?SCHEMA_TASKS, Context),
            case cb_context:resp_status(Ctx) of
                'success' ->
                    lager:debug("records validated"),
                    cb_context:store(Ctx, 'total_rows', length(Records));
                _ -> Ctx
            end
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    QS = cb_context:query_string(Context),
    Category = kz_json:get_value(?QS_CATEGORY, QS),
    Action   = kz_json:get_value(?QS_ACTION, QS),
    IsCSV = is_content_type_csv(Context),
    CSVorJSON = attached_data(Context, IsCSV),
    case kz_tasks:new(cb_context:auth_account_id(Context)
                     ,task_account_id(Context)
                     ,Category
                     ,Action
                     ,cb_context:fetch(Context, total_rows)
                     ,CSVorJSON
                     ,cb_context:req_value(Context, ?RV_FILENAME)
                     )
    of
        {'ok', TaskJObj} ->
            TaskId = kz_json:get_value([<<"_read_only">>, <<"id">>], TaskJObj),
            _ = save_attached_data(set_db(Context), TaskId, CSVorJSON, IsCSV),
            crossbar_util:response(TaskJObj, Context);
        {'error', 'unknown_category_action'=Reason} ->
            crossbar_util:response_bad_identifier(Reason, Context);
        {'error', Reason} ->
            lager:debug("new ~s task ~s cannot be created: ~p", [Category, Action, Reason]),
            crossbar_util:response_400(<<"bad request">>, Reason, Context)
    end.

-spec task_account_id(cb_context:context()) -> kz_term:api_ne_binary().
task_account_id(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> cb_context:auth_account_id(Context);
        AccountId -> AccountId
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, TaskId) ->
    Req = [{<<"Task-ID">>, TaskId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    {'ok', Resp} =
        kz_amqp_worker:call(Req
                           ,fun kapi_tasks:publish_start_req/1
                           ,fun kapi_tasks:start_resp_v/1
                           ),
    case kz_json:get_value(<<"Reply">>, Resp) of
        <<"already_started">> ->
            Msg = kz_json:from_list([{<<"reason">>, <<"task already started">>}
                                    ,{<<"cause">>, TaskId}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        Task ->
            crossbar_util:response(Task, Context)
    end.

-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context, TaskId, ?PATH_STOP) ->
    Req = [{<<"Task-ID">>, TaskId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    {ok, Resp} = kz_amqp_worker:call(Req
                                    ,fun kapi_tasks:publish_stop_req/1
                                    ,fun kapi_tasks:stop_resp_v/1
                                    ),
    case kapi_tasks:reply(Resp) =:= <<"not_running">> of
        false -> crossbar_util:response(kapi_tasks:reply(Resp), Context);
        true ->
            Msg = kz_json:from_list(
                    [{<<"reason">>, <<"task is not running">>}
                    ,{<<"cause">>, TaskId}
                    ]),
            cb_context:add_system_error(bad_identifier, Msg, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, TaskId) ->
    Req = [{<<"Task-ID">>, TaskId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    {'ok', Resp} =
        kz_amqp_worker:call(Req
                           ,fun kapi_tasks:publish_remove_req/1
                           ,fun kapi_tasks:remove_resp_v/1
                           ),
    case kz_json:get_value(<<"Reply">>, Resp) of
        <<"task_running">> ->
            Msg = kz_json:from_list([{<<"message">>, <<"task is running">>}
                                    ,{<<"cause">>, TaskId}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        Task ->
            crossbar_util:response(Task, Context)
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_db(cb_context:context()) -> cb_context:context().
set_db(Context) ->
    cb_context:set_db_name(Context, ?KZ_TASKS_DB).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------

-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(TaskId, Context) ->
    AccountId = cb_context:account_id(Context),
    read(TaskId, set_db(Context), AccountId).

-spec read(kz_term:ne_binary(), cb_context:context(), kz_term:api_binary()) -> cb_context:context().
read(TaskId, Context, 'undefined') ->
    AuthAccountId = cb_context:auth_account_id(Context),
    read(TaskId, Context, AuthAccountId);
read(TaskId, Context, AccountId) ->
    AcceptValue = accept_value(Context),
    read(TaskId, Context, AccountId, AcceptValue).

-spec accept_value(cb_context:context()) -> kz_term:api_ne_binary().
accept_value(Context) ->
    accept_value(cb_context:req_header(Context, <<"accept">>)
                ,cb_context:req_value(Context, <<"accept">>)
                ).

-spec accept_value(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
accept_value('undefined', 'undefined') -> ?DEFAULT_CONTENT_TYPE;
accept_value(Header, 'undefined') -> Header;
accept_value(_Header, <<"csv">>) -> <<"text/csv">>;
accept_value(_Header, Tunneled) -> Tunneled.

-spec read(kz_term:ne_binary(), cb_context:context(), kz_term:api_binary(), kz_term:api_binary()) -> cb_context:context().
read(TaskId, Context, AccountId, Accept) ->
    lager:debug("accept value: ~p", [Accept]),
    read_doc_or_attachment(TaskId, Context, AccountId, cb_modules_util:parse_media_type(Accept)).

-type parsed_accept_values() :: {'error', 'badarg'} | media_values().

-spec read_doc_or_attachment(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), parsed_accept_values()) ->
          cb_context:context().
read_doc_or_attachment(TaskId, Context, AccountId, {'error', 'badarg'}) ->
    lager:info("failed to parse the accept header"),
    read_doc(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)|_]) ->
    read_doc(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)|_]) ->
    read_doc(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"*">>, <<"*">>, _, _, _)|_]) ->
    read_doc(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"text">>, <<"csv">>, _, _, _)|_]) ->
    read_attachment(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"text">>, <<"comma-separated-values">>, _, _, _)|_]) ->
    read_attachment(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [?MEDIA_VALUE(<<"application">>, <<"octet-stream">>, _, _, _)|_]) ->
    read_attachment(TaskId, Context, AccountId);
read_doc_or_attachment(TaskId, Context, AccountId, [_Accept|Accepts]) ->
    lager:debug("failed to handle accept value ~p", [_Accept]),
    read_doc_or_attachment(TaskId, Context, AccountId, Accepts);
read_doc_or_attachment(TaskId, Context, AccountId, []) ->
    lager:info("failed to find valid accept value"),
    read_doc(TaskId, Context, AccountId).

-spec read_doc(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
read_doc(TaskId, Context, AccountId) ->
    Options = [{'key', [AccountId, TaskId]}
              ,{'databases', [?KZ_TASKS_DB]}
              ,{'mapper', crossbar_view:get_value_fun()}
              ],
    handle_read_result(TaskId, Context, crossbar_view:load(Context, ?KZ_TASKS_BY_ACCOUNT, Options)).

handle_read_result(TaskId, OrigContext, ReadContext) ->
    case cb_context:resp_data(ReadContext) of
        [] -> crossbar_util:response_bad_identifier(TaskId, OrigContext);
        [TaskJObj] ->
            JObj = kz_json:set_value(<<"_read_only">>, TaskJObj, kz_json:new()),
            lager:debug("task fetched: ~s", [kz_json:encode(TaskId)]),
            cb_context:setters(ReadContext
                              ,[{fun cb_context:set_doc/2, JObj}
                               ,{fun cb_context:set_resp_data/2, JObj}
                               ]
                              )
    end.

read_attachment(TaskId, Context, AccountId) ->
    ReadContext = read_doc(TaskId, Context, AccountId),
    case cb_context:resp_status(ReadContext) of
        'success' ->
            lager:debug("reading attachment for ~s", [TaskId]),
            read_attachment_file(TaskId
                                ,Context
                                ,requested_attachment_name(Context)
                                );
        _Status ->
            lager:debug("reading ~s failed: ~p", [TaskId, _Status]),
            ReadContext
    end.

-spec requested_attachment_name(cb_context:context()) -> kz_term:ne_binary().
requested_attachment_name(Context) ->
    cb_context:req_value(Context, <<"csv_name">>, ?KZ_TASKS_ANAME_OUT).

-spec csv_path_to_file(kz_term:ne_binary()) -> kz_term:ne_binary().
csv_path_to_file(?PATH_INPUT) ->
    ?KZ_TASKS_ANAME_IN;
csv_path_to_file(?PATH_OUTPUT) ->
    ?KZ_TASKS_ANAME_OUT.

-spec read_attachment_file(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
read_attachment_file(TaskId, Context, AttachmentName) ->
    Type = ?TYPE_CHECK_OPTION(kzd_task:type()),
    crossbar_doc:load_attachment(TaskId, AttachmentName, Type, Context).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    AccountId = cb_context:account_id(Context),
    ViewOptions = [{'startkey', [AccountId, kz_time:now_s(), kz_json:new()]}
                  ,{'endkey', [AccountId]}
                  ,{'databases', [?KZ_TASKS_DB]}
                  ,{'mapper', crossbar_view:get_value_fun()}
                  ,'descending'
                  ],
    crossbar_view:load(Context, ?KZ_TASKS_BY_CREATED, ViewOptions).

-spec req_content_type(cb_context:context()) -> kz_term:ne_binary().
req_content_type(Context) ->
    cb_context:req_header(Context, <<"content-type">>).

-spec is_content_type_csv(cb_context:context()) -> boolean().
is_content_type_csv(Context) ->
    api_util:content_type_matches(req_content_type(Context), ?CSV_CONTENT_TYPES).

-spec attached_data(cb_context:context(), boolean()) -> kz_tasks:input().
attached_data(Context, 'true') ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    kz_json:get_value(<<"contents">>, FileJObj);
attached_data(Context, 'false') ->
    kzd_tasks:records(cb_context:req_data(Context)).

-spec save_attached_data(cb_context:context(), kz_term:ne_binary(), kz_tasks:input(), boolean()) ->
          cb_context:context().
save_attached_data(Context, TaskId, CSV, 'true') ->
    CT = req_content_type(Context),
    Options = [{'content_type', CT}],
    lager:debug("saving ~s attachment in task ~s", [?KZ_TASKS_ANAME_IN, TaskId]),
    crossbar_doc:save_attachment(TaskId, ?KZ_TASKS_ANAME_IN, CSV, Context, Options);
save_attached_data(Context, _TaskId, 'undefined', 'false') ->
    lager:debug("no attachment to save for task ~s", [_TaskId]),
    Context;
save_attached_data(Context, TaskId, Records, 'false') ->
    lager:debug("converting json to csv before saving"),
    lager:debug("csv fields found: ~p", [kz_json:get_keys(hd(Records))]),
    CSV = kz_csv:json_to_iolist(Records),
    lager:debug("saving ~s attachment in task ~s", [?KZ_TASKS_ANAME_IN, TaskId]),
    Options = [{'content_type', <<"text/csv">>}],
    crossbar_doc:save_attachment(TaskId, ?KZ_TASKS_ANAME_IN, CSV, Context, Options).

-spec help(cb_context:context()) -> cb_context:context().
help(Context) ->
    Category = cb_context:req_param(Context, ?QS_CATEGORY),
    Action   = cb_context:req_param(Context, ?QS_ACTION),
    lager:debug("looking for tasks matching category:~s action:~s", [Category, Action]),
    Req = props:filter_undefined(
            [{<<"Category">>, Category}
            ,{<<"Action">>, Action}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_tasks:publish_lookup_req/1
                            ,fun kapi_tasks:lookup_resp_v/1
                            )
    of
        {'ok', JObj} -> help_rep(Context, kz_json:get_value(<<"Help">>, JObj));
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            cb_context:add_system_error('invalid request', Context);
        {'error', _R} ->
            lager:debug("error: ~p", [_R]),
            cb_context:add_system_error('invalid request', Context)
    end.

-spec help_rep(cb_context:context(), kz_json:object()) -> cb_context:context().
help_rep(Context, JObj) ->
    case kz_json:is_empty(JObj) of
        'true' ->
            crossbar_util:response_bad_identifier(<<"unknown category or action">>, Context);
        'false' ->
            Help = kz_json:from_list([{<<"tasks">>, JObj}]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, Help}
                                        ])
    end.
