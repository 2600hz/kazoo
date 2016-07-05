%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Handle client requests for phone_number documents
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(cb_tasks).

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_accepted/2
        ,content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,put/1
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_tasks/include/kazoo_tasks.hrl").

-define(SCHEMA_TASKS, <<"tasks">>).

-define(QS_CATEGORY, <<"category">>).
-define(QS_ACTION, <<"action">>).
-define(RD_RECORDS, <<"records">>).

-define(CSV_OUT, <<"output">>).
-define(CSV_IN, <<"input">>).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
init() ->
    {'ok', _} = application:ensure_all_started('kazoo_tasks'),

    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.tasks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.tasks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.tasks">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.tasks">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.tasks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.tasks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.tasks">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.tasks">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    case {cb_context:req_verb(Context)
         ,cb_context:req_nouns(Context)
         } of
        {?HTTP_GET, [{<<"tasks">>, []}]} -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    case {cb_context:req_verb(Context)
         ,cb_context:req_nouns(Context)
         } of
        {?HTTP_GET, [{<<"tasks">>, []}]} -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_TaskId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_TaskId, ?CSV_OUT) ->
    [?HTTP_GET];
allowed_methods(_TaskId, ?CSV_IN) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /tasks => []
%%    /tasks/task_id => [<<"task_id">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_TaskId) -> 'true'.
resource_exists(_TaskId, ?CSV_OUT) -> 'true';
resource_exists(_TaskId, ?CSV_IN) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, _TaskId) ->
    cta(Context, cb_context:req_verb(Context)).

-spec cta(cb_context:context(), http_method()) -> cb_context:context().
cta(Context, ?HTTP_PUT) ->
    CTA = [{'from_binary', ?CSV_CONTENT_TYPES ++ ?JSON_CONTENT_TYPES}],
    cb_context:add_content_types_accepted(Context, CTA);
cta(Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's Accept header)
%% Of the form {atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _TaskId, ?CSV_OUT) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            cb_context:add_content_types_provided(Context, [{'to_binary', ?CSV_CONTENT_TYPES}]);
        _ -> Context
    end;
content_types_provided(Context, _TaskId, ?CSV_IN) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            cb_context:add_content_types_provided(Context, [{'to_binary', ?CSV_CONTENT_TYPES}]);
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /tasks mights load a list of task objects
%% /tasks/123 might load the task object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_tasks(Context, cb_context:req_verb(Context)).
validate(Context, PathToken) ->
    validate_tasks(Context, PathToken, cb_context:req_verb(Context)).
validate(Context, PathToken1, PathToken2) ->
    validate_tasks(Context, PathToken1, PathToken2, cb_context:req_verb(Context)).

-spec validate_tasks(cb_context:context(), http_method()) -> cb_context:context().
validate_tasks(Context, ?HTTP_GET) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("starting discovery of task APIs"),
            JObj = kz_json:from_list([{<<"tasks">>, kz_tasks:help()}]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, JObj}
                                        ]);
        _AccountId ->
            summary(Context)
    end;
validate_tasks(Context, ?HTTP_PUT) ->
    QS = cb_context:query_string(Context),
    case {kz_json:get_ne_binary_value(?QS_CATEGORY, QS)
         ,kz_json:get_ne_binary_value(?QS_ACTION, QS)
         }
    of
        {?NE_BINARY, ?NE_BINARY} ->
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

-spec validate_tasks(cb_context:context(), path_token(), path_token(), http_method()) ->
                            cb_context:context().
validate_tasks(Context, TaskId, ?CSV_OUT, ?HTTP_GET) ->
    maybe_load_csv_attachment(Context, TaskId, ?KZ_TASKS_ATTACHMENT_NAME_OUT);
validate_tasks(Context, TaskId, ?CSV_IN, ?HTTP_GET) ->
    maybe_load_csv_attachment(Context, TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN).

-spec validate_new_attachment(cb_context:context(), boolean()) -> cb_context:context().
validate_new_attachment(Context, 'true') ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    CSVBinary = kz_json:get_value(<<"contents">>, FileJObj),
    case kz_csv:count_rows(CSVBinary) of
        0 ->
            Msg = kz_json:from_list([{<<"message">>, <<"Empty CSV or some row(s) longer than others or header missing">>}
                                    ]),
            cb_context:add_validation_error(<<"csv">>, <<"format">>, Msg, Context);
        TotalRows ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:store/3, 'total_rows', TotalRows}
                                        ])
    end;
validate_new_attachment(Context, 'false') ->
    Records = kz_json:get_value(?RD_RECORDS, cb_context:req_data(Context)),
    case kz_util:is_empty(Records) of
        'true' ->
            %% For tasks without input data.
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            Ctx = cb_context:validate_request_data(?SCHEMA_TASKS, Context),
            case cb_context:resp_status(Ctx) of
                'success' ->
                    cb_context:store(Ctx, 'total_rows', length(Records));
                _ -> Ctx
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    QS = cb_context:query_string(Context),
    Category = kz_json:get_value(?QS_CATEGORY, QS),
    Action   = kz_json:get_value(?QS_ACTION, QS),
    IsCSV = is_content_type_csv(Context),
    CSVorJSON = attached_data(Context, IsCSV),
    TotalRows = cb_context:fetch(Context, 'total_rows'),
    case kz_tasks:new(cb_context:account_id(Context), Category, Action, TotalRows, CSVorJSON) of
        {'ok', TaskJObj} ->
            TaskId = kz_json:get_value([<<"_read_only">>, <<"id">>], TaskJObj),
            save_attached_data(set_db(Context), TaskId, CSVorJSON, IsCSV),
            crossbar_util:response(TaskJObj, Context);
        {'error', 'no_categories'} ->
            no_categories(Context, Category);
        {'error', 'unknown_category'} ->
            crossbar_util:response_bad_identifier(Category, Context);
        {'error', 'unknown_action'} ->
            crossbar_util:response_bad_identifier(Action, Context);
        {'error', Reason} ->
            lager:debug("new ~s task ~s cannot be created: ~s"
                       ,[Category, Action, kz_json:encode(Reason)]),
            cb_context:add_validation_error(<<"attachment">>, <<"type">>, Reason, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, TaskId) ->
    case kz_tasks:start(TaskId) of
        {'ok', Task} -> crossbar_util:response(Task, Context);
        {'error', 'no_categories'} ->
            no_categories(Context, TaskId);
        {'error', 'already_started'} ->
            Msg = kz_json:from_list([{<<"reason">>, <<"task already started">>}
                                    ,{<<"cause">>, TaskId}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, TaskId) ->
    case kz_tasks:remove(TaskId) of
        {'ok', Task} -> crossbar_util:response(Task, Context);
        {'error', 'task_running'} ->
            Msg = kz_json:from_list([{<<"message">>, <<"task is running">>}
                                    ,{<<"cause">>, TaskId}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec set_db(cb_context:context()) -> cb_context:context().
set_db(Context) ->
    cb_context:set_account_db(Context, ?KZ_TASKS_DB).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(TaskId, Context) ->
    case kz_tasks:read(TaskId) of
        {'error', 'not_found'} ->
            crossbar_util:response_bad_identifier(TaskId, Context);
        {'ok', TaskJObj} ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, TaskJObj}
                                        ])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    AccountId = cb_context:account_id(Context),
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    crossbar_doc:load_view(?KZ_TASKS_BY_ACCOUNT
                          ,ViewOptions
                          ,set_db(Context)
                          ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%% @private
-spec req_content_type(cb_context:context()) -> ne_binary().
req_content_type(Context) ->
    cb_context:req_header(Context, <<"content-type">>).

%% @private
-spec replace_content_type(cb_context:context(), ne_binary()) -> cb_context:context().
replace_content_type(Context, CT) ->
    Key = <<"content-type">>,
    Without = lists:keydelete(Key, 1, cb_context:req_headers(Context)),
    cb_context:set_req_headers(Context, [{Key, CT} | Without]).

%% @private
-spec is_content_type_csv(cb_context:context()) -> boolean().
is_content_type_csv(Context) ->
    [Lhs, Rhs] = binary:split(req_content_type(Context), <<$/>>),
    lists:member({Lhs, Rhs}, ?CSV_CONTENT_TYPES).

%% @private
-spec attached_data(cb_context:context(), boolean()) -> kz_tasks:input().
attached_data(Context, 'true') ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    kz_json:get_value(<<"contents">>, FileJObj);
attached_data(Context, 'false') ->
    kz_json:get_value(?RD_RECORDS, cb_context:req_data(Context)).

%% @private
-spec save_attached_data(cb_context:context(), ne_binary(), kz_tasks:input(), boolean()) ->
                                cb_context:context().
save_attached_data(Context, TaskId, CSV, 'true') ->
    CT = req_content_type(Context),
    Options = [{'content_type', CT}],
    lager:debug("saving ~s attachment in task ~s", [?KZ_TASKS_ATTACHMENT_NAME_IN, TaskId]),
    crossbar_doc:save_attachment(TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN, CSV, Context, Options);
save_attached_data(Context, _TaskId, 'undefined', 'false') ->
    lager:debug("no attachment to save for task ~s", [_TaskId]),
    Context;
save_attached_data(Context, TaskId, Records, 'false') ->
    lager:debug("converting JSON to CSV before saving"),
    Fields = kz_json:get_keys(hd(Records)),
    lager:debug("CSV fields found: ~p", [Fields]),
    %% We assume fields for first record are defined in all other records.
    CSV = [kz_util:iolist_join(",", Fields), "\n"
          ,[ [kz_util:iolist_join(",", [kz_json:get_value(Field, Record) || Field <- Fields]), "\n"]
             || Record <- Records
           ]
          ],
    Context1 = replace_content_type(Context, <<"text/csv">>),
    save_attached_data(Context1, TaskId, iolist_to_binary(CSV), 'true').

%% @private
-spec maybe_load_csv_attachment(cb_context:context(), kz_tasks:task_id(), ne_binary()) ->
                                       cb_context:context().
maybe_load_csv_attachment(Context, TaskId, AName) ->
    Context1 = read(TaskId, Context),
    case cb_context:resp_status(Context1) =:= 'success' of
        'false' -> Context1;
        'true' ->
            lager:debug("trying to fetch attachment for task ~s", [TaskId]),
            load_csv_attachment(Context1, TaskId, AName)
    end.

%% @private
-spec load_csv_attachment(cb_context:context(), kz_tasks:task_id(), ne_binary()) ->
                                 cb_context:context().
load_csv_attachment(Context, TaskId, AName) ->
    RD = kz_json:get_value(<<"_read_only">>, cb_context:resp_data(Context)),
    Filename =
        <<(kz_json:get_value(?QS_CATEGORY, RD))/binary, "_",
          (kz_json:get_value(?QS_ACTION, RD))/binary, "_",
          TaskId/binary, ".csv"
        >>,
    Ctx = crossbar_doc:load_attachment(TaskId
                                      ,AName
                                      ,?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)
                                      ,set_db(Context)
                                      ),
    case cb_context:resp_status(Ctx) of
        'success' ->
            lager:debug("loaded csv ~s from task doc ~s", [AName, TaskId]),
            cb_context:add_resp_headers(
              Ctx
                                       ,[{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                                        ,{<<"Content-Type">>, <<"text/csv">>}
                                        ,{<<"Content-Length">>, byte_size(cb_context:resp_data(Ctx))}
                                        ]);
        _ ->
            lager:debug("no such csv ~s in task doc ~s", [AName, TaskId]),
            Ctx
    end.

%% @private
-spec no_categories(cb_context:context(), ne_binary()) -> cb_context:context().
no_categories(Context, Id) ->
    Msg = kz_json:from_list([{<<"tip">>, <<"No APIs known yet: please try again in a second.">>}
                            ,{<<"cause">>, Id}
                            ]),
    _ = kz_util:spawn(fun kz_tasks:help/0),
    cb_context:add_system_error('bad_identifier', Msg, Context).
