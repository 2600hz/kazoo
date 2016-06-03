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
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_accepted/2
        ,content_types_provided/2
        ,validate/1, validate/2
        ,put/1
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_tasks/include/kazoo_tasks.hrl").

-define(SCHEMA_RECORDS, <<"tasks.records">>).

-define(QS_CATEGORY, <<"category">>).
-define(QS_ACTION, <<"action">>).
-define(RD_RECORDS, <<"records">>).

-define(HELP, <<"help">>).


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
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?HELP) ->
    [?HTTP_GET];
allowed_methods(_TaskId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].

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
resource_exists() -> 'true'.
resource_exists(?HELP) -> 'true';
resource_exists(_TaskId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, ?HELP) ->
    Context;
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
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?HELP) ->
    Context;
content_types_provided(Context, _TaskId) ->
    ctp(Context, cb_context:req_verb(Context)).

-spec ctp(cb_context:context(), http_method()) -> cb_context:context().
-spec ctp(cb_context:context()) -> cb_context:context().
ctp(Context, ?HTTP_GET) ->
    ctp(Context);
ctp(Context, _) ->
    Context.
ctp(Context) ->
    CTP = [{'to_json', ?JSON_CONTENT_TYPES}
          ,{'to_binary', ?CSV_CONTENT_TYPES}
          ],
    cb_context:add_content_types_provided(Context, CTP).

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
validate(Context) ->
    validate_tasks(Context, cb_context:req_verb(Context)).
validate(Context, PathToken) ->
    validate_task(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_tasks(cb_context:context(), http_method()) -> cb_context:context().
validate_tasks(Context, ?HTTP_GET) ->
    summary(Context);
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

-spec validate_task(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_task(Context, ?HELP, ?HTTP_GET) ->
    JObj = kz_json:from_list([{<<"tasks">>, kz_tasks:available()}]),
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, JObj}
                                ]);
validate_task(Context, TaskId, ?HTTP_GET) ->
    Context1 = read(TaskId, Context),
    case cb_context:resp_status(Context1) == 'success'
        andalso is_accept_csv(Context)
    of
        'false' -> Context1;
        'true' ->
            lager:debug("trying to fetch attachment for task ~s", [TaskId]),
            case kz_tasks:attachment_name(TaskId) of
                'undefined' -> crossbar_util:response_bad_identifier(TaskId, Context);
                Name -> load_csv_attachment(Context, TaskId, Name)
            end
    end;
validate_task(Context, TaskId, ?HTTP_PATCH) ->
    read(TaskId, Context);
validate_task(Context, TaskId, ?HTTP_DELETE) ->
    read(TaskId, Context).

-spec validate_new_attachment(cb_context:context(), boolean()) -> cb_context:context().
validate_new_attachment(Context, 'true') ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    CSVBinary = kz_json:get_value(<<"contents">>, FileJObj),
    case kz_tasks:is_csv(CSVBinary) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"Empty CSV or some row(s) longer than others">>}
                                    ]),
            cb_context:add_validation_error(<<"csv">>, <<"format">>, Msg, Context)
    end;
validate_new_attachment(Context, 'false') ->
    cb_context:validate_request_data(?SCHEMA_RECORDS, Context).

%% -spec validate_attachment(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
%% validate_attachment(Context, TaskId, AttachmentId, ?HTTP_GET) ->
%%     load_attachment(Context, TaskId, AttachmentId);
%% validate_attachment(Context, TaskId, AttachmentId, ?HTTP_POST) ->
%%     is_mutable(load_attachment(Context, TaskId, AttachmentId));
%% validate_attachment(Context, TaskId, AttachmentId, ?HTTP_DELETE) ->
%%     is_mutable(load_attachment(Context, TaskId, AttachmentId)).

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
    case kz_tasks:new(cb_context:account_id(Context), Category, Action, CSVorJSON) of
        {'ok', TaskJObj} ->
            TaskId = kz_json:get_value([<<"_read_only">>, <<"id">>], TaskJObj),
            save_attached_data(set_db(Context), TaskId, CSVorJSON, IsCSV),
            crossbar_util:response(TaskJObj, Context);
        {'error', 'no_categories'} ->
            Msg = kz_json:from_list([{<<"tip">>, <<"No APIs known yet: GET /help then try again!">>}
                                    ,{<<"cause">>, Category}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        {'error', T=?NE_BINARY}
          when T == Category; T == Action ->
            crossbar_util:response_bad_identifier(T, Context);
        {'error', Reason} ->
            case kz_json:is_json_object(Reason) of
                'true' ->
                    lager:debug("new ~s task ~s cannot be created: ~s"
                               ,[Category, Action, kz_json:encode(Reason)]),
                    cb_context:add_validation_error(<<"attachment">>, <<"type">>, Reason, Context);
                'false' ->
                    lager:debug("new ~s task ~s cannot be created: ~p", [Category, Action, Reason]),
                    crossbar_util:response('error', Reason, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
%% -spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
%% post(Context, TaskId, ?CSV_ATTACHMENT, AttachmentId) ->
%%     AttachmentId = TaskId,
%%     [{_Filename, FileJObj}] = cb_context:req_files(Context),
%%     Contents = kz_json:get_value(<<"contents">>, FileJObj),
%%     CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
%%     Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)],
%%     case kz_doc:attachment(cb_context:doc(Context), AttachmentId) of
%%         'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
%%         _AttachmentMeta ->
%%             lager:debug("deleting old attachment ~s", [AttachmentId]),
%%             kz_datamgr:delete_attachment(cb_context:account_db(Context), TaskId, AttachmentId)
%%     end,
%%     crossbar_doc:save_attachment(TaskId
%%                                  ,AttachmentId
%%                                  ,Contents
%%                                  ,Context
%%                                  ,Opts
%%                                 ).

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
        {'error', 'already_started'} ->
            Msg = kz_json:from_list([{<<"message">>, <<"task already started">>}
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
    ViewOptions = [{'key', cb_context:account_id(Context), 'undefined'}
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
-spec is_accept_csv(cb_context:context()) -> boolean().
is_accept_csv(Context) ->
    Accept = cb_context:req_header(Context, <<"accept">>),
    [Lhs, Rhs] = binary:split(Accept, <<$/>>),
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
    Name = <<"csv">>,
    CT = req_content_type(Context),
    Filename = cb_modules_util:attachment_name(Name, CT),
    Options = [{'content_type', CT}],
    lager:debug("saving ~s attachment in task ~s", [Name, TaskId]),
    crossbar_doc:save_attachment(TaskId, Filename, CSV, Context, Options);
save_attached_data(Context, TaskId, Records, 'false') ->
    lager:debug("converting JSON to CSV before saving"),
    Fields = kz_json:get_keys(hd(Records)),
    lager:debug("CSV fields found: ~p", [Fields]),
    CSV = [iolist_join(",", Fields), "\n"
          ,[ [iolist_join(<<",">>, [kz_json:get_value(Field, Record) || Field <- Fields]), "\n"]
             || Record <- Records
           ]
          ],
    Context1 = replace_content_type(Context, <<"text/csv">>),
    save_attached_data(Context1, TaskId, iolist_to_binary(CSV), 'true').

%% @private
-spec iolist_join(ne_binary(), list()) -> iolist().
iolist_join(_, []) -> [];
iolist_join(_, [_]=One) -> One;
iolist_join(Sep, [H|T]) ->
    [H, Sep, [[X, Sep] || X <- T]].

%% @private
-spec load_csv_attachment(cb_context:context(), kz_tasks:task_id(), ne_binary()) ->
                                 cb_context:context().
load_csv_attachment(Context, TaskId, AName) ->
    Ctx = crossbar_doc:load_attachment(TaskId
                                      ,AName
                                      ,?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)
                                      ,set_db(Context)
                                      ),
    lager:debug("loaded csv ~s from task doc ~s", [AName, TaskId]),
    cb_context:add_resp_headers(
      Ctx
      ,[{<<"Content-Disposition">>, <<"attachment; filename=", AName/binary>>}
       ,{<<"Content-Type">>, <<"text/csv">>}
       ,{<<"Content-Length">>, byte_size(cb_context:resp_data(Ctx))}
       ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetch a task's attachment (they share the Id)
%% @end
%%--------------------------------------------------------------------
%% -spec load_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
%% load_attachment(Context, TaskId, AttachmentId) ->
%%     Context1 = read(TaskId, Context),
%%     case cb_context:resp_status(Context1) of
%%         'success' ->
%%             Doc = cb_context:doc(Context),
%%             cb_context:add_resp_headers(
%%               crossbar_doc:load_attachment(Doc
%%                                           ,AttachmentId
%%                                           ,?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)
%%                                           ,Context
%%                                           )
%%               ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
%%                ,{<<"Content-Type">>, kz_doc:attachment_content_type(Doc, AttachmentId)}
%%                ,{<<"Content-Length">>, kz_doc:attachment_length(Doc, AttachmentId)}
%%                ]
%%              );
%%         _ ->
%%             Context1
%%     end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
%% -spec is_mutable(cb_context:context()) -> cb_context:context().
%% is_mutable(Context) ->
%%     %%TODO: add_system_error(invalid_method) if task is running
%%     Context.
