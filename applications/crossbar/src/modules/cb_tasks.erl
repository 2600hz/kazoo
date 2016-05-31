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
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,content_types_accepted/3
        ,content_types_provided/3
        ,validate/1, validate/2, validate/3, validate/4
        ,put/1, put/3
        ,post/4
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_tasks/include/kazoo_tasks.hrl").

-define(SCHEMA_CREATE_TASK, <<"tasks">>).

-define(HELP, <<"help">>).
-define(CSV_ATTACHMENT, <<"csv">>).


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
    _ = crossbar_bindings:bind(<<"*.execute.post.tasks">>, ?MODULE, 'post'),
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
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?HELP) ->
    [?HTTP_GET];
allowed_methods(_TaskId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_TaskId, ?CSV_ATTACHMENT) ->
    [?HTTP_PUT].
allowed_methods(_TaskId, ?CSV_ATTACHMENT, _AttachmentId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?HELP) -> 'true';
resource_exists(_TaskId) -> 'true'.
resource_exists(_TaskId, ?CSV_ATTACHMENT) -> 'true'.
resource_exists(_TaskId, ?CSV_ATTACHMENT, _AttachmentId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, _TaskId, ?CSV_ATTACHMENT) ->
    CTA = [{'from_binary', ?CSV_CONTENT_TYPES}],
    cb_context:add_content_types_accepted(Context, CTA);
content_types_accepted(Context, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _TaskId, ?CSV_ATTACHMENT) ->
    CTP = [{'from_binary', ?CSV_CONTENT_TYPES}],
    cb_context:add_content_types_provided(Context, CTP);
content_types_provided(Context, _, _) ->
    Context.

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
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_tasks(Context, cb_context:req_verb(Context)).
validate(Context, PathToken) ->
    validate_task(Context, PathToken, cb_context:req_verb(Context)).
validate(Context, TaskId, ?CSV_ATTACHMENT) ->
    validate_attachment(Context, TaskId, cb_context:req_verb(Context)).
validate(Context, TaskId, ?CSV_ATTACHMENT, AttachmentId) ->
    validate_attachment(Context, TaskId, AttachmentId, cb_context:req_verb(Context)).

-spec validate_tasks(cb_context:context(), http_method()) -> cb_context:context().
validate_tasks(Context, ?HTTP_GET) ->
    summary(Context);
validate_tasks(Context, ?HTTP_PUT) ->
    cb_context:validate_request_data(?SCHEMA_CREATE_TASK, Context).

-spec validate_task(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_task(Context, ?HELP, ?HTTP_GET) ->
    JObj = kz_json:from_list([{<<"tasks">>, kz_tasks:available()}]),
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, JObj}
                                ]);
validate_task(Context, TaskId, ?HTTP_GET) ->
    read(TaskId, Context);
validate_task(Context, TaskId, ?HTTP_PATCH) ->
    read(TaskId, Context);
validate_task(Context, TaskId, ?HTTP_DELETE) ->
    read(TaskId, Context).

-spec validate_attachment(cb_context:context(), path_token(), http_method()) -> cb_context:context().
-spec validate_attachment(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_attachment(Context, TaskId, ?HTTP_PUT) ->
    read(TaskId, Context).
validate_attachment(Context, TaskId, AttachmentId, ?HTTP_GET) ->
    load_attachment(Context, TaskId, AttachmentId);
validate_attachment(Context, TaskId, AttachmentId, ?HTTP_POST) ->
    is_mutable(load_attachment(Context, TaskId, AttachmentId));
validate_attachment(Context, TaskId, AttachmentId, ?HTTP_DELETE) ->
    is_mutable(load_attachment(Context, TaskId, AttachmentId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    ReqData = cb_context:req_data(Context),
    BinM = kz_json:get_value(<<"module">>, ReqData),
    BinF = kz_json:get_value(<<"function">>, ReqData),
    M = kz_util:to_atom(BinM, 'true'),
    F = kz_util:to_atom(BinF, 'true'),
    A = kz_json:get_list_value(<<"arguments">>, ReqData),
    case kz_tasks:new(cb_context:account_id(Context), M, F, A) of
        {'ok', Task} -> crossbar_util:response(Task, Context);
        {'error', {'no_module', _M}} ->
            crossbar_util:response_bad_identifier(BinM, Context);
        {'error', {'no_function', _M, _F, Arity}} ->
            BinA = integer_to_binary(Arity),
            Msg = kz_json:from_list([{<<"cause">>, <<"no such function">>}
                                    ,{<<"M:F/A">>, <<BinM/binary, ":", BinF/binary, "/", BinA/binary>>}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context)
    end.

put(Context, TaskId, ?CSV_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)],
    crossbar_doc:save_attachment(TaskId
                                 ,cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_TASKS_DB)
                                 ,Opts
                                ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, TaskId, ?CSV_ATTACHMENT, AttachmentId) ->
    AttachmentId = TaskId,
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)],
    case kz_doc:attachment(cb_context:doc(Context), AttachmentId) of
        'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
        _AttachmentMeta ->
            lager:debug("deleting old attachment ~s", [AttachmentId]),
            kz_datamgr:delete_attachment(cb_context:account_db(Context), TaskId, AttachmentId)
    end,
    crossbar_doc:save_attachment(TaskId
                                 ,AttachmentId
                                 ,Contents
                                 ,Context
                                 ,Opts
                                ).

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
    crossbar_doc:load(TaskId, set_db(Context), ?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)).

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
    crossbar_doc:load_view(?KZ_TASKS_BY_ACCOUNT, ViewOptions, set_db(Context), fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetch a task's attachment (they share the Id)
%% @end
%%--------------------------------------------------------------------
-spec load_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
load_attachment(Context, TaskId, AttachmentId) ->
    Context1 = read(TaskId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Doc = cb_context:doc(Context),
            cb_context:add_resp_headers(
              crossbar_doc:load_attachment(Doc
                                          ,AttachmentId
                                          ,?TYPE_CHECK_OPTION(?KZ_TASKS_DOC_TYPE)
                                          ,Context
                                          )
              ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
               ,{<<"Content-Type">>, kz_doc:attachment_content_type(Doc, AttachmentId)}
               ,{<<"Content-Length">>, kz_doc:attachment_length(Doc, AttachmentId)}
               ]
             );
        _ ->
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_mutable(cb_context:context()) -> cb_context:context().
is_mutable(Context) ->
    %%TODO: add_system_error(invalid_method) if task is running
    Context.
