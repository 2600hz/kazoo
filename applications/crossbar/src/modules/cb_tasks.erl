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
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,patch/2
         ,delete/2
        ]).

-include("crossbar.hrl").

-define(SCHEMA_CREATE_TASK, <<"tasks">>).


%%%===================================================================
%%% API
%%%===================================================================

init() ->
    {'ok', _} = application:ensure_all_started('kazoo_tasks'),

    %%TODO: auth
    _ = crossbar_bindings:bind(<<"*.allowed_methods.tasks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.tasks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.tasks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.tasks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.tasks">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.tasks">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_TaskId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.

resource_exists(_TaskId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            %%TODO: view. summary(Context, undefined)?
            cb_context:setters(Context, [{fun cb_context:set_resp_data/2, kz_tasks:all()}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        ?HTTP_PUT ->
            cb_context:validate_request_data(?SCHEMA_CREATE_TASK, Context)
    end.

validate(Context, TaskId) ->
    validate_task(Context, TaskId, cb_context:req_verb(Context)).

-spec validate_task(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_task(Context, TaskId, ?HTTP_GET) ->
    case kz_tasks:read(TaskId) of
        {'ok', JObj} ->
            cb_context:setters(Context, [{fun cb_context:set_resp_data/2, JObj}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        {'error', _} ->
            crossbar_util:response_bad_identifier(TaskId, Context)
    end;
validate_task(Context, TaskId, ?HTTP_PATCH) ->
    validate_task(Context, TaskId);
validate_task(Context, TaskId, ?HTTP_DELETE) ->
    validate_task(Context, TaskId).

-spec validate_task(cb_context:context(), path_token()) -> cb_context:context().
validate_task(Context, TaskId) ->
    case kz_tasks:read(TaskId) of
        {'ok', _JObj} -> cb_context:set_resp_status(Context, 'success');
        {'error', _R} ->
            crossbar_util:response_bad_identifier(TaskId, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    ReqData = cb_context:req_data(Context),
    BinM = kz_json:get_value(<<"module">>, ReqData),
    BinF = kz_json:get_value(<<"function">>, ReqData),
    M = kz_util:to_atom(BinM, 'true'),
    F = kz_util:to_atom(BinF, 'true'),
    A = kz_json:get_list_value(<<"arguments">>, ReqData),
    case kz_tasks:new(M, F, A) of
        {'ok', TaskId} ->
            JObj = kz_json:from_list([{<<"id">>, TaskId}]),
            crossbar_util:response(JObj, Context);
        {'error', {'no_module', _M}} ->
            crossbar_util:response_bad_identifier(BinM, Context);
        {'error', {'no_function', _M, _F, Arity}} ->
            BinA = integer_to_binary(Arity),
            Msg = kz_json:from_list([{<<"cause">>, <<"no such function">>}
                                    ,{<<"M:F/A">>, <<BinM/binary, ":", BinF/binary, "/", BinA/binary>>}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db update.
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

%%% End of Module
