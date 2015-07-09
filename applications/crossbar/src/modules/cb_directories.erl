%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%%
%%% Handle CRUD operations for Directories
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_directories).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"directories/crossbar_listing">>).
-define(CB_USERS_LIST, <<"directories/users_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.directories">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.directories">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.directories">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.directories">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.directories">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.directories">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.directories">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
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
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

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
    validate_directories(Context, cb_context:req_verb(Context)).

validate_directories(Context, ?HTTP_GET) ->
    summary(Context);
validate_directories(Context, ?HTTP_PUT) ->
    create(Context).

validate(Context, Id) ->
    validate_directory(Context, Id, cb_context:req_verb(Context)).

validate_directory(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_directory(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_directory(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_directory(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"directories">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_directory_users(Id, Context1);
        _Status -> Context1
    end.

-spec load_directory_users(ne_binary(), cb_context:context()) -> cb_context:context().
load_directory_users(Id, Context) ->
    Context1 = crossbar_doc:load_view(?CB_USERS_LIST
                                      ,[{<<"key">>, Id}]
                                      ,Context
                                      ,fun normalize_users_results/2
                                     ),
    case cb_context:resp_status(Context1) of
        'success' ->
            Users = cb_context:resp_data(Context1),
            Directory = cb_context:resp_data(Context),
            cb_context:set_resp_data(Context, wh_json:set_value(<<"users">>, Users, Directory));
        _Status -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"directories">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing conference document partially with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(DocId, Context) ->
    crossbar_doc:patch_and_validate(DocId, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_doc:set_type(cb_context:doc(Context), <<"directory">>)
                      );
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_users_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_users_results(JObj, Acc) ->
    [wh_json:from_list([{<<"user_id">>, wh_doc:id(JObj)}
                        ,{<<"callflow_id">>, wh_json:get_value(<<"value">>, JObj)}
                       ])
     | Acc
    ].
