%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_blacklists).

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

-define(CB_LIST, <<"blacklists/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.blacklists">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.blacklists">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.blacklists">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.blacklists">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.blacklists">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.blacklists">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.blacklists">>, ?MODULE, 'delete').

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
resource_exists() -> true.
resource_exists(_) -> true.

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
   validate_set(Context, cb_context:req_verb(Context)).

validate(Context, DocId) ->
    validate_set(Context, cb_context:req_verb(Context), DocId).

-spec validate_set(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_set(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_set(Context, ?HTTP_GET) ->
    summary(Context);
validate_set(Context, ?HTTP_PUT) ->
    create(Context).

validate_set(Context, ?HTTP_GET, DocId) ->
    read(DocId, Context);
validate_set(Context, ?HTTP_POST, DocId) ->
    update(DocId, Context);
validate_set(Context, ?HTTP_PATCH, DocId) ->
    validate_patch(DocId, Context);
validate_set(Context, ?HTTP_DELETE, DocId) ->
    read(DocId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
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
    cb_context:validate_request_data(<<"blacklists">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"blacklists">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document partially with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            PatchJObj = wh_doc:public_fields(cb_context:req_data(Context)),
            BlacklistsJObj = wh_json:merge_jobjs(PatchJObj, cb_context:doc(Context1)),

            lager:debug("patched doc, now validating"),
            OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
            cb_context:validate_request_data(<<"blacklists">>, cb_context:set_req_data(Context, BlacklistsJObj), OnSuccess);
        _Status ->
            Context1
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
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = cb_context:doc(Context),
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_type">>, <<"blacklist">>, Doc));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
