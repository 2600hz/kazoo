%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_configs).

-export([init/0
         ,allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/2
         ,get/2
         ,put/2
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.configs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.configs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.configs">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.configs">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.configs">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.configs">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.configs">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.configs">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /configs => []
%%    /configs/foo => [<<"foo">>]
%%    /configs/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'false'.
-spec resource_exists(path_tokens()) -> 'true'.
resource_exists() -> false.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /configs mights load a list of config objects
%% /configs/123 might load the config object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Config) ->
    read(Config, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Config) ->
    create(Config, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Config) ->
    update(Config, Context);
validate(#cb_context{req_verb = ?HTTP_PATCH}=Context, Config) ->
    validate_patch(Config, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Config) ->
    read(Config, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context(), path_token()) -> cb_context:context().
get(Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PATCH, execute the actual action, usually a db save
%% (after a merge).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), cb_context:context()) -> cb_context:context().
create(Config, #cb_context{req_data=JObj, db_name=Db}=Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    case couch_mgr:lookup_doc_rev(Db, Id) of
        {ok, _} -> cb_context:add_system_error(datastore_conflict, Context);
        {error, _} ->
            J = wh_doc:set_id(JObj, Id),
            cb_context:validate_request_data(<<"configs">>, Context#cb_context{req_data=J})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Config, Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Config, Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    validate_request_data(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge partially an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Config, Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    crossbar_doc:patch_and_validate(Id, Context, fun validate_request_data/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates existing instance
%% @end
%%--------------------------------------------------------------------
-spec validate_request_data(ne_binary(), cb_context:context()) -> cb_context:context().
validate_request_data(Id, Context) ->
    OnSuccess = fun(C) -> crossbar_doc:load_merge(Id, C) end,
    cb_context:validate_request_data(<<"configs">>, Context, OnSuccess).
