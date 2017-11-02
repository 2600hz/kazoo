%%% GET|PUT     /:api_version/accounts/:account_id/harrys
%%% POST|DELETE /:api_version/accounts/:account_id/harrys/:harry_id
-module(cb_harrys).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"harrys/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.harrys">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.harrys">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.harrys">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.harrys">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.harrys">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.harrys">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.harrys">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(PathToken::path_token()) -> http_methods().
allowed_methods(_PathToken) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /harrys => []
%%    /harrys/foo => [<<"foo">>]
%%    /harrys/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(PathToken::path_token()) -> 'true'.
resource_exists(_PathToken) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /harrys mights load a list of harry objects
%% /harrys/123 might load the harry object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(Context::cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_harrys(Context, cb_context:req_verb(Context)).

-spec validate(Context::cb_context:context(), Id::path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_harry(Context, Id, cb_context:req_verb(Context)).

-spec validate_harrys(Context::cb_context:context(), HTTPMethod::http_method()) -> cb_context:context().
validate_harrys(Context, ?HTTP_GET) ->
    summary(Context);
validate_harrys(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_harry(Context::cb_context:context(), Id::path_token(), HTTPMethod::http_method()) -> cb_context:context().
validate_harry(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_harry(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_harry(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(Context::cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(Context::cb_context:context(), Id::path_token()) -> cb_context:context().
post(Context, _Id) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(Context::cb_context:context(), Id::path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(Context::cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"harrys">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(Id::ne_binary(), Context::cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"harry">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(Id::ne_binary(), Context::cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"harrys">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(Context::cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(Id::api_binary(), Context::cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"harry">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"harry">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(JObj::kz_json:object(), Acc::kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
