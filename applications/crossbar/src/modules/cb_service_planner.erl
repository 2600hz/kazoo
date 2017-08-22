%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_service_planner).

-export([init/0
        ,authorize/1, authorize/2
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST_SERVICE_PLANES, <<"service_plans/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.authorize.service_planner">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.service_planner">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.service_planner">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.service_planner">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.service_planner">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.service_planner">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.service_planner">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.service_planner">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.service_planner">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) -> is_authorize(Context).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _) -> is_authorize(Context).

-spec is_authorize(cb_context:context()) -> boolean().
is_authorize(Context) ->
    AccountId = cb_context:account_id(Context),
    kz_term:is_not_empty(AccountId)
        andalso kz_services:is_reseller(AccountId).


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

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_PlanId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /security => []
%%    /security/foo => [<<"foo">>]
%%    /security/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_PlanId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /service_planner mights load a list of auth objects
%% /service_planner/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_planner(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PlanId) ->
    validate_planner(Context, PlanId, cb_context:req_verb(Context)).

%% validates /service_planner
-spec validate_planner(cb_context:context(), http_method()) -> cb_context:context().
validate_planner(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST_SERVICE_PLANES, [] ,Context, fun normalize_view_results/2);
validate_planner(Context, ?HTTP_PUT) ->
    create(Context).

%% validates /service_planner/plane_id
-spec validate_planner(cb_context:context(), http_method(), http_method()) -> cb_context:context().
validate_planner(Context, PlanId, ?HTTP_GET) ->
    crossbar_doc:load(PlanId, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type()));
validate_planner(Context, PlanId, ?HTTP_POST) ->
    update(PlanId, Context);
validate_planner(Context, PlanId, ?HTTP_PATCH) ->
    validate_patch(PlanId, Context);
validate_planner(Context, PlanId, ?HTTP_DELETE) ->
    crossbar_doc:load(PlanId, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _PlanId) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _PlanId) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _PlanId) ->
    crossbar_doc:delete(Context).

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
    cb_context:validate_request_data(<<"service_plans">>, Context, OnSuccess).

-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"service_plans">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), kzd_service_plan:type()));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
