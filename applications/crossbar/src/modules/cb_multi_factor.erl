%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% Multi factor authentication configuration API endpoint
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_multi_factor).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST_ATTEMPT_LOG, <<"auth/login_attempt_by_auth_type">>).

-define(ATTEMPTS, <<"attempts">>).
-define(ATTEMPTS_TYPE, <<"login_attempt">>).

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
    _ = crossbar_bindings:bind(<<"*.authorize.multi_factor">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.multi_factor">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.multi_factor">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.multi_factor">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.multi_factor">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.multi_factor">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.multi_factor">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.multi_factor">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.multi_factor">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
                       boolean() |
                       {'halt', cb_context:context()}.
authorize(Context) ->
    case {cb_context:req_nouns(Context), cb_context:req_verb(Context)} of
        {[{<<"multi_factor">>, _}], ?HTTP_GET} -> 'true';
        {[{<<"multi_factor">>, _}], _} -> {'halt', cb_context:add_system_error('forbidden', Context)};
        _ -> 'true'
    end.

-spec authorize(cb_context:context(), path_token()) ->
                       boolean() |
                       {'halt', cb_context:context()}.
authorize(Context, _) ->
    authorize_system_multi_factor(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> 'true'.
authorize(_Context, _, _) -> 'true'.

-spec authorize_system_multi_factor(cb_context:context(), req_nouns(), http_method()) ->
                                           boolean() |
                                           {'halt', cb_context:context()}.
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_GET) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_POST) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_PATCH) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], _) -> {'halt', cb_context:add_system_error('forbidden', C)};
authorize_system_multi_factor(_, _, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?ATTEMPTS, _AttemptId) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /multi_factor => []
%%    /multi_factor/foo => [<<"foo">>]
%%    /multi_factor/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?ATTEMPTS) -> 'true';
resource_exists(_ConfigId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?ATTEMPTS, _AttemptId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /multi_factor mights load a list of auth objects
%% /multi_factor/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_multi_factor(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS) ->
    crossbar_view:load(Context
                      ,?CB_LIST_ATTEMPT_LOG
                      ,[{mapper, fun normalize_attempt_view_result/1}
                       ,{key_map, <<"multi_factor">>}
                       ]
                      );
validate(Context, ConfigId) ->
    case cb_context:req_nouns(Context) of
        [{<<"multi_factor">>, _}] ->
            validate_multi_factor_config(cb_context:set_account_db(Context, ?KZ_AUTH_DB), ConfigId, cb_context:req_verb(Context));
        _ ->
            validate_multi_factor_config(Context, ConfigId, cb_context:req_verb(Context))
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS, AttemptId) ->
    read_attempt_log(AttemptId, Context).

-spec validate_multi_factor(cb_context:context(), req_nouns(), http_method()) -> cb_context:context().
validate_multi_factor(Context, [{<<"multi_factor">>, _}], ?HTTP_GET) ->
    system_summary(Context);
validate_multi_factor(Context, _, ?HTTP_GET) ->
    summary(Context);
validate_multi_factor(Context, _, ?HTTP_PUT) ->
    create(Context).

-spec validate_multi_factor_config(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_multi_factor_config(Context, ConfigId, ?HTTP_GET) ->
    read(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_POST) ->
    update(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_PATCH) ->
    validate_patch(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_DELETE) ->
    read(ConfigId, Context).

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
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
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
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"multi_factor_provider">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>)).

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
    cb_context:validate_request_data(<<"multi_factor_provider">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a login attempt log from MODB
%% @end
%%--------------------------------------------------------------------
-spec read_attempt_log(ne_binary(), cb_context:context()) -> cb_context:context().
read_attempt_log(?MATCH_MODB_PREFIX(YYYY, MM, _) = AttemptId, Context) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    crossbar_doc:load(AttemptId, cb_context:set_account_modb(Context, Year, Month), ?TYPE_CHECK_OPTION(?ATTEMPTS_TYPE)).

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
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    ViewOptions = [{'startkey', [<<"multi_factor">>]}
                  ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
                  ],
    add_available_providers(
      crossbar_doc:load_view(<<"auth/providers_by_type">>, ViewOptions, Context, fun normalize_view_results/2)
     ).

system_summary(Context) ->
    ViewOptions = [{'startkey', [<<"multi_factor">>]}
                  ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
                  ],
    crossbar_doc:load_view(<<"providers/list_by_type">>, ViewOptions, cb_context:set_account_db(Context, ?KZ_AUTH_DB), fun normalize_view_results/2).

-spec add_available_providers(cb_context:context()) -> cb_context:context().
add_available_providers(Context) ->
    C1 = system_summary(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            crossbar_doc:handle_json_success(merge_summary(Context, cb_context:doc(C1)), Context);
        _ -> crossbar_doc:handle_json_success(merge_summary(Context, []), Context)
    end.

-spec merge_summary(cb_context:context(), kz_json:objects()) -> kz_json:object().
merge_summary(Context, Available) ->
    merge_summary(Context, Available, cb_context:resp_status(Context)).

-spec merge_summary(cb_context:context(), kz_json:objects(), cb_context:crossbar_status()) -> kz_json:object().
merge_summary(Context, Available, 'success') ->
    kz_json:from_list(
      [{<<"configured">>, cb_context:doc(Context)}
      ,{<<"multi_factor_providers">>, Available}
      ]
     );
merge_summary(_Context, Available, _) ->
    kz_json:from_list(
      [{<<"configured">>, []}
      ,{<<"multi_factor_providers">>, Available}
      ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = kz_json:set_value(<<"pvt_provider_type">>, <<"multi_factor">>, cb_context:doc(Context)),
    cb_context:set_doc(Context, kz_doc:set_type(Doc, <<"provider">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_attempt_view_result(kz_json:object()) -> kz_json:object().
normalize_attempt_view_result(JObj) ->
    kz_json:get_value(<<"value">>, JObj).
