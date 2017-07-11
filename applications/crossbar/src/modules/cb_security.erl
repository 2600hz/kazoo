%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% Kazoo authentication configuration API endpoint
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_security).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/1, put/2
        ,post/1, post/2
        ,patch/1, patch/2
        ,delete/1, delete/2
        ]).

-include("crossbar.hrl").

-define(DEFAULT_AUTH_MODULES, [<<"cb_user_auth">>
                              ,<<"cb_api_auth">>
                              ,<<"cb_ip_auth">>
                              ,<<"cb_ubiquiti_auth">>
                              ]).

-define(SYSTEM_AUTH_MODULES
       ,kapps_config:get_ne_binaries(?AUTH_CONFIG_CAT, <<"available_auth_modules">>, ?DEFAULT_AUTH_MODULES)
       ).

-define(AVAILABLE_AUTH_MODULES
       ,kz_json:from_list([{<<"available_auth_modules">>, ?SYSTEM_AUTH_MODULES}])
       ).

-define(CB_LIST_ATTEMPT_LOG, <<"auth/login_attempt_by_time">>).

-define(ATTEMPTS, <<"attempts">>).
-define(AUTH_ATTEMPT_TYPE, <<"login_attempt">>).

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
    _ = crossbar_bindings:bind(<<"*.authorize.security">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.security">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.security">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.security">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.security">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.security">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.security">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.security">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.security">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_list_available_module(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context, _) ->
    'true'.

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(_Context, _, _) ->
    'true'.

-spec authorize_list_available_module(cb_context:context(), req_nouns(), http_methods()) -> boolean().
authorize_list_available_module(Context, [{<<"security">>, []}], ?HTTP_GET) ->
    cb_simple_authz:authorize(Context);
authorize_list_available_module(Context, [{<<"security">>, []}], _) ->
    {'halt', cb_context:add_system_error('forbidden', Context)};
authorize_list_available_module(Context, _Nouns, _Verb) ->
    cb_simple_authz:authorize(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?ATTEMPTS, _AttemptId) ->
    [?HTTP_GET].

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
resource_exists(?ATTEMPTS) -> 'true';
resource_exists(_ConfigId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?ATTEMPTS, _AttemptId) -> 'true'.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /security mights load a list of auth objects
%% /security/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_auth_configs(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS) ->
    crossbar_view:load(Context, ?CB_LIST_ATTEMPT_LOG, [{mapper, fun normalize_attempt_view_result/1}]);
validate(Context, Id) ->
    case lists:member(Id, ?SYSTEM_AUTH_MODULES) of
        'true' -> validate_module_configs(Context, Id, cb_context:req_verb(Context));
        'false' -> not_found(Context, Id)
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS, AttemptId) ->
    read_attempt_log(AttemptId, Context).

%% validates /security
-spec validate_auth_configs(cb_context:context(), http_method()) -> cb_context:context().
validate_auth_configs(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        [{<<"security">>, []}] -> summary_available(Context);
        [{<<"security">>, []}, {<<"accounts">>, [?NE_BINARY=_Id]}] -> read(Context);
        _Nouns -> Context
    end;
validate_auth_configs(Context, ?HTTP_PUT) ->
    create(Context);
validate_auth_configs(Context, ?HTTP_POST) ->
    update(?ACCOUNT_AUTH_CONFIG_ID, Context);
validate_auth_configs(Context, ?HTTP_PATCH) ->
    validate_patch(?ACCOUNT_AUTH_CONFIG_ID, Context);
validate_auth_configs(Context, ?HTTP_DELETE) ->
    read(Context).

%% validats /security/{PATH_TOKEN}
-spec validate_module_configs(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_module_configs(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_module_configs(Context, Id, ?HTTP_PUT) ->
    create(Id, Context);
validate_module_configs(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_module_configs(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_module_configs(Context, Id, ?HTTP_DELETE) ->
    validate_delete(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _Id) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:get_value(module_config_path(Id), cb_context:doc(C1), kz_json:new())
                                    );
        _ -> C1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:get_value(module_config_path(Id), cb_context:doc(C1), kz_json:new())
                                    );
        _ -> C1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_doc:delete(Context, 'permanent').

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            DbDoc = cb_context:fetch(Context, 'db_doc', kz_json:new()),
            cb_context:set_resp_data(Context, kz_json:get_value(module_config_path(Id), DbDoc, kz_json:new()));
        _ -> C1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, ?AVAILABLE_AUTH_MODULES}
                                ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    crossbar_doc:load(?ACCOUNT_AUTH_CONFIG_ID, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)).

-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    C1 = crossbar_doc:load(?ACCOUNT_AUTH_CONFIG_ID, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    case cb_context:resp_status(C1) of
        'success' ->
            case kz_json:get_value(module_config_path(Id), cb_context:doc(C1)) of
                'undefined' -> not_found(Context, Id);
                ModConfig ->crossbar_doc:handle_json_success(ModConfig, Context)
            end;
        _ -> not_found(Context, Id)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"auth_config_account">>, Context, OnSuccess).

-spec create(ne_binary(), cb_context:context()) -> cb_context:context().
create(Id, Context) ->
    validate_patch(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(?ACCOUNT_AUTH_CONFIG_ID=Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"auth_config_account">>, Context, OnSuccess);
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"auth_config">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(?ACCOUNT_AUTH_CONFIG_ID=Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2);
validate_patch(Id, Context) ->
    C1 = fix_config_object(Id, cb_context:req_data(Context), cb_context:store(Context, <<"orig_mod_id">>, Id)),
    crossbar_doc:patch_and_validate(?ACCOUNT_AUTH_CONFIG_ID, C1, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the requested module is in auth_modules object, then
%% remove it
%% @end
%%--------------------------------------------------------------------
validate_delete(Id, Context) ->
    C1 = read(Context),
    case cb_context:resp_status(C1) =:= 'success'
        andalso kz_json:is_json_object(module_config_path(Id), cb_context:doc(C1))
    of
        'false' -> not_found(Context, Id);
        'true' ->
            cb_context:set_doc(C1, kz_json:delete_key(module_config_path(Id), cb_context:doc(C1)))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = kz_doc:set_id(cb_context:doc(Context), ?ACCOUNT_AUTH_CONFIG_ID),
    cb_context:set_doc(Context, kz_doc:set_type(Doc, <<"account_config">>));
on_successful_validation(?ACCOUNT_AUTH_CONFIG_ID=Id, Context) ->
    Verb = cb_context:req_verb(Context),
    C1 = crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    OrigModId = cb_context:fetch(Context, <<"orig_mod_id">>),
    handle_singularity_update(C1, Verb, Id, OrigModId, cb_context:resp_status(C1));
on_successful_validation(Id, Context) ->
    Verb = cb_context:req_verb(Context),
    OrigModId = cb_context:fetch(Context, <<"orig_mod_id">>),
    C1 = fix_config_object(Id, cb_context:doc(Context), Context),
    C2 = crossbar_doc:load_merge(?ACCOUNT_AUTH_CONFIG_ID, C1, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    handle_singularity_update(C2, Verb, Id, OrigModId, cb_context:resp_status(C2)).

-spec handle_singularity_update(cb_context:context(), http_method(), ne_binary(), api_binary(), crossbar_status()) ->
                                       cb_context:context().
%% PUT on /security/{MOD_ID}
handle_singularity_update(Context, ?HTTP_PUT, _Id, _OrigModId, 'success') -> Context;
handle_singularity_update(Context, ?HTTP_PUT, _Id, _OrigModId, _) -> on_successful_validation('undefined', Context);
%% PATCH on /security
handle_singularity_update(Context, ?HTTP_PATCH, _Id, 'undefined', 'success') -> Context;
handle_singularity_update(Context, ?HTTP_PATCH, Id, 'undefined', _) -> not_found(Context, Id);
%% PATCH on /security/{MOD_ID}
handle_singularity_update(Context, ?HTTP_PATCH, _Id, OrigModId, 'success') -> check_mod_config_exists(Context, OrigModId);
handle_singularity_update(Context, ?HTTP_PATCH, _id, OrigModId, _) -> not_found(Context, OrigModId);
%% POST on /security
handle_singularity_update(Context, ?HTTP_POST, ?ACCOUNT_AUTH_CONFIG_ID, 'undefined', 'success') -> Context;
handle_singularity_update(Context, ?HTTP_POST, ?ACCOUNT_AUTH_CONFIG_ID=Id, 'undefined', _) -> not_found(Context, Id);
%% POST on /security/{MOD_ID}
handle_singularity_update(Context, ?HTTP_POST, Id, 'undefined', 'success') -> check_mod_config_exists(Context, Id);
handle_singularity_update(Context, ?HTTP_POST, Id, 'undefined', _) -> not_found(Context, Id).

-spec check_mod_config_exists(cb_context:context(), ne_binary()) -> cb_context:context().
check_mod_config_exists(Context, Id) ->
    case kz_json:get_value(module_config_path(Id), cb_context:fetch(Context, 'db_doc')) of
        'undefined' -> not_found(Context, Id);
        _ModConfig -> Context
    end.


-spec fix_config_object(ne_binary(), kz_json:object(), cb_context:context()) -> cb_context:context().
fix_config_object(Id, JObj, Context) ->
    ModConfigs = kz_json:from_list([{Id, JObj}]),
    AuthMods = kz_json:from_list([{<<"auth_modules">>, ModConfigs}]),
    cb_context:setters(Context, [{fun cb_context:set_doc/2, AuthMods}
                                ,{fun cb_context:set_req_data/2, AuthMods}
                                ]).

-spec module_config_path(ne_binary()) -> ne_binaries().
module_config_path(Id) -> [<<"auth_modules">>, Id].

-spec not_found(cb_context:context(), ne_binary()) -> cb_context:context().
not_found(Context, Id) ->
    crossbar_doc:handle_datamgr_errors('not_found', Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a login attempt log from MODB
%% @end
%%--------------------------------------------------------------------
-spec read_attempt_log(ne_binary(), cb_context:context()) -> cb_context:context().
read_attempt_log(?MATCH_MODB_PREFIX(Year, Month, _)=AttemptId, Context) ->
    crossbar_doc:load(AttemptId, cb_context:set_account_modb(Context, Year, Month), ?TYPE_CHECK_OPTION(?AUTH_ATTEMPT_TYPE)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_attempt_view_result(kz_json:object()) -> kz_json:object().
normalize_attempt_view_result(JObj) ->
    kz_json:get_value(<<"value">>, JObj).
