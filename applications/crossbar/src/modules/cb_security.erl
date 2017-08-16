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
        ,post/1
        ,patch/1
        ,delete/1
        ]).

-include("crossbar.hrl").

-define(DEFAULT_AUTH_METHODS, [<<"cb_api_auth">>
                              ,<<"cb_auth">>
                              ,<<"cb_ip_auth">>
                              ,<<"cb_ubiquiti_auth">>
                              ,<<"cb_user_auth">>
                              ]).

-define(SYSTEM_AUTH_METHODS
       ,kapps_config:get_ne_binaries(?AUTH_CONFIG_CAT, <<"available_auth_methods">>, ?DEFAULT_AUTH_METHODS)
       ).

-define(AVAILABLE_AUTH_METHODS
       ,kz_json:from_list([{<<"available_auth_methods">>, ?SYSTEM_AUTH_METHODS}])
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
-spec authorize(cb_context:context()) ->
                       boolean() |
                       {'halt', cb_context:context()}.
authorize(Context) ->
    authorize_list_available_module(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token()) -> 'true'.
authorize(_Context, _) -> 'true'.

-spec authorize(cb_context:context(), path_token(), path_token()) -> 'true'.
authorize(_Context, _, _) -> 'true'.

-spec authorize_list_available_module(cb_context:context(), req_nouns(), http_method()) ->
                                             boolean() |
                                             {'halt', cb_context:context()}.
authorize_list_available_module(_Context, [{<<"security">>, []}], ?HTTP_GET) ->
    'true';
authorize_list_available_module(Context, [{<<"security">>, []}], _) ->
    {'halt', cb_context:add_system_error('forbidden', Context)};
authorize_list_available_module(_Context, _Nouns, _Verb) ->
    'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ATTEMPTS) ->
    [?HTTP_GET].

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
resource_exists(_ConfigId) -> 'false'.

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
    crossbar_view:load(Context, ?CB_LIST_ATTEMPT_LOG, [{mapper, fun normalize_attempt_view_result/1}]).

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
validate_auth_configs(Context, ?HTTP_POST) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    update(ConfigId, Context);
validate_auth_configs(Context, ?HTTP_PATCH) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    validate_patch(ConfigId, Context);
validate_auth_configs(Context, ?HTTP_DELETE) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    C1 = crossbar_doc:load(ConfigId, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    case cb_context:resp_status(C1) of
        'success' -> C1;
        _ ->
            Msg = <<"account does not have customize auth configuration">>,
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, Msg}]),  Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    maybe_flush_config(crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    maybe_flush_config(crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_doc:delete(Context, ?HARD_DELETE).

maybe_flush_config(Context) ->
    case cb_context:resp_status(Context) =:= 'success'
        andalso cb_context:fetch(Context, 'flush', 'false')
    of
        'true' ->
            kapps_account_config:flush(cb_context:account_id(Context), ?AUTH_CONFIG_CAT, <<"hierarchy_merge">>);
        'false' -> Context
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
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, ?AVAILABLE_AUTH_METHODS}
              ],
    cb_context:setters(Context, Setters).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    add_inherited_config(crossbar_doc:load(ConfigId, Context, ?TYPE_CHECK_OPTION(<<"account_config">>))).

-spec add_inherited_config(cb_context:context()) -> cb_context:context().
add_inherited_config(Context) ->
    AccountConfig = case cb_context:resp_status(Context) of
                        'success' -> kz_json:delete_key(<<"id">>, cb_context:resp_data(Context));
                        _ -> kz_json:new()
                    end,
    Props = [{<<"account">>, AccountConfig}
            ,{<<"inherited_config">>, crossbar_auth:get_inherited_config(Context)}
            ],
    crossbar_doc:handle_json_success(kz_json:from_list(Props), Context).

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
    SchemaName = kapps_config_util:account_schema_name(?AUTH_CONFIG_CAT),
    cb_context:validate_request_data(SchemaName, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    C1 = cb_context:store(Context, <<"orig_req_data">>, cb_context:req_data(Context)),
    crossbar_doc:patch_and_validate(Id, C1, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
on_successful_validation(Id, Context) ->
    C1 = crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    case {cb_context:resp_status(C1), cb_context:resp_error_code(C1)} of
        {'success', _} -> check_multi_factor_setting(C1);
        {'error', 404} -> create_config_document(check_multi_factor_setting(C1));
        _ -> C1
    end.

-spec create_config_document(cb_context:context()) -> cb_context:context().
create_config_document(Context) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    Doc = kz_doc:set_id(cb_context:doc(Context), ConfigId),
    crossbar_doc:handle_json_success(kz_doc:set_type(Doc, <<"account_config">>), cb_context:store(Context, flush, true)).

check_multi_factor_setting(Context) ->
    ReqData = cb_context:fetch(Context, <<"orig_req_data">>, cb_context:req_data(Context)),
    kz_json:foldl(fun check_multi_factor_setting/3, Context, kz_json:get_value(<<"auth_modules">>, ReqData)).

check_multi_factor_setting(AuthModule, JObj, Context) ->
    case has_configuration_id(JObj)
        andalso has_account_id_or_db(JObj)
    of
        'false' -> Context;
        'true' -> check_account_hierarchy(AuthModule, JObj, Context);
        ErrMsg -> failed_multi_factor_validation(AuthModule, ErrMsg, Context)
    end.

-spec has_configuration_id(kz_json:object()) -> boolean().
has_configuration_id(JObj) ->
    kz_json:get_ne_binary_value([<<"multi_factor">>, <<"configuration_id">>], JObj) =/= 'undefined'.

-spec has_account_id_or_db(kz_json:object()) -> boolean() | ne_binary().
has_account_id_or_db(JObj) ->
    case kz_json:get_ne_binary_value([<<"multi_factor">>, <<"account_id">>], JObj) of
        'undefined' -> <<"setting multi-factor configuration_id needs setting account_id">>;
        _AccountId -> 'true'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Only allow to set the configuration id if the account who has the configuration is
%% in the tree
%% * if master is setting the config, allow
%% * if the account is the same authenticated account, allow
%% * if authenticated account is parent and wants to set its descendant's, allow
%% * if a include_subaccounts, allow a child account set its parent's config
%% @end
%%--------------------------------------------------------------------
-spec check_account_hierarchy(ne_binary(), kz_json:object(), cb_context:context()) -> cb_context:context().
check_account_hierarchy(AuthModule, JObj, Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSysAdmin = cb_context:is_superduper_admin(AuthAccountId),
    AccountId = kz_json:get_ne_binary_value([<<"multi_factor">>, <<"account_id">>], JObj),
    NotIncludeSubAccounts = kz_json:is_false([<<"multi_factor">>, <<"include_subaccounts">>], JObj),

    case IsSysAdmin
        orelse kz_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true')
        orelse (NotIncludeSubAccounts
                orelse kz_util:is_in_account_hierarchy(AccountId, AuthAccountId, 'true')
               )
    of
        'true' -> Context;
        'false' ->
            ErrMsg = kz_term:to_binary(io_lib:format("multi-factor account_id ~s is not in your account tree", [AccountId])),
            failed_multi_factor_validation(AuthModule, ErrMsg, Context)
    end.

-spec failed_multi_factor_validation(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
failed_multi_factor_validation(AuthModule, ErrMsg, Context) ->
    KeyPath = <<"auth_modules.", AuthModule/binary, ".account_id">>,
    JObj = cb_context:validation_errors(Context),
    ErrorJObj = kz_json:from_list_recursive([{KeyPath, [{<<"required">>, [{<<"message">>, ErrMsg}]}]}]),
    Setters = [{fun cb_context:set_resp_error_code/2, 400}
              ,{fun cb_context:set_resp_status/2, 'error'}
              ,{fun cb_context:set_resp_error_msg/2, <<"validation failed">>}
              ,{fun cb_context:set_resp_data/2, kz_json:new()}
              ,{fun cb_context:set_validation_errors/2, kz_json:merge_jobjs(ErrorJObj, JObj)}
              ],
    cb_context:setters(Context, Setters).

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
