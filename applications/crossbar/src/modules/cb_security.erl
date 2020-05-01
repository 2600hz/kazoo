%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Kazoo authentication configuration API endpoint
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context) ->
    authorize_list_available_module(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token()) -> 'true'.
authorize(_Context, _) -> 'true'.

-spec authorize(cb_context:context(), path_token(), path_token()) -> 'true'.
authorize(_Context, _, _) -> 'true'.

-spec authorize_list_available_module(cb_context:context(), req_nouns(), http_method()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize_list_available_module(_Context, [{<<"security">>, []}], ?HTTP_GET) ->
    'true';
authorize_list_available_module(Context, [{<<"security">>, []}], _) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize_list_available_module(_Context, _Nouns, _Verb) ->
    'true'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ATTEMPTS) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?ATTEMPTS, _AttemptId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /security => []
%%    /security/foo => [<<"foo">>]
%%    /security/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?ATTEMPTS) -> 'true';
resource_exists(_ConfigId) -> 'false'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?ATTEMPTS, _AttemptId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /security might load a list of auth objects
%% /security/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_auth_configs(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS) ->
    crossbar_view:load_modb(Context, ?CB_LIST_ATTEMPT_LOG, [{mapper, crossbar_view:get_value_fun()}]).

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

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            maybe_flush_config(C1),
            RespJObj = maybe_add_multi_factor_metadata(cb_context:resp_data(Context)),
            cb_context:set_resp_data(Context, RespJObj);
        _ -> C1
    end.


%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            maybe_flush_config(C1),
            RespJObj = maybe_add_multi_factor_metadata(cb_context:resp_data(Context)),
            cb_context:set_resp_data(Context, RespJObj);
        _ -> C1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_doc:delete(Context, ?HARD_DELETE).

maybe_flush_config(Context) ->
    case cb_context:fetch(Context, 'flush', 'false') of
        'true' ->
            kapps_account_config:flush(cb_context:account_id(Context), ?AUTH_CONFIG_CAT, <<"hierarchy_merge">>);
        'false' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, ?AVAILABLE_AUTH_METHODS}
              ],
    cb_context:setters(Context, Setters).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    ConfigId = kapps_config_util:account_doc_id(?AUTH_CONFIG_CAT),
    C1 = crossbar_doc:load(ConfigId, Context, ?TYPE_CHECK_OPTION(<<"account_config">>)),
    case cb_context:resp_status(C1) of
        'success' ->
            add_inherited_config(cb_context:set_resp_data(C1, kz_json:delete_key(<<"id">>, cb_context:resp_data(C1))));
        _ ->
            add_inherited_config(cb_context:set_resp_data(C1, kz_json:new()))
    end.

-spec add_inherited_config(cb_context:context()) -> cb_context:context().
add_inherited_config(Context) ->
    Props = [{<<"account">>
             ,maybe_add_multi_factor_metadata(cb_context:resp_data(Context))
             }
            ,{<<"inherited_config">>
             ,maybe_add_multi_factor_metadata(kz_json:from_list([{<<"auth_modules">>, crossbar_auth:get_inherited_config(Context)}]))
             }
            ],
    crossbar_doc:handle_json_success(kz_json:from_list(Props), Context).

-spec maybe_add_multi_factor_metadata(kz_json:object()) -> kz_json:object().
maybe_add_multi_factor_metadata(AuthConfig) ->
    Fun = fun(K, V, Acc) ->
                  case kz_json:get_value(<<"multi_factor">>, V) of
                      'undefined' -> Acc;
                      _MFA -> add_multi_factor_metadata(K, V, Acc)
                  end
          end,
    kz_json:foldl(Fun, AuthConfig, kz_json:get_value(<<"auth_modules">>, AuthConfig, kz_json:new())).

-spec add_multi_factor_metadata(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
add_multi_factor_metadata(AuthModule, JObj, AuthConfig) ->
    AccountId = kz_json:get_value([<<"multi_factor">>, <<"account_id">>], JObj),
    ConfigId = kz_json:get_value([<<"multi_factor">>, <<"configuration_id">>], JObj),

    Path = [<<"auth_modules">>, AuthModule, <<"multi_factor">>, <<"_read_only">>],
    case kz_term:is_not_empty(AccountId)
        andalso kz_term:is_not_empty(ConfigId)
        andalso get_metadata(AccountId, ConfigId)
    of
        'false' -> set_as_system(AuthConfig, Path);
        'undefined' -> set_as_system(AuthConfig, Path);
        Metadata ->
            kz_json:set_value(Path, Metadata, AuthConfig)
    end.

-spec get_metadata(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
get_metadata(AccountId, ConfigId) ->
    case kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), ConfigId) of
        {'ok', JObj} ->
            kz_json:from_list(
              [{<<"name">>, kzd_multi_factor_provider:name(JObj)}
              ,{<<"provider_name">>, kzd_multi_factor_provider:provider_name(JObj)}
              ]);
        _ -> 'undefined'
    end.

-spec set_as_system(kz_json:object(), kz_json:path()) -> kz_json:object().
set_as_system(AuthConfig, Path) ->
    Default = kz_json:from_list(
                [{<<"name">>, <<"Default System Provider">>}
                ,{<<"provider_name">>, kz_mfa_auth:default_provider()}
                ]),
    kz_json:set_value(Path, Default, AuthConfig).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    SchemaName = kapps_config_util:account_schema_name(?AUTH_CONFIG_CAT),
    cb_context:validate_request_data(SchemaName, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    C1 = cb_context:store(Context, <<"orig_req_data">>, cb_context:req_data(Context)),
    crossbar_doc:patch_and_validate(Id, C1, fun update/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
    kz_json:foldl(fun check_multi_factor_setting/3, Context, kz_json:get_value(<<"auth_modules">>, ReqData, kz_json:new())).

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

-spec has_account_id_or_db(kz_json:object()) -> boolean() | kz_term:ne_binary().
has_account_id_or_db(JObj) ->
    case kz_json:get_ne_binary_value([<<"multi_factor">>, <<"account_id">>], JObj) of
        'undefined' -> <<"setting multi-factor configuration_id needs setting account_id">>;
        _AccountId -> 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc Only allow to set the configuration id if the account who has the configuration is
%% in the tree
%% * if master is setting the config, allow
%% * if the account is the same authenticated account, allow
%% * if authenticated account is parent and wants to set its descendant's, allow
%% * if a include_subaccounts, allow a child account set its parent's config
%% @end
%%------------------------------------------------------------------------------
-spec check_account_hierarchy(kz_term:ne_binary(), kz_json:object(), cb_context:context()) -> cb_context:context().
check_account_hierarchy(AuthModule, JObj, Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = kz_json:get_ne_binary_value([<<"multi_factor">>, <<"account_id">>], JObj),

    case cb_context:is_superduper_admin(AuthAccountId)
        orelse kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId, 'true')
        orelse kz_json:is_false([<<"multi_factor">>, <<"include_subaccounts">>], JObj)
        orelse kzd_accounts:is_in_account_hierarchy(AccountId, AuthAccountId)
    of
        'true' -> Context;
        'false' ->
            ErrMsg = kz_term:to_binary(io_lib:format("multi-factor account_id ~s is not in your account tree", [AccountId])),
            failed_multi_factor_validation(AuthModule, ErrMsg, Context)
    end.

-spec failed_multi_factor_validation(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

%%------------------------------------------------------------------------------
%% @doc Load a login attempt log from MODB
%% @end
%%------------------------------------------------------------------------------
-spec read_attempt_log(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read_attempt_log(?MATCH_MODB_PREFIX(Year, Month, _)=AttemptId, Context) ->
    crossbar_doc:load(AttemptId
                     ,cb_context:set_db_name(Context
                                            ,kzs_util:format_account_id(cb_context:account_id(Context), Year, Month)
                                            )
                     ,?TYPE_CHECK_OPTION(?AUTH_ATTEMPT_TYPE)
                     ).
