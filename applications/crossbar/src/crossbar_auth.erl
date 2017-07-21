%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(crossbar_auth).

-export([create_auth_token/2
        ,validate_auth_token/1, validate_auth_token/2
        ,authorize_auth_token/1
        ,log_success_auth/4, log_success_auth/5, log_success_auth/6
        ,log_failed_auth/4, log_failed_auth/5, log_failed_auth/6
        ]).

-include("crossbar.hrl").

-define(DEFAULT_AUTH_EXPIRY, kapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).
-define(TOKEN_AUTH_EXPIRY(Method, AuthConfig)
       ,kz_json:get_integer_value(method_config_path(Method, <<"token_auth_expiry">>), AuthConfig, ?DEFAULT_AUTH_EXPIRY)
       ).

-define(SHOULD_LOG_FAILED,
        kapps_config:get_is_true(?AUTH_CONFIG_CAT, <<"log_failed_attempts">>, 'false')
       ).
-define(SHOULD_LOG_SUCCESS,
        kapps_config:get_is_true(?AUTH_CONFIG_CAT, <<"log_successful_attempts">>, 'false')
       ).
-define(SYSTEM_AUTH_CONFIG,
        kapps_config:get_json(?AUTH_CONFIG_CAT, <<"auth_modules">>, kz_json:new())
       ).

-spec create_auth_token(cb_context:context(), atom()) ->
                               cb_context:context().
create_auth_token(Context, AuthModule) ->
    JObj = cb_context:doc(Context),
    Method = kz_term:to_binary(AuthModule),
    AccountId = kz_json:get_first_defined([<<"account_id">>, [<<"Claims">>, <<"account_id">>]], JObj),
    case kz_json:is_empty(JObj) of
        'true' ->
            Reason = <<"empty creds doc, no auth token created">>,
            lager:debug("~s", [Reason]),
            log_failed_auth(Method, <<"jwt_auth_token">>, Reason, Context, AccountId),
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            create_auth_token(Context, Method, JObj)
    end.

-spec create_auth_token(cb_context:context(), ne_binary(), kz_json:object()) ->
                               cb_context:context().
create_auth_token(Context, Method, JObj) ->
    Data = cb_context:req_data(Context),

    AccountId = kz_json:get_first_defined([<<"account_id">>, [<<"Claims">>, <<"account_id">>]], JObj),
    OwnerId = kz_json:get_first_defined([<<"owner_id">>, [<<"Claims">>, <<"owner_id">>]], JObj),

    AuthConfig = auth_config(AccountId),

    Expiration = case ?TOKEN_AUTH_EXPIRY(Method, AuthConfig) of
                     TokenExp when TokenExp > 0 -> erlang:system_time('seconds') + TokenExp;
                     _ -> 'undefined'
                 end,

    Claims = props:filter_undefined(
               [{<<"account_id">>, AccountId}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"as">>, kz_json:get_value(<<"as">>, Data)}
               ,{<<"method">>, Method}
               ,{<<"exp">>, Expiration}
               ,{<<"mfa_resp">>, kz_json:get_ne_value(<<"multi_factor_response">>, Data)}
                | kz_json:to_proplist(kz_json:get_value(<<"Claims">>, JObj, kz_json:new()))
               ]),

    IsMultiFactor = is_multi_factor_enabled(Claims, AuthConfig),

    case is_auth_module_enabled(Method, AuthConfig)
        andalso maybe_create_token(Context, Claims, AuthConfig, Method, IsMultiFactor)
    of
        'false' ->
            Reason = <<"authentication module ", Method/binary, " is disabled">>,
            log_failed_auth(Method, <<"jwt_auth_token">>, Reason, Context, AccountId, AuthConfig),
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        {'ok', Token} ->
            Setters = [{fun cb_context:set_auth_token/2, Token}
                      ,{fun cb_context:set_auth_doc/2, kz_json:from_list(Claims)}
                      ],
            Props = props:filter_undefined(
                      [{<<"account_id">>, AccountId}
                      ,{<<"owner_id">>, OwnerId}
                      ]),
            RespObj = kz_json:set_values(Props, kz_json:delete_key(<<"Claims">>, JObj)),
            Resp = crossbar_util:response_auth(RespObj, AccountId, OwnerId),

            lager:debug("created new local auth token: ~s", [kz_json:encode(Resp)]),

            log_success_auth(Method, <<"jwt_auth_token">>, <<"authentiaction resulted in token creation">>, Context, AccountId, AuthConfig),

            crossbar_util:response(Resp, cb_context:setters(Context, Setters));
        {'error', R} ->
            Reason = kz_term:to_binary(R),
            lager:debug("could not create new local auth token, ~s", [Reason]),
            log_failed_auth(Method, <<"jwt_auth_token">>, Reason, Context, AccountId, AuthConfig),

            cb_context:add_system_error('invalid_credentials', Context);
        {'error', Reason, RespJObj} ->
            lager:debug("authentication factor module requested that the client should preform second-factor authentication, returning ~s with response ~p"
                       ,[kz_term:to_binary(Reason), RespJObj]
                       ),
            MFAReq = kz_json:from_list(
                       [{<<"message">>, <<"client needs to preform second-factor authentication">>}
                       ,{<<"multi_factor_request">>, RespJObj}
                       ]
                      ),
            cb_context:add_system_error(401, 'invalid_credentials', MFAReq, Context)
    end.

-spec maybe_create_token(cb_context:context(), kz_proplist(), kz_json:object(), ne_binary(), boolean()) ->
                                {'ok', ne_binary()} |
                                {'error', any()} |
                                {'error', any(), any()}.
maybe_create_token(_Context, Claims, _AuthConfig, _Method, 'false') ->
    kz_auth:create_token(Claims);
maybe_create_token(Context, Claims, AuthConfig, Method, 'true') ->
    lager:debug("auth module ~s is configured to use multi factor", [Method]),

    AccountId = props:get_value(<<"account_id">>, Claims),
    NewClaims = props:filter_undefined(
                  [{<<"mfa_options">>, mfa_options(Method, AuthConfig)}
                   | Claims
                  ]),

    case kz_mfa_auth:authenticate(NewClaims) of
        {'ok', 'authenticated'} ->
            Reason = <<"multi factor authentication was successful">>,
            lager:debug("~s, creating local auth token", [Reason]),
            log_success_auth(Method, <<"multi_factor">>, Reason, Context, AccountId, AuthConfig),
            kz_auth:create_token(Claims);
        {'error', 'no_provider'} ->
            Reason = <<"no multi factor authentication provider is configured">>,
            lager:debug("~s, creating local auth token", [Reason]),
            log_failed_auth(Method, <<"multi_factor">>, Reason, Context, AccountId, AuthConfig),
            kz_auth:create_token(Claims);
        {'error', Reason}=Error ->
            log_failed_auth(Method, <<"multi_factor">>, kz_term:to_binary(Reason), Context, AccountId, AuthConfig),
            Error;
        {'error', 401, _MFAReq}=Retry -> Retry
    end.

-spec validate_auth_token(map() | ne_binary()) ->
                                 {ok, kz_json:object()} | {error, any()}.
-spec validate_auth_token(map() | ne_binary(), kz_proplist()) ->
                                 {ok, kz_json:object()} | {error, any()}.
validate_auth_token(Token) ->
    validate_auth_token(Token, []).

validate_auth_token(Token, Options) ->
    case kz_auth:validate_token(Token, Options) of
        {'error', 'no_jwt_signed_token'} -> maybe_db_token(Token);
        Other -> Other
    end.

-spec authorize_auth_token(map() | ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
authorize_auth_token(Token) ->
    kz_auth:authorize_token(Token).

-spec maybe_db_token(map() | ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
maybe_db_token(AuthToken) ->
    kz_datamgr:open_cache_doc(?KZ_TOKEN_DB, AuthToken).

%%--------------------------------------------------------------------
%% @private
%% @doc Check if is authenticator module is enabled or not
%% @end
%%--------------------------------------------------------------------
-spec is_auth_module_enabled(ne_binary(), kz_json:object()) -> boolean().
is_auth_module_enabled(Method, Config) ->
    kz_json:is_true(method_config_path(Method, <<"enabled">>), Config, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc Get Account's authentication configuration
%%  1. If account is Master Account, get config from system_config
%%  2. If Account has kazoo_auth_config use it
%%  3. If Account doesn't have kazoo_auth_config:
%%      3.1. Is the Account reseller?
%%          3.1.1. Get configs from system_config
%%      3.2. If not reseller, get parent's AccountId and go to (1)
%% @end
%%--------------------------------------------------------------------
-spec auth_config(api_ne_binary()) -> kz_json:object().
auth_config('undefined') ->
    system_auth_config();
auth_config(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    account_auth_config(AccountId, master_account_id()).

%%--------------------------------------------------------------------
%% @private
%% get account's configs and walk account's up to Account's reseller
%% then system_config if couldn't find configs in account's
%% @end
%%--------------------------------------------------------------------
-spec account_auth_config(api_ne_binary(), api_ne_binary()) -> kz_json:object().
account_auth_config('undefined', _MasterId) ->
    system_auth_config();
account_auth_config(MasterId, ?MATCH_ACCOUNT_RAW(MasterId)) ->
    lager:debug("reached to the master account, getting auth configs from system_configs"),
    system_auth_config();
account_auth_config(AccountId, MasterId) ->
    IsReseller = kz_services:is_reseller(AccountId),
    auth_config_from_doc(fetch_config(AccountId), AccountId, MasterId, IsReseller).

-spec fetch_config(ne_binary()) -> kz_std_return().
fetch_config(AccountId) ->
    kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ?ACCOUNT_AUTH_CONFIG_ID).

%%--------------------------------------------------------------------
%% @private
%% @doc Check database result, if it's not empty use it otherwise
%% go to parent's account or system config to fetch auth config
%% @end
%%--------------------------------------------------------------------
-spec auth_config_from_doc(kz_std_return(), ne_binary(), api_ne_binary(), boolean()) -> kz_json:object().
auth_config_from_doc({'ok', Configs}, AccountId, MasterId, _IsReseller) ->
    case kz_json:is_empty(Configs) of
        'true' -> account_auth_config(account_parent(AccountId), MasterId);
        'false' ->
            lager:debug("found auth config from ~s", [AccountId]),
            kz_json:set_value(<<"from">>, AccountId, Configs)
    end;
auth_config_from_doc({'error', Reason}, _AccountId, _MasterId, 'true') ->
    Reason =:= 'not_found'
        andalso lager:debug("no auth configs found for reseller account ~s getting system wide configs", [_AccountId]),
    Reason =/= 'not_found'
        andalso lager:debug("failed to get auth configs for reseller account ~s getting system wide configs", [_AccountId]),
    system_auth_config();
auth_config_from_doc({'error', _Reason}, AccountId, MasterId, 'false') ->
    lager:debug("failed to get auth configs for account ~s getting parent account configs", [AccountId]),
    account_auth_config(account_parent(AccountId), MasterId).

%%--------------------------------------------------------------------
%% @private
%% @doc Find Account's parent
%% @end
%%--------------------------------------------------------------------
-spec account_parent(ne_binary()) -> api_binary().
account_parent(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:parent_account_id(JObj);
        {'error', _R} ->
            lager:debug("failed to open parent account's ~s parent: ~p", [AccountId, _R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Get configs from system_config
%% @end
%%--------------------------------------------------------------------
-spec system_auth_config() -> kz_json:object().
system_auth_config() ->
    kz_json:from_list(
      [{<<"from">>, <<"system">>}
      ,{<<"auth_modules">>, ?SYSTEM_AUTH_CONFIG}
      ]
     ).

-spec master_account_id() -> api_ne_binary().
master_account_id() ->
    case kapps_util:get_master_account_id() of
        {'ok', Id} -> Id;
        {'error', _R} ->
            lager:debug("failed to find master account id: ~p", [_R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extract multi factor configuration_id from AuthConfig
%% @end
%%--------------------------------------------------------------------
-spec mfa_options(ne_binary(), kz_json:object()) -> 'undefined' | kz_proplist().
mfa_options(Method, AuthConfig) ->
    ConfigOrigin = kz_json:get_value(<<"from">>, AuthConfig),
    case kz_json:get_ne_binary_value(method_mfa_path(Method, <<"configuration_id">>), AuthConfig) of
        'undefined' -> 'undefined';
        ConfigId ->
            maybe_system_config(ConfigOrigin, ConfigId)
    end.

-spec maybe_system_config(ne_binary(), ne_binary()) -> kz_proplist().
maybe_system_config(<<"system">>, _ConfigId) ->
    'undefined';
maybe_system_config(AccountId, ConfigId) ->
    [{<<"account_id">>, AccountId}
    ,{<<"config_id">>, ConfigId}
    ].


%%--------------------------------------------------------------------
%% @private
%% @doc checks if auth module is configured to do mutli factor auth
%% @end
%%--------------------------------------------------------------------
-spec is_multi_factor_enabled(kz_proplist(), kz_json:object()) -> boolean().
is_multi_factor_enabled(Claims, AuthConfig) ->
    MasterId = master_account_id(),
    Method = props:get_ne_binary_value(<<"method">>, Claims),
    AccountId = props:get_value(<<"account_id">>, Claims),
    ConfigOrigin = kz_json:get_value(<<"from">>, AuthConfig),
    IncludeSubAccounts = kz_json:is_true(method_mfa_path(Method, <<"include_subaccounts">>), AuthConfig),

    kz_json:is_true(method_mfa_path(Method, <<"enabled">>), AuthConfig)
        andalso multi_factor_allowed_for_account(MasterId, AccountId, ConfigOrigin, IncludeSubAccounts).

%%--------------------------------------------------------------------
%% @private
%% @doc If the configuration document is pulled from a parent account
%% then should look for property 'multi_factor.include_subaccounts'
%% if it's 'false' then authentication should to proceed normally
%% without multi factor.
%% @end
%%--------------------------------------------------------------------
-spec multi_factor_allowed_for_account(api_binary(), api_binary(), ne_binary(), boolean()) -> boolean().
multi_factor_allowed_for_account(?NE_BINARY=Master, ?NE_BINARY=Master, _, _) -> 'true';
multi_factor_allowed_for_account(_Master, _AccountId, <<"system">>, _IncludeSubAcc) -> 'true';
multi_factor_allowed_for_account(_Master, AccountId, AccountId, _IncludeSubAcc) -> 'true';
multi_factor_allowed_for_account(_Master, _AccountId, _ParentAccount, IncludeSubAcc) -> IncludeSubAcc.

-spec method_config_path(ne_binary(), ne_binary()) -> ne_binaries().
method_config_path(Method, Key) ->
    [<<"auth_modules">>, Method, Key].

-spec method_mfa_path(ne_binary(), ne_binary()) -> ne_binaries().
method_mfa_path(Method, Key) ->
    [<<"auth_modules">>, Method, <<"multi_factor">>, Key].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Log successful authentiaction if configured to do so
%% @end
%%--------------------------------------------------------------------
-spec log_success_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context()) -> 'ok'.
log_success_auth(AuthModule, AuthType, Reason, Context) ->
    log_success_auth(AuthModule, AuthType, Reason, Context, 'undefined', 'undefined').

-spec log_success_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context(), api_binary()) -> 'ok'.
log_success_auth(AuthModule, AuthType, Reason, Context, AccountId) ->
    log_success_auth(AuthModule, AuthType, Reason, Context, AccountId, 'undefined').

-spec log_success_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context(), api_binary(), api_object()) -> 'ok'.
log_success_auth(AuthModule, AuthType, Reason, Context, 'undefined', AuthConfig) ->
    case cb_context:account_id(Context) of
        'undefined' -> 'ok';
        AccountId -> log_success_auth(AuthModule, AuthType, Reason, Context, AccountId, AuthConfig)
    end;
log_success_auth(AuthModule, AuthType, Reason, Context, AccountId, 'undefined') ->
    log_success_auth(AuthModule, AuthType, Reason, Context, AccountId, auth_config(AccountId));
log_success_auth(AuthModule, AuthType, Reason, Context, AccountId, AuthConfig) ->
    Method = kz_term:to_binary(AuthModule),
    case is_log_type_enabled(<<"success">>, Method, AuthConfig) of
        'false' -> 'ok';
        'true' ->
            log_attempts(Context, AccountId, AuthConfig, Method, <<"success">>, AuthType, Reason)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Log failed authentiaction if configured to do so
%% @end
%%--------------------------------------------------------------------
-spec log_failed_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context()) -> 'ok'.
log_failed_auth(AuthModule, AuthType, Reason, Context) ->
    log_failed_auth(AuthModule, AuthType, Reason, Context, 'undefined', 'undefined').

-spec log_failed_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context(), api_binary()) -> 'ok'.
log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId) ->
    log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId, 'undefined').

-spec log_failed_auth(atom() | ne_binary(), ne_binary(), ne_binary(), cb_context:context(), api_binary(), api_object()) -> 'ok'.
log_failed_auth(AuthModule, AuthType, Reason, Context, 'undefined', AuthConfig) ->
    case cb_context:account_id(Context) of
        'undefined' -> 'ok';
        AccountId -> log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId, AuthConfig)
    end;
log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId, 'undefined') ->
    log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId, auth_config(AccountId));
log_failed_auth(AuthModule, AuthType, Reason, Context, AccountId, AuthConfig) ->
    Method = kz_term:to_binary(AuthModule),
    case is_log_type_enabled(<<"failed">>, Method, AuthConfig) of
        'false' -> 'ok';
        'true' ->
            log_attempts(Context, AccountId, AuthConfig, Method, <<"failed">>, AuthType, Reason)
    end.

-spec is_log_type_enabled(ne_binary(), ne_binary(), kz_json:object()) -> boolean().
is_log_type_enabled(<<"failed">>, Method, AuthConfig) ->
    Key = method_config_path(Method, <<"log_failed_attempts">>),
    kz_json:is_true(Key, AuthConfig, ?SHOULD_LOG_FAILED);
is_log_type_enabled(<<"success">>, Method, AuthConfig) ->
    Key = method_config_path(Method, <<"log_successful_attempts">>),
    kz_json:is_true(Key, AuthConfig, ?SHOULD_LOG_SUCCESS).

-spec log_attempts(cb_context:context(), ne_binary(), kz_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
log_attempts(Context, AccountId, AuthConfig, Method, Status, AuthType, Reason) ->
    MultiFactorOrigin =
        case mfa_options(Method, AuthConfig) of
            'undefined' -> <<"system">>;
            Opts -> props:get_value([<<"mfa_options">>, <<"account_id">>], Opts, <<"system">>)
        end,
    Now = kz_time:current_tstamp(),
    MODB = kz_util:format_account_mod_id(AccountId, Now),

    LogId = kazoo_modb_util:modb_id(Now),

    Props = [{<<"_id">>, LogId}
            ,{<<"auth_type">>, cb_context:fetch(Context, 'auth_type', AuthType)}
            ,{<<"status">>, Status}
            ,{<<"auth_module">>, Method}
            ,{<<"message">>, Reason}
            ,{<<"auth_config_origin">>, kz_json:get_value(<<"from">>, AuthConfig)}
            ,{<<"multi_factor_config_origin">>, MultiFactorOrigin}
            ,{<<"client_headers">>, kz_json:from_list(cb_context:req_headers(Context))}
            ,{<<"client_ip">>, cb_context:client_ip(Context)}
            ,{<<"crossbar_request_id">>, cb_context:req_id(Context)}
            ,{<<"timestamp">>, Now}
            ],
    Doc0 = maybe_add_metadata(cb_context:doc(Context), kz_json:from_list(Props)),
    Doc = kz_doc:update_pvt_parameters(Doc0, MODB, [{'type', <<"login_attempt">>}
                                                   ,{'now', Now}
                                                   ]),
    _ = kazoo_modb:save_doc(MODB, maybe_add_metadata(cb_context:doc(Context), Doc)),
    'ok'.

-spec maybe_add_metadata(api_object(), kz_json:object()) -> kz_json:object().
maybe_add_metadata(ContextDoc, Doc) ->
    case kz_term:is_empty(ContextDoc) of
        'true' -> Doc;
        'false' -> kz_json:set_value(<<"metadata">>, ContextDoc, Doc)
    end.
