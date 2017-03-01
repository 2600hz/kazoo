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
        ]).

-include("crossbar.hrl").

-define(TOKEN_AUTH_EXPIRY, kapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

-define(ACCOUNT_AUTH_CONFIG_ID, <<"kazoo_auth_configs">>).

-define(SHOULD_LOG_FAILED,
        kapps_config:get_is_true(?AUTH_CONFIG_CAT, <<"log_failed_login_attempts">>, 'false')
       ).
-define(SYSTEM_AUTH_CONFIG,
        kapps_config:get_json(?AUTH_CONFIG_CAT, <<"auth_modules">>, kz_json:new())
       ).

-define(ATTEMPT_LOG_ID(Year, Month),
        <<(kz_term:to_binary(Year))/binary
          ,(kz_time:pad_month(Month))/binary
          ,"-"
          ,(kz_binary:rand_hex(16))/binary
        >>
       ).

-spec create_auth_token(cb_context:context(), atom()) ->
                               cb_context:context().
create_auth_token(Context, AuthModule) ->
    JObj = cb_context:doc(Context),
    case kz_json:is_empty(JObj) of
        'true' ->
            lager:debug("empty doc, no auth token created"),
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            create_auth_token(Context, AuthModule, JObj)
    end.

-spec create_auth_token(cb_context:context(), atom(), kz_json:object()) ->
                               cb_context:context().
create_auth_token(Context, AuthModule, JObj) ->
    Data = cb_context:req_data(Context),

    AccountId = kz_json:get_first_defined([<<"account_id">>, [<<"Claims">>, <<"account_id">>]], JObj),
    OwnerId = kz_json:get_first_defined([<<"owner_id">>, [<<"Claims">>, <<"owner_id">>]], JObj),
    Expiration = case ?TOKEN_AUTH_EXPIRY of
                     TokenExp when TokenExp > 0 -> erlang:system_time('seconds') + TokenExp;
                     _ -> 'undefined'
                 end,

    Method = kz_term:to_binary(AuthModule),
    Claims = props:filter_undefined(
               [{<<"account_id">>, AccountId}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"as">>, kz_json:get_value(<<"as">>, Data)}
               ,{<<"method">>, Method}
               ,{<<"exp">>, Expiration}
               ,{<<"mfa_resp">>, kz_json:get_ne_value(<<"multi_factor_response">>, Data)}
                | kz_json:to_proplist(kz_json:get_value(<<"Claims">>, JObj, kz_json:new()))
               ]),
    case maybe_create_token(Claims, Method) of
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
            crossbar_util:response(Resp, cb_context:setters(Context, Setters));
        {'error', R} ->
            lager:debug("could not create new local auth token, ~s", [kz_term:to_binary(R)]),
            cb_context:add_system_error('invalid_credentials', Context);
        {'error', Reason, RespJObj} ->
            lager:debug("authentication factor module requested from client to preform second-factor authentication, returning ~s with response ~p"
                       ,[kz_term:to_binary(Reason), RespJObj]
                       ),
            MFAReq = kz_json:from_list(
                       [{<<"message">>, <<"client needs to preform second-factor authentication">>}
                       ,{<<"multi_factor_request">>, RespJObj}
                       ]
                      ),
            cb_context:add_system_error(401, 'invalid_credentials', MFAReq, Context)
    end.

-spec maybe_create_token(kz_proplist(), ne_binary()) ->
                                {'ok', ne_binary()} |
                                {'error', any()} |
                                {'error', any(), any()}.
maybe_create_token(Claims, Method) ->
    AuthConfig = auth_config(props:get_ne_binary_value(<<"account_id">>, Claims)),
    maybe_create_token(Claims, AuthConfig, Method, is_auth_module_enabled(Method, AuthConfig)).

-spec maybe_create_token(kz_proplist(), kz_json:object(), ne_binary(), boolean()) ->
                                {'ok', ne_binary()} |
                                {'error', any()} |
                                {'error', any(), any()}.
maybe_create_token(Claims, AuthConfig, Method, 'true') ->
    maybe_multi_factor_auth(Claims, AuthConfig, Method, is_multi_factor_enabled(Claims, AuthConfig));
maybe_create_token(_Claims, _AuthConfig, Method, 'false') ->
    {'error', <<"authentication module ", Method/binary, " is disabled">>}.

-spec maybe_multi_factor_auth(kz_proplist(), kz_json:object(), ne_binary(), boolean()) ->
                                     {'ok', ne_binary()} |
                                     {'error', any()} |
                                     {'error', any(), any()}.
maybe_multi_factor_auth(Claims, _AuthConfig, _Method, 'false') ->
    kz_auth:create_token(Claims);
maybe_multi_factor_auth(Claims, AuthConfig, Method, 'true') ->
    lager:debug("auth module ~s is configured to use multi factor", [Method]),
    NewClaims = props:filter_undefined(
                  [{<<"mfa_options">>, mfa_options(Method, AuthConfig)}
                   | Claims
                  ]),
    case kz_mfa_auth:authenticate(NewClaims) of
        {'ok', 'authenticated'} ->
            lager:debug("multi factor authentication was successful, creating local auth token"),
            kz_auth:create_token(Claims);
        {'error', 'no_provider'} ->
            Reason = <<"no multi factor authentication provider is configured">>,
            lager:debug("~s, creating local auth token", [Reason]),
            maybe_log_failed_mfa_auth(NewClaims, AuthConfig, Method, Reason),
            kz_auth:create_token(Claims);
        {'error', Reason}=Error ->
            maybe_log_failed_mfa_auth(NewClaims, AuthConfig, Method, Reason),
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

maybe_db_token(AuthToken) ->
    kz_datamgr:open_cache_doc(?KZ_TOKEN_DB, AuthToken).

-spec maybe_log_failed_mfa_auth(kz_proplist(), kz_json:object(), ne_binary(), atom() | ne_binary()) -> 'ok'.
maybe_log_failed_mfa_auth(Claims, AuthConfig, Method, Reason) ->
    case should_log_failed_attempts(AuthConfig, Method) of
        'true' -> log_failed_mfa_attempts(Claims, AuthConfig, Reason);
        'false' -> 'ok'
    end.

-spec log_failed_mfa_attempts(kz_proplist(), kz_json:object(), atom() | ne_binary()) -> 'ok'.
log_failed_mfa_attempts(Claims, AuthConfig, Reason) ->
    AccountId = props:get_value(<<"account_id">>, Claims),

    Now = kz_time:current_tstamp(),
    ModDb = kz_util:format_account_mod_id(AccountId, Now),

    {Year, Month, _} = erlang:date(),
    LogId = ?ATTEMPT_LOG_ID(Year, Month),

    Doc = kz_json:from_list(
            props:filter_undefined(
              [{<<"_id">>, LogId}
              ,{<<"auth_type">>, <<"multi_factor">>}
              ,{<<"debug_type">>, <<"failed">>}
              ,{<<"message">>, kz_term:to_binary(Reason)}
              ,{<<"auth_config_origin">>, kz_json:get_value(<<"from">>, AuthConfig)}
              ,{<<"mfa_config_origin">>
               ,props:get_value([<<"mfa_options">>, <<"account_id">>], Claims, <<"system">>)
               }
              ,{<<"timestamp">>, Now}
              ,{<<"pvt_account_db">>, ModDb}
              ,{<<"pvt_account_id">>, AccountId}
              ,{<<"pvt_type">>, <<"login_attempt">>}
              ,{<<"pvt_created">>, Now}
              ,{<<"pvt_modified">>, Now}
              ]
             )
           ),
    _ = kazoo_modb:save_doc(ModDb, Doc),
    'ok'.

-spec should_log_failed_attempts(kz_json:object(), ne_binary()) -> boolean().
should_log_failed_attempts(AuthConfig, Method) ->
    case kz_json:is_true(method_config_path(Method, <<"log_failed_login_attempts">>), AuthConfig, 'undefined') of
        'undefined' -> ?SHOULD_LOG_FAILED;
        Boolean -> Boolean
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Check if is authenticator module enabled or not
%% @end
%%--------------------------------------------------------------------
-spec is_auth_module_enabled(ne_binary(), kz_json:object()) -> boolean().
is_auth_module_enabled(Method, Config) ->
    kz_json:is_true(method_config_path(Method, <<"enabled">>), Config, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc Get Account's authentication configuration
%%  1. If Account has kazoo_auth_config use it
%%  2. If account is Master Account, get configs from system_config
%%  3. If Account doesn't have configs defined:
%%      3.1. Is the Account reseller?
%%          3.1.1. Get configs from system_config
%%      3.2. If not reseller get parent's AccountId and go to (1)
%% @end
%%--------------------------------------------------------------------
-spec auth_config(kz_json:object()) -> kz_json:object().
auth_config(AccountId) ->
    account_auth_config(AccountId, master_account_id()).

%%--------------------------------------------------------------------
%% @private
%% get account's configs and walk account's up to Account's reseller
%% then system_config if couldn't find configs in account's
%% @end
%%--------------------------------------------------------------------
-spec account_auth_config(api_binary(), api_binary()) -> kz_json:object().
account_auth_config('undefined', _MasterId) ->
    system_auth_config();
account_auth_config(MasterId, ?NE_BINARY = MasterId) ->
    lager:debug("reached to the master account, getting auth configs from system_configs"),
    system_auth_config();
account_auth_config(AccountId, MasterId) ->
    IsReseller = kz_services:is_reseller(AccountId),
    account_auth_config(AccountId, MasterId, IsReseller).

-spec account_auth_config(ne_binary(), api_binary(), boolean()) -> kz_json:object().
account_auth_config(AccountId, MasterId, IsReseller) ->
    auth_config_from_doc(account_auth_config(AccountId), AccountId, MasterId, IsReseller).

-spec account_auth_config(ne_binary()) -> kz_std_return().
account_auth_config(AccountId) ->
    kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ?ACCOUNT_AUTH_CONFIG_ID).

%%--------------------------------------------------------------------
%% @private
%% @doc Look into database result, if it's not empty use it otherwise
%% either go to parent's account or system config
%% @end
%%--------------------------------------------------------------------
-spec auth_config_from_doc(kz_std_return(), ne_binary(), api_binary(), boolean()) -> kz_json:object().
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
    lager:debug("failed to get auth configs for account ~s getting parent account configs"),
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
    From = kz_json:get_value(<<"from">>, AuthConfig),
    case kz_json:get_value(method_mfa_path(Method, <<"configuration_id">>), AuthConfig) of
        ?NE_BINARY=Id ->
            maybe_system_config(From, Id);
        _Other -> 'undefined'
    end.

maybe_system_config(<<"system">>, _Id) ->
    %% auth config is from system, using default mfa config
    %% (see is_multi_factor_enabled/2 TODO comment)
    'undefined';
maybe_system_config(?NE_BINARY=AccountId, Id) ->
    [{<<"account_id">>, AccountId}
    ,{<<"config_id">>, Id}
    ].


%%--------------------------------------------------------------------
%% @private
%% @doc checks if auth module is configured to do mutli factor auth
%% @end
%%--------------------------------------------------------------------
-spec is_multi_factor_enabled(kz_proplist(), kz_json:object()) -> boolean().
is_multi_factor_enabled(Claims, AuthConfig) ->
    Method = props:get_ne_binary_value(<<"method">>, Claims),
    AccountId = props:get_value(<<"account_id">>, Claims),
    ConfigsFrom = kz_json:get_value(<<"from">>, AuthConfig),
    IncludeSubAccounts = kz_json:is_true(method_mfa_path(Method, <<"include_subaccounts">>), AuthConfig),

    %% TODO: is it good to check to see if there is a config_id here to force account to
    %% use their own mfa config to prevent the child account take advatnage of the system's default provider?
    kz_json:is_true(method_mfa_path(Method, <<"enabled">>), AuthConfig)
        andalso account_mfa_allowed(master_account_id(), AccountId, ConfigsFrom, IncludeSubAccounts).

%%--------------------------------------------------------------------
%% @private
%% @doc If the configuration document is pulled from a parent account
%% then should look for a property 'multi_factor.include_subaccounts',
%% which if 'false' should cause authentication to proceed normally.
%% @end
%%--------------------------------------------------------------------
-spec account_mfa_allowed(api_binary(), api_binary(), ne_binary(), boolean()) -> boolean().
account_mfa_allowed(?NE_BINARY=Master, ?NE_BINARY=Master, _, _) -> 'true';
account_mfa_allowed(_Master, _AccountId, <<"system">>, _IncludeSubAcc) -> 'true';
account_mfa_allowed(_Master, AccountId, AccountId, _IncludeSubAcc) -> 'true';
account_mfa_allowed(_Master, _AccountId, _ParentAccount, IncludeSubAcc) -> IncludeSubAcc.

-spec method_config_path(ne_binary(), ne_binary()) -> ne_binaries().
method_config_path(Method, Key) ->
    [<<"auth_modules">>, Method, Key].

-spec method_mfa_path(ne_binary(), ne_binary()) -> ne_binaries().
method_mfa_path(Method, Key) ->
    [<<"auth_modules">>, Method, <<"multi_factor">>, Key].
