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

-define(AUTH_CONFIG_ID, <<"kazoo_auth_configs">>).

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

    Claims = props:filter_undefined(
               [{<<"account_id">>, AccountId}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"as">>, kz_json:get_value(<<"as">>, Data)}
               ,{<<"method">>, kz_term:to_binary(AuthModule)}
               ,{<<"exp">>, Expiration}
               ,{<<"mfa_resp">>, kz_json:get_ne_value(<<"mfa_service_response">>, Data)}
                | kz_json:to_proplist(kz_json:get_value(<<"Claims">>, JObj, kz_json:new()))
               ]),
    case maybe_create_token(Claims) of
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
            lager:debug("authentication factor module requests from client to preform second-factor authentication, returning ~s with response ~p"
                       ,[kz_term:to_binary(Reason), RespJObj]
                       ),
            MFAReq = kz_json:from_list(
                       [{<<"message">>, <<"needs multi factor authentication result to verify">>}
                       ,{<<"mfa_request">>, RespJObj}
                       ]
                      ),
            cb_context:add_system_error(401
                                       ,'invalid_credentials'
                                       ,MFAReq
                                       ,Context
                                       )
    end.

-spec maybe_create_token(kz_proplist()) ->
                                {'ok', ne_binary()} |
                                {'error', any()} |
                                {'error', any(), any()}.
maybe_create_token(Claims) ->
    AuthConfigs = auth_configs(props:get_ne_binary_value(<<"account_id">>, Claims)),
    AuthModule = props:get_ne_binary_value(<<"method">>, Claims),
    maybe_create_token(Claims, AuthConfigs, is_auth_module_enabled(AuthModule, AuthConfigs)).

-spec maybe_create_token(kz_proplist(), kz_json:object(), boolean()) ->
                                {'ok', ne_binary()} |
                                {'error', any()} |
                                {'error', any(), any()}.
maybe_create_token(Claims, AuthConfigs, 'true') ->
    maybe_multi_factor_auth(Claims, AuthConfigs, is_multi_factor_enabled(Claims, AuthConfigs));
maybe_create_token(Claims, _AuthConfigs, 'false') ->
    AuthModule = props:get_ne_binary_value(<<"method">>, Claims),
    {'error', <<"authentication module ", (kz_term:to_binary(AuthModule))/binary, " is disabled">>}.

-spec maybe_multi_factor_auth(kz_proplist(), kz_json:object(), boolean()) ->
                                     {'ok', ne_binary()} |
                                     {'error', any()} |
                                     {'error', any(), any()}.
maybe_multi_factor_auth(Claims, _AuthConfigs, 'false') ->
    kz_auth:create_token(Claims);
maybe_multi_factor_auth(Claims, AuthConfigs, 'true') ->
    NewClaims = props:filter_undefined(
                  [{<<"mfa_options">>, mfa_options(AuthConfigs)}
                   | Claims
                  ]),
    case kz_mfa_auth:authenticate(NewClaims) of
        {'ok', 'authenticated'} ->
            lager:debug("multi factor authentication was successful, creating local auth token"),
            kz_auth:create_token(Claims);
        {'error', 'no_provider'} ->
            lager:debug("multi factor authentication is not configured, creating local auth token"),
            maybe_log_failed_mfa_auth(NewClaims, AuthConfigs, 'no_provider'),
            kz_auth:create_token(Claims);
        %% {'error', 'provider_disabled'} ->
        %%     lager:debug("multi factor authentication is disabled, creating local auth token"),
        %%     kz_auth:create_token(Claims);
        {'error', Reason}=Error ->
            maybe_log_failed_mfa_auth(NewClaims, AuthConfigs, Reason),
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

-spec maybe_log_failed_mfa_auth(kz_proplist(), kz_json:object(), atom() | ne_binary()) -> 'ok'.
maybe_log_failed_mfa_auth(Claims, AuthConfigs, Reason) ->
    case should_log_failed_attempts(AuthConfigs) of
        'true' -> log_failed_mfa_attempts(Claims, AuthConfigs, Reason);
        'false' -> 'ok'
    end.

-spec log_failed_mfa_attempts(kz_proplist(), kz_json:object(), atom() | ne_binary()) -> 'ok'.
log_failed_mfa_attempts(Claims, AuthConfigs, Reason) ->
    AccountId = props:get_value(<<"account_id">>, Claims),

    Now = kz_time:current_tstamp(),
    ModDb = kz_util:format_account_mod_id(AccountId, Now),

    Doc = kz_json:from_list(
            props:filter_undefined(
              [{<<"auth_type">>, <<"multi_factor">>}
              ,{<<"reason">>, kz_term:to_binary(Reason)}
              ,{<<"auth_config_origin">>, kz_json:get_value(<<"from">>, AuthConfigs)}
              ,{<<"mfa_config_origin">>, props:get_value([<<"mfa_options">>, <<"account_id">>], Claims)}
              ,{<<"pvt_account_db">>, ModDb}
              ,{<<"pvt_account_id">>, AccountId}
              ,{<<"pvt_type">>, <<"login_attempt">>}
              ,{<<"pvt_created">>, Now}
              ,{<<"pvt_modified">>, Now}
              ]
             )
           ),
    _ = kazoo_modb:save_doc(ModDb, Doc, [{'publish_change_notice', 'false'}]),
    'ok'.

-spec should_log_failed_attempts(kz_json:object()) -> boolean().
should_log_failed_attempts(AuthConfigs) ->
    case kz_json:is_true([<<"multi_factor">>, <<"log_failed_login_attempts">>], AuthConfigs) of
        'undefined' ->
            kapps_config:get_is_true(?AUTH_CONFIG_CAT
                                    ,[?AUTH_CONFIG_ID, <<"log_failed_login_attempts">>]
                                    ,'false'
                                    );
        Boolean -> Boolean
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Check if is authenticator module enabled or not
%% @end
%%--------------------------------------------------------------------
-spec is_auth_module_enabled(api_ne_binary(), kz_json:object()) -> boolean().
is_auth_module_enabled('undefined', _Configs) -> 'true';
is_auth_module_enabled(AuthModule, Configs) ->
    kz_json:is_true([AuthModule, <<"enabled">>]
                   ,Configs
                   ,'true'
                   ).

%%--------------------------------------------------------------------
%% @private
%% @doc Get Account's authentication configuration
%%  1. If Account has kazoo_auth_configs use it
%%  2. If account is Master Account, get configs from system_config
%%  3. If Account doesn't have configs defined:
%%      3.1. Is the Account reseller?
%%          3.1.1. Get configs from system_config
%%      3.2. If not reseller get parent's AccountId and go to (1)
%% @end
%%--------------------------------------------------------------------
-spec auth_configs(kz_json:object()) -> kz_json:object().
auth_configs(AccountId) ->
    account_auth_configs(AccountId, master_account_id()).

%%--------------------------------------------------------------------
%% @private
%% get account's configs and walk account's up to Account's reseller
%% then system_config if couldn't find configs in account's
%% @end
%%--------------------------------------------------------------------
-spec account_auth_configs(api_binary(), api_binary()) -> kz_json:object().
account_auth_configs('undefined', _MasterId) ->
    system_auth_config();
account_auth_configs(MasterId, ?NE_BINARY = MasterId) ->
    lager:debug("reached the master account, get system wide configs"),
    system_auth_config();
account_auth_configs(AccountId, MasterId) ->
    IsReseller = kz_services:is_reseller(AccountId),
    account_auth_configs(AccountId, MasterId, IsReseller).

-spec account_auth_configs(ne_binary(), api_binary(), boolean()) -> kz_json:object().
account_auth_configs(AccountId, MasterId, IsReseller) ->
    auth_configs_from_doc(account_auth_configs(AccountId), AccountId, MasterId, IsReseller).

-spec account_auth_configs(ne_binary()) -> kz_std_return().
account_auth_configs(AccountId) ->
    kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ?AUTH_CONFIG_ID).

%%--------------------------------------------------------------------
%% @private
%% @doc Look into database result, if it's not empty use it otherwise
%% either go to parent's account or system config
%% @end
%%--------------------------------------------------------------------
-spec auth_configs_from_doc(kz_std_return(), ne_binary(), api_binary(), boolean()) -> kz_json:object().
auth_configs_from_doc({'ok', Configs}, AccountId, MasterId, _IsReseller) ->
    case kz_json:is_empty(Configs) of
        'true' -> account_auth_configs(account_parent(AccountId), MasterId);
        'false' -> kz_json:set_value(<<"from">>, AccountId, Configs)
    end;
auth_configs_from_doc({'error', Reason}, _AccountId, _MasterId, 'true') ->
    Reason =:= 'not_found'
        andalso lager:debug("no auth configs found for reseller account ~s getting system wide configs"),
    Reason =/= 'not_found'
        andalso lager:debug("failed to get auth configs for reseller account ~s getting system wide configs"),
    system_auth_config();
auth_configs_from_doc({'error', _Reason}, AccountId, MasterId, 'false') ->
    lager:debug("failed to get auth configs for account ~s getting parent account configs"),
    account_auth_configs(account_parent(AccountId), MasterId).

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
    kz_json:set_value(<<"from">>
                     ,<<"system">>
                     ,kapps_config:get_json(?AUTH_CONFIG_CAT, ?AUTH_CONFIG_ID, kz_json:new())
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
%% @doc Extract multi factor configuration_id from AuthConfigs
%% @end
%%--------------------------------------------------------------------
-spec mfa_options(kz_json:object()) -> 'undefined' | kz_proplist().
mfa_options(AuthConfigs) ->
    From = kz_json:get_value(<<"from">>, AuthConfigs),
    case kz_json:get_value([<<"multi_factor">>, <<"configuration_id">>], AuthConfigs) of
        ?NE_BINARY=Id ->
            maybe_system_config(From, Id);
        _Other -> 'undefined'
    end.

maybe_system_config(<<"system">>, _Id) ->
    %% auth config is from system, using default mfa system config
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
is_multi_factor_enabled(Claims, AuthConfigs) ->
    AuthModule = props:get_ne_binary_value(<<"method">>, Claims),
    AccountId = props:get_value(<<"account_id">>, Claims),
    ConfigsFrom = kz_json:get_value(<<"from">>, AuthConfigs),
    IncludeSubAccounts = kz_json:get_value(<<"from">>, AuthConfigs, 'false'),

    AuthModule =/= 'undefined'
        andalso kz_json:is_true([AuthModule, <<"multi_factor">>, <<"enabled">>]
                               ,AuthConfigs
                               ,'false'
                               )
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
