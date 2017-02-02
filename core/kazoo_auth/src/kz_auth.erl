%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_token/1
        ,validate_token/1, validate_token/2
        ,authorize_token/1
        ,access_code/1
        ,authenticate/1
        ,link/3
        ,unlink/1
        ]).

-include("kazoo_auth.hrl").

-define(AUTH_SETTINGS_ID, <<"kazoo_auth_settings">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create Authentication Token based on auth module configuration
%% @end
%%--------------------------------------------------------------------
-spec create_token(kz_proplist()) -> {'ok', ne_binary()} |
                                     {'error', 'algorithm_not_supported'} |
                                     {'error', 'unauthorized'}.
create_token(Claims) ->
    case is_token_creation_allowed(Claims) of
        'true' ->
            kz_auth_jwt:encode(include_claims(Claims));
        'false' -> {'error', 'unauthorized'}
    end.

-spec validate_token(ne_binary() | map()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
validate_token(Token) ->
    validate_token(Token, []).

-spec validate_token(ne_binary() | map(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
validate_token(JWToken, Options) ->
    case kz_auth_jwt:token(JWToken, Options) of
        #{verify_result := 'true'} = Token -> validate_claims(Token, Options);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end.

-spec access_code(kz_json:object()) ->
                         {'ok', kz_json:object()} |
                         {'error', any()}.
access_code(JObj) ->
    Code = kz_json:get_value(<<"code">>, JObj),
    AppId = kz_json:get_first_defined(?APPID_KEYS, JObj),
    RedirectURI = kz_json:get_first_defined(?REDIRECT_URI_KEYS, JObj, <<"postmessage">>),
    kz_auth_util:fetch_access_code(AppId, Code, RedirectURI).

-spec authenticate(map() | ne_binary() | kz_json:object()) ->
                          {'ok', kz_proplist()} |
                          {'error', any()}.
authenticate(JWTToken)
  when is_binary(JWTToken) ->
    case kz_auth_jwt:token(JWTToken) of
        #{verify_result := 'true'} = Token -> authenticate(Token);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end;
authenticate(Token)
  when is_map(Token) ->
    Routines = [fun kz_auth_token_util:add_application/1
               ,fun kz_auth_token_util:add_provider/1
               ,fun kz_auth_token_util:access_code/1
               ,fun kz_auth_token_util:verify/1
               ,fun kz_auth_token_util:id_token/1
               ,fun kz_auth_profile:token/1
               ,fun kz_auth_token_util:create_claims/1
               ],
    case authenticate_fold(Token, Routines) of
        #{claims := _Claims}=Token1 -> verify_claims(Token1);
        _ -> {'error', 'authentication_failed'}
    end;
authenticate(JObj) ->
    Token = kz_auth_util:map_keys_to_atoms(kz_json:to_map(JObj)),
    authenticate(Token#{original => JObj}).

-spec authorize_token(map() | ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
authorize_token(JWToken) ->
    case kz_auth_jwt:token(JWToken) of
        #{verify_result := 'true'} = Token -> ensure_claims(Token);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @private
%% @doc Check auth module settings to see if token generation is allowed
-spec is_token_creation_allowed(kz_proplist()) -> boolean().
is_token_creation_allowed(Claims) ->
    AccountId = props:get_ne_binary_value(<<"account_id">>, Claims),
    AuthCheckers = [fun is_module_enabled/2
                   ],
    case settings(AccountId) of
        'undefined' -> 'true';
        Settings ->
            lists:foldl(fun(Fun, ShouldContinue) ->
                                kz_term:is_true(ShouldContinue)
                                    andalso Fun(Claims, Settings)
                        end
                       ,'true'
                       ,AuthCheckers
                       )
    end.

%% @private
%% @doc Check if is authenticator module enabled or not
-spec is_module_enabled(kz_proplist(), kz_json:object()) -> boolean().
is_module_enabled(Claims, Settings) ->
    AuthModule = auth_module(props:get_ne_binary_value(<<"method">>, Claims)),
    AuthModule =/= 'undefined'
        andalso kz_term:is_true(
                  kz_json:get_first_defined([enabled_path(AuthModule), enabled_path(<<"*">>)]
                                           ,Settings
                                           ,'false'
                                           )
                 ).

-spec enabled_path(ne_binary()) -> list().
enabled_path(Module) -> [Module, <<"enabled">>].

-spec auth_module(api_binary()) -> api_binary().
auth_module('undefined') -> 'undefined';
auth_module(<<"cb_", AuthModule/binary>>) -> AuthModule;
auth_module(AuthModule) -> AuthModule.

%% @private
%% @doc Get Account's authentication configuration
%%  1. If Account has kazoo_auth_settings use it
%%  2. If account is Master Account, get settings from system_config
%%  3. If Account doesn't have settings defined:
%%      3.1. Is the Account reseller?
%%          3.1.1. Get settings from system_config
%%      3.2. If not reseller get parent's AccountId and go to (1)
-spec settings(kz_json:object()) -> kz_json:object().
settings(AccountId) ->
    MasterId = case kapps_util:get_master_account_id() of
                   {'ok', Id} -> Id;
                   {'error', _R} ->
                       lager:debug("failed to find master account id: ~p", [_R]),
                       'undefined'
               end,
    account_settings(AccountId, MasterId).

%% @private
%% get account's settings and walk account's up to Account's reseller then system_config if
%% couldn't find settings in account's
-spec account_settings(api_binary(), api_binary()) -> kz_json:object().
account_settings('undefined', _MasterId) ->
    settings_from_system();
account_settings(MasterId, ?NE_BINARY = MasterId) ->
    lager:debug("reached to the master account, get system wide settings"),
    settings_from_system();
account_settings(AccountId, MasterId) ->
    IsReseller = kz_services:is_reseller(AccountId),
    account_settings(AccountId, MasterId, IsReseller).

-spec account_settings(ne_binary(), api_binary(), boolean()) -> kz_json:object().
account_settings(AccountId, MasterId, IsReseller) ->
    settings_from_doc(account_settings(AccountId), AccountId, MasterId, IsReseller).

-spec account_settings(ne_binary()) -> db_ret().
account_settings(AccountId) ->
    kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ?AUTH_SETTINGS_ID).

-type db_ret() :: {'ok', kz_json:object()} | kz_datamgr:data_error().

%% @private
%% @doc Look into database result, if it's not empty use it otherwise either go to parent's account or system config
-spec settings_from_doc(db_ret(), ne_binary(), api_binary(), boolean()) -> kz_json:object().
settings_from_doc({'ok', Settings}, AccountId, MasterId, _IsReseller) ->
    case kz_json:is_empty(Settings) of
        'true' -> account_settings(account_parent(AccountId), MasterId);
        'false' -> Settings
    end;
settings_from_doc({'error', Reason}, _AccountId, _MasterId, 'true') ->
    Reason =:= 'not_found'
        andalso lager:debug("no auth settings found for reseller account ~s getting system wide settings"),
    Reason =/= 'not_found'
        andalso lager:debug("failed to get auth settings for reseller account ~s getting system wide settings"),
    settings_from_system();
settings_from_doc({'error', _Reason}, AccountId, MasterId, 'false') ->
    lager:debug("failed to get auth settings for account ~s getting parent account settings"),
    account_settings(account_parent(AccountId), MasterId).

%% @private
%% @doc Find out Account's parent
-spec account_parent(ne_binary()) -> api_binary().
account_parent(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:parent_account_id(JObj);
        {'error', _R} ->
            lager:debug("failed to open parent account's ~s parent: ~p", [AccountId, _R]),
            'undefined'
    end.

%% @private
%% @doc Get settings from system_config
-spec settings_from_system() -> kz_json:object().
settings_from_system() ->
    kapps_config:get_json(?CONFIG_CAT, <<"auth_modules">>, default_settings()).

%% @private
%% @doc Default Kazoo Auth settings (A catch-all module name to allow token generation if Auth is not configured)
default_settings() ->
    kz_json:from_list(
      [{<<"*">>
       ,kz_json:from_list([{<<"enabled">>, 'true'}])
       }
      ]
     ).

-spec verify_claims(map()) -> {'ok', kz_proplist()} | {'error', any()}.
verify_claims(#{claims := Claims}) ->
    {'ok', Claims}.

-spec validate_claims(map(), kz_proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
validate_claims(#{payload := #{<<"account_id">> := AccountId} = Payload}, Options) ->
    case props:get_value(<<"account_id">>, Options, AccountId) of
        AccountId ->
            Claims = kz_json:from_map(Payload),
            {'ok', Claims};
        _OtherAccountId ->
            {'error', {401, <<"account header mismatch">>}}
    end;
validate_claims(#{user_map := #{<<"pvt_account_id">> := AccountId
                               ,<<"pvt_owner_id">> := OwnerId
                               }
                 ,payload := Payload
                 }, Options) ->
    case props:get_value(<<"account_id">>, Options, AccountId) of
        AccountId ->
            Props = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, OwnerId}
                    ],
            case kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), OwnerId) of
                {'ok', _Doc} -> {'ok', kz_json:set_values(Props, kz_json:from_map(Payload))};
                _ -> {'error', {404, <<"mapped account does not exist">>}}
            end;
        _OtherAccountId ->
            {'error', {401, <<"account_header_mismatch">>}}
    end;
validate_claims(#{user_map := #{<<"pvt_accounts">> := Accounts}, payload := Payload}, Options) ->
    Keys = maps:keys(Accounts),
    case props:get_value(<<"account_id">>, Options) of
        'undefined' when length(Keys) =:= 1 ->
            [AccountId] = Keys,
            #{AccountId := #{<<"owner_id">> := OwnerId}} = Accounts,
            Props = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, OwnerId}
                    ],
            {'ok', kz_json:set_values(Props, kz_json:from_map(Payload))};
        'undefined' when length(Keys) > 1 ->
            [AccountId | _] = Keys,
            #{AccountId := #{<<"owner_id">> := OwnerId}} = Accounts,
            Props = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, OwnerId}
                    ],
            {'ok', kz_json:set_values(Props, kz_json:from_map(Payload))};
        AccountId when length(Keys) > 0 ->
            case lists:member(AccountId, Keys) of
                'true' ->
                    #{AccountId := #{<<"owner_id">> := OwnerId}} = Accounts,
                    Props = [{<<"account_id">>, AccountId}
                            ,{<<"owner_id">>, OwnerId}
                            ],
                    {'ok', kz_json:set_values(Props, kz_json:from_map(Payload))};
                'false' -> {'error', {401, <<"account header mismatch">>}}
            end;
        _OtherAccountId -> {'error', {404, <<"no associated account_id">>}}
    end;

validate_claims(#{}, _Options) -> {'error', {404, <<"no associated account_id">>}}.

-spec ensure_claims(map()) -> {'ok', kz_json:object()} | {'error', any()}.
ensure_claims(#{payload := Payload}) ->
    Claims = kz_json:from_map(Payload),
    {'ok', Claims}.

-spec include_claims(kz_proplist()) -> kz_proplist().
include_claims(Claims) ->
    case kz_auth_identity:sign(Claims) of
        {'ok', Signature} -> [{<<"identity_sig">>, kz_base64url:encode(Signature)} | Claims];
        _Else ->
            lager:debug("identity signing json token failed : ~p", [_Else]),
            Claims
    end.

-spec authenticate_fold(map(), list()) -> map().
authenticate_fold(Token, []) -> Token;
authenticate_fold(#{key := _Key}=Token, _) -> Token;
authenticate_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> authenticate_fold(NewToken, Routines)
    catch
        _E:_R ->
            lager:debug("exception executing ~p : ~p , ~p", [Fun, _E, _R]),
            kz_util:log_stacktrace(),
            authenticate_fold(Token, Routines)
    end.

-spec link(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
link(AccountId, OwnerId, AuthId) ->
    Props = [{<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_owner_id">>, OwnerId}
            ],
    case kz_datamgr:update_doc(?KZ_AUTH_DB, AuthId, Props) of
        {'ok', _JObj} -> 'ok';
        Error -> Error
    end.

-spec unlink(ne_binary()) -> 'ok' | {'error', any()}.
unlink(AuthId) ->
    Props = [{<<"pvt_account_id">>, null}
            ,{<<"pvt_owner_id">>, null}
            ],
    case kz_datamgr:update_doc(?KZ_AUTH_DB, AuthId, Props) of
        {'ok', _JObj} -> 'ok';
        Error -> Error
    end.
