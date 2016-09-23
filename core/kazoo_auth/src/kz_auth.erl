%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
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
        ]).

-include("kazoo_auth.hrl").

create_token(Claims) ->
    kz_auth_jwt:encode(include_claims(Claims)).

validate_token(Token) ->
    validate_token(Token, []).

validate_token(JWToken, Options) ->
    case kz_auth_jwt:token(JWToken) of
        #{verify_result := 'true'} = Token -> validate_claims(Token, Options);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end.

access_code(JObj) ->
    Code = kz_json:get_value(<<"code">>, JObj),
    AppId = kz_json:get_first_defined(?APPID_KEYS, JObj),
    RedirectURI = kz_json:get_first_defined(?REDIRECT_URI_KEYS, JObj, <<"postmessage">>),
    kz_auth_util:fetch_access_code(AppId, Code, RedirectURI).

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

authorize_token(JWToken) ->
    case kz_auth_jwt:token(JWToken) of
        #{verify_result := 'true'} = Token -> ensure_claims(Token);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_claims(#{claims := Claims}) ->
    {'ok', Claims}.

validate_claims(#{payload := #{<<"account_id">> := AccountId} = Payload}, Options) ->
    case props:get_value(<<"account_id">>, Options, AccountId) of
        AccountId ->
            Claims = kz_json:from_map(Payload),
            {'ok', Claims};
        _OtherAccountId ->
            {'error', 'account_header_mismatch'}
    end;
validate_claims(#{user_map := #{<<"pvt_account_id">> := AccountId}, payload := Payload}, Options) ->
    case props:get_value(<<"account_id">>, Options, AccountId) of
        AccountId ->
            Claims = kz_json:from_map(Payload),
            {'ok', Claims};
        _OtherAccountId ->
            {'error', 'account_header_mismatch'}
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
                'false' -> {'error', 'account_header_mismatch'}
            end;
        _OtherAccountId -> {'error', 'no_associated_accounts'}
    end;

validate_claims(#{}, _Options) -> {'error', 'no associated account_id'}.

ensure_claims(#{payload := Payload}) ->
    Claims = kz_json:from_map(Payload),
    {'ok', Claims}.

include_claims(Claims) ->
    case kz_auth_identity:sign(Claims) of
        {'ok', Signature} -> [{<<"identity_sig">>, kz_base64url:encode(Signature)} | Claims];
        _Else ->
            lager:debug("identity signing json token failed : ~p", [_Else]),
            Claims
    end.

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
