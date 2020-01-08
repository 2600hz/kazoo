%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth).

%%==============================================================================
%% API functions
%%==============================================================================

-export([create_token/1
        ,validate_token/1, validate_token/2
        ,authorize_token/1
        ,access_code/1
        ,authenticate/1
        ,link/3
        ,unlink/1
        ]).

-include("kazoo_auth.hrl").

-spec create_token(kz_term:proplist()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'algorithm_not_supported'}.
create_token(Claims) ->
    kz_auth_jwt:encode(include_claims(Claims)).

-spec validate_token(kz_term:ne_binary() | map()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
validate_token(Token) ->
    validate_token(Token, []).

-spec validate_token(kz_term:ne_binary() | map(), kz_term:proplist()) ->
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

-spec authenticate(map() | kz_term:ne_binary() | kz_json:object()) ->
          {'ok', kz_term:proplist()} |
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
               ,fun kz_auth_token_util:access_token/1
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
    Token = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
    authenticate(Token#{original => JObj}).

-spec authorize_token(map() | kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
authorize_token(JWToken) ->
    case kz_auth_jwt:token(JWToken) of
        #{verify_result := 'true'} = Token -> ensure_claims(Token);
        #{verify_result := 'false', verify_error := Error} -> {'error', Error};
        Error -> Error
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_claims(map()) -> {'ok', kz_term:proplist()} | {'error', any()}.
verify_claims(#{claims := Claims}) ->
    {'ok', Claims}.

-spec validate_claims(map(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
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
                 }
               ,Options
               ) ->
    case props:get_value(<<"account_id">>, Options, AccountId) of
        AccountId ->
            Props = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, OwnerId}
                    ],
            case kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), OwnerId) of
                {'ok', _Doc} -> {'ok', kz_json:set_values(Props, kz_json:from_map(Payload))};
                _ -> {'error', {403, <<"mapped account does not exist">>}}
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
        _OtherAccountId -> {'error', {403, <<"no associated account_id">>}}
    end;

validate_claims(#{}, _Options) -> {'error', {403, <<"no associated account_id">>}}.

-spec ensure_claims(map()) -> {'ok', kz_json:object()} | {'error', any()}.
ensure_claims(#{payload := Payload}) ->
    Claims = kz_json:from_map(Payload),
    {'ok', Claims}.

-spec include_claims(kz_term:proplist()) -> kz_term:proplist().
include_claims(Claims) ->
    Routines = [fun include_identity_sign/1
               ,fun ensure_issuer/1
               ],
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end, Claims, Routines).

-spec include_identity_sign(kz_term:proplist()) -> kz_term:proplist().
include_identity_sign(Claims) ->
    case kz_auth_identity:sign(Claims) of
        {'ok', Signature} ->
            [{<<"identity_sig">>, kz_base64url:encode(Signature)} | Claims];
        _Else ->
            lager:debug("identity signing json token failed : ~p", [_Else]),
            Claims
    end.

-spec ensure_issuer(kz_term:proplist()) -> kz_term:proplist().
ensure_issuer(Claims) ->
    Issuer = props:get_value(<<"iss">>, Claims, <<"kazoo">>),
    props:set_value(<<"iss">>, Issuer, Claims).

-spec authenticate_fold(map(), list()) -> map().
authenticate_fold(Token, []) -> Token;
authenticate_fold(#{key := _Key}=Token, _) -> Token;
authenticate_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> authenticate_fold(NewToken, Routines)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("exception executing ~p : ~p , ~p", [Fun, _E, _R]),
        kz_log:log_stacktrace(ST),
        authenticate_fold(Token, Routines)
        end.

-spec link(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | kz_datamgr:data_error().
link(AccountId, OwnerId, AuthId) ->
    Updates = [{[<<"pvt_account_id">>], AccountId}
              ,{[<<"pvt_owner_id">>], OwnerId}
              ],
    UpdateOptions = [{'update', Updates}],
    case kz_datamgr:update_doc(?KZ_AUTH_DB, AuthId, UpdateOptions) of
        {'ok', _JObj} -> 'ok';
        Error -> Error
    end.

-spec unlink(kz_term:ne_binary()) ->
          'ok' | kz_datamgr:data_error().
unlink(AuthId) ->
    Updates = [{[<<"pvt_account_id">>], 'null'}
              ,{[<<"pvt_owner_id">>], 'null'}
              ],
    UpdateOptions = [{'update', Updates}],
    case kz_datamgr:update_doc(?KZ_AUTH_DB, AuthId, UpdateOptions) of
        {'ok', _JObj} -> 'ok';
        Error -> Error
    end.
