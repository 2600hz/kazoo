%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc Kazoo Duo multi factor authenticator.
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_mfa_duo).

-export([authenticate/2]).

-include("kazoo_auth.hrl").

-define(REQ_VALUES, [<<"user_name">>
                    ,<<"integration_key">>
                    ,<<"secret_key">>
                    ,<<"application_secret_key">>
                    ,<<"api_hostname">>
                    ]).

-define(DUO_PREFIX, <<"TX">>).
-define(APP_PREFIX, <<"APP">>).
-define(AUTH_PREFIX, <<"AUTH">>).

-define(DUO_EXPIRE, 300).
-define(APP_EXPIRE, 3600).

-define(IKEY_LEN, 20).
-define(SKEY_LEN, 40).
-define(AKEY_LEN, 40).

-define(SIG_RESP(AuthSig, AppSig)
       ,[?NE_BINARY=AuthSig, ?NE_BINARY=AppSig]
       ).
-define(SIG_PARTS(Prefix, Cookie, Signature)
       ,[?NE_BINARY=Prefix, ?NE_BINARY=Cookie, ?NE_BINARY=Signature]
       ).

-define(VAL_PART_SEP, <<"|">>).
-define(AUTH_PART_SEP, <<":">>).

%%------------------------------------------------------------------------------
%% @doc Takes the Duo configuration and authenticate the `Claims'.
%% First request is always result in a signed signature response. When `Claims'
%% has the Duo response to the sign signature, it will verifies this Duo's response.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(kz_term:proplist(), kz_json:object()) -> mfa_result().
authenticate(Claims, JObj) ->
    maybe_sign_or_verify(map_config(Claims, JObj)
                        ,props:get_ne_binary_value(<<"mfa_resp">>, Claims)
                        ).

-spec maybe_sign_or_verify(map() | {'error', mfa_error()}, kz_term:api_ne_binary()) -> mfa_result().
maybe_sign_or_verify(#{}=Identity, 'undefined') ->
    sign_request(Identity);
maybe_sign_or_verify(#{}=Identity, SigResponse) ->
    verify_response(Identity, SigResponse);
maybe_sign_or_verify(MFAError, _SigResponse) ->
    MFAError.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sign_request(map()) -> mfa_result().
sign_request(#{<<"user_name">> := UserId
              ,<<"integration_key">> := IKey
              ,<<"secret_key">> := SKey
              ,<<"application_secret_key">> := AKey
              ,<<"duo_expire">> := DuoExpire
              ,<<"app_expire">> := AppExpire
              ,<<"api_hostname">> := Host
              }) ->
    Val = <<UserId/binary, (?VAL_PART_SEP)/binary, IKey/binary>>,

    DuoAuth = sign_value(SKey, Val, ?DUO_PREFIX, DuoExpire),
    AppAuth = sign_value(AKey, Val, ?APP_PREFIX, AppExpire),

    SiqReq = <<DuoAuth/binary, (?AUTH_PART_SEP)/binary, AppAuth/binary>>,

    Resp = [{<<"provider_name">>, <<"duo">>}
           ,{<<"settings">>
            ,kz_json:from_list(
               [{<<"duo_sig_request">>, SiqReq}
               ,{<<"duo_api_hostname">>, Host}
               ]
              )
            }
           ],
    {'error', 401, kz_json:from_list(Resp)}.

-spec sign_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), integer()) -> kz_term:ne_binary().
sign_value(Key, Value, Prefix, Exp) ->
    Expire = expire(Exp),
    BinaryValue = <<Value/binary, (?VAL_PART_SEP)/binary, (kz_term:to_binary(Expire))/binary>>,
    Cookie = <<Prefix/binary, (?VAL_PART_SEP)/binary, (base64:encode(BinaryValue))/binary>>,

    <<Cookie/binary, (?VAL_PART_SEP)/binary, (signature(Key, Cookie))/binary>>.

-spec signature(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
signature(Key, Cookie) ->
    kz_binary:hexencode(crypto:hmac('sha', Key, Cookie)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_response(map(), kz_term:ne_binary()) -> mfa_result().
verify_response(#{<<"integration_key">> := IKey
                 ,<<"secret_key">> := SKey
                 ,<<"application_secret_key">> := AKey
                 }=Identity, SigResponse) ->
    lager:debug("verifying duo payload ~s", [SigResponse]),
    case binary:split(SigResponse, ?AUTH_PART_SEP, ['global']) of
        ?SIG_RESP(AuthSig, AppSig) ->
            AuthUser = parse_value(IKey, SKey, AuthSig, ?AUTH_PREFIX),
            AppUser  = parse_value(IKey, AKey, AppSig, ?APP_PREFIX),
            verify_user(Identity, AuthUser, AppUser);
        _R ->
            lager:debug("invalid duo response"),
            {'error', 'unauthorized'}
    end.

-spec verify_user(map(), kz_term:api_binary(), kz_term:api_binary()) -> mfa_result().
verify_user(_Identity, 'undefined', _AppUser) ->
    {'error', 'unauthorized'};
verify_user(_Identity, _AuthUser, 'undefined') ->
    {'error', 'unauthorized'};
verify_user(_Identity, User, User) ->
    {'ok', 'authenticated'};
verify_user(#{<<"user_name">> := _UserId}, _AuthUser, _AppUser) ->
    lager:debug("user ~s mismatched (user_name: ~s auth_user: ~s app_user: ~s)"
               ,[_UserId, _AuthUser, _AppUser]
               ),
    {'error', 'unauthorized'}.

-spec parse_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
parse_value(IKey, Key, Value, Prefix) ->
    MaxExpire = kz_time:current_unix_tstamp(),
    case binary:split(Value, ?VAL_PART_SEP, ['global']) of
        ?SIG_PARTS(Prefix, Cookie, Signature) ->
            Maps = #{response_prefix => Prefix
                    ,response_cookie => Cookie
                    ,response_signature => Signature
                    ,key => Key
                    ,original_prefix => Prefix
                    ,original_ikey => IKey
                    ,current_seconds => MaxExpire
                    ,verify_result => 'true'
                    ,user_name => 'undefined'
                    },
            do_parse(Maps);
        ?SIG_PARTS(U_Prefix, _Cookie, _Signature) ->
            lager:debug("mismatch prefix, expects ~s got ~s", [Prefix, U_Prefix]),
            'undefined';
        _Else -> 'undefined'
    end.

-spec do_parse(map()) -> kz_term:api_ne_binary().
do_parse(Maps) ->
    Routines = [fun verify_prefix/1
               ,fun verify_signature/1
               ,fun extract_cookie/1
               ,fun verify_integeration_key/1
               ,fun verify_expiration/1
               ,fun is_user_name/1
               ],
    do_parse_fold(Maps, Routines).

-spec do_parse_fold(map(), list()) -> kz_term:api_ne_binary().
do_parse_fold(#{verify_result := 'false'}, _) -> 'undefined';
do_parse_fold(#{user_name := UserId}, []) -> UserId;
do_parse_fold(Maps, [Fun | Funs]) ->
    do_parse_fold(Fun(Maps), Funs).

-spec verify_prefix(map()) -> map().
verify_prefix(#{response_prefix := RespPrefix
               ,original_prefix := OrigPrefix
               }=Maps) ->
    PrefixCheck = RespPrefix =:= OrigPrefix,
    PrefixCheck
        orelse lager:debug("invalid prefix, expects ~s got ~s", [OrigPrefix, RespPrefix]),
    Maps#{verify_result => PrefixCheck}.

-spec verify_signature(map()) -> map().
verify_signature(#{response_signature := UserSig
                  ,response_cookie := Cookie
                  ,response_prefix := Prefix
                  ,key := Key
                  }=Maps) ->
    LocalSig = signature(Key, <<Prefix/binary, (?VAL_PART_SEP)/binary, Cookie/binary>>),
    SignatureCheck = signature(Key, LocalSig) =:= signature(Key, UserSig),
    SignatureCheck
        orelse lager:debug("invalid ~s signature", [Prefix]),
    Maps#{verify_result => SignatureCheck}.

-spec extract_cookie(map()) -> map().
extract_cookie(#{response_cookie := Cookie}=Maps) ->
    case binary:split(base64:decode(Cookie), ?VAL_PART_SEP, [global]) of
        ?SIG_PARTS(UserId, UserIKey, UserExpire) ->
            Maps#{user_name => UserId
                 ,user_ikey => UserIKey
                 ,user_expire => UserExpire
                 };
        _Malform ->
            lager:debug("invalid cookie payload"),
            Maps#{verify_result => 'false'}
    end.

-spec verify_integeration_key(map()) -> map().
verify_integeration_key(#{user_ikey := UserIKey
                         ,original_ikey := IKey
                         }=Maps) ->
    CheckIKey = UserIKey =:= IKey,
    CheckIKey
        orelse lager:debug("invalid integration key"),
    Maps#{verify_result => CheckIKey}.

-spec verify_expiration(map()) -> map().
verify_expiration(#{user_expire := UserExpire
                   ,current_seconds := MaxExpire
                   ,response_prefix := RespPrefix
                   }=Maps) ->
    CheckExpire = kz_term:to_integer(MaxExpire) =< kz_term:to_integer(UserExpire),
    CheckExpire
        orelse lager:debug("~s part of duo response is expired (expects lower than ~b got ~b )"
                          ,[RespPrefix, kz_term:to_integer(MaxExpire), kz_term:to_integer(UserExpire)]
                          ),
    Maps#{verify_result => CheckExpire}.

-spec is_user_name(map()) -> map().
is_user_name(#{user_name := UserId}=Maps) ->
    Maps#{verify_result => kz_term:is_not_empty(UserId)};
is_user_name(Maps) ->
    lager:debug("failed to verify user_name"),
    Maps#{verify_result => 'false'}.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec map_config(kz_term:proplist(), kz_json:object()) -> map() | {'error', mfa_error()}.
map_config(Claims, JObj) ->
    Identity = maps:from_list(
                 [{<<"user_name">>, props:get_value(<<"owner_id">>, Claims)}
                 ,{<<"integration_key">>, kz_json:get_value(<<"integration_key">>, JObj)}
                 ,{<<"secret_key">>, kz_json:get_value(<<"secret_key">>, JObj)}
                 ,{<<"application_secret_key">>, kz_json:get_value(<<"application_secret_key">>, JObj)}
                 ,{<<"api_hostname">>, kz_json:get_value(<<"api_hostname">>, JObj)}
                 ,{<<"duo_expire">>, kz_json:get_integer_value(<<"duo_expire">>, JObj, ?DUO_EXPIRE)}
                 ,{<<"app_expire">>, kz_json:get_integer_value(<<"app_expire">>, JObj, ?APP_EXPIRE)}
                 ]),
    case validate_values(Identity) of
        'true' -> Identity;
        <<KeyError/binary>> -> {'error', {'configuration', <<"invalid duo configuration, ", KeyError/binary>>}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_values(map()) -> 'true' | kz_term:ne_binary().
validate_values(Identity) ->
    try is_valid_identity(Identity)
    catch
        'error':{'badkey', Key} ->
            kz_term:to_binary(io_lib:format("duo ~s config key is missing", [Key]));
        'throw':{'error', Key} ->
            kz_term:to_binary(io_lib:format("duo ~s config key is invalid", [Key]))
    end.

-spec is_valid_identity(map()) -> 'true'.
is_valid_identity(Identity) ->
    lists:all(fun(ReqV) ->
                      validate_value(ReqV, Identity)
                          orelse throw({'error', ReqV})
              end
             ,?REQ_VALUES
             ).

-spec validate_value(kz_term:ne_binary(), map()) -> boolean().
validate_value(<<"user_name">> = K, Identity) ->
    kz_term:is_ne_binary(maps:get(K, Identity));
validate_value(<<"integration_key">> = K, Identity) ->
    IKey = maps:get(K, Identity),
    kz_term:is_ne_binary(IKey)
        andalso erlang:size(IKey) == ?IKEY_LEN;
validate_value(<<"secret_key">> = K, Identity) ->
    SKey = maps:get(K, Identity),
    kz_term:is_ne_binary(SKey)
        andalso erlang:size(SKey) >= ?SKEY_LEN;
validate_value(<<"application_secret_key">> = K, Identity) ->
    AKey = maps:get(K, Identity),
    kz_term:is_ne_binary(AKey)
        andalso erlang:size(AKey) >= ?AKEY_LEN;
validate_value(<<"api_hostname">> = K, Identity) ->
    kz_term:is_ne_binary(maps:get(K, Identity));
validate_value(_K, _Identity) ->
    'true'.

-spec expire(integer()) -> integer().
-ifdef(TEST).
expire(?TEST_DUO_SIGN_EXPIRE) -> 1486768575 + ?TEST_DUO_SIGN_EXPIRE;
expire(Exp)                   -> kz_time:current_unix_tstamp() + Exp.
-else.
expire(Exp) -> kz_time:current_unix_tstamp() + Exp.
-endif.
