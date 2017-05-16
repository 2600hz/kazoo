%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_identity).


%% ====================================================================
%% API functions
%% ====================================================================
-export([verify/1
        ,sign/1
        ,token/1
        ]).

-include("kazoo_auth.hrl").

-define(PVT_SIGNING_SECRET, <<"pvt_signature_secret">>).

-spec sign(map() | kz_proplist() | kz_json:object()) -> {'ok', ne_binary()} | {'error', any()}.
sign(Claims)
  when is_map(Claims) ->
    #{jwt_identity_signature_secret := ServerSecret
     ,jwt_user_id_signature_hash := Hash
     } = Kazoo = kz_auth_providers:kazoo_auth_provider(),

    case identity_secret(#{auth_provider => Kazoo
                          ,payload => Claims
                          })
    of
        #{identity_secret := IdentitySecret
         ,auth_id := Identity
         } ->
            HashMethod = kz_util:to_atom(Hash, 'true'),
            CryptoKey = <<IdentitySecret/binary, ServerSecret/binary>>,
            {'ok', crypto:hmac(HashMethod, CryptoKey, Identity)};
        #{} ->
            lager:info("unable to sign identity claims without a valid identity secret"),
            {'error', {500, 'invalid_identity_secret'}};
        Error ->
            lager:info("unable to sign identity claims: ~p", [Error]),
            Error
    end;
sign(Claims)
  when is_list(Claims) ->
    sign(maps:from_list(Claims));
sign(Claims) ->
    sign(kz_json:to_map(Claims)).


-spec identity_secret(map()) -> map() | {'error', any()}.
identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId
                              ,<<"owner_id">> := OwnerId
                              }
                 }=Token) ->
    AccountDb = kz_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => OwnerId
                              ,auth_db_id => OwnerId
                              });

identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId}
                 }=Token) ->
    AccountDb = kz_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => AccountId
                              ,auth_db_id => AccountId
                              });

identity_secret(#{auth_app := #{pvt_user_prefix := Prefix}
                 ,auth_provider := #{jwt_user_id_claim := IdentityField}
                 ,payload := Claims
                 }=Token) ->
    case maps:get(IdentityField, Claims, 'undefined') of
        'undefined' ->
            lager:debug("unable to find ~s in claims", [IdentityField]),
            {'error', 'invalid_claims'};
        Identity ->
            Key = <<Prefix/binary, "-", Identity/binary>>,
            get_identity_secret(Token#{auth_db => ?KZ_AUTH_DB
                                      ,auth_db_id => Key
                                      ,auth_id => Identity
                                      })
    end;

identity_secret(#{auth_provider := #{jwt_user_id_claim := IdentityField
                                    ,name := Name
                                    }
                 ,payload := Claims
                 }=Token) ->
    case maps:get(IdentityField, Claims, 'undefined') of
        'undefined' ->
            lager:debug("unable to find ~s in claims", [IdentityField]),
            {'error', 'invalid_claims'};
        Identity ->
            Key = <<Name/binary, "-", Identity/binary>>,
            get_identity_secret(Token#{auth_db => ?KZ_AUTH_DB
                                      ,auth_db_id => Key
                                      ,auth_id => Identity
                                      })
    end;

identity_secret(#{}) -> {'error', {500, 'invalid_identity_provider'}}.


-spec get_identity_secret(map()) -> map() | {'error', any()}.
get_identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                     ,auth_db := Db
                     ,auth_id := Key
                     }=Token) ->
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', JObj} -> check_kazoo_secret(Token#{user_doc => JObj, user_map => kz_json:to_map(JObj)});
        {'error', 'not_found'} ->
            lager:debug("kazoo identity secret not found for ~s/~s", [Db, Key]),
            {'error', {500, 'invalid_identity_provider'}}
    end;

get_identity_secret(#{options := #{force_profile_update := 'true'}}=Token) ->
    from_profile(Token);
get_identity_secret(#{auth_provider := #{profile_cache_timer := _Timer}
                     ,auth_db_id := Key
                     }=Token) ->
    case kz_cache:fetch_local(?PROFILE_CACHE, Key) of
        {'ok', _} -> get_identity(Token);
        {'error', 'not_found'} -> from_profile(Token)
    end;
get_identity_secret(Token) ->
    get_identity(Token).

-spec get_identity(map()) -> map() | {'error', any()}.
get_identity(#{auth_db := Db
              ,auth_db_id := Key
              }=Token) ->
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', JObj} -> check_cache_expiration(Token, JObj);
        {'error', 'not_found'} -> from_profile(Token)
    end.

-spec check_cache_expiration(map(), kz_json:object()) -> map() | {'error', any()}.
check_cache_expiration(#{auth_provider := #{cached_profile_field := ProfileField
                                           ,cached_profile_claim_field := Claim
                                           }
                        ,payload := Claims
                        }=Token, JObj) ->
    ClaimValue = maps:get(Claim, Claims, 'undefined'),
    ProfileValue = kz_json:get_value([<<"profile">>, ProfileField], JObj),
    case ClaimValue > ProfileValue of
        'true' ->
            lager:debug("cached profile (~s) is stale, refreshing", [ProfileValue]),
            from_profile(Token);
        'false' ->
            lager:debug("using cached profile ~s", [ProfileValue]),
            check_secret(Token#{user_doc => JObj, user_map => kz_json:to_map(JObj)})
    end;
check_cache_expiration(#{}=Token, JObj) ->
    check_secret(Token#{user_doc => JObj, user_map => kz_json:to_map(JObj)}).

-spec check_secret(map()) -> map() | {'error', any()}.
check_secret(#{auth_provider := #{profile_signature_secret_field := Field}
              ,user_doc := JObj
              } = Token) ->
    case kz_json:get_value([<<"profile">>, Field], JObj) of
        'undefined' ->
            lager:debug("identity profile secret field '~s' not found", [Field]),
            {'error', 'invalid_profile'};
        Secret -> Token#{identity_secret => Secret}
    end;
check_secret(#{auth_provider := #{name := Name}}) ->
    lager:debug("provider ~s does not support profile signature secret field", [Name]),
    {'error', {500, 'invalid_identity_provider'}}.

-spec from_profile(map()) -> map() | {'error', any()}.
from_profile(Token) ->
    case kz_auth_profile:token(Token) of
        #{user_doc := _Doc}=Token1 -> check_secret(Token1);
        #{profile_error_code := Error} -> {'error', Error};
        Error -> Error
    end.

-spec check_kazoo_secret(map()) -> map() | {'error', any()}.
check_kazoo_secret(#{user_doc := JObj}=Token) ->
    case kz_json:get_value(?PVT_SIGNING_SECRET, JObj) of
        'undefined' -> update_kazoo_secret(Token);
        Secret -> Token#{identity_secret => Secret}
    end.

-spec update_kazoo_secret(map()) -> map() | {'error', any()}.
update_kazoo_secret(Token) ->
    lager:debug("generating new kazoo signing secret"),
    update_kazoo_secret(Token, kz_util:rand_hex_binary(16)).

-spec update_kazoo_secret(map(), ne_binary()) -> map() | {'error', any()}.
update_kazoo_secret(#{auth_db := Db
                     ,auth_db_id := Key
                     }=Token, Secret) ->
    case kz_datamgr:update_doc(Db, Key, [{?PVT_SIGNING_SECRET, Secret}]) of
        {'ok', _} -> Token#{identity_secret => Secret};
        _Error ->
            lager:info("unable to store the kazoo signing secret on ~s/~s: ~p", [Db, Key, _Error]),
            {'error', {500, 'datastore_fault'}}
    end.

-spec token(map()) -> map().
token(#{identify_verified := _}=Token) -> Token;
token(#{auth_provider := #{name := <<"kazoo">>
                          ,jwt_identity_signature_secret := Secret
                          ,jwt_user_id_signature_hash := Hash
                          }
       ,payload := #{<<"identity_sig">> := IdentitySig}
       }=Token) ->
    case kz_util:is_not_empty(IdentitySig) andalso identity_secret(Token) of
        'false' ->
            lager:info("unable to verify identity without a valid identity secret"),
            Token#{identify_verified => 'false', identity_error => 'invalid_identity_signature'};
        {'error', Error} ->
            lager:info("unable to verify identity claims: ~p", [Error]),
            Token#{identify_verified => 'false', identity_error => Error};
        Token1 -> verify_identity_signature(Token1, Secret, Hash, IdentitySig)
    end;
token(#{payload := Payload
       ,auth_provider := #{jwt_user_id_signature_hash := Hash
                          ,jwt_user_id_signature_secret := Secret
                          ,jwt_user_id_signature_claim := IdentitySigField
                          }
       }=Token) ->
    IdentitySig = maps:get(IdentitySigField, Payload, 'undefined'),
    case kz_util:is_not_empty(IdentitySig) andalso identity_secret(Token) of
        'false' ->
            lager:debug("unable to get identity signature from field '~s'", [IdentitySigField]),
            Token#{identify_verified => 'false', identity_error => 'invalid_identity_signature'};
        {'error', Error} ->
            lager:info("unable to verify identity claims: ~p", [Error]),
            Token#{identify_verified => 'false', identity_error => Error};
        Token1 -> verify_identity_signature(Token1, Secret, Hash, IdentitySig)
    end;
token(#{}=Token) -> Token#{identify_verified => 'true'}.

-spec verify_identity_signature(map(), ne_binary(), ne_binary(), ne_binary()) -> map().
verify_identity_signature(#{identity_secret := IdentitySecret, auth_id := Identity}=Token,  Secret, Hash, IdentitySig) ->
    lager:debug("verifying key for identity '~s'", [Identity]),
    IdentitySignature = kz_base64url:decode(IdentitySig),
    HashMethod = kz_util:to_atom(Hash, 'true'),
    CryptoKey = <<IdentitySecret/binary, Secret/binary>>,
    ExpectedSignature = crypto:hmac(HashMethod, CryptoKey, Identity),
    verify_identity_signature(Token, IdentitySignature, ExpectedSignature).

-spec verify_identity_signature(map(), ne_binary(), ne_binary()) -> map().
verify_identity_signature(Token, ExpectedSignature, ExpectedSignature) ->
    Token#{identify_verified => 'true'};
verify_identity_signature(Token, _IdentitySignature, _ExpectedSignature) ->
     lager:info("provided identity signature (~s) did not match the expected signature", [_IdentitySignature]),
     Token#{identify_verified => 'false', identity_error => 'invalid_identity_signature'}.

-spec verify(map()) -> boolean().
verify(Token) ->
    #{identify_verified := Verified} = token(Token),
    Verified.

%% ====================================================================
%% Internal functions
%% ====================================================================
