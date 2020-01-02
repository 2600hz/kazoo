%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_identity).


%%==============================================================================
%% API functions
%%==============================================================================

-export([verify/1
        ,sign/1
        ,token/1
        ,reset_system_secret/0
        ,reset_secret/1
        ,reset_doc_secret/1
        ,has_doc_secret/1
        ]).

-include("kazoo_auth.hrl").

-define(PVT_SIGNING_SECRET, <<"pvt_signature_secret">>).

%%------------------------------------------------------------------------------
%% @doc Sign the identity (a user_id or an account_id).
%% Secret is consist of identity secret and a provider secret
%% (Kazoo is the only provider for signing).
%% @end
%%------------------------------------------------------------------------------
-spec sign(map() | kz_term:proplist() | kz_json:object()) -> {'ok', kz_term:ne_binary()} | {'error', any()}.
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
            HashMethod = kz_term:to_atom(Hash, 'true'),
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

%%------------------------------------------------------------------------------
%% @doc Find the identity based on Provider and Claims, returns the secret.
%% @end
%%------------------------------------------------------------------------------
-spec identity_secret(map()) -> map() | {'error', any()}.
identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId
                              ,<<"device_id">> := DeviceId
                              }
                 }=Token) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => DeviceId
                              ,auth_db_id => DeviceId
                              });

identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId
                              ,<<"owner_id">> := OwnerId
                              }
                 }=Token) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => OwnerId
                              ,auth_db_id => OwnerId
                              });

identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId}
                 }=Token) ->
    AccountDb = kzs_util:format_account_db(AccountId),
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

%%------------------------------------------------------------------------------
%% @doc Fetch the identity from DB or profile (if it's an OAuth provider).
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Fetch the identity profile from DB cache.
%% @end
%%------------------------------------------------------------------------------
-spec get_identity(map()) -> map() | {'error', any()}.
get_identity(#{auth_db := Db
              ,auth_db_id := Key
              }=Token) ->
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', JObj} -> check_cache_expiration(Token, JObj);
        {'error', 'not_found'} -> from_profile(Token)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if cached key document is expired or not, if it is read it from profile
%% to force refreshing.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Read the identity secret from profile (Only third-party OAuth provider).
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Get the identity profile from the third-party OAuth provider.
%% @end
%%------------------------------------------------------------------------------
-spec from_profile(map()) -> map() | {'error', any()}.
from_profile(Token) ->
    case kz_auth_profile:token(Token) of
        #{user_doc := _Doc}=Token1 -> check_secret(Token1);
        #{profile_error_code := {'error', _Err} = Error} -> Error;
        #{profile_error_code := Error} -> {'error', Error};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Get the identity secret from doc (Only Kazoo provider).
%% Create it if it's not present.
%% @end
%%------------------------------------------------------------------------------
-spec check_kazoo_secret(map()) -> map() | {'error', any()}.
check_kazoo_secret(#{user_doc := JObj}=Token) ->
    case kz_json:get_value(?PVT_SIGNING_SECRET, JObj) of
        'undefined' ->
            lager:debug("no signing secret on ~s, generating", [kz_doc:id(JObj)]),
            case update_kazoo_secret(Token) of
                #{}=NewToken -> NewToken;
                {'error', _} ->
                    {'error', {500, 'datastore_fault'}}
            end;
        Secret -> Token#{identity_secret => Secret}
    end.

-spec update_kazoo_secret(map()) -> map() | {'error', any()}.
update_kazoo_secret(#{auth_db := Db
                     ,auth_db_id := Key
                     }=Token) ->
    lager:debug("generating new kazoo signing secret for ~s/~s", [Db, Key]),
    update_kazoo_secret(Token, generate_new_kazoo_signing_secret()).

-spec update_kazoo_secret(map(), kz_term:ne_binary()) ->
          map() | kz_datamgr:data_error().
update_kazoo_secret(#{auth_db := Db}=Token, Secret) ->
    update_kazoo_secret(Token, Secret, kzs_util:db_classification(Db)).

-spec update_kazoo_secret(map(), kz_term:ne_binary(), kz_data:db_classification()) ->
          map() | kz_datamgr:data_error().
update_kazoo_secret(#{auth_db := AccountDb, auth_db_id := DocId}=Token, Secret, 'account') ->
    case kzs_util:format_account_id(AccountDb) of
        DocId -> update_account_secret(Token, Secret);
        _AccountId -> update_doc_secret(Token, Secret)
    end;
update_kazoo_secret(Token, Secret, _Type) -> update_doc_secret(Token, Secret).

-spec update_account_secret(map(), kz_term:ne_binary()) ->
          map() | kz_datamgr:data_error().
update_account_secret(#{auth_db_id := AccountId}=Token, Secret) ->
    Updates = [{[?PVT_SIGNING_SECRET], Secret}],
    handle_updated_secret(Token, kzd_accounts:update(AccountId, Updates)).

-spec update_doc_secret(map(), kz_term:ne_binary()) ->
          map() | kz_datamgr:data_error().
update_doc_secret(#{auth_db := Db
                   ,auth_db_id := Key
                   }=Token
                 ,Secret
                 ) ->
    Updates = [{[?PVT_SIGNING_SECRET], Secret}],
    UpdateOptions = [{'update', Updates}],
    handle_updated_secret(Token, kz_datamgr:update_doc(Db, Key, UpdateOptions)).

-spec handle_updated_secret(map(), {'ok', kz_json:object()} | kz_datamgr:data_error()) ->
          map() | kz_datamgr:data_error().
handle_updated_secret(Token, {'ok', Doc}) ->
    Token#{identity_secret => kz_json:get_value(?PVT_SIGNING_SECRET, Doc)};
handle_updated_secret(#{auth_db := _Db, auth_db_id := _Key}=_Token, {'error', _Reason}=Error) ->
    lager:info("unable to store the kazoo signing secret on ~s/~s: ~p", [_Db, _Key, _Reason]),
    Error.

%%------------------------------------------------------------------------------
%% @doc Verify the identity signature from a Token map.
%% Returns the map with `identify_verified' is set to result of verification.
%% @end
%%------------------------------------------------------------------------------
-spec token(map()) -> map().
token(#{identify_verified := _}=Token) -> Token;
token(#{auth_provider := #{name := <<"kazoo">>
                          ,jwt_identity_signature_secret := Secret
                          ,jwt_user_id_signature_hash := Hash
                          }
       ,payload := #{<<"identity_sig">> := IdentitySig}
       }=Token) ->
    case kz_term:is_not_empty(IdentitySig)
        andalso identity_secret(Token)
    of
        'false' ->
            lager:info("unable to verify identity without a valid identity secret"),
            Token#{identify_verified => 'false'
                  ,identity_error => 'invalid_identity_signature'
                  };
        {'error', Error} ->
            lager:info("unable to verify identity claims: ~p", [Error]),
            Token#{identify_verified => 'false'
                  ,identity_error => Error
                  };
        Token1 -> verify_identity_signature(Token1, Secret, Hash, IdentitySig)
    end;
token(#{payload := Payload
       ,auth_provider := #{jwt_user_id_signature_hash := Hash
                          ,jwt_user_id_signature_secret := Secret
                          ,jwt_user_id_signature_claim := IdentitySigField
                          }
       }=Token) ->
    IdentitySig = maps:get(IdentitySigField, Payload, 'undefined'),
    case kz_term:is_not_empty(IdentitySig)
        andalso identity_secret(Token)
    of
        'false' ->
            lager:debug("unable to get identity signature from field '~s'", [IdentitySigField]),
            Token#{identify_verified => 'false'
                  ,identity_error => 'invalid_identity_signature'
                  };
        {'error', Error} ->
            lager:info("unable to verify identity claims: ~p", [Error]),
            Token#{identify_verified => 'false'
                  ,identity_error => Error
                  };
        Token1 -> verify_identity_signature(Token1, Secret, Hash, IdentitySig)
    end;
token(#{}=Token) -> Token#{identify_verified => 'true'}.

-spec verify_identity_signature(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> map().
verify_identity_signature(#{identity_secret := IdentitySecret
                           ,auth_id := Identity
                           }=Token
                         ,Secret
                         ,Hash
                         ,IdentitySig
                         ) ->
    lager:debug("verifying key for identity '~s'", [Identity]),
    IdentitySignature = kz_base64url:decode(IdentitySig),
    HashMethod = kz_term:to_atom(Hash, 'true'),
    CryptoKey = <<IdentitySecret/binary, Secret/binary>>,
    ExpectedSignature = crypto:hmac(HashMethod, CryptoKey, Identity),
    verify_identity_signature(Token, IdentitySignature, ExpectedSignature).

-spec verify_identity_signature(map(), kz_term:ne_binary(), kz_term:ne_binary()) -> map().
verify_identity_signature(Token, ExpectedSignature, ExpectedSignature) ->
    Token#{identify_verified => 'true'};
verify_identity_signature(Token, _IdentitySignature, _ExpectedSignature) ->
    lager:info("provided identity signature (~s) did not match the expected signature", [_IdentitySignature]),
    Token#{identify_verified => 'false'
          ,identity_error => 'invalid_identity_signature'
          }.

%%------------------------------------------------------------------------------
%% @doc Returns a boolean of the Token map's verification result.
%% @end
%%------------------------------------------------------------------------------
-spec verify(map()) -> boolean().
verify(Token) ->
    #{identify_verified := Verified} = token(Token),
    Verified.

%%------------------------------------------------------------------------------
%% @doc Reset system key (provider identity secret).
%% @end
%%------------------------------------------------------------------------------
-spec reset_system_secret() -> {'ok', kz_json:object()} | {'error', any()}.
reset_system_secret() ->
    lager:warning("resetting system identity secret"),
    kapps_config:set_string(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID, ?KAZOO_GEN_SIGNATURE_SECRET).

%%------------------------------------------------------------------------------
%% @doc Reset account/user identity secret.
%% @end
%%------------------------------------------------------------------------------
-spec reset_secret(map() | kz_term:proplist() | kz_json:object()) -> 'ok' | {'error', any()}.
reset_secret(#{<<"account_id">> := Account
              ,<<"owner_id">> := OwnerId
              }) ->
    AccountDb = kzs_util:format_account_db(Account),
    reset_identity_secret(#{auth_db => AccountDb
                           ,auth_id => OwnerId
                           ,auth_db_id => OwnerId
                           });
reset_secret(#{<<"account_id">> := Account}) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(AccountId),
    reset_identity_secret(#{auth_db => AccountDb
                           ,auth_id => AccountId
                           ,auth_db_id => AccountId
                           });
reset_secret(#{}) ->
    {'error', 'invalid_identity_provider'};
reset_secret(Claims)
  when is_list(Claims) ->
    reset_secret(maps:from_list(Claims));
reset_secret(Claims) ->
    reset_secret(kz_json:to_map(Claims)).

%%------------------------------------------------------------------------------
%% @doc Set a new `?PVT_SIGNING_SECRET' value in the given doc.
%% @end
%%------------------------------------------------------------------------------
-spec reset_doc_secret(kz_json:object()) -> kz_json:object().
reset_doc_secret(JObj) ->
    kz_json:set_value(?PVT_SIGNING_SECRET, generate_new_kazoo_signing_secret(), JObj).

%%------------------------------------------------------------------------------
%% @doc Check if `?PVT_SIGNING_SECRET' is a non-empty value
%% @end
%%------------------------------------------------------------------------------
-spec has_doc_secret(kz_json:object()) -> boolean().
has_doc_secret(JObj) ->
    kz_json:get_ne_binary_value(?PVT_SIGNING_SECRET, JObj) =/= 'undefined'.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Read identity secret from DB first and update it.
%% @end
%%------------------------------------------------------------------------------
-spec reset_identity_secret(map()) -> 'ok' | {'error', any()}.
reset_identity_secret(#{auth_db := Db
                       ,auth_id := Key
                       }=Token) ->
    lager:debug("resetting identity secret, auth_db ~s auth_id ~s", [Db, Key]),
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', _JObj} ->
            case update_kazoo_secret(Token) of
                #{} -> 'ok';
                {'error', _}=Error -> Error
            end;
        {'error', _Reason}=Error ->
            lager:debug("failed to read identity document, auth_db ~s auth_id ~s: ~p", [Db, Key, _Reason]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Generate a new Kazoo signing secret.
%% @end
%%------------------------------------------------------------------------------
-spec generate_new_kazoo_signing_secret() -> kz_term:ne_binary().
generate_new_kazoo_signing_secret() ->
    kz_binary:rand_hex(16).
