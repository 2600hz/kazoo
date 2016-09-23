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
        #{} -> {'error', 'no_identity_secret'};
        Error -> Error
    end;
sign(Claims)
  when is_list(Claims) ->
    sign(maps:from_list(Claims));
sign(Claims) ->
    sign(kz_json:to_map(Claims)).


identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId
                              ,<<"owner_id">> := OwnerId
                              }
                 }=Token) ->
    AccountDb = kz_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => OwnerId
                              });

identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                 ,payload := #{<<"account_id">> := AccountId}
                 }=Token) ->
    AccountDb = kz_util:format_account_db(AccountId),
    get_identity_secret(Token#{auth_db => AccountDb
                              ,auth_id => AccountId
                              });

identity_secret(#{auth_app := #{pvt_user_prefix := Prefix}
                 ,auth_provider := #{jwt_user_id_claim := IdentityField}
                 ,payload := Claims
                 }=Token) ->
    case maps:get(IdentityField, Claims, 'undefined') of
        'undefined' -> {'error', 'no_value_from_claims'};
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
        'undefined' -> {'error', 'no_value_from_claims'};
        Identity ->
            Key = <<Name/binary, "-", Identity/binary>>,
            get_identity_secret(Token#{auth_db => ?KZ_AUTH_DB
                                      ,auth_db_id => Key
                                      ,auth_id => Identity
                                      })
    end;

identity_secret(#{}) -> {'error', 'no_configuration'}.


get_identity_secret(#{auth_provider := #{name := <<"kazoo">>}
                     ,auth_db := Db
                     ,auth_id := Key
                     }=Token) ->
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', JObj} -> check_kazoo_secret(Token#{user_doc => JObj, user_map => kz_json:to_map(JObj)});
        {'error', 'not_found'}=Error -> Error
    end;

get_identity_secret(#{auth_db := Db
                     ,auth_db_id := Key
                     }=Token) ->
    case kz_datamgr:open_cache_doc(Db, Key) of
        {'ok', JObj} -> check_secret(Token#{user_doc => JObj, user_map => kz_json:to_map(JObj)});
        {'error', 'not_found'} -> from_profile(Token)
    end.

check_secret(#{auth_provider := #{profile_signature_secret_field := Field}
              ,user_doc := JObj
              } = Token) ->
    case kz_json:get_value([<<"profile">>, Field], JObj) of
        'undefined' ->
            lager:debug("secret field '~s' not found in ~p", [Field, JObj]),
            from_profile(Token);
        Secret -> Token#{identity_secret => Secret}
    end;
check_secret(#{auth_provider := #{name := Name}}) ->
    lager:debug("provider ~s does not support profile signature secret field", [Name]),
    {'error', 'provider_not_supported'}.

from_profile(#{user_doc := _}) -> {'error', 'profile_checked'};
from_profile(Token) ->
    case kz_auth_profile:token(Token) of
        #{user_doc := _Doc}=Token1 -> check_secret(Token1);
        Error -> Error
    end.


check_kazoo_secret(#{user_doc := JObj}=Token) ->
    case kz_json:get_value(?PVT_SIGNING_SECRET, JObj) of
        'undefined' -> update_kazoo_secret(Token);
        Secret -> Token#{identity_secret => Secret}
    end.

update_kazoo_secret(Token) ->
    update_kazoo_secret(Token, kz_util:rand_hex_binary(16)).

update_kazoo_secret(#{auth_db := Db
                     ,auth_db_id := Key
                     }=Token, Secret) ->
    case kz_datamgr:update_doc(Db, Key, [{?PVT_SIGNING_SECRET, Secret}]) of
        {'ok', _} -> Token#{identity_secret => Secret};
        Error -> Error
    end.


token(#{identify_verified := _}=Token) -> Token;
token(#{auth_provider := #{name := <<"kazoo">>
                          ,jwt_identity_signature_secret := Secret
                          ,jwt_user_id_signature_hash := Hash
                          }
       ,payload := #{<<"identity_sig">> := IdentitySig}
       }=Token) ->
    case identity_secret(Token) of
        #{identity_secret := IdentitySecret
         ,auth_id := Identity
         } = Token1 ->
            IdentitySignature = kz_base64url:decode(IdentitySig),
            HashMethod = kz_util:to_atom(Hash, 'true'),
            CryptoKey = <<IdentitySecret/binary, Secret/binary>>,
            Token1#{identify_verified => IdentitySignature =:= crypto:hmac(HashMethod, CryptoKey, Identity)};
        #{} = Token1 -> Token1#{identify_verified => 'false'};
        {'error', _Err} -> Token#{identify_verified => 'false'}
    end;

token(#{payload := Payload
       ,auth_provider := #{jwt_user_id_signature_hash := Hash
                          ,jwt_user_id_signature_secret := Secret
                          ,jwt_user_id_signature_claim := IdentitySigField
                          }
       }=Token) ->
    IdentitySig = maps:get(IdentitySigField, Payload, 'undefined'),
    case identity_secret(Token) of
        #{identity_secret := IdentitySecret
         ,auth_id := Identity
         } = Token1 when IdentitySig =/= 'undefined' ->
            lager:debug("verifying key for identity '~s'", [Identity]),
            IdentitySignature = kz_base64url:decode(IdentitySig),
            HashMethod = kz_util:to_atom(Hash, 'true'),
            CryptoKey = <<IdentitySecret/binary, Secret/binary>>,
            Token1#{identify_verified => IdentitySignature =:= crypto:hmac(HashMethod, CryptoKey, Identity)};
        #{} = Token1 -> Token1#{identify_verified => 'false'};
        {'error', _Err} -> Token#{identify_verified => 'false'}
    end;

token(#{}=Token) -> Token#{identify_verified => 'true'}.

verify(Token) ->
    #{identify_verified := Verified} = token(Token),
    Verified.


%% ====================================================================
%% Internal functions
%% ====================================================================
