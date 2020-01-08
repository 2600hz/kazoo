%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_jwt).

-include("kazoo_auth.hrl").

-export([verify/1
        ,decode/1, decode/2
        ,encode/1
        ,token/1, token/2
        ,parse/1
        ]).

-type jwt() :: map().

-define(ALG_RS256, <<"RS256">>).
-define(ALG_RS384, <<"RS384">>).
-define(ALG_RS512, <<"RS512">>).

-define(ALGORITHMS, [?ALG_RS256
                    ,?ALG_RS384
                    ,?ALG_RS512
                    ]).

-define(DEFAULT_ALGORITHM, ?ALG_RS256).

-spec decode(kz_term:ne_binary()) -> {'ok', kz_term:proplist(), kz_term:proplist()} | {'error', any()}.
decode(JWTToken) ->
    decode(JWTToken, 'true').

-spec decode(kz_term:ne_binary() | map(), boolean()) -> {'ok', kz_term:proplist(), kz_term:proplist()} | {'error', any()}.
decode(JWTToken, Verify) when is_binary(JWTToken) ->
    case parse(JWTToken) of
        {'ok', Token} -> decode(Token, Verify);
        Error -> Error
    end;
decode(#{header := Header, payload := Payload} = Token, 'true') ->
    case verify(Token) of
        'true' -> {'ok', maps:to_list(Header), maps:to_list(Payload)};
        'false' -> {'error', 'verify_failed'}
    end;
decode(#{header := Header, payload := Payload}, 'false') ->
    {'ok', maps:to_list(Header), maps:to_list(Payload)}.

-spec verify(kz_term:ne_binary() | map()) -> boolean().
verify(JWTToken) when is_binary(JWTToken) ->
    case parse(JWTToken) of
        {'ok', Token} -> verify(Token);
        _Error -> 'false'
    end;
verify(#{} = JWTToken) ->
    #{verify_result := Verified} = do_verify(JWTToken),
    Verified.

-spec do_verify(map()) -> map().
do_verify(#{} = JWTToken) ->
    Routines = [fun verify_header/1
               ,fun verify_signature/1
               ,fun verify_expiration/1
               ,fun verify_identity/1
               ],
    do_verify_fold(JWTToken#{verify_result => 'true'}, Routines).

-spec do_verify_fold(map(), list()) -> map().
do_verify_fold(#{verify_result := 'false'} = Token, _) -> Token;
do_verify_fold(#{} = Token, []) -> Token;
do_verify_fold(Token, [Fun | Funs]) ->
    do_verify_fold(Fun(Token), Funs).

-spec verify_header(map()) -> map().
verify_header(#{header := #{<<"typ">> := <<"JWT">>, <<"alg">> := Alg}}=Token) ->
    case lists:member(Alg, ?ALGORITHMS) of
        'true' -> Token;
        'false' ->
            lager:info("request contained invalid jwt algorithm: ~p", [Alg]),
            Token#{verify_result => 'false', verify_error => 'algorithm_not_supported'}
    end;
verify_header(#{header := #{<<"alg">> := Alg}}=Token) ->
    case lists:member(Alg, ?ALGORITHMS) of
        'true' -> Token;
        'false' ->
            lager:info("request contained invalid jwt algorithm: ~p", [Alg]),
            Token#{verify_result => 'false', verify_error => 'algorithm_not_supported'}
    end;
verify_header(#{header := #{<<"typ">> := _Typ}}=Token) ->
    lager:info("invalid jwt typ header ~p", [_Typ]),
    Token#{verify_result => 'false', verify_error => 'invalid_jwt'}.

-spec verify_signature(map()) -> map().
verify_signature(#{header := #{<<"alg">> := Alg}
                  ,header64 := Header
                  ,payload64 := Payload
                  ,signature := Signature
                  ,jwt_public_key := Key
                  }=Token) ->
    Verify = <<Header/binary, ".", Payload/binary>>,
    DigestType = alg_2_digest_type(Alg),
    case public_key:verify(Verify, DigestType, Signature, Key) of
        'true' -> Token;
        'false' ->
            lager:info("incorrect jwt signature"),
            Token#{verify_result => 'false', verify_error => 'invalid_jwt_signature'}
    end;
verify_signature(#{}=Token) ->
    case kz_auth_keys:from_token(Token) of
        {'ok', Key} -> verify_signature(Token#{jwt_public_key => Key});
        {'error', _Error} ->
            lager:info("unable to find jwt signature for verification: ~p", [_Error]),
            Token#{verify_result => 'false', verify_error => 'invalid_jwt_signature'}
    end;
verify_signature(Token) ->
    lager:info("unexpected/invalid jwt signature"),
    Token#{verify_result => 'false', verify_error => 'invalid_jwt'}.

-spec verify_expiration(map()) -> map().
verify_expiration(#{payload := #{<<"exp">> := Expiration}}=Token) ->
    case Expiration - epoch() > 0 of
        'true' -> Token;
        'false' ->
            lager:info("jwt exp ~p has elapsed", [Expiration]),
            Token#{verify_result => 'false', verify_error => 'token_expired'}
    end;
verify_expiration(Token) -> Token.

-spec verify_identity(map()) -> map().
verify_identity(Token) ->
    lager:debug("verifying claimed identity"),
    #{identify_verified := Verified} = Token1 = kz_auth_identity:token(Token),
    case Verified of
        'true' -> Token1;
        'false' ->
            IdentityError = maps:get(identity_error, Token1, 'invalid_identity'),
            lager:info("claimed identity could not be verified: ~p", [IdentityError]),
            Token1#{verify_result => 'false', verify_error => IdentityError}
    end.

-spec alg_2_digest_type(binary()) -> atom().
alg_2_digest_type(<<"RS256">>) -> 'sha256';
alg_2_digest_type(<<"RS384">>) -> 'sha384';
alg_2_digest_type(<<"RS512">>) -> 'sha512';
alg_2_digest_type(_)           -> 'undefined'.

-spec epoch() -> integer().
epoch() -> erlang:system_time('seconds').

-spec encode(kz_term:proplist() | map()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any()}.
encode(Claims) when is_list(Claims) ->
    case props:get_value(<<"iss">>, Claims) of
        'undefined' -> {'error', 'no_issuer'};
        Issuer -> encode(#{claims => Claims
                          ,issuer => Issuer
                          })
    end;

encode(Map) when is_map(Map) ->
    Routines = [fun add_app/1
               ,fun add_kid/1
               ,fun add_key/1
               ,fun add_alg/1
               ,fun add_digest/1
               ,fun add_header/1
               ,fun add_claims/1
               ,fun add_payload/1
               ,fun sign/1
               ],
    case kz_auth_util:run(Map, Routines) of
        {'ok', #{jwt := #{token := Token}}} ->
            lager:debug("JWT encoded"),
            {'ok', Token};
        {'ok', _M} ->
            lager:debug("JWT encode incomplete : ~p", [_M]),
            {'error', <<"no jwt token encoded">>};
        {'error', #{error := Error} = _M} ->
            lager:debug("JWT encode error : ~p", [_M]),
            {'error', Error};
        {'error', _M} ->
            lager:debug("JWT encode unknown error : ~p", [_M]),
            {'error', <<"unknown error">>}
    end.

-spec add_app(map()) -> map().
add_app(#{auth_app := _}=Map) -> Map;
add_app(#{issuer := Issuer} = Map) ->
    case kz_auth_apps:get_auth_app(Issuer) of
        {'error', Error} -> {'error', Map#{error => Error}};
        #{}=App -> Map#{auth_app => App}
    end.

-spec add_kid(map()) -> map().
add_kid(#{kid := _}=Map) -> Map;
add_kid(#{auth_app := #{pvt_server_key := Key}} = Map) ->
    Map#{kid => Key};
add_kid(Map) ->
    {'error', Map#{error => <<"no key identifier">>}}.

-spec add_key(map()) -> map().
add_key(#{key := _}=Map) -> Map;
add_key(#{kid := KeyId} = Map) ->
    case kz_auth_keys:private_key(KeyId) of
        {'ok', Key} -> Map#{key => Key};
        {'error', Error} -> {'error', Map#{error => Error}}
    end.

-spec add_alg(map()) -> map().
add_alg(#{alg := _}=Map) -> Map;
add_alg(#{auth_app := #{jwt_algorithm := Alg}} = Map) ->
    Map#{alg => Alg};
add_alg(Map) ->
    Map#{alg => ?DEFAULT_ALGORITHM}.

-spec add_digest(map()) -> map().
add_digest(#{digest := _}=Map) -> Map;
add_digest(#{alg := Alg} = Map) ->
    Map#{digest => alg_2_digest_type(Alg)}.

-spec add_header(map()) -> map().
add_header(#{alg := Alg
            ,kid := KeyId
            } = Map) ->
    Head = [{<<"alg">>, Alg}
           ,{<<"typ">>, <<"JWT">>}
           ,{<<"kid">>, KeyId}
           ],
    Header = kz_base64url:encode(kz_json:encode(kz_json:from_list(Head))),
    Map#{jwt => #{header => #{keys => Head
                             ,encoded => Header
                             }
                 }}.

-spec add_claims(map()) -> map().
add_claims(#{claims := ClaimsMap
            ,jwt := JWT
            } = Map)
  when is_map(ClaimsMap) ->
    ClaimsSet = lists:map(fun({K,V}) -> {kz_term:to_binary(K), V} end, maps:to_list(ClaimsMap)),
    Map#{jwt => JWT#{claims => ClaimsSet}};
add_claims(#{claims := Claims
            ,jwt := JWT
            } = Map) ->
    Map#{jwt => JWT#{claims => Claims}}.

-spec add_payload(map()) -> map().
add_payload(#{jwt := #{claims := ClaimsSet
                      ,header := #{encoded := Header}
                      } = JWT
             } = Map) ->
    Claims = kz_base64url:encode(kz_json:encode(kz_json:from_list(ClaimsSet))),
    Payload = <<Header/binary, ".", Claims/binary>>,
    Map#{jwt => JWT#{payload => Payload}}.

-spec sign(map()) -> map().
sign(#{digest := Digest
      ,key := Key
      ,jwt := #{payload := Payload} = JWT
      } = Map) ->
    Signature = kz_base64url:encode(public_key:sign(Payload, Digest, Key)),
    Map#{jwt => JWT#{signature => Signature
                    ,token => <<Payload/binary, ".", Signature/binary>>
                    }}.

-spec set_provider(map()) -> map().
set_provider(#{payload := #{<<"iss">> := Issuer}}=Token) ->
    Token#{auth_provider => kz_auth_providers:provider_by_issuer(Issuer)};
set_provider(Token) -> Token.

-spec parse(kz_term:ne_binary()) -> {'ok', jwt()} | {'error', 'invalid_jwt'}.
parse(JWTToken) when is_binary(JWTToken) ->
    case binary:split(JWTToken, <<".">>, ['global']) of
        [Header, Payload, Signature] ->
            Map = #{header => kz_json:to_map(kz_json:decode(kz_base64url:decode(Header)))
                   ,payload => kz_json:to_map(kz_json:decode(kz_base64url:decode(Payload)))
                   ,signature => kz_base64url:decode(Signature)
                   ,header64 => Header
                   ,payload64 => Payload
                   ,signature64 => Signature
                   ,original => JWTToken
                   },
            {'ok', set_provider(Map)};
        _ ->
            lager:info("unable to parse jwt: ~s", [JWTToken]),
            {'error', 'invalid_jwt'}
    end.

-spec token(kz_term:ne_binary() | map()) -> map().
token(JWTToken) ->
    token(JWTToken, []).

-spec token(kz_term:ne_binary() | map(), kz_term:proplist()) -> map().
token(JWTToken, Options) when is_binary(JWTToken) ->
    case parse(JWTToken) of
        {'ok', Token} -> token(Token, Options);
        Error -> Error
    end;
token(#{} = Token, Options) ->
    do_verify(Token#{options => maps:from_list(Options)}).
