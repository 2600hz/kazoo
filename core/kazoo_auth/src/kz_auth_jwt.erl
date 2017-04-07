%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

-spec decode(ne_binary()) -> {'ok', kz_proplist(), kz_proplist()} | {'error', any()}.
decode(JWTToken) ->
    decode(JWTToken, 'true').

-spec decode(ne_binary() | map(), boolean()) -> {'ok', kz_proplist(), kz_proplist()} | {'error', any()}.
decode(JWTToken, Verify) when is_binary(JWTToken) ->
    case parse(JWTToken) of
        {'ok', Token} -> decode(Token, Verify);
        Error -> Error
    end;
decode(#{header := Header, payload := Payload} = Token, 'true') ->
    case verify(Token) of
        'true' -> {'ok', kz_json:from_map(Header), kz_json:from_map(Payload)};
        'false' -> {'error', 'verify_failed'}
    end;
decode(#{header := Header, payload := Payload}, 'false') ->
    {'ok', kz_json:from_map(Header), kz_json:from_map(Payload)}.

-spec verify(ne_binary() | map()) -> boolean().
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
        'false' -> Token#{verify_result => 'false', verify_error => <<"jwt algorithm not supported">>}
    end;
verify_header(#{header := #{<<"alg">> := Alg}}=Token) ->
    case lists:member(Alg, ?ALGORITHMS) of
        'true' -> Token;
        'false' -> Token#{verify_result => 'false', verify_error => <<"jwt algorithm not supported">>}
    end;
verify_header(Token) ->
    Token#{verify_result => 'false', verify_error => <<"jwt header type not supported">>}.

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
        'false' -> Token#{verify_result => 'false', verify_error => <<"signature verification failed">>}
    end;
verify_signature(#{}=Token) ->
    case kz_auth_keys:from_token(Token) of
        {'ok', Key} -> verify_signature(Token#{jwt_public_key => Key});
        {'error', Error} -> Token#{verify_result => 'false', verify_error => Error}
    end;
verify_signature(Token) ->
    Token#{verify_result => 'false', verify_error => <<"invalid jwt">>}.


-spec verify_expiration(map()) -> map().
verify_expiration(#{payload := #{<<"exp">> := Expiration}}=Token) ->
    case Expiration - epoch() > 0 of
        'true' -> Token;
        'false' -> Token#{verify_result => 'false', verify_error => <<"token expired">>}
    end;
verify_expiration(Token) -> Token.

-spec verify_identity(map()) -> map().
verify_identity(Token) ->
    lager:debug("verifying identity"),
    #{identify_verified := Verified} = Token1 = kz_auth_identity:token(Token),
    case Verified of
        'true' -> Token1;
        'false' -> Token1#{verify_result => 'false'
                          ,verify_error => maps:get(identity_error, Token1, <<"identity verify failed">>)
                          }
    end.


-spec alg_2_digest_type(binary()) -> atom().
alg_2_digest_type(<<"RS256">>) -> 'sha256';
alg_2_digest_type(<<"RS384">>) -> 'sha384';
alg_2_digest_type(<<"RS512">>) -> 'sha512';
alg_2_digest_type(_)           -> 'undefined'.

-spec epoch() -> integer().
epoch() -> erlang:system_time('seconds').

-spec encode(kz_proplist() | map()) ->
                    {'ok', ne_binary()} |
                    {'error', any()}.
encode(Claims) when is_list(Claims) ->
    case props:get_value(<<"iss">>, Claims) of
        undefined -> {'error', 'no_issuer'};
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
        {ok, #{jwt := #{token := Token}}} -> {ok, Token};
        {ok, _M} ->
            lager:debug("JWT encode incomplete : ~p", [_M]),
            {error, <<"no jwt token encoded">>};
        {error, #{error := Error} = _M} ->
            lager:debug("JWT encode error : ~p", [_M]),
            {error, Error};
        {error, _M} ->
            lager:debug("JWT encode unknown error : ~p", [_M]),
            {error, <<"unknown error">>}
    end.

add_app(#{auth_app := _}=Map) -> Map;
add_app(#{issuer := Issuer} = Map) ->
    case kz_auth_apps:get_auth_app(Issuer) of
        {'error', Error} -> {error, Map#{error => Error}};
        #{}=App -> Map#{auth_app => App}
    end.

add_kid(#{kid := _}=Map) -> Map;
add_kid(#{auth_app := #{pvt_server_key := Key}} = Map) ->
    Map#{kid => Key};
add_kid(Map) ->
    {error, Map#{error => <<"no key identifier">>}}.

add_key(#{key := _}=Map) -> Map;
add_key(#{kid := KeyId} = Map) ->
    case kz_auth_keys:private_key(KeyId) of
        {'ok', Key} -> Map#{key => Key};
        {'error', Error} -> {error, Map#{error => Error}}
    end.

add_alg(#{alg := _}=Map) -> Map;
add_alg(#{auth_app := #{jwt_algorithm := Alg}} = Map) ->
    Map#{alg => Alg};
add_alg(Map) ->
    Map#{alg => ?DEFAULT_ALGORITHM}.

add_digest(#{digest := _}=Map) -> Map;
add_digest(#{alg := Alg} = Map) ->
    Map#{digest => alg_2_digest_type(Alg)}.

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

%%
%% -spec encode(ne_binary(), kz_proplist(), ne_binary() | {ne_binary(), public_key:rsa_private_key()}) ->
%%                     {'ok', ne_binary()} |
%%                     {'error', any()}.
%% encode(Alg, ClaimsSet, KeyId = ?NE_BINARY) ->
%%     {'ok', Key} = kz_auth_keys:private_key(KeyId),
%%     encode(Alg, ClaimsSet, {KeyId, Key});
%% encode(Alg, ClaimsSet, {KeyId, Key}) ->
%%     Head = [{<<"alg">>, Alg}
%%            ,{<<"typ">>, <<"JWT">>}
%%            ,{<<"kid">>, KeyId}
%%            ],
%%     Header = kz_base64url:encode(kz_json:encode(kz_json:from_list(Head))),
%%     Claims = kz_base64url:encode(kz_json:encode(kz_json:from_list(ClaimsSet))),
%%     Payload = <<Header/binary, ".", Claims/binary>>,
%%     case sign(Alg, Payload, Key) of
%%         'undefined' -> {'error', 'algorithm_not_supported'};
%%         Signature -> {'ok', <<Payload/binary, ".", Signature/binary>>}
%%     end.
%%
%% -spec sign(ne_binary(), ne_binary(),  public_key:rsa_private_key()) -> api_binary().
%% sign(Alg, Payload, Key) ->
%%     case alg_2_digest_type(Alg) of
%%         undefined -> undefined;
%%         Crypto -> kz_base64url:encode(public_key:sign(Payload, Crypto, Key))
%%     end.

-spec set_provider(map()) -> map().
set_provider(#{payload := #{<<"iss">> := Issuer}}=Token) ->
    Token#{auth_provider => kz_auth_providers:provider_by_issuer(Issuer)}.


-spec parse(ne_binary()) -> {'ok', jwt()} | {'error', 'invalid_token'}.
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
        _ -> {'error', 'no_jwt_signed_token'}
    end.

-spec token(ne_binary() | map()) -> map().
token(JWTToken) ->
    token(JWTToken, []).

-spec token(ne_binary() | map(), kz_proplist()) -> map().
token(JWTToken, Options) when is_binary(JWTToken) ->
    case parse(JWTToken) of
        {'ok', Token} -> token(Token, Options);
        Error -> Error
    end;
token(#{} = Token, Options) ->
    do_verify(Token#{options => maps:from_list(Options)}).
