%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_keys).

-export([get_public_key_from_cert/1]).
-export([get_public_key_from_private_key/1]).
-export([gen_private_key/0]).
-export([get_public_key/2]).
-export([get_private_key_from_file/1]).
-export([from_pem/1, to_pem/1]).
-export([lookup/1, store/2]).
-export([from_token/1]).

-export([kazoo_private_key/1, kazoo_public_key/1]).

-include("kazoo_auth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

get_public_key_from_cert(PathToCert) ->
    {ok, PemBin} = file:read_file(PathToCert),
    PemEntries = public_key:pem_decode(PemBin),
    {value, CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
    {_, DerCert, _} = CertEntry,
    Decoded = public_key:pkix_decode_cert(DerCert, otp),
    PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    PublicKey.

get_public_key_from_private_key(#'RSAPrivateKey'{modulus=Mod, publicExponent=Exp}) ->
    #'RSAPublicKey'{modulus=Mod, publicExponent = Exp}.

get_public_key(Mod, Exp) ->
    #'RSAPublicKey'{modulus=Mod, publicExponent = Exp}.

get_private_key_from_file(Path) ->
    {ok, PemBin} = file:read_file(Path),
    [PemEntry | _] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(PemEntry).

-spec from_pem(binary()) -> any().
from_pem(PemContents) ->
    [PemEntry] = public_key:pem_decode(PemContents),
    public_key:pem_entry_decode(PemEntry).

-spec to_pem(public_key:rsa_public_key() | public_key:rsa_private_key()) -> any().
to_pem(RSA) ->
    public_key:pem_encode([public_key:pem_entry_encode(element(1, RSA), RSA)]).

lookup(KeyId) ->
    kz_cache:fetch_local(?PK_CACHE, KeyId).

store(KeyId, Key) ->
    lager:debug("storing public key ~p in cache", [KeyId]),
    kz_cache:store_local(?PK_CACHE, KeyId, Key).

from_token(#{}=Token) ->
    Routines = [fun maybe_get_key/1
               ,fun maybe_cached/1
               ,fun maybe_discovery/1
               ,fun maybe_discovery_url/1
               ,fun fetch_from_url/1
               ,fun fetch_key/1
               ,fun extract_key/1
               ],
    case from_token_fold(Token, Routines) of
        #{key := Key, cached := true} -> {'ok', Key};
        #{key_id := KeyId, key := Key, cached := false} ->
            store(KeyId, Key),
            {'ok', Key};
        _Other ->
            lager:debug("public key not found : ~p", [_Other]),
            {'error', 'not_found'}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
from_token_fold(Token, []) -> Token;
from_token_fold(#{key := _Key}=Token, _) -> Token;
from_token_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> from_token_fold(NewToken, Routines)
    catch
        _E:_R ->
            lager:debug("error running public key routine ~p : ~p , ~p", [Fun, _E, _R]),
            kz_util:log_stacktrace(),
            from_token_fold(Token, Routines)
    end.

maybe_get_key(#{key_id := _KeyId}=Token) -> Token;
maybe_get_key(#{auth_provider := #{public_key_jwt_field := Field
                                  ,public_key_jwt_location := Loc
                                  }
               }=Token) ->
    case key_id(Loc, Field, Token) of
        'undefined' -> Token;
        KeyId -> Token#{key_id => KeyId}
    end;
maybe_get_key(#{auth_provider := #{name := <<"kazoo">>}
               ,header := #{<<"kid">> := KeyId}
               } = Token) ->
    Token#{key_id => KeyId};
maybe_get_key(#{}=Token) -> Token.

key_id(<<"payload">>, Field, #{payload := Payload}) ->
    maps:get(Field, Payload, 'undefined');
key_id(<<"header">>, Field, Token) ->
    #{header := #{Field := KeyId}} = Token,
    KeyId;
key_id(_, _, _) -> 'undefined'.

maybe_cached(#{key_id := KeyId}=Token) ->
    case lookup(KeyId) of
        {'ok', Key} ->
            lager:debug("public key '~s' fetched from cache", [KeyId]),
            Token#{key => Key, cached => true};
        _ -> Token#{cached => false}
    end;
maybe_cached(#{}=Token) -> Token#{cached => false}.

maybe_discovery(#{key_id := _KeyId
                 ,auth_provider := #{discovery := DiscoveryUrl}
                 }=Token) ->
    case kz_auth_util:get_json_from_url(DiscoveryUrl) of
        {'ok', JObj} -> Token#{discovery => JObj};
        _ -> Token
    end;
maybe_discovery(#{key_id := KeyId
                 ,auth_provider := #{name := <<"kazoo">>}
                 }=Token) ->
    Token#{key => kazoo_public_key(KeyId)};
maybe_discovery(#{}=Token) -> Token.

maybe_discovery_url(#{discovery := JObj
                     ,auth_provider := #{public_key_discovery_field := Field}
                     }=Token) ->
    case kz_json:get_value(Field, JObj) of
        'undefined' -> Token;
        KeysUrl -> Token#{discovery_url => KeysUrl}
    end;
maybe_discovery_url(#{}=Token) -> Token.

fetch_from_url(#{discovery_url := Url}=Token) ->
    case kz_auth_util:get_json_from_url(Url) of
        {'ok', JObj} -> Token#{key_doc => JObj};
        _ -> Token
    end;
fetch_from_url(#{key_id := KeyId
                ,auth_provider := #{public_key_base_url := Url}
                }= Token) ->
    lager:debug("fetching public key from ~s~s", [Url, KeyId]),
    URL = <<Url/binary, KeyId/binary>>,
    case kz_auth_util:get_json_from_url(URL) of
        {'ok', JObj} -> Token#{key_doc => JObj};
        _Err ->
            lager:debug("error ~p obtaininig public key", [_Err]),
            Token
    end;
fetch_from_url(#{}=Token) -> Token.

fetch_key(#{key_doc := KeyDoc
           ,auth_provider := #{public_key_field := Field
                              ,public_key_method := <<"field">>
                              }
           }= Token) ->
    lager:debug("getting public key from '~s' field in downloaded json : ~p", [Field, KeyDoc]),
    case kz_json:get_value(Field, KeyDoc) of
        'undefined' ->
            lager:debug("public key not found from '~s' field in downloaded json : ~p", [Field, KeyDoc]),
            Token;
        Pem -> Token#{key_value => Pem}
    end;
fetch_key(#{key_id := KeyId
           ,key_doc := KeyDoc
           ,auth_provider := #{public_key_lookup_field := Field
                              ,public_key_method := <<"lookup">>
                              }
           }= Token) ->
    case kz_json:find_value(Field, KeyId, kz_json:get_value(<<"keys">>, KeyDoc)) of
        'undefined' -> Token;
        JObj -> Token#{key_value => kz_json:to_map(JObj)}
    end;
fetch_key(#{}=Token) -> Token.

extract_key(#{key_value := #{<<"n">> := N0, <<"e">> := E0}} = Token) ->
    N1 = kz_base64url:decode(N0),
    E1 = kz_base64url:decode(E0),
    N = binary:decode_unsigned(N1),
    E = binary:decode_unsigned(E1),
    Token#{key => get_public_key(N, E)};
extract_key(#{key_value := Pem} = Token) ->
    lager:debug("decoding public key from obtained pem : ~p", [Pem]),
    Token#{key => from_pem(Pem)};
extract_key(#{}=Token) ->
    lager:debug("public key not obtained : ~p", [Token]),
    Token.

kazoo_public_key(KeyId) ->
    {'ok', Key} = kazoo_private_key(KeyId),
    get_public_key_from_private_key(Key).

kazoo_private_key(KeyId) ->
    case lookup({'private', KeyId}) of
        {'error', 'not_found'} -> kazoo_load_private_key(KeyId);
        Found -> Found
    end.

kazoo_load_private_key(KeyId) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, KeyId) of
        {'ok', JObj} -> kazoo_load_private_key_attachment(JObj);
        {'error', 'not_found'} -> kazoo_new_private_key(KeyId)
    end.

kazoo_load_private_key_attachment(JObj) ->
    KeyId = kz_doc:id(JObj),
    case kz_datamgr:fetch_attachment(?KZ_AUTH_DB, KeyId, ?SYSTEM_KEY_ATTACHMENT_NAME) of
        {'ok', PemContents} ->
            Key = from_pem(PemContents),
            store({'private', KeyId}, Key),
            {'ok', Key};
        {'error', 'not_found'} -> kazoo_gen_private_key(JObj)
    end.

kazoo_new_private_key(KeyId) ->
    Doc = [{<<"pvt_type">>, <<"system_key">>}
          ,{<<"_id">>, KeyId}
          ],
    JObj = kz_doc:update_pvt_parameters(kz_json:from_list(Doc), ?KZ_AUTH_DB),
    case kz_datamgr:save_doc(?KZ_AUTH_DB, JObj) of
        {'ok', Saved} -> kazoo_gen_private_key(Saved);
        {'error', 'conflict'} -> kazoo_private_key(KeyId);
        {'error', _Err}=Err ->
            lager:debug("error ~p saving new system key ~s", [_Err, KeyId]),
            Err
    end.

kazoo_gen_private_key(JObj) ->
    {'ok', Key} = gen_private_key(),
    KeyId = kz_doc:id(JObj),
    Options = [{'doc_type', <<"system_key">>}
              ,{'rev', kz_doc:revision(JObj)}
              ,{'content_type', ?SYSTEM_KEY_ATTACHMENT_CTYPE}
              ],
    case kz_datamgr:put_attachment(?KZ_AUTH_DB, KeyId, ?SYSTEM_KEY_ATTACHMENT_NAME, to_pem(Key), Options) of
        {'ok', _} -> store({'private', KeyId}, Key),
                     {'ok', Key};
        {'error', 'conflict'} -> kazoo_private_key(KeyId);
        {'error', _Err}=Err ->
            lager:debug("error ~p saving generated system key ~s", [_Err, KeyId]),
            Err
    end.

%% kazoo_private_key_from_file(KeyId) ->
%%     Path = list_to_binary([code:priv_dir('kazoo_oauth'), "/keys/", kz_util:to_list(KeyId), ".pem"]),
%%     get_private_key_from_file(Path).

gen_private_key() ->
    {'ok', MPInts} = kz_auth_rsa:gen_rsa(?RSA_KEY_SIZE, ?RSA_KEY_SIZE + 1),
    [E, N, D, P, Q, DMP1, DMQ1, IQMP] = erlint(MPInts),
    Key = #'RSAPrivateKey'{version = 'two-prime',
                           modulus = N,
                           publicExponent = E,
                           privateExponent = D,
                           prime1 = P,
                           prime2 = Q,
                           exponent1 = DMP1,
                           exponent2 = DMQ1,
                           coefficient = IQMP},
    {'ok', Key}.

%% erlint(MPInts) -> [ crypto:erlint(X) || X <- MPInts ].
erlint(MPInts) when is_list(MPInts) -> [erlint(X) || X <- MPInts ];
erlint(<<Size:32, Int:Size/unit:8>>) -> Int.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% public_key:pem_encode([public_key:pem_entry_encode('RSAPublicKey', K2)]).

