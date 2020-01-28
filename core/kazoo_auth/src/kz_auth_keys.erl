%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_keys).

-export([get_public_key_from_cert/1]).
-export([get_public_key_from_private_key/1]).
-export([gen_private_key/0, gen_private_key/1]).
-export([get_public_key/2]).
-export([get_private_key_from_file/1]).
-export([from_pem/1, to_pem/1]).
-export([lookup/1, store/2, clear/0, clear/1]).
-export([from_token/1]).

-export([private_key/1, public_key/1]).

-export([new_private_key/1, new_private_key/2]).

-export([reset_kazoo_private_key/0, reset_private_key/1]).

-include("kazoo_auth.hrl").

-type rsa_key() :: public_key:rsa_private_key() | public_key:rsa_public_key().

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_public_key_from_cert(file:filename_all()) -> public_key:rsa_public_key().
get_public_key_from_cert(PathToCert) ->
    {'ok', PemBin} = file:read_file(PathToCert),
    PemEntries = public_key:pem_decode(PemBin),
    {'value', CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
    {_, DerCert, _} = CertEntry,
    Decoded = public_key:pkix_decode_cert(DerCert, 'otp'),
    PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    PublicKey.

-spec get_public_key_from_private_key(public_key:rsa_private_key()) -> public_key:rsa_public_key().
get_public_key_from_private_key(#'RSAPrivateKey'{modulus=Mod, publicExponent=Exp}) ->
    #'RSAPublicKey'{modulus=Mod, publicExponent = Exp}.

-spec get_public_key(any(), any()) -> public_key:rsa_public_key().
get_public_key(Mod, Exp) ->
    #'RSAPublicKey'{modulus=Mod, publicExponent = Exp}.

-spec get_private_key_from_file(file:filename_all()) -> public_key:rsa_private_key().
get_private_key_from_file(Path) ->
    {'ok', PemBin} = file:read_file(Path),
    [PemEntry | _] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(PemEntry).

-spec from_pem(binary()) -> any().
from_pem(PemContents) ->
    [PemEntry] = public_key:pem_decode(PemContents),
    public_key:pem_entry_decode(PemEntry).

-spec to_pem(public_key:rsa_public_key() | public_key:rsa_private_key()) -> any().
to_pem(RSA) ->
    public_key:pem_encode([public_key:pem_entry_encode(element(1, RSA), RSA)]).

-spec lookup(any()) -> {'ok', any()} | {'error', 'not_found'}.
lookup(KeyId) ->
    kz_cache:fetch_local(?PK_CACHE, KeyId).

-spec store(any(), any()) -> 'ok'.
store(KeyId, Key) ->
    lager:info("storing key ~p PEM in cache", [KeyId]),
    kz_cache:store_local(?PK_CACHE, KeyId, Key, cache_options(KeyId)).

-spec clear() -> 'ok'.
clear() ->
    kz_cache:flush_local(?PK_CACHE).

-spec clear(any()) -> 'ok'.
clear(KeyId) ->
    kz_cache:erase_local(?PK_CACHE, KeyId).

-spec cache_options(any()) -> list().
cache_options({'private', KeyId}) ->
    [{'origin', {'db', ?KZ_AUTH_DB, KeyId}}];
cache_options(KeyId) ->
    [{'origin', {'db', ?KZ_AUTH_DB, KeyId}}].

-spec from_token(map()) -> {'ok', rsa_key()} | {'error', 'not_found'}.
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
        #{key := Key, cached := 'true'} -> {'ok', Key};
        #{key_id := KeyId, key := Key, cached := 'false'} ->
            store(KeyId, Key),
            {'ok', Key};
        _Other ->
            lager:debug("public key not found : ~p", [_Other]),
            {'error', 'not_found'}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_token_fold(map(), list()) -> map().
from_token_fold(Token, []) -> Token;
from_token_fold(#{key := _Key}=Token, _) -> Token;
from_token_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> from_token_fold(NewToken, Routines)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("error running public key routine ~p : ~p , ~p", [Fun, _E, _R]),
        kz_log:log_stacktrace(ST),
        from_token_fold(Token, Routines)
        end.

-spec maybe_get_key(map()) -> map().
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
maybe_get_key(#{header := #{<<"kid">> := KeyId}} = Token) ->
    Token#{key_id => KeyId};
maybe_get_key(#{}=Token) -> Token.

-spec key_id(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> kz_term:api_binary().
key_id(<<"payload">>, Field, #{payload := Payload}) ->
    maps:get(Field, Payload, 'undefined');
key_id(<<"header">>, Field, Token) ->
    #{header := #{Field := KeyId}} = Token,
    KeyId;
key_id(_, _, _) -> 'undefined'.

-spec maybe_cached(map()) -> map().
maybe_cached(#{key_id := KeyId}=Token) ->
    case lookup(KeyId) of
        {'ok', Key} ->
            lager:debug("public key '~s' fetched from cache", [KeyId]),
            Token#{key => Key, cached => true};
        _ -> Token#{cached => false}
    end;
maybe_cached(#{}=Token) -> Token#{cached => false}.

-spec maybe_discovery(map()) -> map().
maybe_discovery(#{key_id := _KeyId
                 ,auth_provider := #{discovery := DiscoveryUrl}
                 }=Token) ->
    lager:debug("getting discovery document from ~s", [DiscoveryUrl]),
    case kz_auth_util:get_json_from_url(DiscoveryUrl) of
        {'ok', JObj} ->
            lager:debug_unsafe("obtained discovery document : ~s", [kz_json:encode(JObj,[pretty])]),
            Token#{discovery => JObj};
        _ -> Token
    end;
maybe_discovery(#{key_id := KeyId
                 ,auth_provider := #{name := <<"kazoo">>}
                 }=Token) ->
    Token#{key => public_key(KeyId)};
maybe_discovery(#{payload := #{<<"iss">> := <<"http", _/binary>> = Issuer}}=Token) ->
    DiscoveryUrl = <<Issuer/binary, "/.well-known/openid-configuration">>,
    case kz_auth_util:get_json_from_url(DiscoveryUrl) of
        {'ok', JObj} ->
            lager:debug_unsafe("obtained discovery document : ~s", [kz_json:encode(JObj,[pretty])]),
            Token#{discovery => JObj};
        _ -> Token
    end;
maybe_discovery(#{}=Token) ->
    Token.

-spec maybe_discovery_url(map()) -> map().
maybe_discovery_url(#{discovery := JObj
                     ,auth_provider := #{public_key_discovery_field := Field}
                     }=Token) ->
    lager:debug("verifying that ~s is in discovery document", [Field]),
    case kz_json:get_value(Field, JObj) of
        'undefined' -> Token;
        KeysUrl ->
            lager:debug("keys url ~s found in discovery document", [KeysUrl]),
            Token#{discovery_url => KeysUrl}
    end;
maybe_discovery_url(#{discovery := JObj
                     }=Token) ->
    Field = <<"jwks_uri">>,
    lager:debug("verifying that ~s is in discovery document", [Field]),
    case kz_json:get_value(Field, JObj) of
        'undefined' -> Token;
        KeysUrl ->
            lager:debug("keys url ~s found in discovery document", [KeysUrl]),
            Token#{discovery_url => KeysUrl}
    end;
maybe_discovery_url(#{}=Token) -> Token.

-spec fetch_from_url(map()) -> map().
fetch_from_url(#{discovery_url := Url}=Token) ->
    lager:debug("fetching keys from ~s", [Url]),
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
            lager:debug("error ~p obtaining public key", [_Err]),
            Token
    end;
fetch_from_url(#{}=Token) -> Token.

-spec fetch_key(map()) -> map().
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
    lager:debug("looking up public key sets in '~s' field in downloaded json : ~p", [Field, KeyDoc]),
    case kz_json:find_value(Field, KeyId, kz_json:get_value(<<"keys">>, KeyDoc)) of
        'undefined' ->
            lager:debug("public key not found from '~s' field in downloaded json : ~p", [Field, KeyDoc]),
            Token;
        JObj -> Token#{key_value => kz_json:to_map(JObj)}
    end;
fetch_key(#{key_id := KeyId
           ,key_doc := KeyDoc
           }= Token) ->
    Field = <<"kid">>,
    lager:debug("looking up public key sets in '~s' field in downloaded json : ~p", [Field, KeyDoc]),
    case kz_json:find_value(Field, KeyId, kz_json:get_value(<<"keys">>, KeyDoc)) of
        'undefined' ->
            lager:debug("public key not found from '~s' field in downloaded json : ~p", [Field, KeyDoc]),
            Token;
        JObj -> Token#{key_value => kz_json:to_map(JObj)}
    end;
fetch_key(#{}=Token) -> Token.

-spec extract_key(map()) -> map().
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

-spec public_key(kz_term:ne_binary()) -> public_key:rsa_public_key().
public_key(KeyId) ->
    {'ok', Key} = private_key(KeyId),
    get_public_key_from_private_key(Key).

-spec private_key(kz_term:ne_binary()) -> {'ok', public_key:rsa_private_key()} | {'error', any()}.
private_key(KeyId) ->
    case lookup({'private', KeyId}) of
        {'error', 'not_found'} -> load_private_key(KeyId);
        Found -> Found
    end.

-spec load_private_key(kz_term:ne_binary()) -> {'ok', public_key:rsa_private_key()} | {'error', any()}.
load_private_key(KeyId) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, KeyId) of
        {'ok', JObj} ->
            lager:info("found key doc ~s(~s)", [KeyId, kz_doc:revision(JObj)]),
            load_private_key_attachment(JObj);
        {'error', 'not_found'} ->
            lager:info("failed to find key ~s in db", [KeyId]),
            new_private_key(KeyId)
    end.

-spec load_private_key_attachment(kz_json:object()) -> {'ok', public_key:rsa_private_key()} | {'error', any()}.
load_private_key_attachment(JObj) ->
    KeyId = kz_doc:id(JObj),
    case kz_datamgr:fetch_attachment(?KZ_AUTH_DB, KeyId, ?SYSTEM_KEY_ATTACHMENT_NAME) of
        {'ok', PemContents} ->
            Key = from_pem(PemContents),
            store({'private', KeyId}, Key),
            {'ok', Key};
        {'error', 'not_found'} ->
            lager:debug("failed to find PEM attachment on ~s(~s), generating it", [KeyId, kz_doc:revision(JObj)]),
            {'ok', Key} = gen_private_key(),
            save_private_key(JObj, Key)
    end.

-spec new_private_key(kz_term:ne_binary()) -> {'ok', public_key:rsa_private_key()} | {'error', any()}.
new_private_key(KeyId) ->
    {'ok', Key} = gen_private_key(),
    new_private_key(KeyId, Key).

-spec new_private_key(kz_term:ne_binary(), public_key:rsa_private_key()) -> {'ok', public_key:rsa_private_key()} | {'error', any()}.
new_private_key(KeyId, Key) ->
    Doc = [{<<"pvt_type">>, <<"system_key">>}
          ,{<<"_id">>, KeyId}
          ],
    JObj = kz_doc:update_pvt_parameters(kz_json:from_list(Doc), ?KZ_AUTH_DB),

    case kz_datamgr:save_doc(?KZ_AUTH_DB, JObj) of
        {'ok', Saved} ->
            lager:info("created new key ~s(~s)", [KeyId, kz_doc:revision(Saved)]),
            save_private_key(Saved, Key);
        {'error', 'conflict'} ->
            lager:info("conflict adding key ~s(~s)", [KeyId, kz_doc:revision(JObj)]),
            private_key(KeyId);
        {'error', _Err}=Err ->
            lager:info("error ~p saving new system key ~s", [_Err, KeyId]),
            Err
    end.

-spec save_private_key(kz_json:object(), public_key:rsa_private_key()) -> {'ok', public_key:rsa_private_key()}.
save_private_key(JObj, Key) ->
    KeyId = kz_doc:id(JObj),
    Options = [{'doc_type', <<"system_key">>}
              ,{'rev', kz_doc:revision(JObj)}
              ,{'content_type', ?SYSTEM_KEY_ATTACHMENT_CTYPE}
              ],
    case kz_datamgr:put_attachment(?KZ_AUTH_DB, KeyId, ?SYSTEM_KEY_ATTACHMENT_NAME, to_pem(Key), Options) of
        {'ok', _JObj} ->
            store({'private', KeyId}, Key),
            {'ok', Key};
        {'error', 'conflict'} ->
            lager:info("conflict saving ~s(~s)", [KeyId, kz_doc:revision(JObj)]),
            private_key(KeyId);
        {'error', _Err}=Err ->
            lager:info("error ~p saving generated system key ~s", [_Err, KeyId]),
            Err
    end.

-spec gen_private_key() -> {'ok', public_key:rsa_private_key()}.
gen_private_key() ->
    gen_private_key(?RSA_KEY_SIZE).

-spec gen_private_key(integer()) -> {'ok', public_key:rsa_private_key()}.
gen_private_key(Size) ->
    Key = public_key:generate_key({'rsa', Size, Size + 1}),
    {'ok', Key}.

%% @equiv reset_private_key(kz_auth_apps:get_auth_app(<<"kazoo">>))
-spec reset_kazoo_private_key() -> {'ok', kz_term:ne_binary()} | {'error', any()}.
reset_kazoo_private_key() ->
    lager:warning("trying to reset kazoo private key"),
    reset_private_key(kz_auth_apps:get_auth_app(<<"kazoo">>)).

%%------------------------------------------------------------------------------
%% @doc Resets Kazoo private key. First get `KeyId' from config.
%% Checks if the document exists, if not create it. Generate a new private key
%% and put it in cache.
%% @end
%%------------------------------------------------------------------------------
-spec reset_private_key(map() | kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()} | {'error', any()}.
reset_private_key(#{pvt_server_key := KeyId}) ->
    reset_private_key(KeyId);
reset_private_key(#{}) ->
    {'error', 'invalid_identity_provider'};
reset_private_key(?NE_BINARY=KeyId) ->
    lager:warning("deleting private key ~s", [KeyId]),
    case kz_datamgr:del_doc(?KZ_AUTH_DB, KeyId) of
        {'ok', _}=OK -> OK;
        {'error', _Reason}=Error ->
            lager:error("failed to delete private key ~s: ~p", [KeyId, _Reason]),
            Error
    end.
