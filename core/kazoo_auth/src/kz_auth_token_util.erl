%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(kz_auth_token_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_application/1
        ,add_provider/1
        ,verify/1
        ,access_code/1
        ,create_claims/1
        ,id_token/1
        ]).

-include("kazoo_auth.hrl").

-spec add_application(map()) -> map().
add_application(#{auth_app := _App}=Token) -> Token;
add_application(#{clientId := ClientId}=Token) ->
    Token#{auth_app => kz_auth_apps:get_auth_app(ClientId)};
add_application(#{client_id := ClientId}=Token) ->
    Token#{auth_app => kz_auth_apps:get_auth_app(ClientId)};
add_application(#{}=Token) -> Token.

-spec add_provider(map()) -> map().
add_provider(#{auth_app := #{pvt_auth_provider := Provider}}=Token) ->
    Token#{auth_provider => kz_auth_providers:get_auth_provider(Provider)};
add_provider(#{claims := #{iss :=Issuer}}=Token) ->
    Token#{auth_provider => kz_auth_providers:provider_by_issuer(Issuer)};
add_provider(#{}=Token) -> Token.


-spec access_code(map()) -> map().
access_code(#{code := Code
             ,original := JObj
             } = Token) ->
    AppId = kz_json:get_first_defined(?APPID_KEYS, JObj),
    RedirectURI = kz_json:get_first_defined(?REDIRECT_URI_KEYS, JObj, <<"postmessage">>),
    lager:debug("trying to get access_code ~p for ~p with redirect ~p", [Code, AppId, RedirectURI]),
    case kz_auth_util:fetch_access_code(AppId, Code, RedirectURI) of
        {'ok', CodeJObj} ->
            Map = kz_maps:keys_to_atoms(kz_json:to_map(CodeJObj)),
            (maps:merge(Token, Map))#{original => kz_json:merge_jobjs(JObj, CodeJObj)};
        _Else ->
            lager:error("not expected ~p", [_Else]),
            Token
    end;
access_code(#{} = Token) -> Token.

-spec verify(map()) -> map().
verify(#{verified_token := _}=Token) -> Token;
verify(#{auth_provider := #{token_info_url := TokenInfoUrl}
        ,access_token := AccessToken
        }=Token) ->
    URL = <<TokenInfoUrl/binary, AccessToken/binary>>,
    case kz_http:get(kz_term:to_list(URL)) of
        {'ok', 200, _RespHeaders, RespBody} ->
            RespJObj = kz_json:decode(RespBody),
            Token#{verified_token => RespJObj};
        Else ->
            lager:info("unable to verify oauth token: ~p", [Else]),
            Token#{verified_token => kz_json:new()}
    end;
verify(#{}=Token) -> Token#{verified_token => kz_json:new()}.

-spec create_claims(map()) -> map().
create_claims(#{user_doc := Doc, profile := Profile}=Token) ->
    case build_claims(?JWT_CLAIMS, [Doc, Profile]) ++
        build_claims(?JWT_MAP_CLAIMS, Token)
    of
        [] -> Token;
        Claims -> Token#{claims => Claims}
    end;
create_claims(#{}=Token) -> Token.

-type claim_map_input() :: map() | kz_json:objects().

-spec build_claims(kz_proplist(), claim_map_input()) -> kz_proplist().
build_claims(ClaimsMap, BuildFrom) ->
    build_claims(ClaimsMap, BuildFrom, []).

-spec build_claims(kz_proplist(), claim_map_input(), kz_proplist()) -> kz_proplist().
build_claims([], _, Claims) -> Claims;
build_claims([{K1, K2} | KVs], Map, Claims)
  when is_map(Map) ->
    case kz_maps:get(K1, Map) of
        'undefined' -> build_claims(KVs, Map, Claims);
        V -> build_claims(KVs, Map, [{K2, V} | Claims])
    end;
build_claims([{K1, K2} | KVs], JObjs, Claims)
  when is_list(JObjs) ->
    case kz_json:find(K1, JObjs) of
        'undefined' -> build_claims(KVs, JObjs, Claims);
        V -> build_claims(KVs, JObjs, [{K2, V} | Claims])
    end.

-spec id_token(map()) -> map().
id_token(#{claims := _Claims}=Token) -> Token;
id_token(#{id_token := IdToken}=Token) ->
    case kz_auth_jwt:decode(IdToken) of
        {'ok', _Header, Claims} -> Token#{claims => Claims};
        _ -> Token
    end;
id_token(#{}=Token) -> Token#{claims => kz_json:new()}.

%% ====================================================================
%% Internal functions
%% ====================================================================
