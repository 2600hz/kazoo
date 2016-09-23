%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
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

add_application(#{auth_app := _App}=Token) -> Token;
add_application(#{clientId := ClientId}=Token) ->
    Token#{auth_app => kz_auth_apps:get_auth_app(ClientId)};
add_application(#{client_id := ClientId}=Token) ->
    Token#{auth_app => kz_auth_apps:get_auth_app(ClientId)};
add_application(#{}=Token) -> Token.

add_provider(#{auth_app := #{pvt_auth_provider := Provider}}=Token) ->
    Token#{auth_provider => kz_auth_providers:get_auth_provider(Provider)};
add_provider(#{claims := #{iss :=Issuer}}=Token) ->
    Token#{auth_provider => kz_auth_providers:provider_by_issuer(Issuer)};
add_provider(#{}=Token) -> Token.


access_code(#{code := Code
             ,original := JObj
             } = Token) ->
    AppId = kz_json:get_first_defined(?APPID_KEYS, JObj),
    RedirectURI = kz_json:get_first_defined(?REDIRECT_URI_KEYS, JObj, <<"postmessage">>),
    case kz_auth_util:fetch_access_code(AppId, Code, RedirectURI) of
        {'ok', CodeJObj} ->
            Map = kz_auth_util:map_keys_to_atoms(kz_json:to_map(CodeJObj)),
            (maps:merge(Token, Map))#{original => kz_json:merge_jobjs(JObj, CodeJObj)};
        _ -> Token
    end;
access_code(#{} = Token) -> Token.

verify(#{verified_token := _}=Token) -> Token;
verify(#{auth_provider := #{token_info_url := TokenInfoUrl}
        ,access_token := AccessToken
        }=Token) ->
    URL = <<TokenInfoUrl/binary, AccessToken/binary>>,
    case kz_http:get(kz_util:to_list(URL)) of
        {'ok', 200, _RespHeaders, RespXML} ->
            Token#{verified_token => kz_json:decode(RespXML)};
        Else ->
            lager:info("unable to verify oauth token: ~p", [Else]),
            Token#{verified_token => kz_json:new()}
    end;
verify(#{}=Token) -> Token#{verified_token => kz_json:new()}.

create_claims(#{user_doc := Doc, profile := Profile}=Token) ->
    case lists:foldl(fun({K1, K2}, Acc) ->
                             case kz_json:find(K1, [Doc, Profile]) of
                                 'undefined' -> Acc;
                                 V -> [{K2, V} | Acc]
                             end
                     end, [], ?JWT_CLAIMS)
    of
        [] -> Token;
        Claims -> Token#{claims => Claims}
    end;
create_claims(#{}=Token) -> Token.

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


