%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_client).

-include("kazoo_auth.hrl").

-define(AUTH_BY_SYSTEM_IDS, <<"auth/auth_by_system_ids">>).

%% ====================================================================
%% API functions
%% ====================================================================
-export([token_for_app/1, token_for_app/2
        ,token_for_auth_id/1, token_for_auth_id/2
        ]).


-spec token_for_auth_id(ne_binary()) -> {ok | error, map()}.
token_for_auth_id(AuthId) ->
    token_for_auth_id(AuthId, #{}).

-spec token_for_auth_id(ne_binary(), map()) -> {ok | error, map()}.
token_for_auth_id(AuthId, Options) ->
    Map = #{options => Options#{auth_id => AuthId}},
    Routines = [fun add_subject/1
               ,fun add_app/1
               ,fun add_provider_claims/1
               ,fun add_app_claims/1
               ,fun add_optional_claims/1
               ,fun add_audience/1
               ,fun add_scope/1
               ,fun add_subject_claim/1
               ,fun request_token/1
               ,fun authorization_header/1
               ],
    kz_auth_util:run(Map, Routines).


-spec token_for_app(ne_binary()) -> {ok | error, map()}.
token_for_app(AppId) ->
    token_for_app(AppId, #{}).

-spec token_for_app(ne_binary(), map()) -> {ok | error, map()}.
token_for_app(AppId, Options) ->
    Map = #{app_id => AppId
           ,options => Options
           },
    Routines = [fun add_app/1
               ,fun add_provider_claims/1
               ,fun add_app_claims/1
               ,fun add_optional_claims/1
               ,fun add_audience/1
               ,fun add_scope/1
               ,fun add_subject/1
               ,fun add_subject_claim/1
               ,fun request_token/1
               ,fun authorization_header/1
               ],
    kz_auth_util:run(Map, Routines).


%% ====================================================================
%% Internal functions
%% ====================================================================

add_app(#{app_id := AppId}=Map) ->
    case kz_auth_apps:get_auth_app(AppId, 'app_and_provider') of
        {'error', _} = Error -> Map#{error => Error};
        #{}=App -> kz_maps:merge(Map#{claims => #{iss => AppId}}, App)
    end;
add_app(#{subject := #{pvt_app_id := AppId}}=Map) ->
    case kz_auth_apps:get_auth_app(AppId, 'app_and_provider') of
        {'error', _} = Error -> Map#{error => Error};
        #{}=App -> kz_maps:merge(Map#{claims => #{iss => AppId}}, App)
    end;
add_app(Map) ->
    Map#{error => <<"no app_id">>}.

add_provider_claims(#{auth_provider := #{jwt_flow := #{claims := ProviderClaims}}
                     ,claims := Claims
                     }=Map) ->
    Map#{claims => maps:merge(Claims, add_claims(ProviderClaims))};
add_provider_claims(#{}=Map) -> Map.

add_app_claims(#{auth_app := #{jwt_flow := #{claims := AppClaims}}
                ,claims := Claims
                }=Map) ->
    Map#{claims => maps:merge(Claims, add_claims(AppClaims))};
add_app_claims(#{}=Map) -> Map.

add_optional_claims(#{options := #{claims := OptionalClaims}
                     ,claims := Claims
                     }=Map) ->
    Map#{claims => maps:merge(Claims, OptionalClaims)};
add_optional_claims(#{}=Map) -> Map.

add_audience(#{options := #{audience := Aud}
              ,claims := Claims
              }=Map) ->
    Map#{claims => Claims#{aud => Aud}};
add_audience(#{auth_app := #{jwt_flow := #{audience := Aud}}
              ,claims := Claims
              }=Map) ->
    Map#{claims => Claims#{aud => Aud}};
add_audience(#{auth_provider := #{jwt_flow := #{audience := Aud}}
              ,claims := Claims
              }=Map) ->
    Map#{claims => Claims#{aud => Aud}};
add_audience(#{}=Map) -> Map.

add_scope(#{claims := Claims
           ,options := #{scopes := Scopes}
           }=Map) ->
    Scope = kz_binary:join(Scopes, <<" ">>),
    Map#{claims => Claims#{scope => Scope}};
add_scope(#{}=Map) -> Map.


add_subject(#{auth_app := #{name := AppId}
             ,options := #{subject := {AccountId, OwnerId}}
             }=Map) ->
    Options = [{key, [AppId, AccountId, OwnerId]}
              ,'include_docs'
              ],
    case kz_datamgr:get_single_result(?KZ_AUTH_DB, ?AUTH_BY_SYSTEM_IDS, Options) of
        {'ok', Doc} ->
            Subject = kz_json:to_map(kz_json:get_value(<<"doc">>, Doc)),
            Map#{subject => kz_maps:keys_to_atoms(Subject, 'false')};
        _ -> Map
    end;
add_subject(#{auth_app := #{name := AppId}
             ,options := #{account_id := AccountId
                          ,owner_id := OwnerId
                          }
             }=Map) ->
    Options = [{key, [AppId, AccountId, OwnerId]}
              ,'include_docs'
              ],
    case kz_datamgr:get_single_result(?KZ_AUTH_DB, ?AUTH_BY_SYSTEM_IDS, Options) of
        {'ok', Doc} ->
            Subject = kz_json:to_map(kz_json:get_value(<<"doc">>, Doc)),
            Map#{subject => kz_maps:keys_to_atoms(Subject, 'false')};
        _ -> Map
    end;
add_subject(#{options := #{subject := SubjectId}
             }=Map) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, SubjectId) of
        {'ok', Doc} ->
            Subject = kz_json:to_map(Doc),
            Map#{subject => kz_maps:keys_to_atoms(Subject, 'false')};
        _ -> Map
    end;
add_subject(#{options := #{auth_id := AuthId}
             }=Map) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, AuthId) of
        {'ok', Doc} ->
            Subject = kz_json:to_map(Doc),
            Map#{subject => kz_maps:keys_to_atoms(Subject, 'false')};
        _ -> Map
    end;
add_subject(#{}=Map) -> Map.

add_subject_claim(#{options := #{sub := Sub}
                   ,claims := Claims
                   }=Map) ->
    Map#{claims => Claims#{sub => Sub}};
add_subject_claim(#{auth_app := #{jwt_flow := #{sub := Sub}}
                   ,subject := #{profile := Profile} = Subject
                   ,claims := Claims
                   }=Map) ->
    case kz_maps:get(Sub, Profile) of
        undefined -> case kz_maps:get(kz_term:to_atom(Sub, 'true'), Subject) of
                         undefined -> Map;
                         Value -> Map#{claims => Claims#{sub => Value}}
                     end;
        Value -> Map#{claims => Claims#{sub => Value}}
    end;
add_subject_claim(#{auth_provider := #{jwt_flow := #{sub := Sub}}
                   ,subject := #{profile := Profile} = Subject
                   ,claims := Claims
                   }=Map) ->
    case kz_maps:get(Sub, Profile) of
        undefined -> case kz_maps:get(kz_term:to_atom(Sub, 'true'), Subject) of
                         undefined -> Map;
                         Value -> Map#{claims => Claims#{sub => Value}}
                     end;
        Value -> Map#{claims => Claims#{sub => Value}}
    end;
add_subject_claim(#{}=Map) -> Map.


authorization_header(#{token := #{token_type := TokenType
                                 ,access_token := AccessToken
                                 } = Token
                      }=Map) ->
    Authorization = <<TokenType/binary, " ", AccessToken/binary>>,
    Map#{token => Token#{authorization => Authorization}};
authorization_header(Map) -> Map.

request_token(#{subject := #{pvt_refresh_token := Token}}=Map) ->
    refresh_token_flow(Map#{refresh_token => Token});
request_token(Map) ->
    jwt_flow(Map).

jwt_flow(Map) ->
    Routines = [fun jwt_url/1
               ,fun jwt_issuer/1
               ,fun jwt_assertion/1
               ,fun jwt_request/1
               ],
    kz_auth_util:run(Map, Routines).

jwt_url(#{auth_app := #{jwt_flow := #{auth_url := URL}}}=Map) ->
    Map#{auth_url => URL};
jwt_url(#{auth_provider := #{jwt_flow := #{auth_url := URL}}}=Map) ->
    Map#{auth_url => URL};
jwt_url(#{auth_app := #{auth_url := URL}}=Map) ->
    Map#{auth_url => URL};
jwt_url(#{auth_provider := #{auth_url := URL}}=Map) ->
    Map#{auth_url => URL};
jwt_url(Map) ->
    {error, Map#{error => <<"no auth_url">>}}.

jwt_issuer(#{auth_app := #{jwt_flow := #{iss := Issuer}}
            ,claims := Claims
            }=Map) ->
    Map#{claims => Claims#{iss => Issuer}};
jwt_issuer(Map) -> Map.

jwt_assertion(Map) ->
    case kz_auth_jwt:encode(Map) of
        {ok, JWT} -> Map#{jwt => JWT};
        {error, Error} -> {error, Map#{error => Error}}
    end.

jwt_request(#{auth_url := URL
             ,jwt := JWT
             }=Map) ->
    GrantType = kz_term:to_list(kz_util:uri_encode(?OAUTH_GRANT_TYPE)),
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}
              ,{"User-Agent", "Kazoo"}
              ],
    Fields = [{"assertion", kz_term:to_list(kz_util:uri_encode(JWT))}
             ,{"grant_type", GrantType}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
    case kz_http:post(kz_term:to_list(URL), Headers, Body) of
        {'ok', 200, RespHeaders, RespBody} ->
            JObj = kz_json:decode(RespBody),
            M = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
            Map#{token => M#{json => JObj
                            ,headers => RespHeaders
                            }
                };
        {_Reply, _Code, _RespHeaders, RespBody} ->
            lager:debug("received ~p/~p : ~p", [_Reply, _Code, RespBody]),
            Map#{error => kz_term:to_binary(io_lib:format("unable to request service token: ~p", [RespBody]))}
    end.

refresh_token_flow(#{auth_app := #{name := AppId
                                  ,pvt_secret := Secret
                                  }
                    ,auth_provider := #{auth_url := URL}
                    ,refresh_token := RefreshToken
                    } = Map) ->
    lager:debug("getting token : refresh ~p",[RefreshToken]),
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}],
    Fields = [{"client_id", kz_term:to_list(AppId)}
             ,{"client_secret",kz_term:to_list(Secret)}
             ,{"grant_type","refresh_token"}
             ,{"refresh_token",kz_term:to_list(RefreshToken)}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=")] end, Fields)), "&"),
    case kz_http:post(kz_term:to_list(URL), Headers, Body) of
        {'ok', 200, RespHeaders, RespBody} ->
            JObj = kz_json:decode(RespBody),
            M = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
            Map#{token => M#{json => JObj
                            ,headers => RespHeaders
                            }
                };
        {_Reply, _Code, _RespHeaders, RespBody} ->
            lager:debug("received ~p/~p : ~p", [_Reply, _Code, RespBody]),
            Map#{error => kz_term:to_binary(io_lib:format("unable to request service token: ~p", [RespBody]))}
    end.


add_claims(Claims) ->
    maps:from_list(lists:map(fun add_claim/1, Claims)).

add_claim(<<"iat">>) -> {iat, kz_time:current_unix_tstamp()-500};
add_claim(<<"exp">>) -> {exp, kz_time:current_unix_tstamp()+(2 * ?MILLISECONDS_IN_SECOND)}.
