%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_profile).

-export([token/1]).

-include("kazoo_auth.hrl").

-define(UPDATE_CHK_FIELDS, [<<"pvt_refresh_token">>
                           ,<<"pvt_static_token">>
                           ,<<"scope">>
                           ,<<"scopes">>
                           ,<<"email">>
                           ,<<"verified_email">>
                           ,<<"access_type">>
                           ,<<"profile">>
                           ,<<"pvt_account_id">>
                           ,<<"pvt_owner_id">>
                           ,<<"display_name">>
                           ,<<"photo_url">>
                           ]).

-define(PROFILE_EMAIL_FIELDS, [<<"email">>
                              ,<<"emailAddress">>
                              ,<<"email_address">>
                              ,<<"mail">>
                              ]).

-define(TOKEN_VERIFIED_EMAIL_FIELDS, [<<"email_verified">>
                                     ,<<"verified_email">>
                                     ]).

-define(SCOPE_SEPARATORS, [<<" ">>
                          ,<<",">>
                          ,<<";">>
                          ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec token(map()) -> map().
token(Token) ->
    Routines = [fun maybe_load_profile/1
               ,fun maybe_add_user_identity/1
               ,fun maybe_add_user_email/1
               ,fun maybe_add_display_name/1
               ,fun maybe_add_photo_url/1
               ,fun maybe_add_user/1
               ],
    token_fold(Token, Routines).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec token_fold(map(), list()) -> map().
token_fold(Token, []) -> Token;
token_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> token_fold(NewToken, Routines)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("exception executing ~p : ~p , ~p", [Fun, _E, _R]),
        kz_log:log_stacktrace(ST),
        token_fold(Token, Routines)
        end.

-spec profile_access_verb(map()) -> 'get' | 'post'.
profile_access_verb(Provider) ->
    case kz_term:to_atom(kz_maps:get('profile_access_verb', Provider, 'get'), 'true') of
        'get' -> 'get';
        'post' -> 'post';
        _ -> 'get'
    end.

-spec maybe_load_profile(map()) -> map().
maybe_load_profile(#{profile := _Profile} = Token) -> Token;
maybe_load_profile(#{user_map := #{<<"profile">> := Profile}} = Token) -> Token#{profile => kz_json:from_map(Profile)};
maybe_load_profile(#{auth_provider := #{profile_url := _ProfileURL} = Provider
                    ,access_token := AccessToken
                    } = Token) ->
    Options = [{'headers_as_is', 'true'}
              ,{'ssl', [{'versions', ['tlsv1.2']}]}
              ],
    URL = kz_term:to_list(profile_url(Token)),
    Verb = profile_access_verb(Provider),
    {'ok',{_,_, Host, _, _, _}} = http_uri:parse(URL),
    Headers = [{<<"host">>, Host}
               | profile_authorization_headers(Token, AccessToken)
              ],
    lager:debug("getting profile (~s) from ~s", [Verb, URL]),
    case kz_http:req(Verb, URL, Headers, <<>>, Options) of
        {'ok', 200, _RespHeaders, RespXML} ->
            Token#{profile => kz_json:decode(RespXML)};
        {'ok', 401, _RespHeaders, _RespXML} ->
            lager:info("received code ~b while getting auth profile from ~s", [401, URL]),
            Token#{profile_error_code => {401, 'invalid_profile'}, profile => kz_json:new()};
        {'ok', 404, _RespHeaders, _RespXML} ->
            lager:info("received faked code ~b while getting auth profile from ~s", [404, URL]),
            Token#{profile_error_code => {403, 'profile_not_found'}, profile => kz_json:new()};
        {'ok', Code, _RespHeaders, _RespXML} ->
            lager:debug("received code ~b while getting auth profile from ~s : ~p", [Code, URL, _RespXML]),
            props:to_log(_RespHeaders, <<"PROFILE ERROR">>),
            Token#{profile_error_code => {Code, 'invalid_profile'}, profile => kz_json:new()};
        {'error', _Error} ->
            lager:debug("failed to get auth profile: ~p", [_Error]),
            Token#{profile_error => {500, 'profile_unavailable'}, profile => kz_json:new()}
    end;
maybe_load_profile(#{auth_provider := #{profile_url := _ProfileURL}
                    ,original := Original
                    }=Token) ->
    maybe_load_profile(Token#{access_token => Original});
maybe_load_profile(#{} = Token) -> Token#{profile => kz_json:new()}.

-spec profile_authorization(map(), kz_term:ne_binary()) -> binary().
profile_authorization(#{auth_provider := Provider} = Token, AccessToken) ->
    case maps:get(profile_access_auth_type, Provider, <<"token">>) of
        <<"token">> ->
            lager:debug("using profile access token authorization header"),
            DefaultTokenType = maps:get('profile_access_token_type', Provider, <<"Bearer">>),
            TokenType = maps:get('token_type', Token, DefaultTokenType),
            <<TokenType/binary, " ",AccessToken/binary>>;
        <<"api_key">> ->
            lager:debug("using profile api key authorization header"),
            list_to_binary(["API_KEY ", maps:get('profile_access_api_key', Provider, <<>>)]);
        <<"url">> -> <<>>
    end.

-spec profile_authorization_headers(map(), kz_term:ne_binary()) -> kz_term:proplist().
profile_authorization_headers(Provider, AccessToken) ->
    case profile_authorization(Provider, AccessToken) of
        <<>> -> [];
        Authorization -> [{"Authorization",kz_term:to_list(Authorization)}]
    end.

-spec profile_url(map()) -> binary().
profile_url(#{auth_provider := #{profile_url := ProfileURL} = Provider
             ,access_token := AccessToken
             }=Token) ->
    case maps:get('profile_access_auth_type', Provider, <<"token">>) of
        <<"token">> -> maybe_compose_profile_url(ProfileURL, Token);
        <<"api_key">> -> maybe_compose_profile_url(ProfileURL, Token);
        <<"url">> -> maybe_compose_profile_url(<<ProfileURL/binary, AccessToken/binary>>, Token)
    end.

-define(PROFILE_URL_REGEX, <<"\:([^\/]+)">>).
-define(PROFILE_URL_REGEX_OPTIONS, [{'capture', 'all_but_first', 'binary'}, 'global']).
-define(PROFILE_URL_REPLACE_OPTIONS, ['global', {'return', 'binary'}]).

-spec maybe_compose_profile_url(kz_term:ne_binary(), map()) -> binary().
maybe_compose_profile_url(Url, Token) ->
    case re:run(Url, ?PROFILE_URL_REGEX, ?PROFILE_URL_REGEX_OPTIONS) of
        {'match', [_ | _] = Fields} -> compose_profile_url(Url, lists:flatten(Fields), Token);
        _ -> Url
    end.

-spec compose_profile_url(kz_term:ne_binary(), kz_term:ne_binaries(), map()) -> binary().
compose_profile_url(Url, Fields, Token) ->
    Payload = maps:get('payload', Token, #{}),
    lists:foldl(fun(Field, Acc) ->
                        V = kz_maps:get(Field, Payload, <<>>),
                        re:replace(Acc, <<":", Field/binary>>, V, ?PROFILE_URL_REPLACE_OPTIONS)
                end
               ,Url
               ,Fields
               ).

-spec maybe_add_user_identity(map()) -> map().
maybe_add_user_identity(#{user_identity := _Identity} = Token) -> Token;
maybe_add_user_identity(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_user_identity(#{auth_provider := #{profile_identity_field := Field}
                         ,profile := Profile
                         } = Token) ->
    case kz_json:get_first_defined([Field], Profile) of
        'undefined' ->
            lager:debug("user identity from field '~s' not found in profile: ~s"
                       ,[Field, kz_json:encode(Profile)]
                       ),
            Token;
        Identity ->
            lager:debug("found user identity ~p", [Identity]),
            Token#{user_identity => Identity}
    end;
maybe_add_user_identity(#{auth_provider := #{profile_identity_fields := Fields}
                         ,profile := Profile
                         } = Token) ->
    Keys = lists:filter(fun(Field) -> kz_json:get_value(Field, Profile) =/= 'undefined' end, Fields),
    case length(Keys) =:= length(Fields) of
        'false' ->
            lager:debug("user identity from fields '~p' not found into ~p", [Fields, Profile]),
            Token;
        'true' ->
            [First | Others] = Fields,
            V1 = kz_term:to_binary(kz_json:get_value(First, Profile)),
            Identity = lists:foldl(fun(K, Acc) ->
                                           V = kz_term:to_binary(kz_json:get_value(K, Profile)),
                                           <<Acc/binary, "-", V/binary>>
                                   end
                                  ,V1
                                  ,Others
                                  ),
            lager:debug("found user identity ~p", [Identity]),
            Token#{user_identity => Identity}
    end;
maybe_add_user_identity(#{auth_provider := #{name := Prov}}=Token) ->
    lager:debug("provider '~s' doesn't support identity profile info", [Prov]),
    Token.

-spec maybe_add_display_name(map()) -> map().
maybe_add_display_name(#{display_name := _DisplayName} = Token) -> Token;
maybe_add_display_name(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_display_name(#{auth_provider := #{profile_displayName_field := Field}
                        ,profile := Profile
                        } = Token) ->
    case kz_json:get_first_defined([Field], Profile) of
        'undefined' ->
            lager:debug("user displayName from field '~p' not found into ~p", [Field, Profile]),
            Token;
        DisplayName ->
            lager:debug("found user displayName ~p", [DisplayName]),
            Token#{display_name => DisplayName}
    end;
maybe_add_display_name(#{auth_provider := #{name := Prov}}=Token) ->
    lager:debug("provider '~s' doesn't support displayName profile info", [Prov]),
    Token.

-spec maybe_add_photo_url(map()) -> map().
maybe_add_photo_url(#{photo_url := _PhotoUrl} = Token) -> Token;
maybe_add_photo_url(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_photo_url(#{auth_provider := #{profile_photo_url_field := Field}
                     ,profile := Profile
                     } = Token) ->
    case kz_json:get_first_defined([Field], Profile) of
        'undefined' ->
            lager:debug("user photoUrl from field '~p' not found into ~p", [Field, Profile]),
            Token;
        PhotoUrl ->
            lager:debug("found user photoUrl ~p", [PhotoUrl]),
            Token#{photo_url => PhotoUrl}
    end;
maybe_add_photo_url(Token) -> Token.

-spec maybe_add_user_email(map()) -> map().
maybe_add_user_email(#{user_email := _UserEmail} = Token) -> Token;
maybe_add_user_email(#{verified_token := VerifiedToken} = Token)
  when VerifiedToken =/= ?EMPTY_JSON_OBJECT ->
    Token#{user_email => kz_json:get_first_defined(?PROFILE_EMAIL_FIELDS, VerifiedToken)};
maybe_add_user_email(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_user_email(#{auth_provider := #{profile_email_field := Field}
                      ,profile := Profile
                      } = Token) ->
    Payload = kz_json:from_map(maps:get('payload', Token, #{})),
    case kz_json:find_first_defined([Field | ?PROFILE_EMAIL_FIELDS], [Payload, Profile]) of
        'undefined' ->
            lager:debug("user email from ~p not found", [Field]),
            Token#{user_email => 'undefined'};
        EMail ->
            lager:debug("found user email ~p", [EMail]),
            Token#{user_email => EMail}
    end;
maybe_add_user_email(#{profile := Profile} = Token) ->
    Payload = kz_json:from_map(maps:get('payload', Token, #{})),
    case kz_json:find_first_defined(?PROFILE_EMAIL_FIELDS, [Payload, Profile]) of
        'undefined' ->
            lager:debug("user email from known fields not found"),
            Token#{user_email => 'undefined'};
        EMail ->
            lager:debug("found user email ~p", [EMail]),
            Token#{user_email => EMail}
    end;
maybe_add_user_email(#{auth_provider := #{name := Prov}}=Token) ->
    lager:debug("provider '~s' doesn't support email profile info", [Prov]),
    Token.

-spec maybe_add_user(map()) -> map().
maybe_add_user(#{user_doc := _DocObj, user_map := _Map} = Token) -> Token;
maybe_add_user(#{user_doc := DocObj} = Token) -> Token#{user_map => kz_json:to_map(DocObj)};
maybe_add_user(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_user(#{auth_app := #{pvt_user_prefix := Prefix}
                ,user_identity := Identity
                } = Token) ->
    DocId = <<Prefix/binary, "-",Identity/binary>>,
    lager:debug("loading profile data from ~s/~s", [?KZ_AUTH_DB, DocId]),
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', OAuthDoc} -> maybe_update_user(DocId, OAuthDoc, Token);
        {'error', 'not_found'} -> update_user(DocId, format_user_doc(Token), Token)
    end;
maybe_add_user(#{auth_provider := #{name := Prefix}
                ,user_identity := Identity
                } = Token) ->
    DocId = <<Prefix/binary, "-",Identity/binary>>,
    lager:debug("loading profile data from ~s/~s", [?KZ_AUTH_DB, DocId]),
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', OAuthDoc} -> maybe_update_user(DocId, OAuthDoc, Token);
        {'error', 'not_found'} -> update_user(DocId, format_user_doc(Token), Token)
    end;
maybe_add_user(#{} = Token) ->
    lager:debug("identity not set, skip adding user"),
    Token.

-spec ensure_profile_properties(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist(), map()) -> map().
ensure_profile_properties(DocId, Missing, Props, #{} = Token) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', Doc} ->
            case Missing -- kz_json:get_keys(Doc) of
                [] -> do_update_user(DocId, Props, Token);
                _StillMissing ->
                    lager:info("missing properties when updating user : ~p", [kz_binary:join(_StillMissing)]),
                    Token#{profile_error_code => {403, 'invalid_profile'}, profile => kz_json:new()}
            end;
        _ ->
            lager:info("missing properties when updating user : ~p", [kz_binary:join(Missing)]),
            Token#{profile_error_code => {403, 'invalid_profile'}, profile => kz_json:new()}
    end.

-spec update_user(kz_term:ne_binary(), kz_term:proplist(), map()) -> map().
update_user(DocId, Props, #{auth_provider := #{profile_required_props := RequiredProps}} = Token) ->
    case RequiredProps -- props:get_keys(Props) of
        [] -> do_update_user(DocId, Props, Token);
        Missing -> ensure_profile_properties(DocId, Missing, Props, Token)
    end;
update_user(DocId, Props, Token) ->
    do_update_user(DocId, Props, Token).

-spec do_update_user(kz_term:ne_binary(), kz_term:proplist(), map()) -> map().
do_update_user(DocId, Props, Token) ->
    UpdateOptions = [{'update', Props}],
    case kz_datamgr:update_doc(?KZ_AUTH_DB, DocId, UpdateOptions) of
        {'ok', DocObj} ->
            maybe_cache_user(Token#{user_doc => DocObj
                                   ,user_map => kz_json:to_map(DocObj)
                                   }
                            ,DocId
                            );
        {'error', _Error} ->
            lager:debug("unable to update auth document ~s: ~p", [DocId, _Error]),
            Token#{profile_error_code => {500, 'datastore_fault'}
                  ,profile => kz_json:new()
                  }
    end.

-spec maybe_cache_user(map(), kz_term:ne_binary()) -> map().
maybe_cache_user(#{auth_provider := #{profile_cache_timer := Timer}
                  ,user_identity := Identity
                  } = Token
                ,DocId) ->
    Props = [{'origin', {'db', ?KZ_AUTH_DB, DocId}}
            ,{'expires', Timer}
            ],
    kz_cache:store_local(?PROFILE_CACHE, DocId, Identity, Props),
    Token;
maybe_cache_user(Token, _DocId) -> Token.

-spec maybe_update_user(kz_term:ne_binary(), kz_json:object(), map()) -> map().
maybe_update_user(DocId, JObj, Token) ->
    Props = format_user_doc(Token),
    case updates_needed(JObj, Props) of
        [] -> maybe_required_properties_missing(Token, Props, JObj);
        Updates -> update_user(DocId, Updates, Token)
    end.

-spec maybe_required_properties_missing(map(), kz_term:proplist(), kz_json:object()) -> map().
maybe_required_properties_missing(#{auth_provider := #{profile_required_props := RequiredProps}
                                   } = Token, Props, JObj) ->
    case RequiredProps -- props:get_keys(Props) of
        [] -> maybe_cache_user(Token#{user_doc => JObj
                                     ,user_map => kz_json:to_map(JObj)
                                     }
                              ,kz_doc:id(JObj)
                              );
        Missing ->
            lager:info("missing properties when checking user : ~p", [kz_binary:join(Missing)]),
            Token#{profile_error_code => {403, 'invalid_profile'}, profile => kz_json:new()}
    end;
maybe_required_properties_missing(Token, _Props, JObj) ->
    Token#{user_doc => JObj
          ,user_map => kz_json:to_map(JObj)
          }.

-spec updates_needed(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
updates_needed(JObj, Props) ->
    lists:foldl(fun(K, KVs) ->
                        case {props:get_value(K, Props)
                             ,kz_json:get_value(K, JObj)
                             }
                        of
                            {'undefined', _} -> KVs;
                            {Value, Value} -> KVs;
                            {Value, _} ->[{K, Value} | KVs]
                        end
                end
               ,[]
               ,?UPDATE_CHK_FIELDS
               ).

-spec format_user_doc(map()) -> kz_term:proplist().
format_user_doc(#{auth_provider := #{name := ProviderId} = Provider
                 ,profile := Profile
                 ,user_identity := Identity
                 }=Token) ->
    Verified = maps:get('verified_token', Token, kz_json:new()),
    Original = maps:get('original', Token, kz_json:new()),
    Lookup = lists:filter(fun kz_json:is_json_object/1, [Profile, Verified, Original]),
    Scope = kz_json:find_first_defined([<<"scope">>], Lookup, <<>>),
    App = maps:get('auth_app', Token, #{}),
    AppId = maps:get('name', App, 'undefined'),
    AppAccountId = maps:get('pvt_account_id', App, 'undefined'),
    EMail = maps:get('user_email', Token, 'undefined'),

    Mapping = maps:get('profile_account_mapping', Provider, #{}),
    MapFields = maps:fold(fun(K, V, Acc) ->
                                  case kz_json:get_value(V, Profile) of
                                      'undefined' -> Acc;
                                      Value -> [{kz_term:to_binary(K), Value} | Acc]
                                  end
                          end
                         ,[]
                         ,Mapping
                         ),

    Props = [{<<"email">>, EMail}
            ,{<<"verified_email">>, kz_json:get_first_defined(?TOKEN_VERIFIED_EMAIL_FIELDS, Verified)}
            ,{<<"access_type">>, maps:get('access_type', Token, 'undefined')}
            ,{<<"scope">>, Scope}
            ,{<<"scopes">>, binary:split(Scope, ?SCOPE_SEPARATORS, ['global'])}
            ,{<<"profile">>, Profile}
            ,{<<"display_name">>, maps:get('display_name', Token, 'undefined')}
            ,{<<"photo_url">>, maps:get('photo_url', Token, 'undefined')}
            ,{<<"pvt_app_id">>, AppId}
            ,{<<"pvt_app_provider_id">>, ProviderId}
            ,{<<"pvt_app_account_id">>, AppAccountId}
            ,{<<"pvt_account_id">>, maps:get('linked_account_id', Token, 'undefined')}
            ,{<<"pvt_owner_id">>, maps:get('linked_owner_id', Token, 'undefined')}
            ,{<<"pvt_type">>, <<"user">>}
            ,{<<"pvt_user_identity">>, Identity}
            ,{<<"pvt_refresh_token">>, maps:get('refresh_token', Token, 'undefined')}
            ,{<<"pvt_static_token">>, maps:get('static_token', Token, 'undefined')}
            ] ++ MapFields,
    props:filter_empty(Props).
