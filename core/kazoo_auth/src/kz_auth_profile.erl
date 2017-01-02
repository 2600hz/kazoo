%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_profile).

-export([token/1]).

-include("kazoo_auth.hrl").

-define(UPDATE_CHK_FIELDS, [<<"refresh_token">>
                           ,<<"scope">>
                           ,<<"scopes">>
                           ,<<"email">>
                           ,<<"verified_email">>
                           ,<<"access_type">>
                           ,<<"profile">>
                           ,<<"pvt_account_id">>
                           ,<<"pvt_owner_id">>
                           ]).

-define(PROFILE_EMAIL_FIELDS, [<<"email">>
                              ,<<"emailAddress">>
                              ,<<"email_address">>
                              ]).

-define(SCOPE_SEPARATORS, [<<" ">>
                          ,<<",">>
                          ,<<";">>
                          ]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec token(map()) -> map().
token(Token) ->
    Routines = [fun maybe_load_profile/1
               ,fun maybe_add_user_identity/1
               ,fun maybe_add_user_email/1
               ,fun maybe_add_user/1
               ],
    token_fold(Token, Routines).


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec token_fold(map(), list()) -> map().
token_fold(Token, []) -> Token;
token_fold(Token, [Fun | Routines]) ->
    try Fun(Token) of
        NewToken -> token_fold(NewToken, Routines)
    catch
        _E:_R ->
            lager:debug("exception executing ~p : ~p , ~p", [Fun, _E, _R]),
            kz_util:log_stacktrace(),
            token_fold(Token, Routines)
    end.

-spec maybe_load_profile(map()) -> map().
maybe_load_profile(#{profile := _Profile} = Token) -> Token;
maybe_load_profile(#{user_map := #{<<"profile">> := Profile}} = Token) -> Token#{profile => kz_json:from_map(Profile)};
maybe_load_profile(#{auth_provider := #{profile_url := _ProfileURL}
                    ,access_token := AccessToken
                    } = Token) ->
    Headers = profile_authorization_headers(Token, AccessToken),
    URL = profile_url(Token),
    lager:debug("getting profile from ~s", [URL]),
    case kz_http:get(kz_util:to_list(URL), Headers) of
        {'ok', 200, _RespHeaders, RespXML} ->
            Token#{profile => kz_json:decode(RespXML)};
        {'ok', 401, _RespHeaders, _RespXML} ->
            lager:debug("received code ~b while getting auth profile from ~s", [401, URL]),
            Token#{profile_error_code => {401, <<"unauthorized token">>}, profile => kz_json:new()};
        {'ok', 404, _RespHeaders, _RespXML} ->
            lager:debug("received faked code ~b while getting auth profile from ~s", [404, URL]),
            Token#{profile_error_code => {404, <<"profile not found">>}, profile => kz_json:new()};
        {'ok', Code, _RespHeaders, _RespXML} ->
            lager:debug("received code ~b while getting auth profile from ~s", [Code, URL]),
            Token#{profile_error_code => {Code, <<"unspecified error getting profile">>}, profile => kz_json:new()};
        {'error', Error} ->
            lager:debug("failed to get auth profile: ~p", [Error]),
            Token#{profile_error => {500, Error}, profile => kz_json:new()}
    end;
maybe_load_profile(#{auth_provider := #{profile_url := _ProfileURL}
                    ,original := Original
                    }=Token) ->
    maybe_load_profile(Token#{access_token => Original});
maybe_load_profile(#{} = Token) -> Token#{profile => kz_json:new()}.

-spec profile_authorization(map(), ne_binary()) -> binary().
profile_authorization(#{auth_provider := Provider} = Token, AccessToken) ->
    case maps:get(profile_access_auth_type, Provider, <<"token">>) of
        <<"token">> ->
            DefaultTokenType = maps:get(profile_access_token_type, Provider, <<"Bearer">>),
            TokenType = maps:get(token_type, Token, DefaultTokenType),
            <<TokenType/binary, " ",AccessToken/binary>>;
        <<"api_key">> ->
            list_to_binary(["API_KEY ", maps:get(profile_access_api_key, Provider, <<>>)]);
        <<"url">> -> <<>>
    end.

-spec profile_authorization_headers(map(), ne_binary()) -> kz_proplist().
profile_authorization_headers(Provider, AccessToken) ->
    case profile_authorization(Provider, AccessToken) of
        <<>> -> [];
        Authorization -> [{"Authorization",kz_util:to_list(Authorization)}]
    end.

-spec profile_url(map()) -> binary().
profile_url(#{auth_provider := #{profile_url := ProfileURL} = Provider
             ,access_token := AccessToken
             }) ->
    case maps:get(profile_access_auth_type, Provider, <<"token">>) of
        <<"token">> -> ProfileURL;
        <<"api_key">> -> ProfileURL;
        <<"url">> -> <<ProfileURL/binary, AccessToken/binary>>
    end.

-spec maybe_add_user_identity(map()) -> map().
maybe_add_user_identity(#{user_identity := _Identity} = Token) -> Token;
maybe_add_user_identity(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_user_identity(#{auth_provider := #{profile_identity_field := Field}
                         ,profile := Profile
                         } = Token) ->
    case kz_json:get_first_defined([Field], Profile) of
        'undefined' ->
            lager:debug("user identity from field '~p' not found into ~p", [Field, Profile]),
            Token;
        Identity ->
            lager:debug("found user identity ~p", [Identity]),
            Token#{user_identity => Identity}
    end;
maybe_add_user_identity(#{auth_provider := #{name := Prov}}=Token) ->
    lager:debug("provider '~s' doesn't support identity profile info", [Prov]),
    Token.


-spec maybe_add_user_email(map()) -> map().
maybe_add_user_email(#{user_email := _UserEmail} = Token) -> Token;
maybe_add_user_email(#{verified_token := VerifiedToken} = Token) ->
    Token#{user_email => kz_json:get_first_defined(?PROFILE_EMAIL_FIELDS, VerifiedToken)};
maybe_add_user_email(#{profile_error_code := _Error} = Token) -> Token;
maybe_add_user_email(#{auth_provider := #{profile_email_field := Field}
                      ,profile := Profile
                      } = Token) ->
    Payload = kz_json:from_map(maps:get(payload, Token, #{})),
    case kz_json:find_first_defined([Field | ?PROFILE_EMAIL_FIELDS], [Payload, Profile]) of
        'undefined' ->
            lager:debug("user email from ~p not found", [Field]),
            Token#{user_email => 'undefined'};
        EMail ->
            lager:debug("found user email ~p", [EMail]),
            Token#{user_email => EMail}
    end;
maybe_add_user_email(#{profile := Profile} = Token) ->
    Payload = kz_json:from_map(maps:get(payload, Token, #{})),
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
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', OAuthDoc} -> maybe_update_user(DocId, OAuthDoc, Token);
        {'error', 'not_found'} -> update_user(DocId, format_user_doc(Token), Token)
    end;
maybe_add_user(#{auth_provider := #{name := Prefix}
                ,user_identity := Identity
                } = Token) ->
    DocId = <<Prefix/binary, "-",Identity/binary>>,
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', OAuthDoc} -> maybe_update_user(DocId, OAuthDoc, Token);
        {'error', 'not_found'} -> update_user(DocId, format_user_doc(Token), Token)
    end;
maybe_add_user(#{} = Token) ->
    lager:debug("identity not set, skip adding user"),
    Token.

-spec ensure_profile_properties(ne_binary(), kz_proplist(), kz_proplist(), map()) -> map().
ensure_profile_properties(DocId, Missing, Props, #{} = Token) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DocId) of
        {'ok', Doc} ->
            case Missing -- kz_json:get_keys(Doc) of
                [] -> do_update_user(DocId, Props, Token);
                _StillMissing ->
                    lager:debug("missing properties when updating user : ~p", [kz_util:join_binary(_StillMissing)]),
                    Token#{profile_error_code => {'error', {404, <<"missing profile properties">>}}}
            end;
        _ ->
            lager:debug("missing properties when updating user : ~p", [kz_util:join_binary(Missing)]),
            Token#{profile_error_code => {'error', {404, <<"missing profile properties">>}}}
    end.

-spec update_user(ne_binary(), kz_proplist(), map()) -> map().
update_user(DocId, Props, #{auth_provider := #{profile_required_props := RequiredProps}} = Token) ->
    case RequiredProps -- props:get_keys(Props) of
        [] -> do_update_user(DocId, Props, Token);
        Missing -> ensure_profile_properties(DocId, Missing, Props, Token)
    end;
update_user(DocId, Props, Token) ->
    do_update_user(DocId, Props, Token).

-spec do_update_user(ne_binary(), kz_proplist(), map()) -> map().
do_update_user(DocId, Props, Token) ->
    case kz_datamgr:update_doc(?KZ_AUTH_DB, DocId, Props) of
        {'ok', DocObj} -> maybe_cache_user(Token#{user_doc => DocObj
                                                 ,user_map => kz_json:to_map(DocObj)
                                                 }
                                          ,DocId
                                          );
        {'error', Error} ->
            lager:debug("unable to update auth document ~s: ~p", [DocId, Error]),
            Token#{profile_error_code => Error}
    end.

-spec maybe_cache_user(map(), ne_binary()) -> map().
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

-spec maybe_update_user(ne_binary(), kz_json:object(), map()) -> map().
maybe_update_user(DocId, JObj, Token) ->
    Props = format_user_doc(Token),
    case updates_needed(JObj, Props) of
        [] -> maybe_required_properties_missing(Token, Props, JObj);
        Updates -> update_user(DocId, Updates, Token)
    end.

-spec maybe_required_properties_missing(map(), kz_proplist(), kz_json:object()) -> map().
maybe_required_properties_missing(#{auth_provider := #{profile_required_props := RequiredProps}
                                   } = Token, Props, JObj) ->
    case RequiredProps -- props:get_keys(Props) of
        [] -> maybe_cache_user(Token#{user_doc => JObj
                                     ,user_map => kz_json:to_map(JObj)
                                     }, kz_doc:id(JObj));
        Missing ->
            lager:debug("missing properties when checking user : ~p", [kz_util:join_binary(Missing)]),
            Token#{profile_error_code => {'error', {404, <<"missing profile properties">>}}}
    end;
maybe_required_properties_missing(Token, _Props, JObj) ->
    Token#{user_doc => JObj
          ,user_map => kz_json:to_map(JObj)
          }.



-spec updates_needed(kz_json:object(), kz_proplist()) -> kz_proplist().
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

-spec format_user_doc(map()) -> kz_proplist().
format_user_doc(#{auth_provider := #{name := ProviderId} = Provider
                 ,profile := Profile
                 ,user_identity := Identity
                 }=Token) ->
    Verified = maps:get(verified_token, Token, kz_json:new()),
    Original = maps:get(original, Token, kz_json:new()),
    Scope = kz_json:find_first_defined([<<"scope">>], [Profile, Verified, Original], <<>>),
    App = maps:get(auth_app, Token, #{}),
    AppId = maps:get(name, App, 'undefined'),
    AppAccountId = maps:get(pvt_account_id, App, 'undefined'),
    EMail = maps:get(user_email, Token, 'undefined'),

    Mapping = maps:get(profile_account_mapping, Provider, #{}),
    MapFields = maps:fold(fun(K, V, Acc) ->
                                  case kz_json:get_value(V, Profile) of
                                      'undefined' -> Acc;
                                      Value -> [{kz_util:to_binary(K), Value} | Acc]
                                  end
                          end, [], Mapping),

    Props = [{<<"email">>, EMail}
            ,{<<"verified_email">>, kz_json:get_value(<<"verified_email">>, Verified)}
            ,{<<"access_type">>, maps:get(access_type, Token, 'undefined')}
            ,{<<"scope">>, Scope}
            ,{<<"scopes">>, binary:split(Scope, ?SCOPE_SEPARATORS, ['global'])}
            ,{<<"refresh_token">>, maps:get(refresh_token, Token, 'undefined')}
            ,{<<"profile">>, Profile}
            ,{<<"pvt_app_id">>, AppId}
            ,{<<"pvt_app_provider_id">>, ProviderId}
            ,{<<"pvt_app_account_id">>, AppAccountId}
            ,{<<"pvt_account_id">>, maps:get(linked_account_id, Token, 'undefined')}
            ,{<<"pvt_owner_id">>, maps:get(linked_owner_id, Token, 'undefined')}
            ,{<<"pvt_type">>, <<"user">>}
            ,{<<"pvt_user_identity">>, Identity}
            ] ++ MapFields,
    props:filter_empty(Props).
