%% @author root
%% @doc @todo Add description to kazoo_oauth_util.


-module(kazoo_oauth_util).

-include("kazoo_oauth.hrl").

-export([get_oauth_provider/1
         ,get_oauth_app/1
         ,get_oauth_service_app/1
        ]).
-export([token/2
         ,verify_token/2
         ,refresh_token/4
         ,refresh_token/5
        ]).
-export([jwt/2, jwt/3]).
-export([authorization_header/1]).

-spec authorization_header(oauth_token()) -> api_binary().
authorization_header(#oauth_token{type=Type,token=Token}) ->
    <<Type/binary, " ", Token/binary>>.

get_oauth_provider(ProviderId) ->
    case couch_mgr:open_doc(?OAUTH_DB, ProviderId) of
        {'ok', JObj} -> {'ok', oauth_provider_from_jobj(ProviderId, JObj)};
        {'error', _} ->
            {'error', <<"OAUTH - Provider ", ProviderId/binary, " not found">>}
    end.

oauth_provider_from_jobj(ProviderId, JObj) ->
    #oauth_provider{name=ProviderId
                    ,auth_url= wh_json:get_value(<<"oauth_url">>, JObj)
                    ,tokeninfo_url= wh_json:get_value(<<"tokeninfo_url">>, JObj) 
                    ,profile_url= wh_json:get_value(<<"profile_url">>, JObj) 
                    ,servers= wh_json:get_value(<<"servers">>, JObj) 
                    ,scopes= wh_json:get_value(<<"scopes">>, JObj)
                   }.

get_oauth_app(AppId) ->
    case couch_mgr:open_doc(?OAUTH_DB, AppId) of
        {'ok', JObj} ->
            ProviderId = wh_json:get_value(<<"pvt_oauth_provider">>, JObj),
            {'ok', Provider} = get_oauth_provider(ProviderId),
            {'ok', oauth_app_from_jobj(AppId, Provider, JObj)};
        {'error', _} ->
            {'error', <<"OAUTH - App ", AppId/binary, " not found">>}
    end.

oauth_app_from_jobj(AppId, Provider, JObj) ->
    #oauth_app{name=AppId
               ,account_id=wh_json:get_value(<<"pvt_account_id">>, JObj) 
               ,secret=wh_json:get_value(<<"pvt_secret">>, JObj) 
               ,user_prefix=wh_json:get_value(<<"pvt_user_prefix">>, JObj) 
               ,provider=Provider}.
        
get_oauth_service_app(AppId) ->
    case couch_mgr:open_doc(?OAUTH_DB, AppId) of
        {'ok', JObj} ->
            ProviderId = wh_json:get_value(<<"pvt_oauth_provider">>, JObj),
            {'ok', Provider} = get_oauth_provider(ProviderId),
            load_service_app_keys(
              oauth_service_from_jobj(AppId, Provider, JObj)
             );
        {'error', _Error} ->
            lager:debug("service_app ~p",[_Error]),
            {'error', <<"OAUTH - App ", AppId/binary, " not found">>}
    end.

oauth_service_from_jobj(AppId, Provider, JObj) ->
    #oauth_service_app{name=AppId
                       ,account_id=wh_json:get_value(<<"pvt_account_id">>, JObj) 
                       ,email=wh_json:get_value(<<"email">>, JObj) 
                       ,public_key_fingerprints=wh_json:get_value(<<"public_key_fingerprints">>, JObj) 
                       ,provider=Provider}.

load_service_app_keys(#oauth_service_app{name=AppId}=App) ->
    case couch_mgr:fetch_attachment(?OAUTH_DB, AppId, <<"public_key.pem">>) of
        {'ok', PublicKey} ->
            case couch_mgr:fetch_attachment(?OAUTH_DB, AppId, <<"private_key.pem">>) of
                {'ok', PrivateKey} -> 
                    {'ok',  oauth_service_app_from_keys(PublicKey, PrivateKey, App)};
                {'error', _R}=Error ->
                    lager:debug("unable to fetch private key from ~s: ~p", [AppId, _R]),
                    Error
            end;
        {'error', _R}=Error -> 
            lager:debug("unable to fetch public key from ~s: ~p", [AppId, _R]),
            Error
    end.

oauth_service_app_from_keys(PublicKey, PrivateKey, App) ->
    App#oauth_service_app{public_key=pem_to_rsa(PublicKey)
                          ,private_key = pem_to_rsa(PrivateKey)}.

pem_to_rsa(PemFileContents) ->
    [RSAEntry] = public_key:pem_decode(PemFileContents),
    public_key:pem_entry_decode(RSAEntry).	

jwt(#oauth_service_app{email=AccountEmail}=App, Scope) ->
    jwt(App, Scope, AccountEmail).
jwt(#oauth_service_app{email=AccountEmail
                       ,private_key=PrivateKey
                       ,public_key=PublicKey
                       ,provider=#oauth_provider{auth_url=URL}}
   ,Scope, EMail) ->
    
    JObj = wh_json:from_list([{<<"iss">>, EMail}
                              ,{<<"aud">>, URL}
                              ,{<<"scope">>, Scope}
                              ,{<<"iat">>,current_tstamp_unix()-500}
                              ,{<<"exp">>,current_tstamp_unix()+2000}
                             ]),
    
      
    JWT64 = base64:encode(wh_json:encode(JObj)),
    JWT64_HEADER = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9">>,
    JWT_FOR_SIGN = <<JWT64_HEADER/binary, ".", JWT64/binary>>,
    JWT_SIGNATURE = public_key:sign(JWT_FOR_SIGN, 'sha256', PrivateKey),
    %% JWT_SIGNATURE = crypto:rsa_sign('sha256', JWT_FOR_SIGN, PrivateKey),
    JWT_SIGNATURE64 = base64:encode(JWT_SIGNATURE),
    Assertion = <<JWT64_HEADER/binary, ".", JWT64/binary, ".", JWT_SIGNATURE64/binary>>,
    _Verify = public_key:verify(JWT_FOR_SIGN, 'sha256', JWT_SIGNATURE, PublicKey),
    Assertion.

-spec token( api_binary() | oauth_app(), api_binary() | oauth_refresh_token() ) -> {'ok', oauth_token()} | {'error', any()}.
token(AppId, UserId) when is_binary(AppId) ->
    lager:debug("getting oauth-app ~p",[AppId]),
    case get_oauth_app(AppId) of
        {'ok', App} -> token(App,UserId);
        Error -> Error
    end;
token(#oauth_app{user_prefix=UserPrefix}=App, UserId) when is_binary(UserId) ->
    DocId = <<UserPrefix/binary,"-", UserId/binary>>,
    case couch_mgr:open_doc(?OAUTH_DB, DocId) of
        {'ok', JObj} -> token(App, #oauth_refresh_token{token=wh_json:get_value(<<"refresh_token">>, JObj)});
        {'error', _R}=Error ->
            lager:debug("unable to get oauth user id ~s: ~p", [DocId, _R]),
            Error
    end;
token(_, 'undefined') -> {'error',<<"User doesn't have RefreshToken">>};
token(#oauth_app{name=AppId
                ,secret=Secret
                ,provider=#oauth_provider{auth_url=AUTH_URL}}
     ,#oauth_refresh_token{token=RefreshToken}) ->
    lager:debug("getting token : refresh ~p",[RefreshToken]),
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}],
    Fields = [{"client_id", wh_util:to_list(AppId)}
              ,{"client_secret",wh_util:to_list(Secret)}
              ,{"grant_type","refresh_token"}
              ,{"refresh_token",wh_util:to_list(RefreshToken)}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=")] end, Fields)), "&"),
    case ibrowse:send_req(wh_util:to_list(AUTH_URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespXML} ->
            JObj = wh_json:decode(RespXML),
            Token = wh_json:get_value(<<"access_token">>, JObj),
            Type = wh_json:get_value(<<"token_type">>, JObj),
            Expires = wh_json:get_integer_value(<<"expires_in">>, JObj),
            {'ok', #oauth_token{token=Token
                                ,type=Type
                                ,expires=Expires
                                ,issued=wh_util:current_tstamp()}};
        Else ->
            lager:info("unable to get oauth token: ~p", [Else]),
            {'error', Else}
	end.
	
-spec verify_token(api_binary() | oauth_provider(), api_binary()) -> {'ok', api_object()} | {'error', api_binary()}.
verify_token(ProviderId, AccessToken) when is_binary(ProviderId) ->
    case get_oauth_provider(ProviderId) of
        {'ok', Provider} -> verify_token(Provider, AccessToken);
        {'error', _}=Error -> Error
    end;
verify_token(#oauth_provider{tokeninfo_url=TokenInfoUrl}, AccessToken) ->
    URL = <<TokenInfoUrl/binary,AccessToken/binary>>,
    case ibrowse:send_req(wh_util:to_list(URL), [], 'get') of
        {'ok', "200", _RespHeaders, RespXML} ->
            JObj = wh_json:decode(RespXML),
            case wh_json:get_value(<<"error">>, JObj) of
                'undefined' -> {'ok',JObj};
                _ -> {'error', JObj}
            end;
        Else ->
            lager:info("unable to verify oauth token: ~p", [Else]),
            {'error', Else}
    end.

-spec refresh_token( api_binary() | oauth_app(), api_binary(), api_binary(), wh_proplist() ) -> {'ok', api_object()} | {'error', any()}.
refresh_token(App, Scope, AuthorizationCode, ExtraHeaders) ->
    refresh_token(App, Scope, AuthorizationCode, ExtraHeaders, <<"postmessage">>).

-spec refresh_token( api_binary() | oauth_app(), api_binary(), api_binary(), wh_proplist(), api_binary() ) -> {'ok', api_object()} | {'error', any()}.
refresh_token(#oauth_app{name=ClientId
                     ,secret=Secret
                     ,provider=#oauth_provider{auth_url=URL}}
         ,Scope, AuthorizationCode, ExtraHeaders, RedirectURI) ->
    Headers = [{"Content-Type","application/x-www-form-urlencoded"} | ExtraHeaders],
    Fields = [{"client_id",ClientId}
              ,{"redirect_uri",RedirectURI}
              ,{"client_secret", Secret}
              ,{"grant_type","authorization_code"}
              ,{"scope",Scope}
              ,{"code",AuthorizationCode}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K, wh_util:to_list(V)], "=") ] end, Fields)),"&"),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespXML} -> {'ok', wh_json:decode(RespXML)};
        Else ->
            lager:debug("unable to get new oauth token: ~p", [Else]),
            {'error', Else}
    end.


-spec current_tstamp_unix() -> non_neg_integer().
current_tstamp_unix() -> gregorian_seconds_to_unix_seconds(calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

%% there are 86400 seconds in a day
%% there are 62167219200 seconds between Jan 1, 0000 and Jan 1, 1970
-define(UNIX_EPOCH_AS_GREG_SECONDS, 62167219200).

-spec gregorian_seconds_to_unix_seconds(integer() | string() | binary()) -> non_neg_integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    wh_util:to_integer(GregorianSeconds) - ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec unix_seconds_to_gregorian_seconds(integer() | string() | binary()) -> non_neg_integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    wh_util:to_integer(UnixSeconds) + ?UNIX_EPOCH_AS_GREG_SECONDS.
