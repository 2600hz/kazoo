%% @author root
%% @doc @todo Add description to kazoo_oauth_util.


-module(kazoo_oauth_util).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_oauth_provider/1, get_oauth_app/1, get_oauth_service_app/1]).
-export([token/2, verify_token/2, new_token/4, new_token/5]).
-export([jwt/2]).
-export([authorization_header/1]).
-export([refresh_token/1, refresh_token/2]).


-spec authorization_header(oauth_token()) -> api_binary().
authorization_header(#oauth_token{type=Type,token=Token}) ->
    <<Type/binary, " ", Token/binary>>.

get_oauth_provider(ProviderId) ->
    case couch_mgr:open_doc(?OAUTH_DB, ProviderId) of
        {'ok', JObj} -> {'ok', #oauth_provider{name=ProviderId
                                       ,auth_url= wh_json:get_value(<<"oauth_url">>, JObj)
                                       ,tokeninfo_url= wh_json:get_value(<<"tokeninfo_url">>, JObj) 
                                       ,profile_url= wh_json:get_value(<<"profile_url">>, JObj) 
                                       ,servers= wh_json:get_value(<<"servers">>, JObj) 
                                       ,scopes= wh_json:get_value(<<"scopes">>, JObj)
                                       }};
        _ ->
            {'error', <<"OAUTH - Provider ", ProviderId/binary, " not found">>}
    end.

get_oauth_app(AppId) ->
    case couch_mgr:open_doc(?OAUTH_DB, AppId) of
        {'ok', JObj} ->
            ProviderId = wh_json:get_value(<<"pvt_oauth_provider">>, JObj),
            {'ok', Provider} = get_oauth_provider(ProviderId),
            {'ok', #oauth_app{name=AppId
                           ,account_id=wh_json:get_value(<<"pvt_account_id">>, JObj) 
                           ,secret=wh_json:get_value(<<"pvt_secret">>, JObj) 
                           ,user_prefix=wh_json:get_value(<<"pvt_user_prefix">>, JObj) 
                           ,provider=Provider } };
        _ ->
            {'error', <<"OAUTH - App ", AppId/binary, " not found">>}
    end.

get_oauth_service_app(AppId) ->
    case couch_mgr:open_doc(?OAUTH_DB, AppId) of
        {'ok', JObj} ->
            ProviderId = wh_json:get_value(<<"pvt_oauth_provider">>, JObj),
            {'ok', Provider} = get_oauth_provider(ProviderId),
            load_service_app_keys(#oauth_service_app{name=AppId
                           ,account_id=wh_json:get_value(<<"pvt_account_id">>, JObj) 
                           ,email=wh_json:get_value(<<"email">>, JObj) 
                           ,public_key_fingerprints=wh_json:get_value(<<"public_key_fingerprints">>, JObj) 
                           ,provider=Provider } );
        _ ->
            {'error', <<"OAUTH - App ", AppId/binary, " not found">>}
    end.

load_service_app_keys(#oauth_service_app{name=AppId}=App) ->
    case couch_mgr:fetch_attachment(?OAUTH_DB, AppId, <<"public_key.pem">>) of
        {'ok', PublicKey} ->
            case couch_mgr:fetch_attachment(?OAUTH_DB, AppId, <<"private_key.pem">>) of
                {'ok', PrivateKey} -> 
                    {'ok', App#oauth_service_app{public_key=pem_to_rsa(PublicKey)
                                                ,private_key = pem_to_rsa(PrivateKey)}};
                Error -> {'error', Error}
            end;
        Error -> {'error', Error}
    end.

pem_to_rsa(PemFileContents) ->
	[RSAEntry] = public_key:pem_decode(PemFileContents),
	public_key:pem_entry_decode(RSAEntry).	




 
jwt(#oauth_service_app{email=AccountEmail,private_key=KEY,public_key=PUBKEY,provider=#oauth_provider{auth_url=URL}}, SCOPE) ->
	JWT_HEADER = <<"{\"alg\":\"RS256\",\"typ\":\"JWT\"}">>,
	JWT64_HEADER = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9">>,
	Encoded = base64:encode(JWT_HEADER),
	Encoded2 = base64:encode_to_string(JWT_HEADER),
	Decoded = base64:decode(Encoded),
	Decoded2 = base64:decode_to_string(Encoded2),
	
	Fields = [
			  {<<"iss">>, AccountEmail}
			 ,{<<"aud">>, URL}
			 ,{<<"scope">>, SCOPE}
			 ,{<<"iat">>,wh_util:current_tstamp_unix()-500}
			 ,{<<"exp">>,wh_util:current_tstamp_unix()+2000}
			 ],
	lager:info("Fields ~p",[Fields]),
	
	
	JObj = wh_json:set_values(Fields, wh_json:new()),
	lager:info("JObj ~p",[JObj]),
	JWT = wh_util:to_binary(wh_json:encode(JObj)),
	lager:info("JWT ~p",[JWT]),
	lager:info("JWT String ~s",[JWT]),

	JWT64 = base64:encode(JWT),
	JWT64_STRING = base64:encode_to_string(JWT),
	lager:info("JWT64 ~p , ~p",[JWT64, JWT64_STRING]),
	lager:info("JWT64 String ~s , ~s",[JWT64, JWT64_STRING]),
	
	JWT_FOR_SIGN = <<JWT64_HEADER/binary, ".", JWT64/binary>>,
	lager:info("JWT_FOR_SIGN ~p",[JWT_FOR_SIGN]),
	lager:info("JWT_FOR_SIGN String ~s",[JWT_FOR_SIGN]),


	JWT_SIGNATURE = public_key:sign(JWT_FOR_SIGN, 'sha256', KEY),
	%JWT_SIGNATURE = crypto:rsa_sign('sha256', JWT_FOR_SIGN, KEY),
	
	JWT_SIGNATURE64 = base64:encode(JWT_SIGNATURE),
	JWT_SIGNATURE64_STRING = base64:encode_to_string(JWT_SIGNATURE),

	lager:info("JWT_SIGNATURE ~p",[JWT_SIGNATURE]),
	lager:info("JWT_SIGNATURE64 ~p",[JWT_SIGNATURE64]),
	lager:info("JWT_SIGNATURE64 String ~s",[JWT_SIGNATURE64_STRING]),
	
	Assertion = <<JWT64_HEADER/binary, ".", JWT64/binary, ".", JWT_SIGNATURE64/binary>>,
	lager:info("Assertion ~p",[Assertion]),
	lager:info("Assertion ~s",[Assertion]),
	
	Verify = public_key:verify(JWT_FOR_SIGN, 'sha256', JWT_SIGNATURE, PUBKEY),
	lager:info("Verify ~p",[Verify]),
	
	
	Assertion.

	


-spec token( api_binary() | oauth_app(), api_binary() | oauth_refresh_token() ) -> {'ok', oauth_token()} | {'error', any()}.
token(AppId, UserId) when is_binary(AppId) ->
    lager:debug("getting oauth-app ~p",[AppId]),
    case get_oauth_app(AppId) of
        {'ok', App} -> token(App,UserId);
        Error -> Error
    end;
token(#oauth_app{user_prefix=UserPrefix} = App, UserId) when is_binary(UserId) ->
    lager:debug("getting userid ~s-~s",[UserPrefix,UserId]),
    case couch_mgr:open_doc(?OAUTH_DB, <<UserPrefix/binary,"-", UserId/binary>>) of
        {'ok', JObj} -> token(App, #oauth_refresh_token{token=wh_json:get_value(<<"refresh_token">>, JObj)});
        Error -> {'error', Error}
    end;
token(_, 'undefined') ->
    {'error',<<"User doesn't have RefreshToken">>};
token(#oauth_app{name=AppId,secret=Secret,provider=#oauth_provider{auth_url=AUTH_URL}},#oauth_refresh_token{token=RefreshToken}) ->
    lager:debug("getting token : refresh ~p",[RefreshToken]),
	Headers = [{"Content-Type","application/x-www-form-urlencoded"}],
	Fields = [{"client_id", wh_util:to_list(AppId)}
			 ,{"client_secret",wh_util:to_list(Secret)}
			 ,{"grant_type","refresh_token"}
			 ,{"refresh_token",wh_util:to_list(RefreshToken)}
			 ],
    lager:debug("fields ~p ~p",[AUTH_URL, Fields]),
	Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
	case ibrowse:send_req(wh_util:to_list(AUTH_URL), Headers, 'post', Body) of
		{'ok', "200", _RespHeaders, RespXML} ->
			JObj = wh_json:decode(RespXML),
			Token = wh_json:get_value(<<"access_token">>, JObj),
			Type = wh_json:get_value(<<"token_type">>, JObj),
			Expires = wh_json:get_integer_value(<<"expires_in">>, JObj),
			{'ok', #oauth_token{token=Token,type=Type, expires=Expires, issued=wh_util:current_tstamp()}};
		A -> lager:info("Error ~p",[A]),
             {'error',A}
	end.
	
	
-spec verify_token(api_binary() | oauth_provider(), api_binary()) -> {'ok', api_object()} | {'error', api_binary()}.
verify_token(ProviderId, AccessToken) when is_binary(ProviderId) ->
    case get_oauth_provider(ProviderId) of
        {'ok', Provider} -> verify_token(Provider, AccessToken);
        Error -> Error
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
        Error -> lager:info("Error ~p",[Error]),
             {'error',Error}
    end.

-spec new_token( api_binary() | oauth_app(), api_binary(), api_binary(), wh_proplist() ) -> {'ok', api_object()} | {'error', any()}.
new_token(App, Scope, AuthorizationCode, ExtraHeaders) ->
    new_token(App, Scope, AuthorizationCode, ExtraHeaders, <<"postmessage">>).

-spec new_token( api_binary() | oauth_app(), api_binary(), api_binary(), wh_proplist(), api_binary() ) -> {'ok', api_object()} | {'error', any()}.
new_token(#oauth_app{name=ClientId, secret=Secret, provider=#oauth_provider{auth_url=URL}}, Scope, AuthorizationCode, ExtraHeaders, RedirectURI) ->
    Headers = [{"Content-Type","application/x-www-form-urlencoded"} | ExtraHeaders],
    Fields = [{"client_id",ClientId}
             ,{"redirect_uri",RedirectURI}
             ,{"client_secret", Secret}
             ,{"grant_type","authorization_code"}
             ,{"scope",Scope}
             ,{"code",AuthorizationCode}
             ],
    lager:debug("new_token fields ~p",[Fields]),
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K, wh_util:to_list(V)], "=") ] end, Fields)),"&"),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespXML} ->
            JObj = wh_json:decode(RespXML),
            lager:debug("NEW TOKEN ~p",[JObj]),
            {'ok', JObj};
        A -> lager:debug("error ~p",[A]), {'error', A}
    end.

-spec refresh_token(api_object()) -> api_binary().    
refresh_token(JObj) ->
    App = wh_json:get_value(<<"client_id">>, JObj),
    refresh_token(App, JObj).

-spec refresh_token(api_binary() | oauth_app(), api_object()) -> api_binary().    
refresh_token(AppId, JObj) when is_binary(AppId) ->
    {'ok', App} = get_oauth_app(AppId),
    refresh_token(App, JObj);    
refresh_token(#oauth_app{}=App, JObj) ->
    AuthorizationCode = wh_json:get_value(<<"code">>, JObj),
    Scope = wh_json:get_value(<<"scope">>, JObj),    
    lager:debug("refresh_token ~p ~p",[wh_json:get_value(<<"id_token">>, JObj),  AuthorizationCode]),
    case wh_json:get_value(<<"id_token1">>, JObj) of
        'undefined' ->
            lager:debug("id_token not found!"),
            case new_token(App,Scope,AuthorizationCode,[]) of
                {'ok', RefreshTokenObj} -> RefreshTokenObj;
                _ -> 'undefined'
            end;        
        RefreshTokenObj -> wh_json:set_value(<<"id_token">>, RefreshTokenObj, wh_json:new())
    end.

    


    

%% ====================================================================
%% Internal functions
%% ====================================================================


