%% @author root
%% @doc @todo Add description to kazoo_oauth_client.


-module(kazoo_oauth_client).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([authenticate/1, authenticate/3]).

authenticate(JObj) ->
	case {wh_json:get_value(<<"access_token">>, JObj), wh_json:get_value(<<"provider">>, JObj)} of
		{'undefined', 'undefined'} -> {'error', <<"OAUTH missing parameters AccessToken and Provider">>};
		{AccessToken, 'undefined'} -> {'error', <<"OAUTH missing parameter Provider">>};
		{'undefined', ProviderId} -> {'error', <<"OAUTH missing parameter AccessToken">>};
		{AccessToken, ProviderId} -> authenticate(AccessToken, ProviderId, JObj)
	end.

authenticate(AccessToken, ProviderId, JObj) ->
	case kazoo_oauth_util:verify_token(ProviderId, AccessToken) of
		{'ok', Token} -> maybe_add_oauth_user(JObj, Token);
		Error -> Error
	end.







maybe_add_oauth_user(JObj, TokenObj) ->
	AppId = wh_json:get_value(<<"issued_to">>, TokenObj, <<"invalid_issued_to">>),
	case kazoo_oauth_util:get_oauth_app(AppId) of
		{'ok', #oauth_app{user_prefix=Prefix}=App } ->
			UserID = wh_json:get_value(<<"user_id">>, TokenObj),
			DocId = <<Prefix/binary, "-",UserID/binary>>,
			lager:info("DocId ~p",[DocId]),
			case couch_mgr:open_doc(?OAUTH_DB,DocId) of
				{'ok', OAuthDoc} ->	maybe_update_oauth_doc(DocId, JObj, TokenObj, App, OAuthDoc);
				_ -> maybe_save_oauth_doc(DocId, JObj, TokenObj, App)
			end;
		Error -> Error
	end.

	

maybe_save_oauth_doc(DocId, JObj, TokenObj, App) ->
    lager:debug("JObj ~p",[JObj]),
    lager:debug("TokenObj ~p",[TokenObj]),
	AuthorizationCode = wh_json:get_value(<<"code">>, JObj),
	Scope = wh_json:get_value(<<"scope">>, TokenObj),
    RefreshTokenObj = kazoo_oauth_util:refresh_token(App, JObj),
	save_oauth_doc(App, DocId, JObj, TokenObj,RefreshTokenObj).
					
save_oauth_doc(App, DocId, JObj, TokenObj, RefreshTokenObj) ->
	Doc = [
		  {<<"email">>, wh_json:get_value(<<"email">>, TokenObj) }
		  ,{<<"verified_email">>, wh_json:get_value(<<"verified_email">>, TokenObj) }
		  ,{<<"access_type">>, wh_json:get_value(<<"access_type">>, TokenObj) }
		  ,{<<"scope">>, wh_json:get_value(<<"scope">>, TokenObj) }
		  ,{<<"scopes">>, binary:split(wh_json:get_value(<<"scope">>, TokenObj),<<" ">>) }
		  ,{<<"refresh_token">>, wh_json:get_value(<<"refresh_token">>, RefreshTokenObj) }
		   ],
	case couch_mgr:update_doc(?OAUTH_DB, DocId, Doc) of
		{'ok', DocObj} -> load_profile(App, DocId, JObj, TokenObj, DocObj);
        _ -> {'error', <<"OAUTH - error saving oauthdoc">> }
    end.
	
maybe_update_oauth_doc(DocId, JObj, TokenObj, App, AuthObj) ->
	Fields = [<<"scope">>,<<"email">>,<<"verified_email">>],
	case lists:any(fun(Field) ->
						   wh_json:get_value(Field, TokenObj) =/= wh_json:get_value(Field, AuthObj)
				   end, Fields) of
		'true' -> maybe_save_oauth_doc(DocId, JObj, TokenObj, App);
		'false' -> load_profile(App, DocId, JObj, TokenObj, AuthObj)
	end.
	



load_profile(#oauth_app{provider=#oauth_provider{profile_url=ProfileURL}}=App, DocId, JObj, TokenObj, AuthDoc) ->
	lager:debug("load_profile ~p",[AuthDoc]),
	TokenType = wh_json:get_value(<<"token_type">>, JObj),
	AccessToken = wh_json:get_value(<<"access_token">>, JObj),
	Authorization = <<TokenType/binary, " ",AccessToken/binary>>,
	Headers = [
			   {"Authorization",wh_util:to_list(Authorization)}
			  ],
	case ibrowse:send_req(wh_util:to_list(ProfileURL), Headers, 'get', <<>>, [{response_format, binary}]) of
		{'ok', "200", _RespHeaders, RespXML} ->
			lager:info("Profile Resp ~p ,  ~ts",[_RespHeaders, RespXML]),
			ProfileJObj = wh_json:decode(RespXML),
			Doc = wh_json:set_values([
									  {<<"Token">>, JObj}
									 ,{<<"VerifiedToken">>, TokenObj}
									 ,{<<"Profile">>, ProfileJObj}
									 ,{<<"AuthDoc">>, AuthDoc}
									  ], wh_json:new()),
			{'ok', Doc};
		Error -> {'error', <<"OAUTH - Error fetching Profile">>}
	end.
	


%% ====================================================================
%% Internal functions
%% ====================================================================


