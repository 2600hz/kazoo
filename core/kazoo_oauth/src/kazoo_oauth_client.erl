%% @author root
%% @doc @todo Add description to kazoo_oauth_client.


-module(kazoo_oauth_client).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([authenticate/1, authenticate/3]).

authenticate(JObj) ->
    case {wh_json:get_value(<<"access_token">>, JObj)
         ,wh_json:get_value(<<"provider">>, JObj)}
    of
        {'undefined', 'undefined'} ->
            {'error', <<"OAUTH missing parameters AccessToken and Provider">>};
        {'undefined', _} ->
            {'error', <<"OAUTH missing parameter AccessToken">>};
        {_, 'undefined'} ->
            {'error', <<"OAUTH missing parameter Provider">>};
        {AccessToken, ProviderId} -> authenticate(AccessToken, ProviderId, JObj)
    end.

authenticate(AccessToken, ProviderId, JObj) ->
    case kazoo_oauth_util:verify_token(ProviderId, AccessToken) of
        {'ok', Token} -> maybe_add_oauth_user(JObj, Token);
        {'error', _R}=Error ->
            lager:debug("unable to verify oauth access token: ~p", [_R]),
            Error
    end.

maybe_add_oauth_user(JObj, TokenObj) ->
    AppId = wh_json:get_value(<<"issued_to">>, TokenObj, <<"invalid_issued_to">>),
    case kazoo_oauth_util:get_oauth_app(AppId) of
        {'ok', #oauth_app{}=App} ->
            add_oauth_user(App, JObj, TokenObj);
        {'error', _R}=Error ->
            lager:debug("unable to get oauth application: ~p", [_R]),
            Error
    end.

add_oauth_user(#oauth_app{user_prefix=Prefix}=App, JObj, TokenObj) ->
    UserID = wh_json:get_value(<<"user_id">>, TokenObj),
    DocId = <<Prefix/binary, "-",UserID/binary>>,
    case kz_datamgr:open_doc(?KZ_OAUTH_DB, DocId) of
        {'ok', OAuthDoc} ->
            maybe_update_oauth_doc(DocId, JObj, TokenObj, App, OAuthDoc);
        {'error', 'not_found'} ->
            maybe_save_oauth_doc(DocId, JObj, TokenObj, App)
    end.

maybe_save_oauth_doc(DocId, JObj, TokenObj, App) ->
    AuthorizationCode = wh_json:get_value(<<"code">>, JObj),
    Scope = wh_json:get_value(<<"scope">>, TokenObj),
    AccessType = wh_json:get_value(<<"access_type">>, TokenObj),
    RefreshTokenObj = get_refresh_token(AccessType, App, Scope, AuthorizationCode),
    save_oauth_doc(App, DocId, JObj, TokenObj, RefreshTokenObj).


-spec get_refresh_token(ne_binary(), ne_binary() | oauth_app()
                       ,ne_binary(), ne_binary()) -> wh_json:object().
get_refresh_token(<<"offline">>, App, Scope, AuthorizationCode) ->
    case kazoo_oauth_util:refresh_token(App, Scope, AuthorizationCode, []) of
        {'ok', Token} -> Token;
        _ -> wh_json:new()
    end;
get_refresh_token(_ , _, _ , _) -> wh_json:new().


save_oauth_doc(App, DocId, JObj, TokenObj, RefreshTokenObj) ->
    Doc = props:filter_undefined([{<<"email">>, wh_json:get_value(<<"email">>, TokenObj) }
           ,{<<"verified_email">>, wh_json:get_value(<<"verified_email">>, TokenObj) }
           ,{<<"access_type">>, wh_json:get_value(<<"access_type">>, TokenObj) }
           ,{<<"scope">>, wh_json:get_value(<<"scope">>, TokenObj) }
           ,{<<"scopes">>, binary:split(wh_json:get_value(<<"scope">>, TokenObj), <<" ">>) }
           ,{<<"refresh_token">>, wh_json:get_value(<<"refresh_token">>, RefreshTokenObj) }
          ]),
    case kz_datamgr:update_doc(?KZ_OAUTH_DB, DocId, Doc) of
        {'ok', DocObj} -> load_profile(App, JObj, TokenObj, DocObj);
        {'error', _R} ->
            lager:debug("unable to update oauth document ~s: ~p", [DocId, _R]),
            {'error', <<"OAUTH - error saving oauthdoc">> }
    end.

maybe_update_oauth_doc(DocId, JObj, TokenObj, App, AuthObj) ->
    Fields = [<<"scope">>, <<"email">>, <<"verified_email">>],
    case lists:any(fun(Field) ->
                           wh_json:get_value(Field, TokenObj)
                               =/= wh_json:get_value(Field, AuthObj)
                   end, Fields)
    of
        'true' -> maybe_save_oauth_doc(DocId, JObj, TokenObj, App);
        'false' -> load_profile(App, JObj, TokenObj, AuthObj)
    end.

load_profile(#oauth_app{provider=#oauth_provider{profile_url=ProfileURL}}, JObj, TokenObj, AuthDoc) ->
    TokenType = wh_json:get_value(<<"token_type">>, JObj, <<"Bearer">>),
    AccessToken = wh_json:get_value(<<"access_token">>, JObj),
    Authorization = <<TokenType/binary, " ",AccessToken/binary>>,
    Headers = [{"Authorization",wh_util:to_list(Authorization)}],
    case kz_http:get(wh_util:to_list(ProfileURL), Headers) of
        {'ok', 200, _RespHeaders, RespXML} ->
            lager:info("loaded outh profile: ~p",[RespXML]),
            ProfileJObj = wh_json:decode(RespXML),
            Doc = wh_json:from_list([{<<"Token">>, JObj}
                                     ,{<<"VerifiedToken">>, TokenObj}
                                     ,{<<"Profile">>, ProfileJObj}
                                     ,{<<"AuthDoc">>, AuthDoc}
                                    ]),
            {'ok', Doc};
        _Else ->
            lager:debug("failed to get oauth profile: ~p", [_Else]),
            {'error', <<"OAUTH - Error fetching Profile">>}
    end.



%% ====================================================================
%% Internal functions
%% ====================================================================
