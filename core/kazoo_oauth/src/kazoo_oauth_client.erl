%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_oauth_client).

-include("kazoo_oauth.hrl").

-export([authenticate/1, authenticate/3]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(kz_json:object()) -> {'ok', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
authenticate(JObj) ->
    case {kz_json:get_value(<<"access_token">>, JObj)
         ,kz_json:get_value(<<"provider">>, JObj)
         }
    of
        {'undefined', 'undefined'} ->
            {'error', <<"OAUTH missing parameters AccessToken and Provider">>};
        {'undefined', _} ->
            {'error', <<"OAUTH missing parameter AccessToken">>};
        {_, 'undefined'} ->
            {'error', <<"OAUTH missing parameter Provider">>};
        {AccessToken, ProviderId} -> authenticate(AccessToken, ProviderId, JObj)
    end.

-spec authenticate(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
authenticate(AccessToken, ProviderId, JObj) ->
    case kazoo_oauth_util:verify_token(ProviderId, AccessToken) of
        {'ok', Token} -> maybe_add_oauth_user(JObj, Token);
        {'error', _R}=Error ->
            lager:debug("unable to verify oauth access token: ~p", [_R]),
            Error
    end.

maybe_add_oauth_user(JObj, TokenObj) ->
    AppId = kz_json:get_value(<<"issued_to">>, TokenObj, <<"invalid_issued_to">>),
    case kazoo_oauth_util:get_oauth_app(AppId) of
        {'ok', #oauth_app{}=App} ->
            add_oauth_user(App, JObj, TokenObj);
        {'error', _R}=Error ->
            lager:debug("unable to get oauth application: ~p", [_R]),
            Error
    end.

add_oauth_user(#oauth_app{user_prefix=Prefix}=App, JObj, TokenObj) ->
    UserID = kz_json:get_value(<<"user_id">>, TokenObj),
    DocId = <<Prefix/binary, "-",UserID/binary>>,
    case kz_datamgr:open_doc(?KZ_OAUTH_DB, DocId) of
        {'ok', OAuthDoc} ->
            maybe_update_oauth_doc(DocId, JObj, TokenObj, App, OAuthDoc);
        {'error', 'not_found'} ->
            maybe_save_oauth_doc(DocId, JObj, TokenObj, App)
    end.

maybe_save_oauth_doc(DocId, JObj, TokenObj, App) ->
    AuthorizationCode = kz_json:get_value(<<"code">>, JObj),
    Scope = kz_json:get_value(<<"scope">>, TokenObj),
    AccessType = kz_json:get_value(<<"access_type">>, TokenObj),
    RefreshTokenObj = get_refresh_token(AccessType, App, Scope, AuthorizationCode),
    save_oauth_doc(App, DocId, JObj, TokenObj, RefreshTokenObj).


-spec get_refresh_token(kz_term:ne_binary(), kz_term:ne_binary() | oauth_app()
                       ,kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
get_refresh_token(<<"offline">>, App, Scope, AuthorizationCode) ->
    case kazoo_oauth_util:refresh_token(App, Scope, AuthorizationCode, []) of
        {'ok', Token} -> Token;
        _ -> kz_json:new()
    end;
get_refresh_token(_ , _, _ , _) -> kz_json:new().


save_oauth_doc(App, DocId, JObj, TokenObj, RefreshTokenObj) ->
    Doc = props:filter_undefined([{<<"email">>, kz_json:get_value(<<"email">>, TokenObj) }
                                 ,{<<"verified_email">>, kz_json:get_value(<<"verified_email">>, TokenObj) }
                                 ,{<<"access_type">>, kz_json:get_value(<<"access_type">>, TokenObj) }
                                 ,{<<"scope">>, kz_json:get_value(<<"scope">>, TokenObj) }
                                 ,{<<"scopes">>, binary:split(kz_json:get_value(<<"scope">>, TokenObj), <<" ">>) }
                                 ,{<<"refresh_token">>, kz_json:get_value(<<"refresh_token">>, RefreshTokenObj) }
                                 ]),
    UpdateOptions = [{'update', Doc}],
    case kz_datamgr:update_doc(?KZ_OAUTH_DB, DocId, UpdateOptions) of
        {'ok', DocObj} -> load_profile(App, JObj, TokenObj, DocObj);
        {'error', _R} ->
            lager:debug("unable to update oauth document ~s: ~p", [DocId, _R]),
            {'error', <<"OAUTH - error saving oauthdoc">> }
    end.

maybe_update_oauth_doc(DocId, JObj, TokenObj, App, AuthObj) ->
    Fields = [<<"scope">>, <<"email">>, <<"verified_email">>],
    case lists:any(fun(Field) ->
                           kz_json:get_value(Field, TokenObj)
                               =/= kz_json:get_value(Field, AuthObj)
                   end, Fields)
    of
        'true' -> maybe_save_oauth_doc(DocId, JObj, TokenObj, App);
        'false' -> load_profile(App, JObj, TokenObj, AuthObj)
    end.

load_profile(#oauth_app{provider=#oauth_provider{profile_url=ProfileURL}}, JObj, TokenObj, AuthDoc) ->
    TokenType = kz_json:get_value(<<"token_type">>, JObj, <<"Bearer">>),
    AccessToken = kz_json:get_value(<<"access_token">>, JObj),
    Authorization = <<TokenType/binary, " ",AccessToken/binary>>,
    Headers = [{"Authorization",kz_term:to_list(Authorization)}],
    case kz_http:get(kz_term:to_list(ProfileURL), Headers) of
        {'ok', 200, _RespHeaders, RespXML} ->
            lager:info("loaded oauth profile: ~p",[RespXML]),
            ProfileJObj = kz_json:decode(RespXML),
            Doc = kz_json:from_list([{<<"Token">>, JObj}
                                    ,{<<"VerifiedToken">>, TokenObj}
                                    ,{<<"Profile">>, ProfileJObj}
                                    ,{<<"AuthDoc">>, AuthDoc}
                                    ]),
            {'ok', Doc};
        _Else ->
            lager:debug("failed to get oauth profile: ~p", [_Else]),
            {'error', <<"OAUTH - Error fetching Profile">>}
    end.



%%==============================================================================
%% Internal functions
%%==============================================================================
