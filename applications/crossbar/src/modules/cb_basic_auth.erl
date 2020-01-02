%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Basic auth module
%%% This is a simple auth mechanism, once the user has acquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_basic_auth).

-export([init/0
        ,authenticate/1
        ]).

-include("crossbar.hrl").

-define(DEFAULT_BASIC_AUTH_TYPE, <<"md5">>).
-define(BASIC_AUTH_KEY, <<"basic_auth_type">>).
-define(BASIC_AUTH_TYPE, kapps_config:get_ne_binary(?AUTH_CONFIG_CAT, ?BASIC_AUTH_KEY, ?DEFAULT_BASIC_AUTH_TYPE)).

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(USERNAME_LIST, <<"users/list_by_username">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.early_authenticate">>, ?MODULE, 'authenticate'),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec authenticate(cb_context:context()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
authenticate(Context) ->
    authenticate(Context, cb_context:auth_token_type(Context)).

-spec authenticate(cb_context:context(), atom()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
authenticate(Context, 'basic') ->
    _ = cb_context:put_reqid(Context),
    case kz_buckets:consume_tokens(?APP_NAME
                                  ,cb_modules_util:bucket_name(Context)
                                  ,cb_modules_util:token_cost(Context)
                                  )
    of
        'true' -> check_basic_token(Context, cb_context:auth_token(Context));
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'stop', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, _TokenType) -> 'false'.

-spec check_basic_token(cb_context:context(), kz_term:api_binary()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
check_basic_token(_Context, <<>>) -> 'false';
check_basic_token(_Context, 'undefined') -> 'false';
check_basic_token(Context, AuthToken) ->
    case kz_cache:peek_local(?CACHE_NAME, {'basic_auth', AuthToken}) of
        {'ok', JObj} -> is_expired(Context, JObj);
        {'error', 'not_found'} -> maybe_check_credentials(Context, AuthToken)
    end.

-spec maybe_check_credentials(cb_context:context(), kz_term:api_binary()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
maybe_check_credentials(Context, AuthToken) ->
    lager:debug("checking basic token: '~s'", [AuthToken]),
    case binary:split(base64:decode(AuthToken), <<":">>) of
        [AccountId, Credentials] -> check_credentials(Context, AccountId, Credentials);
        _ -> lager:debug("basic token '~s' check failed", [AuthToken]),
             'false'
    end.

-spec check_credentials(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
check_credentials(Context, AccountId, Credentials) ->
    lager:debug("checking credentials '~s' for account '~s'", [Credentials, AccountId]),
    BasicType = kapps_account_config:get(AccountId, ?AUTH_CONFIG_CAT, ?BASIC_AUTH_KEY, ?BASIC_AUTH_TYPE),
    check_credentials(Context, AccountId, Credentials, BasicType).

-spec check_credentials(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:ne_binary()) ->
          'false' |
          {'true' | 'stop', cb_context:context()}.
check_credentials(Context, AccountId, {Username, Password}, _BasicType) ->
    {MD5, _SHA1} = cb_modules_util:pass_hashes(Username, Password),
    check_credentials(Context, AccountId, MD5, <<"md5">>);
check_credentials(Context, AccountId, Credentials, <<"sha">>) ->
    case get_credential_doc(AccountId, ?ACCT_SHA1_LIST, Credentials) of
        'undefined' -> 'false';
        JObj -> is_expired(Context, JObj)
    end;
check_credentials(Context, AccountId, Credentials, <<"md5">>) ->
    case get_credential_doc(AccountId, ?ACCT_MD5_LIST, Credentials) of
        'undefined' -> 'false';
        JObj -> is_expired(Context, JObj)
    end;
check_credentials(Context, AccountId, Credentials, BasicType) ->
    case binary:split(Credentials, <<":">>) of
        [User, Pass] -> check_credentials(Context, AccountId, {User, Pass}, BasicType);
        _ -> 'false'
    end.

-spec get_credential_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
get_credential_doc(AccountId, View, Key) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    Options = [{'key', Key}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, View, Options) of
        {'ok', [JObj]} -> kz_json:get_value(<<"doc">>, JObj);
        _ -> 'undefined'
    end.

-spec is_expired(cb_context:context(), kz_json:object()) ->
          boolean() |
          {'stop', cb_context:context()}.
is_expired(Context, JObj) ->
    AccountId = kz_doc:account_id(JObj),
    AccountDb = kzs_util:format_account_db(AccountId),
    case kzd_accounts:is_expired(AccountId) of
        'false' ->
            EndpointId = kz_doc:id(JObj),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            AuthToken = cb_context:auth_token(Context),
            kz_cache:store_local(?CACHE_NAME, {'basic_auth', AuthToken}, JObj, CacheProps),
            {'true', set_auth_doc(Context, JObj)};
        {'true', Expired} ->
            _ = kz_process:spawn(fun crossbar_util:maybe_disable_account/1, [AccountId]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            Context1 = cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context),
            {'stop', Context1}
    end.

-spec set_auth_doc(cb_context:context(), kz_json:object()) ->
          cb_context:context().
set_auth_doc(Context, JObj) ->
    AuthAccountId = kz_doc:account_id(JObj),
    OwnerId = kz_doc:id(JObj),
    Setters = [{fun cb_context:set_auth_doc/2, auth_doc(JObj)}
              ,{fun cb_context:set_auth_account_id/2, AuthAccountId}
               | maybe_add_is_admins(AuthAccountId, OwnerId)
              ],
    cb_context:setters(Context, Setters).

-spec auth_doc(kz_json:object()) -> kz_json:object().
auth_doc(JObj) ->
    kz_json:from_list(
      [{<<"account_id">>, kz_doc:account_id(JObj)}
      ,{<<"identity_sig">>, kzd_users:signature_secret(JObj)}
      ,{<<"iss">>, <<"kazoo">>}
      ,{<<"method">>, kz_term:to_binary(?MODULE)}
      ,{<<"owner_id">>, kz_doc:id(JObj)}
      ]).

-spec maybe_add_is_admins(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> cb_context:setters().
maybe_add_is_admins(?NE_BINARY = AuthAccountId, ?NE_BINARY = OwnerId) ->
    [{fun cb_context:set_is_superduper_admin/2, cb_context:is_superduper_admin(AuthAccountId)}
    ,{fun cb_context:set_is_account_admin/2, cb_context:is_account_admin(AuthAccountId, OwnerId)}
    ];
maybe_add_is_admins(?NE_BINARY = AuthAccountId, 'undefined') ->
    [{fun cb_context:set_is_superduper_admin/2, cb_context:is_superduper_admin(AuthAccountId)}];
maybe_add_is_admins(_, _) ->
    [].
