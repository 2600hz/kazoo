%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Basic auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_basic_auth).

-export([init/0
        ,authenticate/1
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".auth">>).
-define(DEFAULT_BASIC_AUTH_TYPE, <<"md5">>).
-define(BASIC_AUTH_KEY, <<"basic_auth_type">>).
-define(BASIC_AUTH_TYPE, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, ?BASIC_AUTH_KEY, ?DEFAULT_BASIC_AUTH_TYPE)).

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(USERNAME_LIST, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
-spec authenticate(cb_context:context(), atom()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authenticate(Context) ->
    authenticate(Context, cb_context:auth_token_type(Context)).

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
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, _TokenType) -> 'false'.

-spec check_basic_token(cb_context:context(), api_binary()) ->
                               'false' |
                               {'true' | 'halt', cb_context:context()}.
check_basic_token(_Context, <<>>) -> 'false';
check_basic_token(_Context, 'undefined') -> 'false';
check_basic_token(Context, AuthToken) ->
    case kz_cache:peek_local(?CACHE_NAME, {'basic_auth', AuthToken}) of
        {'ok', JObj} -> is_expired(Context, JObj);
        {'error', 'not_found'} -> maybe_check_credentials(Context, AuthToken)
    end.

-spec maybe_check_credentials(cb_context:context(), api_binary()) ->
                                     'false' |
                                     {'true' | 'halt', cb_context:context()}.
maybe_check_credentials(Context, AuthToken) ->
    lager:debug("checking basic token: '~s'", [AuthToken]),
    case binary:split(base64:decode(AuthToken), <<":">>) of
        [AccountId, Credentials] -> check_credentials(Context, AccountId, Credentials);
        _ -> lager:debug("basic token '~s' check failed", [AuthToken]),
             'false'
    end.

-spec check_credentials(cb_context:context(), ne_binary(), api_binary()) ->
                               'false' |
                               {'true' | 'halt', cb_context:context()}.
check_credentials(Context, AccountId, Credentials) ->
    lager:debug("checking credentials '~s' for account '~s'", [Credentials, AccountId]),
    BasicType = kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT, ?BASIC_AUTH_KEY, ?BASIC_AUTH_TYPE),
    check_credentials(Context, AccountId, Credentials, BasicType).

-spec check_credentials(cb_context:context(), ne_binary(), ne_binary() | {ne_binary(), ne_binary()}, ne_binary()) ->
                               'false' |
                               {'true' | 'halt', cb_context:context()}.
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

-spec get_credential_doc(ne_binary(), ne_binary(), ne_binary()) -> api_object().
get_credential_doc(AccountId, View, Key) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Options = [{'key', Key}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, View, Options) of
        {'ok', [JObj]} -> kz_json:get_value(<<"doc">>, JObj);
        _ -> 'undefined'
    end.

-spec is_expired(cb_context:context(), kz_json:object()) ->
                        boolean() |
                        {'halt', cb_context:context()}.
is_expired(Context, JObj) ->
    AccountId = kz_doc:account_id(JObj),
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_util:is_account_expired(AccountId) of
        'false' ->
            EndpointId = kz_doc:id(JObj),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            AuthToken = cb_context:auth_token(Context),
            kz_cache:store_local(?CACHE_NAME, {'basic_auth', AuthToken}, JObj, CacheProps),
            {'true', set_auth_doc(Context, JObj)};
        {'true', Expired} ->
            _ = kz_util:spawn(fun kz_util:maybe_disable_account/1, [AccountId]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            Context1 = cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context),
            {'halt', Context1}
    end.

-spec set_auth_doc(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
set_auth_doc(Context, JObj) ->
    Setters = [{fun cb_context:set_auth_doc/2, JObj}
              ,{fun cb_context:set_auth_account_id/2 ,kz_doc:account_id(JObj)}
              ],
    cb_context:setters(Context, Setters).
