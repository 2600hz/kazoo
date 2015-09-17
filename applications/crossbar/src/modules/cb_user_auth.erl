
%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% User auth module
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_user_auth).

-export([init/0
         ,allowed_methods/0, allowed_methods/1 %% only accept 0 or 1 path token
         ,resource_exists/0, resource_exists/1
         ,authorize/1
         ,authenticate/1
         ,validate/1, validate/2
         ,put/1, put/2
         ,post/2
        ]).

-include("../crossbar.hrl").

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(LIST_BY_RESETID, <<"users/list_by_resetid">>).
-define(DEFAULT_LANGUAGE, <<"en-us">>).
-define(USER_AUTH_TOKENS, whapps_config:get_integer(?CONFIG_CAT, <<"user_auth_tokens">>, 35)).

-define(RECOVERY, <<"recovery">>).
-define(RESET_ID, <<"resetid">>).
-define(PWD_RESETID_SIZE, 40).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.user_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.user_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.user_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.user_auth">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.user_auth">>, ?MODULE, 'post').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() -> [?HTTP_PUT].
allowed_methods(?RECOVERY) -> [?HTTP_PUT, ?HTTP_POST];
allowed_methods(_) -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_tokens()) -> boolean().
resource_exists() -> 'true'.
resource_exists(?RECOVERY) -> 'true';
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"user_auth">>, _}]) -> 'true';
authorize_nouns(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"user_auth">>, []}]) -> 'true';
authenticate_nouns([{<<"user_auth">>, [?RECOVERY]}]) -> 'true';
authenticate_nouns([{<<"user_auth">>, [?RECOVERY, _ResetId]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    lager:debug(">>> validate/1"),
    Context1 = consume_tokens(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:validate_request_data(<<"user_auth">>, Context, fun maybe_authenticate_user/1);
        _Status -> Context1
    end.

validate(Context, ?RECOVERY) ->
    Verb = cb_context:req_verb(Context),
    validate_recovery(Context, Verb);
validate(Context, Token) ->
    lager:debug(">>> validate/2 "),
    Context1 = cb_context:set_account_db(Context, ?KZ_TOKEN_DB),
    maybe_get_auth_token(Context1, Token).

-spec validate_recovery(cb_context:context(), http_method()) -> cb_context:context().
validate_recovery(Context, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"user_auth_recovery">>, Context, fun maybe_recover_user_password/1);
validate_recovery(Context, ?HTTP_POST) ->
    lager:debug(">>> validate/2 RECOVERY"),
    lager:debug(">>> validate/2  ~p", [cb_context:req_data(Context)]),
    cb_context:validate_request_data(<<"user_auth_recovery_reset">>, Context, fun maybe_reset_password/1).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    lager:debug(">>> put/1"),
    _ = cb_context:put_reqid(Context),
    crossbar_util:create_auth_token(Context, ?MODULE).

put(Context, ?RECOVERY) ->
    lager:debug(">>> put/2"),
    _ = cb_context:put_reqid(Context),
    reset_users_password__step_1(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?RECOVERY) ->
    lager:debug(">>> post/2"),
    _ = cb_context:put_reqid(Context),
    reset_users_password__step_2(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_auth_token(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_get_auth_token(Context, Token) ->
    Context1 = crossbar_doc:load(Token, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            AuthAccountId = cb_context:auth_account_id(Context),
            AccountId = cb_context:account_id(Context),
            create_auth_resp(Context1, Token, AccountId, AuthAccountId);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_auth_resp(cb_context:context(), ne_binary(), ne_binary(),  ne_binary()) ->
                              cb_context:context().
create_auth_resp(Context, Token, AccountId, AccountId) ->
    lager:debug("account ~s is same as auth account", [AccountId]),
    RespData = cb_context:resp_data(Context),
    crossbar_util:response(
      crossbar_util:response_auth(RespData)
      ,cb_context:set_auth_token(Context, Token)
     );
create_auth_resp(Context, _AccountId, _Token, _AuthAccountId) ->
    lager:debug("forbidding token for account ~s and auth account ~s", [_AccountId, _AuthAccountId]),
    cb_context:add_system_error('forbidden', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalize the account name by converting the name to lower case
%% and then removing all non-alphanumeric characters.
%%
%% This can possibly return an empty binary.
%% @end
%%--------------------------------------------------------------------
-spec normalize_account_name(api_binary()) -> api_binary().
normalize_account_name(AccountName) ->
    wh_util:normalize_account_name(AccountName).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Attempt to lookup and compare the user creds in the provided accounts.
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec maybe_authenticate_user(cb_context:context()) -> cb_context:context().
-spec maybe_authenticate_user(cb_context:context(), ne_binary(), ne_binary(), ne_binary() | ne_binaries()) ->
                                     cb_context:context().

maybe_authenticate_user(Context) ->
    JObj = cb_context:doc(Context),
    Credentials = wh_json:get_value(<<"credentials">>, JObj),
    Method = wh_json:get_value(<<"method">>, JObj, <<"md5">>),
    AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = wh_json:get_first_defined([<<"account_realm">>, <<"realm">>], JObj),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', _} ->
            lager:debug("failed to find account DB from realm ~s", [AccountRealm]),
            cb_context:add_system_error('invalid_credentials', Context);
        {'ok', <<_/binary>> = Account} ->
            maybe_account_is_enabled(Context, Credentials, Method, Account);
        {'ok', Accounts} ->
            maybe_accounts_are_enabled(Context, Credentials, Method, Accounts)
    end.

maybe_authenticate_user(Context, Credentials, <<"md5">>, <<_/binary>> = Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    Context1 = crossbar_doc:load_view(?ACCT_MD5_LIST
                                      ,[{'key', Credentials}]
                                      ,cb_context:set_account_db(Context, AccountDb)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> load_md5_results(Context1, cb_context:doc(Context1));
        _Status ->
            lager:debug("credentials do not belong to any user: ~s: ~p", [_Status, cb_context:doc(Context1)]),
            cb_context:add_system_error('invalid_credentials', Context1)
    end;
maybe_authenticate_user(Context, Credentials, <<"sha">>, <<_/binary>> = Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Context1 = crossbar_doc:load_view(?ACCT_SHA1_LIST
                                      ,[{'key', Credentials}]
                                      ,cb_context:set_account_db(Context, AccountDb)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> load_sha1_results(Context1, cb_context:doc(Context1));
        _Status ->
            lager:debug("credentials do not belong to any user"),
            cb_context:add_system_error('invalid_credentials', Context)
    end;
maybe_authenticate_user(Context, _Creds, _Method, _Account) ->
    lager:debug("invalid creds by method ~s", [_Method]),
    cb_context:add_system_error('invalid_credentials', Context).

-spec maybe_accounts_are_enabled(cb_context:context(), ne_binary(), ne_binary(), ne_binaries()) ->
                                        cb_context:context().
maybe_accounts_are_enabled(Context, _, _, []) ->
    lager:debug("no account(s) specified"),
    cb_context:add_system_error('invalid_credentials', Context);
maybe_accounts_are_enabled(Context, Credentials, Method, [Account|Accounts]) ->
    Context1 = maybe_account_is_enabled(Context, Credentials, Method, Account),
    case cb_context:resp_status(Context1) of
        'success' -> Context1;
        _Status -> maybe_accounts_are_enabled(Context, Credentials, Method, Accounts)
    end.

-spec maybe_account_is_enabled(cb_context:context(), ne_binary(), ne_binary(), ne_binary()) ->
                                      cb_context:context().
maybe_account_is_enabled(Context, Credentials, Method, Account) ->
    case wh_util:is_account_enabled(Account) of
        'true' ->
            maybe_authenticate_user(Context, Credentials, Method, Account);
        'false' ->
            lager:debug("account ~p is disabled", [Account]),
            Props = [{'details', <<"account_disabled">>}],
            cb_context:add_system_error('forbidden', wh_json:from_list(Props), Context)
    end.

-spec load_sha1_results(cb_context:context(), wh_json:objects() | wh_json:object()) ->
                               cb_context:context().
load_sha1_results(Context, [JObj|_]) ->
    lager:debug("found more that one user with SHA1 creds, using ~s", [wh_doc:id(JObj)]),
    cb_context:set_doc(Context, wh_json:get_value(<<"value">>, JObj));
load_sha1_results(Context, []) ->
    cb_context:add_system_error('invalid_credentials', Context);
load_sha1_results(Context, JObj) ->
    lager:debug("found SHA1 credentials belong to user ~s", [wh_doc:id(JObj)]),
    cb_context:set_doc(Context, wh_json:get_value(<<"value">>, JObj)).

-spec load_md5_results(cb_context:context(), wh_json:objects() | wh_json:object()) ->
                              cb_context:context().
load_md5_results(Context, [JObj|_]) ->
    lager:debug("found more that one user with MD5 creds, using ~s", [wh_doc:id(JObj)]),
    cb_context:set_doc(Context, wh_json:get_value(<<"value">>, JObj));
load_md5_results(Context, []) ->
    lager:debug("failed to find a user with MD5 creds"),
    cb_context:add_system_error('invalid_credentials', Context);
load_md5_results(Context, JObj) ->
    lager:debug("found MD5 credentials belong to user ~s", [wh_doc:id(JObj)]),
    cb_context:set_doc(Context, wh_json:get_value(<<"value">>, JObj)).

%% @private
-spec maybe_reset_password(cb_context:context()) -> cb_context:context().
maybe_reset_password(Context) ->
    ResetId = wh_json:get_ne_value(?RESET_ID, cb_context:req_data(Context)),
    {AccountDb,Rest} = get_pwd_reset_id(ResetId),
    lager:debug(">>> 2 ~p", [{AccountDb,Rest}]),
    ViewOptions = [{'key', ResetId}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?LIST_BY_RESETID, ViewOptions) of
        {'ok', [UserDoc]} ->
            lager:debug("matched user document using resetid '~s'", [ResetId]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                         ,{fun cb_context:set_doc/2, UserDoc}
                                        ]);
        _ ->
            lager:debug("invalid password resetid"),
            cb_context:add_validation_error(
              ?RESET_ID
              ,<<"not_found">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"The provided resetid was not found">>}
                  ,{<<"cause">>, ResetId}
                 ])
              ,Context
             )
    end.

%% @private
-spec maybe_recover_user_password(cb_context:context()) -> cb_context:context().
maybe_recover_user_password(Context) ->
    JObj = cb_context:doc(Context),
    AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = wh_json:get_first_defined([<<"account_realm">>, <<"realm">>], JObj),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', C} -> C;
        {'ok', [Account|_]} -> maybe_load_username(Account, Context);
        {'ok', Account} ->     maybe_load_username(Account, Context)
    end.

%% @private
-spec maybe_load_username(ne_binary(), cb_context:context()) -> cb_context:context().
maybe_load_username(Account, Context) ->
    JObj = cb_context:doc(Context),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    lager:debug("attempting to load user name in db: ~s", [AccountDb]),
    Username = wh_json:get_value(<<"username">>, JObj),
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', [User]} ->
            case wh_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                'false' ->
                    lager:debug("the user name '~s' was found and is not disabled, continue", [Username]),
                    cb_context:setters(Context, [{fun cb_context:set_account_db/2, Account}
                                                 ,{fun cb_context:set_doc/2, wh_json:get_value(<<"doc">>, User)}
                                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                                ]);
                'true' ->
                    lager:debug("the user name '~s' was found but is disabled", [Username]),
                    cb_context:add_validation_error(
                      <<"username">>
                      ,<<"forbidden">>
                      ,wh_json:from_list(
                         [{<<"message">>, <<"The provided user name is disabled">>}
                          ,{<<"cause">>, Username}
                         ])
                      ,Context
                     )
            end;
        _ ->
            cb_context:add_validation_error(
              <<"username">>
              ,<<"not_found">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"The provided user name was not found">>}
                  ,{<<"cause">>, Username}
                 ])
              ,Context
             )
    end.

%% @private
-spec reset_users_password__step_1(cb_context:context()) -> cb_context:context().
reset_users_password__step_1(Context) ->
    ResetId = make_pwd_reset_id(cb_context:account_id(Context)),
    UserDoc = cb_context:doc(Context),
    NewUserDoc = wh_json:set_value(?RESET_ID, ResetId, UserDoc),

    Context1 = crossbar_doc:save(
                 cb_context:setters(Context, [{fun cb_context:set_req_verb/2, ?HTTP_POST}%%%%?
                                              ,{fun cb_context:set_doc/2, NewUserDoc}
                                             ])
                ),
    case cb_context:resp_status(Context1) of
        'success' ->
            ReqData = cb_context:req_data(Context),
            Email = wh_json:get_ne_binary_value(<<"email">>, UserDoc),
            Link = make_pwd_reset_link(ReqData, ResetId),
            lager:debug("created password reset link: ~s", [Link]),
            Notify = [{<<"Email">>, Email}
                      ,{<<"First-Name">>, wh_json:get_value(<<"first_name">>, UserDoc)}
                      ,{<<"Last-Name">>,  wh_json:get_value(<<"last_name">>, UserDoc)}
                      ,{<<"Password-Reset-Link">>, Link}
                      ,{<<"Account-ID">>, wh_doc:account_id(UserDoc)}
                      ,{<<"Account-DB">>, wh_doc:account_db(UserDoc)}
                      ,{<<"Request">>, wh_json:delete_key(<<"username">>, ReqData)}
                      | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                     ],
            'ok' = wapi_notifications:publish_pwd_recovery(Notify),
            Msg = <<"Request for password reset handled, email sent to: ", Email/binary>>,
            crossbar_util:response(Msg, Context);
        _Status ->
            Context1
    end.

%% @private
-spec make_pwd_reset_id(ne_binary()) -> ne_binary().
make_pwd_reset_id(AccountId) ->
    Rest = wh_util:rand_hex_binary((?PWD_RESETID_SIZE - 32) / 2),
    <<AccountId/binary, Rest/binary>>.

%% @private
-spec get_pwd_reset_id(ne_binary()) -> {ne_binary(), ne_binary()}.
get_pwd_reset_id(ResetId) ->
    <<Account:32/binary, Rest/binary>> = ResetId,
    {wh_util:format_account_id(Account, 'encoded'), Rest}.

%% @private
-spec make_pwd_reset_link(wh_json:object(), ne_binary()) -> ne_binary().
make_pwd_reset_link(ReqData, ResetId) ->
    UIURL = wh_json:get_ne_binary_value(<<"url">>, ReqData),
    [Url|_] = binary:split(UIURL, <<"#">>),
    <<Url/binary, "/?", ResetId/binary>>.

%% %% @private
-spec reset_users_password__step_2(cb_context:context()) -> cb_context:context().
reset_users_password__step_2(Context) ->
    lager:debug(">>> reset_users_password__step_2"),
    crossbar_util:create_auth_token(Context, ?MODULE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account(api_binary(), api_binary(), api_binary(), cb_context:context()) ->
                          {'ok', ne_binary() | ne_binaries()} |
                          {'error', cb_context:context()}.
find_account('undefined', 'undefined', 'undefined', Context) ->
    {'error', Context};
find_account('undefined', 'undefined', AccountName, Context) ->
    case whapps_util:get_accounts_by_name(AccountName) of
        {'ok', AccountDb} ->
            lager:debug("found account by name '~s': ~s", [AccountName, AccountDb]),
            {'ok', AccountDb};
        {'multiples', AccountDbs} ->
            lager:debug("the account name returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            C = cb_context:add_validation_error(
                  <<"account_name">>
                  ,<<"not_found">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"The provided account name could not be found">>}
                      ,{<<"cause">>, AccountName}
                     ])
                  ,Context
                 ),
            find_account('undefined', 'undefined', 'undefined', C)
    end;
find_account('undefined', AccountRealm, AccountName, Context) ->
    case whapps_util:get_account_by_realm(AccountRealm) of
        {'ok', 'undefined'} ->
            lager:debug("failed to find account ~s by name", [AccountName]),
            C = cb_context:add_validation_error(
                  <<"account_name">>
                  ,<<"not_found">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"The provided account name could not be found">>}
                      ,{<<"cause">>, AccountName}
                     ])
                  ,Context
                 ),
            find_account('undefined', 'undefined', 'undefined', C);
        {'ok', AccountDb} ->
            lager:debug("found account by realm '~s': ~s", [AccountRealm, AccountDb]),
            {'ok', AccountDb};
        {'multiples', AccountDbs} ->
            lager:debug("the account realm returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            C = cb_context:add_validation_error(
                  <<"account_realm">>
                  ,<<"not_found">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"The provided account realm could not be found">>}
                      ,{<<"cause">>, AccountRealm}
                     ])
                  ,Context
                 ),
            find_account('undefined', 'undefined', AccountName, C)
    end;
find_account(PhoneNumber, AccountRealm, AccountName, Context) ->
    case wh_number_manager:lookup_account_by_number(PhoneNumber) of
        {'ok', AccountId, _} ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            lager:debug("found account by phone number '~s': ~s", [PhoneNumber, AccountDb]),
            {'ok', AccountDb};
        {'error', _} ->
            C = cb_context:add_validation_error(
                  <<"phone_number">>
                  ,<<"not_found">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"The provided phone number could not be found">>}
                      ,{<<"cause">>, PhoneNumber}
                     ])
                  ,Context
                 ),
            find_account('undefined', AccountRealm, AccountName, C)
    end.

-spec consume_tokens(cb_context:context()) -> cb_context:context().
consume_tokens(Context) ->
    case kz_buckets:consume_tokens_until(?APP_NAME
                                         ,cb_modules_util:bucket_name(Context)
                                         ,cb_modules_util:token_cost(Context, ?USER_AUTH_TOKENS)
                                        )
    of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('too_many_requests', Context)
    end.
