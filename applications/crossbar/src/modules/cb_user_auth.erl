%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% User auth module
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_user_auth).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/1, validate/2
        ,put/1, put/2
        ,post/2
        ]).

-include("crossbar.hrl").

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(LIST_BY_RESET_ID, <<"users/list_by_reset_id">>).
-define(LIST_BY_MTIME, <<"users/list_by_mtime">>).
-define(DEFAULT_LANGUAGE, <<"en-us">>).
-define(USER_AUTH_TOKENS, kapps_config:get_integer(?CONFIG_CAT, <<"user_auth_tokens">>, 35)).

-define(RECOVERY, <<"recovery">>).
-define(RESET_ID, <<"reset_id">>).
-define(RESET_ID_SIZE_DEFAULT, 137).
-define(RESET_ID_SIZE,
        case kapps_config:get_integer(?CONFIG_CAT, <<"reset_id_size">>, ?RESET_ID_SIZE_DEFAULT) of
            _TooBig when _TooBig >= 180 -> ?RESET_ID_SIZE_DEFAULT;
            _TooSmall when _TooSmall < 42 -> ?RESET_ID_SIZE_DEFAULT;
            Ok -> Ok
        end).
-define(RESET_PVT_TYPE, <<"password_reset">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.user_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.user_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.user_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.user_auth">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.user_auth">>, ?MODULE, 'post'),
    ok.

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
allowed_methods(_AuthToken) -> [?HTTP_GET].

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
resource_exists(_AuthToken) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"user_auth">>, _}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

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
    Context1 = consume_tokens(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:validate_request_data(<<"user_auth">>, Context, fun maybe_authenticate_user/1);
        _Status -> Context1
    end.

validate(Context, ?RECOVERY) ->
    case cb_context:req_verb(Context) of
        ?HTTP_PUT ->
            Schema = <<"user_auth_recovery">>,
            OnSuccess = fun maybe_load_user_doc_via_creds/1;
        ?HTTP_POST ->
            Schema = <<"user_auth_recovery_reset">>,
            OnSuccess = fun maybe_load_user_doc_via_reset_id/1
    end,
    cb_context:validate_request_data(Schema, Context, OnSuccess);
validate(Context, AuthToken) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_TOKEN_DB),
    maybe_get_auth_token(Context1, AuthToken).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    _ = cb_context:put_reqid(Context),
    crossbar_auth:create_auth_token(Context, ?MODULE).

put(Context, ?RECOVERY) ->
    _ = cb_context:put_reqid(Context),
    save_reset_id_then_send_email(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?RECOVERY) ->
    _ = cb_context:put_reqid(Context),
    Context1 = crossbar_doc:save(Context),
    DocForCreation =
        kz_json:from_list(
          [{<<"account_id">>, kz_util:format_account_id(cb_context:account_db(Context1))}
          ,{<<"owner_id">>, kz_doc:id(cb_context:doc(Context1))}
          ]),
    Context2 = cb_context:set_doc(Context1, DocForCreation),
    crossbar_auth:create_auth_token(Context2, ?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_auth_token(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_get_auth_token(Context, AuthToken) ->
    case AuthToken =:= cb_context:auth_token(Context) of
        'true' ->
            AuthAccountId = cb_context:auth_account_id(Context),
            AccountId = cb_context:account_id(Context),
            create_auth_resp(Context, AccountId, AuthAccountId);
        'false' -> cb_context:add_system_error('invalid_credentials', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_auth_resp(cb_context:context(), ne_binary(),  ne_binary()) ->
                              cb_context:context().
create_auth_resp(Context, AccountId, AccountId) ->
    lager:debug("account ~s is same as auth account", [AccountId]),
    crossbar_util:response(crossbar_util:response_auth(cb_context:auth_doc(Context))
                          ,Context
                          );
create_auth_resp(Context, _AccountId, _AuthAccountId) ->
    lager:debug("forbidding token for account ~s and auth account ~s"
               ,[_AccountId, _AuthAccountId]),
    cb_context:add_system_error('forbidden', Context).

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
    Credentials = kz_json:get_value(<<"credentials">>, JObj),
    Method = kz_json:get_value(<<"method">>, JObj, <<"md5">>),
    AccountName = kz_util:normalize_account_name(kz_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = kz_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = kz_json:get_first_defined([<<"account_realm">>, <<"realm">>], JObj),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', _} ->
            lager:debug("failed to find account DB from realm ~s", [AccountRealm]),
            cb_context:add_system_error('invalid_credentials', Context);
        {'ok', ?NE_BINARY=Account} ->
            maybe_auth_account(Context, Credentials, Method, Account);
        {'ok', Accounts} ->
            maybe_auth_accounts(Context, Credentials, Method, Accounts)
    end.

maybe_authenticate_user(Context, Credentials, <<"md5">>, ?NE_BINARY=Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    Context1 = crossbar_doc:load_view(?ACCT_MD5_LIST
                                     ,[{'key', Credentials}]
                                     ,cb_context:set_account_db(Context, AccountDb)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> load_md5_results(Context1, cb_context:doc(Context1));
        _Status ->
            lager:debug("credentials do not belong to any user: ~s: ~p"
                       ,[_Status, cb_context:doc(Context1)]),
            cb_context:add_system_error('invalid_credentials', Context1)
    end;
maybe_authenticate_user(Context, Credentials, <<"sha">>, ?NE_BINARY=Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
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

-spec maybe_auth_account(cb_context:context(), ne_binary(), ne_binary(), ne_binary()) ->
                                cb_context:context().
maybe_auth_account(Context, Credentials, Method, Account) ->
    Context1 = maybe_authenticate_user(Context, Credentials, Method, Account),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_account_is_expired(Context1, Account);
        _Status -> Context1
    end.

-spec maybe_auth_accounts(cb_context:context(), ne_binary(), ne_binary(), ne_binaries()) ->
                                 cb_context:context().
maybe_auth_accounts(Context, _, _, []) ->
    lager:debug("no account(s) specified"),
    cb_context:add_system_error('invalid_credentials', Context);
maybe_auth_accounts(Context, Credentials, Method, [Account|Accounts]) ->
    Context1 = maybe_authenticate_user(Context, Credentials, Method, Account),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_account_is_expired(Context1, Account);
        _Status ->
            maybe_auth_accounts(Context, Credentials, Method, Accounts)
    end.

-spec maybe_account_is_expired(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_account_is_expired(Context, Account) ->
    case kz_util:is_account_expired(Account) of
        'false' -> maybe_account_is_enabled(Context, Account);
        {'true', Expired} ->
            _ = kz_util:spawn(fun kz_util:maybe_disable_account/1, [Account]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context)
    end.

-spec maybe_account_is_enabled(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_account_is_enabled(Context, Account) ->
    case kz_util:is_account_enabled(Account) of
        'true' -> Context;
        'false' ->
            lager:debug("account ~p is disabled", [Account]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account disabled">>}]
                 ),
            cb_context:add_validation_error(<<"account">>, <<"disabled">>, Cause, Context)
    end.

-spec load_sha1_results(cb_context:context(), kz_json:objects() | kz_json:object()) ->
                               cb_context:context().
load_sha1_results(Context, [JObj|_]) ->
    lager:debug("found more that one user with SHA1 creds, using ~s", [kz_doc:id(JObj)]),
    cb_context:set_doc(Context, kz_json:get_value(<<"value">>, JObj));
load_sha1_results(Context, []) ->
    cb_context:add_system_error('invalid_credentials', Context);
load_sha1_results(Context, JObj) ->
    lager:debug("found SHA1 credentials belong to user ~s", [kz_doc:id(JObj)]),
    cb_context:set_doc(Context, kz_json:get_value(<<"value">>, JObj)).

-spec load_md5_results(cb_context:context(), kz_json:objects() | kz_json:object()) ->
                              cb_context:context().
load_md5_results(Context, [JObj|_]) ->
    lager:debug("found more that one user with MD5 creds, using ~s", [kz_doc:id(JObj)]),
    cb_context:set_doc(Context, kz_json:get_value(<<"value">>, JObj));
load_md5_results(Context, []) ->
    lager:debug("failed to find a user with MD5 creds"),
    cb_context:add_system_error('invalid_credentials', Context);
load_md5_results(Context, JObj) ->
    lager:debug("found MD5 credentials belong to user ~s", [kz_doc:id(JObj)]),
    cb_context:set_doc(Context, kz_json:get_value(<<"value">>, JObj)).


%% @private
-spec maybe_load_user_doc_via_creds(cb_context:context()) -> cb_context:context().
maybe_load_user_doc_via_creds(Context) ->
    JObj = cb_context:doc(Context),
    AccountName = kz_util:normalize_account_name(kz_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = kz_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = kz_json:get_first_defined([<<"account_realm">>, <<"realm">>], JObj),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', C} -> C;
        {'ok', [Account|_]} -> maybe_load_user_doc_by_username(Account, Context);
        {'ok', Account} ->     maybe_load_user_doc_by_username(Account, Context)
    end.

%% @private
-spec maybe_load_user_doc_by_username(ne_binary(), cb_context:context()) -> cb_context:context().
maybe_load_user_doc_by_username(Account, Context) ->
    JObj = cb_context:doc(Context),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    lager:debug("attempting to lookup user name in db: ~s", [AccountDb]),
    Username = kz_json:get_value(<<"username">>, JObj),
    ViewOptions = [{'key', Username}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', [User]} ->
            case kz_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                'false' ->
                    lager:debug("user name '~s' was found and is not disabled, continue", [Username]),
                    Doc = kz_json:get_value(<<"doc">>, User),
                    cb_context:setters(Context, [{fun cb_context:set_account_db/2, Account}
                                                ,{fun cb_context:set_doc/2, Doc}
                                                ,{fun cb_context:set_resp_status/2, 'success'}
                                                ]);
                'true' ->
                    lager:debug("user name '~s' was found but is disabled", [Username]),
                    Msg =
                        kz_json:from_list(
                          [{<<"message">>, <<"The provided user name is disabled">>}
                          ,{<<"cause">>, Username}
                          ]),
                    cb_context:add_validation_error(<<"username">>, <<"forbidden">>, Msg, Context)
            end;
        _ ->
            Msg =
                kz_json:from_list(
                  [{<<"message">>, <<"The provided user name was not found">>}
                  ,{<<"cause">>, Username}
                  ]),
            cb_context:add_validation_error(<<"username">>, <<"not_found">>, Msg, Context)
    end.

%% @private
-spec save_reset_id_then_send_email(cb_context:context()) -> cb_context:context().
save_reset_id_then_send_email(Context) ->
    MoDb = kazoo_modb:get_modb(cb_context:account_db(Context)),
    ResetId = reset_id(MoDb),
    UserDoc = cb_context:doc(Context),
    %% Not much chance for doc to already exist
    {'ok',_} = kazoo_modb:save_doc(MoDb, create_resetid_doc(ResetId, kz_doc:id(UserDoc))),
    Email = kz_json:get_ne_binary_value(<<"email">>, UserDoc),
    lager:debug("created recovery id, sending email to '~s'", [Email]),
    UIURL = kz_json:get_ne_binary_value(<<"ui_url">>, cb_context:req_data(Context)),
    Link = reset_link(UIURL, ResetId),
    lager:debug("created password reset link: ~s", [Link]),
    Notify = [{<<"Email">>, Email}
             ,{<<"First-Name">>, kz_json:get_value(<<"first_name">>, UserDoc)}
             ,{<<"Last-Name">>,  kz_json:get_value(<<"last_name">>, UserDoc)}
             ,{<<"Password-Reset-Link">>, Link}
             ,{<<"Account-ID">>, kz_doc:account_id(UserDoc)}
             ,{<<"Account-DB">>, kz_doc:account_db(UserDoc)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    'ok' = kapi_notifications:publish_password_recovery(Notify),
    Msg = <<"Request for password reset handled, email sent to: ", Email/binary>>,
    crossbar_util:response(Msg, Context).


%% @private
-spec maybe_load_user_doc_via_reset_id(cb_context:context()) -> cb_context:context().
maybe_load_user_doc_via_reset_id(Context) ->
    ResetId = kz_json:get_ne_binary_value(?RESET_ID, cb_context:req_data(Context)),
    MoDb = ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _Y, _M) = reset_id(ResetId),
    lager:debug("looking up password reset doc: ~s", [ResetId]),
    case kazoo_modb:open_doc(MoDb, ResetId) of
        {'ok', ResetIdDoc} ->
            lager:debug("found password reset doc"),
            AccountDb = ?MATCH_ACCOUNT_ENCODED(A, B, Rest),
            Context1 = crossbar_doc:load(kz_json:get_value(<<"pvt_userid">>, ResetIdDoc)
                                        ,cb_context:set_account_db(Context, AccountDb)
                                        ,?TYPE_CHECK_OPTION(kzd_user:type())
                                        ),
            NewUserDoc =
                kz_json:set_value(<<"require_password_update">>, 'true', cb_context:doc(Context1)),
            cb_context:setters(Context1, [{fun cb_context:set_resp_status/2, 'success'}
                                         ,{fun cb_context:set_doc/2, NewUserDoc}
                                         ]);
        _ ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"The provided reset_id did not resolve to any user">>}
                    ,{<<"cause">>, ResetId}
                    ]),
            cb_context:add_validation_error(<<"user">>, <<"not_found">>, Msg, Context)
    end.


%% @private
-spec reset_id(ne_binary()) -> ne_binary().
reset_id(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, YYYY, MM)) ->
    <<Y1:1/binary, Y2:1/binary, Y3:1/binary, Y4:1/binary>> = YYYY,
    <<M1:1/binary, M2:1/binary>> = MM,
    SomeNoise = kz_util:rand_hex_binary((?RESET_ID_SIZE - (32 + 4 + 2 + 3 + 1)) div 2),
    <<N1:1/binary, N2:1/binary, N3:1/binary, Noise/binary>> = SomeNoise,
    <<(?MATCH_ACCOUNT_RAW(A, B, Rest))/binary,
      N1/binary, Y1/binary, Y4/binary,
      N2/binary, M2/binary, Y2/binary,
      N3/binary, Y3/binary, M1/binary,
      Noise/binary
    >>;
reset_id(<<AccountId:32/binary,
           _N1:1/binary, Y1:1/binary, Y4:1/binary,
           _N2:1/binary, M2:1/binary, Y2:1/binary,
           _N3:1/binary, Y3:1/binary, M1:1/binary,
           _Noi:8, _se/binary
         >>) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest) = kz_util:to_lower_binary(AccountId),
    YYYY = <<Y1/binary, Y2/binary, Y3/binary, Y4/binary>>,
    MM = <<M1/binary, M2/binary>>,
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, YYYY, MM).

%% @private
-spec reset_link(ne_binary(), ne_binary()) -> ne_binary().
reset_link(UIURL, ResetId) ->
    case binary:match(UIURL, <<$?>>) of
        'nomatch' -> <<UIURL/binary, "?recovery=", ResetId/binary>>;
        _ -> <<UIURL/binary, "&recovery=", ResetId/binary>>
    end.

%% @private
-spec create_resetid_doc(ne_binary(), ne_binary()) -> kz_json:object().
create_resetid_doc(ResetId, UserId) ->
    kz_json:from_list(
      [{<<"_id">>, ResetId}
      ,{<<"pvt_userid">>, UserId}
      ,{<<"pvt_created">>, kz_util:current_tstamp()}
      ,{<<"pvt_type">>, ?RESET_PVT_TYPE}
      ]).

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
    case kapps_util:get_accounts_by_name(AccountName) of
        {'ok', AccountDb} ->
            lager:debug("found account by name '~s': ~s", [AccountName, AccountDb]),
            {'ok', AccountDb};
        {'multiples', AccountDbs} ->
            lager:debug("the account name returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            Msg =
                kz_json:from_list(
                  [{<<"message">>, <<"The provided account name could not be found">>}
                  ,{<<"cause">>, AccountName}
                  ]),
            C = cb_context:add_validation_error(<<"account_name">>, <<"not_found">>, Msg, Context),
            find_account('undefined', 'undefined', 'undefined', C)
    end;
find_account('undefined', AccountRealm, AccountName, Context) ->
    case kapps_util:get_account_by_realm(AccountRealm) of
        {'ok', _AccountDb}=OK ->
            lager:debug("found account by realm '~s': ~s", [AccountRealm, _AccountDb]),
            OK;
        {'multiples', AccountDbs} ->
            lager:debug("the account realm returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            Msg =
                kz_json:from_list(
                  [{<<"message">>, <<"The provided account realm could not be found">>}
                  ,{<<"cause">>, AccountRealm}
                  ]),
            C = cb_context:add_validation_error(<<"account_realm">>, <<"not_found">>, Msg, Context),
            find_account('undefined', 'undefined', AccountName, C)
    end;
find_account(PhoneNumber, AccountRealm, AccountName, Context) ->
    case knm_number:lookup_account(PhoneNumber) of
        {'ok', AccountId, _} ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            lager:debug("found account by phone number '~s': ~s", [PhoneNumber, AccountDb]),
            {'ok', AccountDb};
        {'error', _} ->
            Msg =
                kz_json:from_list(
                  [{<<"message">>, <<"The provided phone number could not be found">>}
                  ,{<<"cause">>, PhoneNumber}
                  ]),
            C = cb_context:add_validation_error(<<"phone_number">>, <<"not_found">>, Msg, Context),
            find_account('undefined', AccountRealm, AccountName, C)
    end.

-spec consume_tokens(cb_context:context()) -> cb_context:context().
consume_tokens(Context) ->
    BucketName = cb_modules_util:bucket_name(Context),
    TokenCost = cb_modules_util:token_cost(Context, ?USER_AUTH_TOKENS),
    case kz_buckets:consume_tokens_until(?APP_NAME, BucketName, TokenCost) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('too_many_requests', Context)
    end.
