%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
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
        ]).

-include("../crossbar.hrl").

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(USERNAME_LIST, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.user_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.user_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.user_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.user_auth">>, ?MODULE, 'put').

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
allowed_methods(<<"recovery">>) -> [?HTTP_PUT].

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
resource_exists(<<"recovery">>) -> 'true';
resource_exists(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(#cb_context{req_nouns=[{<<"user_auth">>, _}]}) -> 'true';
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"user_auth">>, _}]}) -> 'true';
authenticate(#cb_context{req_nouns=[{<<"user_auth">>, [<<"recovery">>]}]}) -> 'true';
authenticate(_) -> 'false'.

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
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    cb_context:validate_request_data(<<"user_auth">>, Context, fun maybe_authenticate_user/1).

validate(#cb_context{req_verb = ?HTTP_PUT}=Context, <<"recovery">>) ->
    cb_context:validate_request_data(<<"user_auth_recovery">>, Context, fun maybe_recover_user_password/1).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    _ = cb_context:put_reqid(Context),
    create_token(Context).
put(Context, <<"recovery">>) ->
    _ = cb_context:put_reqid(Context),
    reset_users_password(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
normalize_account_name('undefined') -> 'undefined';
normalize_account_name(AccountName) ->
    << <<Char>> || <<Char>> <= wh_util:to_lower_binary(AccountName)
                   ,(Char >= $a andalso Char =< $z) orelse (Char >= $0 andalso Char =< $9) >>.

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
-spec maybe_authenticate_user(cb_context:context(), ne_binary(), ne_binary(), wh_json:key()) ->
                                     cb_context:context().

maybe_authenticate_user(#cb_context{doc=JObj}=Context) ->
    Credentials = wh_json:get_value(<<"credentials">>, JObj),
    Method = wh_json:get_value(<<"method">>, JObj, <<"md5">>),
    AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = wh_json:get_value(<<"account_realm">>, JObj,
                                     wh_json:get_value(<<"realm">>, JObj)),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', _} ->
            lager:debug("failed to find account DB from realm ~s", [AccountRealm]),
            cb_context:add_system_error('invalid_credentials', Context);
        {'ok', Account} ->
            maybe_authenticate_user(Context, Credentials, Method, Account)
    end.

maybe_authenticate_user(Context, _, _, []) ->
    lager:debug("no account(s) specified"),
    cb_context:add_system_error('invalid_credentials', Context);
maybe_authenticate_user(Context, Credentials, Method, [Account|Accounts]) ->
    case maybe_authenticate_user(Context, Credentials, Method, Account) of
        #cb_context{resp_status='success'}=Context1 -> Context1;
        _ -> maybe_authenticate_user(Context, Credentials, Method, Accounts)
    end;
maybe_authenticate_user(Context, Credentials, <<"md5">>, Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    case crossbar_doc:load_view(?ACCT_MD5_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status='success'
                    ,doc=[JObj|_]
                   } ->
            lager:debug("found more that one user with MD5 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status='success'
                               ,doc=wh_json:get_value(<<"value">>, JObj)
                              };
        #cb_context{resp_status='success'
                    ,doc=JObj
                   } when JObj =/= []->
            lager:debug("found MD5 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status='success'
                               ,doc=wh_json:get_value(<<"value">>, JObj)
                              };
        _C ->
            lager:debug("credentials do not belong to any user: ~s: ~p", [cb_context:resp_status(_C), cb_context:doc(_C)]),
            cb_context:add_system_error('invalid_credentials', Context)
    end;
maybe_authenticate_user(Context, Credentials, <<"sha">>, Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case crossbar_doc:load_view(?ACCT_SHA1_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status='success'
                    ,doc=[JObj|_]
                   } ->
            lager:debug("found more that one user with SHA1 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status='success'
                               ,doc=wh_json:get_value(<<"value">>, JObj)
                              };
        #cb_context{resp_status='success'
                    ,doc=JObj
                   } when JObj =/= []->
            lager:debug("found SHA1 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status='success'
                               ,doc=wh_json:get_value(<<"value">>, JObj)
                              };
        _ ->
            lager:debug("credentials do not belong to any user"),
            cb_context:add_system_error('invalid_credentials', Context)
    end;
maybe_authenticate_user(Context, _, _, _) ->
    lager:debug("invalid creds"),
    cb_context:add_system_error('invalid_credentials', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_recover_user_password(cb_context:context()) -> cb_context:context().
maybe_recover_user_password(#cb_context{doc=JObj}=Context) ->
    AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
    PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
    AccountRealm = wh_json:get_value(<<"account_realm">>, JObj
                                     ,wh_json:get_value(<<"realm">>, JObj)),
    case find_account(PhoneNumber, AccountRealm, AccountName, Context) of
        {'error', C} -> C;
        {'ok', [Account|_]} -> maybe_load_username(Account, Context);
        {'ok', Account} -> maybe_load_username(Account, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_load_username(ne_binary(), cb_context:context()) -> cb_context:context().
maybe_load_username(Account, #cb_context{doc=JObj}=Context) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    lager:debug("attempting to load username in db: ~s", [AccountDb]),
    Username = wh_json:get_value(<<"username">>, JObj),
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?USERNAME_LIST, ViewOptions) of
        {'ok', [User]} ->
            case wh_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                'false' ->
                    lager:debug("the username '~s' was found and is not disabled, continue", [Username]),
                    Context#cb_context{resp_status='success'
                                       ,doc=wh_json:get_value(<<"doc">>, User)
                                       ,db_name=AccountDb
                                      };
                'true' ->
                    lager:debug("the username '~s' was found but is disabled", [Username]),
                    cb_context:add_validation_error(<<"username">>
                                                    ,<<"forbidden">>
                                                    ,<<"The provided username is disabled">>
                                                    ,Context)
            end;                    
        _ ->
            cb_context:add_validation_error(<<"username">>
                                            ,<<"not_found">>
                                            ,<<"The provided username was not found">>
                                            ,Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token(cb_context:context()) -> cb_context:context().
create_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        'true' ->
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            AccountId = wh_json:get_value(<<"account_id">>, JObj, <<>>),
            OwnerId = wh_json:get_value(<<"owner_id">>, JObj, <<>>),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"owner_id">>, OwnerId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {'ok', Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    JObj1 = wh_json:set_value(<<"apps">>, load_installed_apps(AccountId, OwnerId), JObj),
                    crossbar_util:response(crossbar_util:response_auth(JObj1)
                                            ,Context#cb_context{auth_token=AuthToken
                                                                ,auth_doc=Doc
                                                               });
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error('invalid_credentials', Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec load_installed_apps(ne_binary(), ne_binary()) -> ne_binaries().
load_installed_apps(AccountId, UserId) ->
    AccoundDb = wh_util:format_account_id(AccountId, 'encoded'),
    Routines = [fun(_) -> load_apps_ids(AccoundDb) end
                ,fun(Ids) -> load_apps(AccoundDb, UserId, Ids) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, [], Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec load_apps_ids(ne_binary()) -> ne_binaries() | [].
load_apps_ids(AccoundDb) ->
    case couch_mgr:get_all_results(AccoundDb, <<"apps_store/crossbar_listing">>) of
        {'ok', JObjs} ->
            lists:foldl(
                fun(JObj, Acc) ->
                    case wh_json:get_value([<<"value">>, <<"installed">>], JObj) of
                        'false' -> Acc;
                        'true' -> 
                            [wh_json:get_value(<<"key">>, JObj)|Acc]
                    end
                end
                ,[]
                ,JObjs);
        {'error', _E} ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec load_apps(ne_binary(), ne_binary(), ne_binaries()) -> ne_binaries() | [].
-spec load_apps(ne_binary(), ne_binary(), ne_binaries(), ne_binaries()) -> ne_binaries() | [].

load_apps(AccoundDb, UserId, Ids) ->
    load_apps(AccoundDb, UserId, Ids, wh_json:new()).

load_apps(_, _, [], Acc) ->
    Acc;
load_apps(AccoundDb, UserId, [Id|Ids], Acc) ->
    case couch_mgr:open_doc(AccoundDb, Id) of
        {'ok', JObj} ->
            case wh_json:get_value([<<"installed">>, <<"all">>], JObj) of
                'true' ->
                    load_apps(AccoundDb
                              ,UserId
                              ,Ids
                              ,wh_json:set_value(Id, get_app_info(JObj), Acc));
                _ ->
                    case is_app_used_by_user(JObj, UserId) of
                        'true' ->
                            load_apps(AccoundDb
                                      ,UserId
                                      ,Ids
                                      ,wh_json:set_value(Id, get_app_info(JObj), Acc));
                        'false' ->
                            load_apps(AccoundDb, UserId, Ids, Acc)
                    end
            end;
        {'error', _E} ->
            load_apps(AccoundDb, UserId, Ids, Acc)
    end.

is_app_used_by_user(AppJObj, UserId) ->
    lists:foldl(
        fun(User, Acc) ->
            case wh_json:get_value(<<"id">>, User) =:= UserId of
                'true' -> 'true';
                'false' -> Acc
            end
        end
        ,'false'
        ,wh_json:get_value([<<"installed">>, <<"users">>], AppJObj)).

get_app_info(JObj) ->
    wh_json:set_values([{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
                        ,{<<"i18n">>, wh_json:get_value(<<"i18n">>, JObj)}
                       ]
                       ,wh_json:new()).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to generate random strings
%% @end
%%--------------------------------------------------------------------
-spec reset_users_password(cb_context:context()) -> cb_context:context().
reset_users_password(#cb_context{doc=JObj, req_data=Data}=Context) ->
    Password = wh_util:rand_hex_binary(16),
    {MD5, SHA1} = cb_modules_util:pass_hashes(wh_json:get_value(<<"username">>, JObj), Password),
    Email = wh_json:get_value(<<"email">>, JObj),

    JObj1 = wh_json:set_values([{<<"pvt_md5_auth">>, MD5}
                                ,{<<"pvt_sha1_auth">>, SHA1}
                                ,{<<"require_password_update">>, 'true'}
                               ], JObj),

    case crossbar_doc:save(Context#cb_context{doc=JObj1, req_verb = ?HTTP_POST}) of
        #cb_context{resp_status='success'} -> 
            Notify = [{<<"Email">>, Email}
                      ,{<<"First-Name">>, wh_json:get_value(<<"first_name">>, JObj)}
                      ,{<<"Last-Name">>, wh_json:get_value(<<"last_name">>, JObj)}
                      ,{<<"Password">>, Password}
                      ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
                      ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
                      ,{<<"Request">>, wh_json:delete_key(<<"username">>, Data)}
                      | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                     ],
            'ok' = wapi_notifications:publish_pwd_recovery(Notify),
            crossbar_util:response(<<"Password reset, email send to:", Email/binary>>, Context);
        Else ->
            Else
    end.

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
        {'ok', 'undefined'} ->
            lager:debug("failed to find account ~s by name", [AccountName]),
            C = cb_context:add_validation_error(<<"account_name">>
                                                ,<<"not_found">>
                                                ,<<"The provided account name could not be found">>
                                                ,Context),
            find_account('undefined', 'undefined', 'undefined', C);
        {'ok', AccountDb} ->
            lager:debug("found account by name '~s': ~s", [AccountName, AccountDb]),
            {'ok', AccountDb};
        {'multiples', AccountDbs} ->
            lager:debug("the account name returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            C = cb_context:add_validation_error(<<"account_name">>
                                                ,<<"not_found">>
                                                ,<<"The provided account name could not be found">>
                                                ,Context),
            find_account('undefined', 'undefined', 'undefined', C)
    end;
find_account('undefined', AccountRealm, AccountName, Context) ->
    case whapps_util:get_account_by_realm(AccountRealm) of
        {'ok', 'undefined'} ->
            lager:debug("failed to find account ~s by name", [AccountName]),
            C = cb_context:add_validation_error(<<"account_name">>
                                                ,<<"not_found">>
                                                ,<<"The provided account name could not be found">>
                                                ,Context),
            find_account('undefined', 'undefined', 'undefined', C);
        {'ok', AccountDb} ->
            lager:debug("found account by realm '~s': ~s", [AccountRealm, AccountDb]),
            {'ok', AccountDb};
        {'multiples', AccountDbs} ->
            lager:debug("the account realm returned multiple results"),
            {'ok', AccountDbs};
        {'error', _} ->
            C = cb_context:add_validation_error(<<"account_realm">>
                                                ,<<"not_found">>
                                                ,<<"The provided account realm could not be found">>
                                                ,Context),
            find_account('undefined', 'undefined', AccountName, C)
    end;
find_account(PhoneNumber, AccountRealm, AccountName, Context) ->
    case wh_number_manager:lookup_account_by_number(PhoneNumber) of
        {'ok', AccountId, _} ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            lager:debug("found account by phone number '~s': ~s", [PhoneNumber, AccountDb]),
            {'ok', AccountDb};
        {'error', _} -> 
            C = cb_context:add_validation_error(<<"phone_number">>
                                                ,<<"not_found">>
                                                ,<<"The provided phone number could not be found">>
                                                ,Context),
            find_account('undefined', AccountRealm, AccountName, C)
    end.
