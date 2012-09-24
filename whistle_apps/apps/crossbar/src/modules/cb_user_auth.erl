%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP, INC
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

-include("include/crossbar.hrl").

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(USERNAME_LIST, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.user_auth">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.user_auth">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.user_auth">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.user_auth">>, ?MODULE, put).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['PUT'].
allowed_methods(<<"recovery">>) ->
    ['PUT'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_tokens()) -> boolean().
resource_exists() -> true.
resource_exists(<<"recovery">>) -> true;
resource_exists(_) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize/1 :: (#cb_context{}) -> boolean().
authorize(#cb_context{req_nouns=[{<<"user_auth">>, _}]}) ->
    true;
authorize(#cb_context{}) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"user_auth">>, _}]}) ->
    true;
authenticate(#cb_context{req_nouns=[{<<"user_auth">>, [<<"recovery">>]}]}) ->
    true;
authenticate(#cb_context{}) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_data=Data, req_verb = <<"put">>}=Context) ->
    _ = cb_context:put_reqid(Context),
    lager:debug("validating user_auth"),
    case catch(wh_json_validator:is_valid(Data, <<"user_auth">>)) of
        {fail, Errors} ->
            lager:debug("fail json validation"),
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            lager:debug("pass json validation"),
            Credentials = wh_json:get_value(<<"credentials">>, JObj),
            Method = wh_json:get_value(<<"method">>, JObj, <<"md5">>),
            AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
            PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
            AccountRealm = wh_json:get_value(<<"account_realm">>, JObj,
                                             wh_json:get_value(<<"realm">>, JObj)),
            case crossbar_util:find_account_db(PhoneNumber, AccountRealm, AccountName) of
                {error, Errors} ->
                    lager:debug("failed to find account DB"),
                    crossbar_util:response_invalid_data(Errors, Context);
                {ok, AccountDb} ->
                    lager:debug("found account DB"),
                    authorize_user(Context, Credentials, Method, AccountDb);
                {multiples, AccountDbs} ->
                    lager:debug("found multiple account DBs"),
                    authorize_user(Context, Credentials, Method, AccountDbs)
            end
    end.

validate(#cb_context{req_data=Data, req_verb = <<"put">>}=Context, <<"recovery">>) ->
    _ = cb_context:put_reqid(Context),
    case wh_json_validator:is_valid(Data, <<"user_auth_recovery">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
            PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
            AccountRealm = wh_json:get_value(<<"account_realm">>, JObj
                                             ,wh_json:get_value(<<"realm">>, JObj)),
            case crossbar_util:find_account_db(PhoneNumber, AccountRealm, AccountName, false) of
                {error, Errors} -> crossbar_util:response_invalid_data(Errors, Context);
                {ok, AccountDb} ->
                    lager:debug("attempting to load username in db: ~s", [AccountDb]),
                    Username = wh_json:get_value(<<"username">>, JObj),
                    case couch_mgr:get_results(AccountDb, ?USERNAME_LIST, [{key, Username}
                                                                           ,include_docs
                                                                          ]) of
                        {ok, [User]} ->
                            case wh_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                                false ->
                                    lager:debug("the username '~s' was found and is not disabled, continue", [Username]),
                                    Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"doc">>, User), db_name=AccountDb};
                                true ->
                                    lager:debug("the username '~s' was found but is disabled", [Username]),
                                    Error = wh_json:set_value([<<"username">>, <<"disabled">>]
                                                              ,<<"The user is disabled">>
                                                              ,wh_json:new()),
                                    crossbar_util:response_invalid_data(Error, Context)
                            end;                    
                        _ ->
                            Error = wh_json:set_value([<<"username">>, <<"not_found">>]
                                                      ,<<"The provided user name was not found">>
                                                          ,wh_json:new()),
                            crossbar_util:response_invalid_data(Error, Context)
                    end
            end
    end;
validate(Context, _Path) ->
    _ = cb_context:put_reqid(Context),
    lager:debug("bad path: ~p", [_Path]),
    lager:debug("req verb: ~s", [Context#cb_context.req_verb]),
    crossbar_util:response_faulty_request(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
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
-spec normalize_account_name/1 :: ('undefined' | ne_binary()) -> ne_binary().
normalize_account_name(undefined) ->
    undefined;
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
-spec authorize_user/4 :: (#cb_context{}, ne_binary(), ne_binary(), [ne_binary(),...] | [] | ne_binary()) -> #cb_context{}.
authorize_user(Context, _, _, []) ->
    lager:debug("no account(s) specified"),
    crossbar_util:response(error, <<"invalid credentials">>, 401, Context);
authorize_user(Context, Credentials, Method, [AccountDb|AccountDbs]) ->
    case authorize_user(Context, Credentials, Method, AccountDb) of
        #cb_context{resp_status=success}=Context1 ->
            lager:debug("authz user creds: ~s", [AccountDb]),
            Context1;
        _ ->
            authorize_user(Context, Credentials, Method, AccountDbs)
    end;
authorize_user(Context, Credentials, <<"md5">>, AccountDb) ->
    case crossbar_doc:load_view(?ACCT_MD5_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            lager:debug("found more that one user with MD5 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} when JObj =/= []->
            lager:debug("found MD5 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        _ ->
            lager:debug("credentials do not belong to any user"),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end;
authorize_user(Context, Credentials, <<"sha">>, AccountDb) ->
    case crossbar_doc:load_view(?ACCT_SHA1_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            lager:debug("found more that one user with SHA1 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} when JObj =/= []->
            lager:debug("found SHA1 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        _ ->
            lager:debug("credentials do not belong to any user"),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end;
authorize_user(Context, _, _, _) ->
    lager:debug("invalid creds"),
    crossbar_util:response(error, <<"invalid credentials">>, 401, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/1 :: (#cb_context{}) -> #cb_context{}.
create_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        true ->
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context);
        false ->
            AccountId = wh_json:get_value(<<"account_id">>, JObj, <<>>),
            OwnerId = wh_json:get_value(<<"owner_id">>, JObj, <<>>),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"owner_id">>, OwnerId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {ok, Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(wh_json:from_list([{<<"account_id">>, AccountId}
                                                              ,{<<"owner_id">>, OwnerId}])
                                           ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
                {error, R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to generate random strings
%% @end
%%--------------------------------------------------------------------
-spec reset_users_password/1 :: (#cb_context{}) -> #cb_context{}.
reset_users_password(#cb_context{doc=JObj, req_data=Data}=Context) ->
    Password = wh_util:rand_hex_binary(16),
    {MD5, SHA1} = cb_modules_util:pass_hashes(wh_json:get_value(<<"username">>, JObj), Password),
    Email = wh_json:get_value(<<"email">>, JObj),

    JObj1 = wh_json:set_values([{<<"pvt_md5_auth">>, MD5}
                                ,{<<"pvt_sha1_auth">>, SHA1}
                                ,{<<"require_password_update">>, true}
                               ], JObj),

    case crossbar_doc:save(Context#cb_context{doc=JObj1, req_verb = <<"post">>}) of
        #cb_context{resp_status=success} -> 
            Notify = [{<<"Email">>, Email}
                      ,{<<"First-Name">>, wh_json:get_value(<<"first_name">>, JObj)}
                      ,{<<"Last-Name">>, wh_json:get_value(<<"last_name">>, JObj)}
                      ,{<<"Password">>, Password}
                      ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
                      ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
                      ,{<<"Request">>, wh_json:delete_key(<<"username">>, Data)}
                      | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                     ],
            ok = wapi_notifications:publish_pwd_recovery(Notify),
            crossbar_util:response(<<"Password reset, email send to:", Email/binary>>, Context);
        Else ->
            Else
    end.
