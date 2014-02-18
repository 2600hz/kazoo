%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% User auth module
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cb_google_auth).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,authorize/1
        ,authenticate/1
        ,validate/1
        ,put/1
        ]).

-include("../crossbar.hrl").

-define(USERNAME_LIST, <<"users/list_by_username">>).
-define(DEFAULT_LANGUAGE, <<"en-US">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.google_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.google_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.google_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.google_auth">>, ?MODULE, 'put').

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
allowed_methods() -> [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"google_auth">>, _}]) -> 'true';
authorize_nouns(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"google_auth">>, _}]) -> 'true';
authenticate_nouns(_) -> 'false'.

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
validate(Context) ->
    cb_context:validate_request_data(<<"google_auth">>, Context, fun maybe_authenticate_user/1).


-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    _ = cb_context:put_reqid(Context),
    create_token(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
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

maybe_authenticate_user(Context) ->
    JObj = cb_context:doc(Context),
    case kazoo_oauth_client:authenticate(JObj) of
        {'ok', OAuth} ->
            lager:debug("verified oauth: ~p",[OAuth]),
            maybe_account_is_disabled(Context#cb_context{doc=OAuth
                                                        ,resp_status='success'});
        {'error', _R} ->
            lager:debug("error verifying token: ~p",[_R]),
            cb_context:add_system_error('invalid_credentials', Context)			
    end.

-spec maybe_account_is_disabled(cb_context:context()) -> cb_context:context().
maybe_account_is_disabled(Context) ->
    JObj = cb_context:doc(Context),		
    case wh_json:get_value([<<"AuthDoc">>,<<"pvt_account_id">>], JObj) of
        'undefined' -> Context;
        Account ->
            case wh_util:is_account_enabled(Account) of
                'true' -> maybe_load_username(Account, Context);
                'false' ->
                    lager:debug("account ~s is disabled", [Account]),
                    Props = [{'details', <<"account_disabled">>}],
                    cb_context:add_system_error('forbidden', Props, Context)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_load_username(ne_binary(), cb_context:context()) -> cb_context:context().
maybe_load_username(Account, Context) ->
    JObj = cb_context:doc(Context),	
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Username = wh_json:get_value([<<"AuthDoc">>,<<"pvt_username">>], JObj),
    lager:debug("attempting to load username in db: ~s", [AccountDb]),
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?USERNAME_LIST, ViewOptions) of
        {'ok', [User]} ->
            case wh_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                'false' ->
                    lager:debug("the username '~s' was found and is not disabled, continue", [Username]),
                    UserDoc = wh_json:get_value(<<"doc">>, User),
                    User2 = wh_json:set_values([{<<"account_id">>, wh_json:get_value(<<"pvt_account_id">>, UserDoc)}
                                                ,{<<"owner_id">>, wh_json:get_value(<<"_id">>, UserDoc)}
                                               ], UserDoc),
                    cb_context:setters(Context, [{fun cb_context:set_account_db/2, Account}
                                                 ,{fun cb_context:set_doc/2, wh_json:set_value(<<"User">>, User2, JObj)} 
                                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                                ]);
                'true' ->
                    lager:debug("the username '~s' was found but is disabled", [Username]),
                    cb_context:add_validation_error(<<"username">>
                                                    ,<<"forbidden">>
                                                    ,<<"The provided username is disabled">>
                                                    ,Context
                                                   )
            end;
        _ ->
            cb_context:add_validation_error(<<"username">>
                                            ,<<"not_found">>
                                            ,<<"The provided username was not found">>
                                            ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token(cb_context:context()) -> cb_context:context().
create_token(Context) ->
    JObj = cb_context:doc(Context),
    case wh_json:get_value(<<"User">>, JObj) of
        'undefined' -> 
            Profile = wh_json:get_value(<<"Profile">>, JObj),
            crossbar_util:response(
              wh_json:from_list([{<<"profile">>, Profile}])
              ,Context
             );
        _Else -> create_auth_token(Context)
    end.


create_auth_token(Context) ->
    JObj = cb_context:doc(Context),
    lager:info("created auth token for user ~s", [wh_json:get_value(<<"User">>, JObj)]),
    case wh_json:is_empty(JObj) of
        'true' -> crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            AccountId = wh_json:get_value([<<"User">>,<<"pvt_account_id">>], JObj, <<>>),
            OwnerId = wh_json:get_value([<<"User">>,<<"_id">>], JObj, <<>>),
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
                    JObj1 = populate_resp(wh_json:get_value(<<"User">>, JObj), AccountId, OwnerId),
                    JObj2 = crossbar_util:response_auth(JObj1),
                    JObj3 = wh_json:set_value(<<"profile">>, wh_json:get_value(<<"Profile">>, JObj), JObj2),
                    crossbar_util:response(JObj3
                                          ,cb_context:setters(Context, [{fun cb_context:set_auth_token/2, AuthToken}
                                                                       ,{fun cb_context:set_auth_doc/2, Doc}
                                                                       ])
                                          );
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error('invalid_credentials', Context)
            end
    end.

-spec populate_resp(wh_json:object(), ne_binary(), ne_binary()) -> wh_json:object().
populate_resp(JObj, AccountId, UserId) ->
    Routines = [fun(J) -> wh_json:set_value(<<"apps">>, load_apps(AccountId, UserId), J) end
                ,fun(J) -> wh_json:set_value(<<"language">>, get_language(AccountId, UserId), J) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec load_apps(ne_binary(), ne_binary()) -> api_object().
load_apps(AccountId, UserId) ->
    MasterAccountDb = get_master_account_db(),
    Lang = get_language(AccountId, UserId),
    case couch_mgr:get_all_results(MasterAccountDb, <<"apps_store/crossbar_listing">>) of
        {'error', _E} ->
            lager:error("failed to load lookup apps in ~p", [MasterAccountDb]),
            'undefined';
        {'ok', JObjs} -> filter_apps(JObjs, Lang)
    end.

-spec filter_apps(wh_json:objects(), ne_binary()) -> wh_json:objects().
filter_apps(JObjs, Lang) -> filter_apps(JObjs, Lang, []).

-spec filter_apps(wh_json:objects(), ne_binary(), wh_json:objects()) -> wh_json:objects().
filter_apps([], _, Acc) -> Acc;
filter_apps([JObj|JObjs], Lang, Acc) ->
    App = wh_json:get_value(<<"value">>, JObj, wh_json:new()),
    NewApp = wh_json:from_list([{<<"id">>, wh_json:get_value(<<"id">>, App)}
                                ,{<<"name">>, wh_json:get_value(<<"name">>, App)}
                                ,{<<"api_url">>, wh_json:get_value(<<"api_url">>, App)}
                                ,{<<"label">>, wh_json:get_value([<<"i18n">>, Lang, <<"label">>], App)}
                               ]),
    filter_apps(JObjs, Lang, [NewApp|Acc]).

-spec get_language(ne_binary(), ne_binary()) -> ne_binary().
get_language(AccountId, UserId) ->
    case crossbar_util:get_user_lang(AccountId, UserId) of
        {'ok', Lang} -> Lang;
        'error' ->
            case crossbar_util:get_account_lang(AccountId) of
                {'ok', Lang} -> Lang;
                'error' -> ?DEFAULT_LANGUAGE
            end
    end.

-spec get_master_account_db() -> ne_binary().
get_master_account_db() ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    wh_util:format_account_id(MasterAccountId, 'encoded').
