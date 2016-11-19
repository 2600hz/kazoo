%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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

-include("crossbar.hrl").

-define(DEFAULT_LANGUAGE, <<"en-US">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.google_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.google_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.google_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.google_auth">>, ?MODULE, 'put'),
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
    case kazoo_oauth_client:authenticate(cb_context:doc(Context)) of
        {'ok', OAuth} ->
            lager:debug("verified oauth: ~p",[OAuth]),
            Context1 = cb_context:set_doc(cb_context:set_resp_status(Context, 'success')
                                         ,OAuth),
            maybe_account_is_disabled(Context1);
        {'error', _R} ->
            lager:debug("error verifying token: ~p",[_R]),
            cb_context:add_system_error('invalid_credentials', Context)
    end.

-spec maybe_account_is_disabled(cb_context:context()) -> cb_context:context().
maybe_account_is_disabled(Context) ->
    JObj = cb_context:doc(Context),
    case kz_json:get_value([<<"AuthDoc">>,<<"pvt_account_id">>], JObj) of
        'undefined' -> Context;
        Account ->
            case kz_util:is_account_enabled(Account) of
                'true' -> maybe_load_username(Account, Context);
                'false' ->
                    lager:debug("account ~s is disabled", [Account]),
                    cb_context:add_system_error(
                      'forbidden'
                                               ,kz_json:from_list([{<<"cause">>, <<"account_disabled">>}])
                                               ,Context
                     )
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
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    Username = kz_json:get_value([<<"AuthDoc">>,<<"pvt_username">>], JObj),
    lager:debug("attempting to load username in db: ~s", [AccountDb]),
    ViewOptions = [{'key', Username}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', [User]} ->
            case kz_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                'false' ->
                    lager:debug("the username '~s' was found and is not disabled, continue", [Username]),
                    UserDoc = kz_json:get_value(<<"doc">>, User),
                    User2 =
                        kz_json:set_values(
                          [{<<"account_id">>, kz_doc:account_id(UserDoc)}
                          ,{<<"owner_id">>, kz_doc:id(UserDoc)}
                          ]
                                          ,UserDoc
                         ),
                    cb_context:setters(
                      Context
                                      ,[{fun cb_context:set_account_db/2, Account}
                                       ,{fun cb_context:set_doc/2, kz_json:set_value(<<"User">>, User2, JObj)}
                                       ,{fun cb_context:set_resp_status/2, 'success'}
                                       ]
                     );
                'true' ->
                    lager:debug("the username '~s' was found but is disabled", [Username]),
                    cb_context:add_validation_error(
                      <<"username">>
                                                   ,<<"forbidden">>
                                                   ,kz_json:from_list([
                                                                       {<<"message">>, <<"The provided user name is disabled">>}
                                                                      ,{<<"cause">>, Username}
                                                                      ])
                                                   ,Context
                     )
            end;
        _ ->
            cb_context:add_validation_error(
              <<"username">>
                                           ,<<"not_found">>
                                           ,kz_json:from_list([
                                                               {<<"message">>, <<"The provided user name was not found">>}
                                                              ,{<<"cause">>, Username}
                                                              ])
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
    case kz_json:get_value(<<"User">>, JObj) of
        'undefined' ->
            Profile = kz_json:get_value(<<"Profile">>, JObj),
            crossbar_util:response(
              kz_json:from_list([{<<"profile">>, Profile}])
                                  ,Context
             );
        _Else -> create_auth_token(Context)
    end.


create_auth_token(Context) ->
    JObj = cb_context:doc(Context),
    lager:info("created auth token for user ~s", [kz_json:get_value(<<"User">>, JObj)]),
    case kz_json:is_empty(JObj) of
        'true' -> crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            AccountId = kz_json:get_value([<<"User">>,<<"pvt_account_id">>], JObj, <<>>),
            OwnerId = kz_json:get_value([<<"User">>,<<"_id">>], JObj, <<>>),
            Token = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, OwnerId}
                    ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                    ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                    ,{<<"method">>, kz_util:to_binary(?MODULE)}
                    ],
            case kz_datamgr:save_doc(?KZ_TOKEN_DB, kz_json:from_list(Token)) of
                {'ok', Doc} ->
                    AuthToken = kz_doc:id(Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    JObj2 = crossbar_util:response_auth(kz_json:get_value(<<"User">>, JObj), AccountId, OwnerId),
                    JObj3 = kz_json:set_value(<<"profile">>, kz_json:get_value(<<"Profile">>, JObj), JObj2),
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
