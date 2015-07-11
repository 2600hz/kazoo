%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Shared token auth module, this module validates the token
%%% against a trusted central token server.  If the token
%%% is valid then it will create a local token.  It will
%%% also import the account/user from the central server.
%%%
%%% Used to propagate accounts amongst independent services.
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it 'proxies' crossbar auth requests to an external URL
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_shared_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),
    Url = whapps_config:get_string(<<"crossbar.shared_auth">>, <<"authoritative_crossbar">>),
    lager:debug("shared auth started up, using ~s as authoritative crossbar", [Url]),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.shared_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.shared_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.shared_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.shared_auth">>, ?MODULE, 'put').

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
allowed_methods() ->
    [?HTTP_PUT, ?HTTP_GET].

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
-spec authorize(cb_context:context(), req_nouns()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

authorize(_, [{<<"shared_auth">>, _}]) ->'true';
authorize(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(http_method(), req_nouns()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate(?HTTP_PUT, [{<<"shared_auth">>, []}]) -> 'true';
authenticate(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function runs for each side of the shared auth request
%%
%% The Requestor (PUT):
%%     IE: Create (PUT) a local auth token
%% This request bypasses authentication, test the 'shared_token' against our
%% authorative server.  Basicly preform a noraml 'get' to this module with the
%% shared token as the auth token.  If it succeeds we will send 'ourself' the
%% account id, otherwise the token was not known to the auth server.
%%
%% The Authority (GET):
%%     IE: Fetch (GET) the account and user of a shared token
%% If we are validating a 'get' request then we are the authoriative box.  This means
%% another box is using their 'shared_tokens' as our auth token and it validated.
%% So lets figure out what account they belong to and return the complete account
%% definition so they can import it.
%%
%% If the authoriate box is running with noauth[n|z] then just send back a 401 to the
%% requestor because we wont know what account to fetch
%%
%% Failure here returns 400 or 401
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate_request(cb_context:context(), http_method(), api_object()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context), cb_context:auth_doc(Context)).

validate_request(Context, ?HTTP_PUT, _) ->
    _ = cb_context:put_reqid(Context),
    XBarUrl = whapps_config:get_string(<<"crossbar.shared_auth">>, <<"authoritative_crossbar">>),
    SharedToken = wh_json:get_value(<<"shared_token">>, cb_context:req_data(Context)),
    case authenticate_shared_token(SharedToken, XBarUrl) of
        {'ok', Payload} ->
            lager:debug("authoritive shared auth request succeeded"),
            RemoteData = wh_json:get_value(<<"data">>, wh_json:decode(Payload)),
            case import_missing_data(RemoteData) of
                'true' ->
                    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                                 ,{fun cb_context:set_doc/2, RemoteData}
                                                 ,{fun cb_context:set_auth_token/2, SharedToken}
                                                ]);
                'false' ->
                    cb_context:add_system_error('datastore_fault', Context)
            end;
        {'forbidden', _} ->
            lager:debug("authoritive shared auth request forbidden"),
            cb_context:add_system_error('invalid_credentials', Context);
        {'error', _}=E ->
            lager:debug("authoritive shared auth request error: ~p", [E]),
            cb_context:add_system_error('datastore_unreachable', Context)
    end;

validate_request(Context, ?HTTP_GET, 'undefined') ->
    _ = cb_context:put_reqid(Context),
    lager:debug("valid shared auth request received but there is no authorizing doc (noauth running?)"),
    cb_context:add_system_error('invalid_credentials', Context);

validate_request(Context, ?HTTP_GET, JObj) ->
    _ = cb_context:put_reqid(Context),
    lager:debug("valid shared auth request received, creating response"),
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    UserId = wh_json:get_value(<<"owner_id">>, JObj),
    case kz_account:fetch(AccountId) of
        {'ok', Account} ->
            Db = wh_util:format_account_id(AccountId, 'encoded'),
            case couch_mgr:open_doc(Db, UserId) of
                {'ok', User} ->
                    RespData = wh_json:from_list([{<<"account">>, Account}
                                                  ,{<<"user">>, User}
                                                 ]),
                    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                                ,{fun cb_context:set_resp_data/2, RespData}
                                                ]);
                {'error', R} ->
                    lager:debug("failed to get user for response ~p", [R]),
                    cb_context:add_system_error('datastore_fault', Context)
            end;
        {'error', R} ->
            lager:debug("failed to get account for response ~p", [R]),
            cb_context:add_system_error('datastore_fault', Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
put(Context) ->
    _ = cb_context:put_reqid(Context),
    create_local_token(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_local_token(cb_context:context()) -> cb_context:context().
create_local_token(Context) ->
    JObj = cb_context:doc(Context),
    Token = wh_json:from_list([{<<"account_id">>, wh_json:get_value([<<"account">>, <<"_id">>], JObj, <<>>)}
                               ,{<<"owner_id">>, wh_json:get_value([<<"user">>, <<"_id">>], JObj, <<>>)}
                               ,{<<"created">>, wh_util:current_tstamp()}
                               ,{<<"modified">>, wh_util:current_tstamp()}
                               ,{<<"method">>, wh_util:to_binary(?MODULE)}
                               ,{<<"shared_token">>, cb_context:auth_token(Context)}
                              ]),
    case couch_mgr:save_doc(?KZ_TOKEN_DB, Token) of
        {'ok', Doc} ->
            AuthToken = wh_doc:id(Doc),
            lager:debug("created new local auth token ~s", [AuthToken]),
            Context1 = cb_context:set_doc(cb_context:set_auth_token(Context, AuthToken)
                                          ,Doc),
            crossbar_util:response(crossbar_util:response_auth(JObj), Context1);
        {'error', R} ->
            lager:debug("could not create new local auth token, ~p", [R]),
            cb_context:add_system_error('invalid_credentials', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a crossbar request to the authoriative server to authorize
%% the shared token and get the account/user for the token
%% @end
%%--------------------------------------------------------------------
-spec authenticate_shared_token(api_binary(), nonempty_string()) ->
                                       {'ok', string() | binary()} |
                                       {'error', atom()} |
                                       {'forbidden', atom()}.
authenticate_shared_token('undefined', _) ->
    {'forbidden', 'missing_shared_token'};
authenticate_shared_token(SharedToken, XBarUrl) ->
    Url = lists:flatten(XBarUrl, "/shared_auth"),
    Headers = [{"Accept", "application/json"}
               ,{"X-Auth-Token", wh_util:to_list(SharedToken)}
              ],
    lager:debug("validating shared token ~s via ~s", [SharedToken, Url]),
    case ibrowse:send_req(Url, Headers, 'get') of
        {'ok', "200", _, Resp} -> {'ok', Resp};
        {'ok', "401", _, _} -> {'forbidden', 'shared_token_rejected'};
        Resp -> {'error', Resp}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure those exist locally.
%% @end
%%--------------------------------------------------------------------
-spec import_missing_data(wh_json:object()) -> boolean().
import_missing_data(RemoteData) ->
    Account = wh_json:get_value(<<"account">>, RemoteData),
    AccountId = wh_doc:account_id(Account),
    User = wh_json:get_value(<<"user">>, RemoteData),
    UserId = wh_doc:id(User),
    import_missing_account(AccountId, Account) andalso
        import_missing_user(AccountId, UserId, User).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the account exists (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec import_missing_account(api_binary(), api_object()) -> boolean().
import_missing_account('undefined', _Account) ->
    lager:debug("shared auth reply did not define an account id"),
    'false';
import_missing_account(_AccountId, 'undefined') ->
    lager:debug("shared auth reply did not define an account definition"),
    'false';
import_missing_account(AccountId, Account) ->
    %% check if the acount datbase exists
    Db = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:db_exists(Db) of
        %% if the account database exists make sure it has the account
        %% definition, because when couch is acting up it can skip this
        'true' ->
            lager:debug("remote account db ~s alread exists locally", [AccountId]),
            %% make sure the account definition is in the account, if not
            %% use the one we got from shared auth
            case kz_account:fetch(AccountId) of
                {'error', 'not_found'} ->
                    lager:debug("missing local account definition, creating from shared auth response"),
                    Doc = wh_doc:delete_revision(Account),
                    Event = <<"*.execute.post.accounts">>,
                    Context1 = crossbar_bindings:fold(Event
                                                     ,[cb_context:set_doc(
                                                         cb_context:set_account_db(cb_context:new(), Db)
                                                                          ,Doc)
                                                       ,AccountId
                                                      ]),
                    case cb_context:resp_status(Context1) of
                        'success' ->
                            lager:debug("udpated account definition"),
                            'true';
                        _ ->
                            lager:debug("could not update account definition"),
                            'false'
                    end;
                {'ok', _} ->
                    lager:debug("account definition exists locally"),
                    'true'
            end;
        'false' ->
            lager:debug("remote account db ~s does not exist locally, creating", [AccountId]),
            Doc = wh_doc:delete_revision(Account),
            Event = <<"*.execute.put.accounts">>,
            Context1 = crossbar_bindings:fold(Event, [cb_context:set_doc(cb_context:new(), Doc)]),
            case cb_context:resp_status(Context1) of
                'success' ->
                    lager:debug("imported account"),
                    'true';
                _ ->
                    lager:debug("could not import account"),
                    'false'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the user exists locally (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec import_missing_user(api_binary(), api_binary(), api_object()) -> boolean().
import_missing_user(_, 'undefined', _) ->
    lager:debug("shared auth reply did not define an user id"),
    'false';
import_missing_user(_, _, 'undefined') ->
    lager:debug("shared auth reply did not define an user object"),
    'false';
import_missing_user(AccountId, UserId, User) ->
    Db = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:lookup_doc_rev(Db, UserId) of
        {'ok', _} ->
            lager:debug("remote user ~s already exists locally in account ~s", [UserId, AccountId]),
            'true';
        _Else ->
            Doc = wh_doc:delete_revision(User),
            Event = <<"*.execute.put.users">>,
            Context1 = crossbar_bindings:fold(Event, [cb_context:set_doc(
                                                        cb_context:set_account_db(cb_context:new(), Db)
                                                                         ,Doc)]),
            case cb_context:resp_status(Context1) of
                'success' ->
                    lager:debug("imported user ~s in account ~s", [UserId, AccountId]),
                    'true';
                _ ->
                    lager:debug("could not import user ~s in account ~s", [UserId, AccountId]),
                    'false'
            end
    end.
